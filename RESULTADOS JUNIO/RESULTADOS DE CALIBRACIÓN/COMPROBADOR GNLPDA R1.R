rm(list=ls())

# ==============================================================================
# FRAMEWORK GNLPDA CENTRALIZADO Y PARALELIZADO CON EL ENFOQUE FUTURE (R-JULIA)
# ==============================================================================

# 1. Librerías básicas y del ecosistema future
library(MASS)
library(caret)
library(future)
library(future.apply)

# 1.1 Cargar las funciones de soporte para limpiar las variables
source("POLEXP R1.R")

# CONFIGURACIÓN DE RUTAS DE LA TESIS
ruta_datasets    <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS/"
ruta_particiones <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS\\SEMILLAS GENERADAS/"
ruta_resultados  <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\RESULTADOS_GNLPDA/"
ruta_presultados <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\RESULTADOS DETALLE GNLPDA 33 CORRIDAS/"

if(!dir.exists(ruta_resultados)) dir.create(ruta_resultados, recursive = TRUE)

# 2. Configurar el Plan de Paralelización (Estrategia Multisession)
num_cores <- max(1, availableCores() - 1)
plan(multisession, workers = num_cores)

cat("Ecosistema 'future' inicializado exitosamente con:", num_cores, "hilos de procesamiento.\n")

# ── BUCLE PRINCIPAL: Procesar Instancia por Instancia (Secuencial) ────────────
archivos_csv <- list.files(path = ruta_datasets, pattern = "\\.csv$", full.names = TRUE)
archivos_csv <- archivos_csv[!grepl("_partitions", archivos_csv)]

for (archivo in archivos_csv) {
  
  nombre_base  <- tools::file_path_sans_ext(basename(archivo))
  archivo_part <- file.path(ruta_particiones, paste0(nombre_base, "_partitions.csv"))
  archivo_detalle_gnlpda <- file.path(ruta_presultados, paste0("Resultados_Detalle_", nombre_base, ".csv"))
  
  if (!file.exists(archivo_part) || !file.exists(archivo_detalle_gnlpda)) {
    cat("Saltando:", nombre_base, "(Falta archivo de particiones o detalle de Julia)\n")
    next
  }
  
  cat("\n==================================================================\n")
  cat("Iniciando Reconstrucción Paralela GNLPDA para:", nombre_base, "\n")
  cat("==================================================================\n")
  
  # Cargar bases originales y bitácora histórica de Julia
  df_original    <- read.csv(archivo)
  df_particiones <- read.csv(archivo_part)
  res_gnlpda     <- read.csv(archivo_detalle_gnlpda) 
  
  ncol_datos <- ncol(df_original)
  colnames(df_original)[ncol_datos] <- "Clase"
  df_original$Clase <- as.factor(df_original$Clase)
  
  # Generar la expansión polinomial completa de Grado 2 antes de entrar al lazo paralelo
  cat(" Ejecutando expansión polinomial base en Julia...\n")
  df_expandido_completo <- polexpj(df_original[, -ncol_datos], grado = 2)
  df_expandido_completo$Clase <- df_original$Clase
  
  # DEFINICIÓN DEL FILTRO DE SELECCIÓN DE CORRIDAS (Mapeo estricto a 30 bloques)
  # Omitimos la corrida 11 (ruidosa), la 32 y la 33 para cerrar en 30 exactas
  corridas_seleccionadas <- 1:33
  
  # ----------------------------------------------------------------------------
  # LAZO PARALELO: Ejecución concurrente de las 30 corridas seleccionadas
  # ----------------------------------------------------------------------------
  resultados_lista <- future_lapply(seq_along(corridas_seleccionadas), function(i) {
    
    run <- corridas_seleccionadas[i] # Corrida real en la bitácora de Julia/Particiones
    
    # Inicializar las variables locales
    accuracy   <- 0
    kappa      <- 0
    macro_f1   <- 0
    macro_rec  <- 0
    macro_prec <- 0
    
    # -> ARRANQUE DEL CRONÓMETRO DE CPU INDIVIDUAL PARA EL TRABAJADOR
    tiempo_inicio <- proc.time()
    
    columna_run <- paste0("Run_", run)
    
    # Máscaras booleanas indexadas por la partición estricta
    es_train <- df_particiones[[columna_run]] == "Train"
    es_test  <- df_particiones[[columna_run]] == "Test"
    
    # Filtrar el subconjunto de variables óptimas guardadas para esta corrida específica
    df_gnlpda_optimo <- obtener_dataset_gnlpda(df_expandido_completo, res_gnlpda, idx_fila = run)
    
    train_gnlpda <- df_gnlpda_optimo[es_train, , drop = FALSE]
    test_gnlpda  <- df_gnlpda_optimo[es_test, , drop = FALSE]
    
    tryCatch({
      # 1. Ajustar el modelo definitivo y predecir
      modelo_final         <- MASS::lda(Clase ~ ., data = train_gnlpda)
      predicciones_finales <- predict(modelo_final, newdata = test_gnlpda)$class
      cm                   <- caret::confusionMatrix(predicciones_finales, test_gnlpda$Clase)
      
      # 2. Extraer métricas globales
      accuracy <- cm$overall["Accuracy"]
      kappa    <- cm$overall["Kappa"]
      
      # 3. Separación implícita en función de si son 2 o más clases
      if (length(unique(train_gnlpda$Clase)) <= 2) {
        macro_f1   <- cm$byClass["F1"]
        macro_rec  <- cm$byClass["Sensitivity"]
        macro_prec <- cm$byClass["Precision"]
      } else {
        macro_f1   <- mean(cm$byClass[, "F1"], na.rm = TRUE)
        macro_rec  <- mean(cm$byClass[, "Sensitivity"], na.rm = TRUE)
        macro_prec <- mean(cm$byClass[, "Precision"], na.rm = TRUE)
      }
      
    }, error = function(e) {
      # Al estar inicializadas arriba, la asignación estándar reescribe de forma segura
      accuracy   <- 0
      kappa      <- 0
      macro_f1   <- 0
      macro_rec  <- 0
      macro_prec <- 0
    })
    
    # -> DETENCIÓN DEL CRONÓMETRO Y CÓMPUTO DEL TIEMPO TRANSCURRIDO
    tiempo_total    <- proc.time() - tiempo_inicio
    tiempo_segundos <- as.numeric(tiempo_total["elapsed"])
    
    # EXTRACCIÓN DE METADATOS STRUCTURALES DEL AG EN JULIA
    vars_e1    <- res_gnlpda$Vars_E1[run]
    nombres_e1 <- res_gnlpda$Nombres_E1[run]
    vars_e2    <- res_gnlpda$Vars_E2[run]
    nombres_e2 <- res_gnlpda$Nombres_E2[run]
    time_julia <- res_gnlpda$Tiempo[run]
    ahorro_ag  <- res_gnlpda$Ahorro[run]
    
    # Retornar el registro consolidado e integrado
    data.frame(
      Bloque_Friedman   = i,
      Corrida_Original  = run,
      Accuracy          = accuracy,
      Kappa             = kappa,
      Macro_F1          = macro_f1,
      Macro_Rec         = macro_rec,
      Macro_Prec        = macro_prec,
      Tiempo_R_Segundos = tiempo_segundos,
      Vars_E1           = vars_e1,
      Nombres_E1        = nombres_e1,
      Vars_E2           = vars_e2,
      Nombres_E2        = nombres_e2,
      Tiempo_Julia_AG   = time_julia,
      Ahorro_Espacio    = ahorro_ag,
      stringsAsFactors  = FALSE
    )
    
  }, future.seed = TRUE)
  
  # 3. Consolidar la lista devuelta por future_lapply en un solo DataFrame
  df_res_instancia <- do.call(rbind, resultados_lista)
  write.csv(df_res_instancia, 
            file = file.path(ruta_resultados, paste0(nombre_base, "_res_gnlpda.csv")), 
            row.names = FALSE)
  
  cat("Resultados consolidados guardados con éxito para:", nombre_base, "\n")
}

# 4. Cerrar el plan paralelo volviendo al flujo secuencial
plan(sequential)
cat("\n==================================================================\n")
cat("Proceso completado. Clúster de 'future' liberado para GNLPDA.\n")
cat("==================================================================\n")

