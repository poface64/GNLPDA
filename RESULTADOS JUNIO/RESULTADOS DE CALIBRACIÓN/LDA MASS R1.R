rm(list=ls())

# ==============================================================================
# FRAMEWORK LDA DE MASS PARALELIZADO CON EL ENFOQUE FUTURE (R)
# ==============================================================================

# 1. Librerías básicas y del ecosistema future
library(MASS)
library(caret)
library(future)
library(future.apply)

# CONFIGURACIÓN DE RUTAS
ruta_datasets    <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS/"
ruta_particiones <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS\\SEMILLAS GENERADAS/"
ruta_resultados  <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\RESULTADOS_LDA/"

if(!dir.exists(ruta_resultados)) dir.create(ruta_resultados, recursive = TRUE)

# 2. Configurar el Plan de Paralelización (Estrategia Multisession)
num_cores <- max(1, availableCores() - 1)
plan(multisession, workers = num_cores)

cat("Ecosistema 'future' inicializado exitosamente con:", num_cores, "hilos de procesamiento.\n")

# Obtener los nombres de los datasets originales
archivos_csv <- list.files(path = ruta_datasets, pattern = "\\.csv$", full.names = TRUE)
archivos_csv <- archivos_csv[!grepl("_partitions", archivos_csv)][-c(2,9,10)]

# BUCLE PRINCIPAL: Procesar Instancia por Instancia (Secuencial)
for (archivo in archivos_csv) {
  
  nombre_base  <- tools::file_path_sans_ext(basename(archivo))
  archivo_part <- file.path(ruta_particiones, paste0(nombre_base, "_partitions.csv"))
  
  if (!file.exists(archivo_part)) {
    cat("Saltando:", nombre_base, "(No se encontró su archivo de particiones)\n")
    next
  }
  
  cat("\n==================================================================\n")
  cat("Iniciando Evaluación LDA (MASS) para:", nombre_base, "\n")
  cat("==================================================================\n")
  
  # Cargar datos originales y su matriz de índices compartida
  df_datos       <- na.omit(read.csv(archivo))
  df_particiones <- read.csv(archivo_part)
  
  ncol_datos <- ncol(df_datos)
  X_completo <- as.matrix(df_datos[, -ncol_datos])
  y_completo <- as.factor(df_datos[, ncol_datos])
  
  n_corridas <- 33
  
  # ----------------------------------------------------------------------------
  # LAZO PARALELO: Ejecución concurrente de las 33 corridas estadísticas
  # ----------------------------------------------------------------------------
  resultados_lista <- future_lapply(1:n_corridas, function(run) {
    
    # -> ARRANQUE DEL CRONÓMETRO DE CPU INDIVIDUAL PARA EL TRABAJADOR
    tiempo_inicio <- proc.time()
    
    columna_run <- paste0("Run_", run)
    
    # Máscaras booleanas indexadas por la partición estricta
    es_train <- df_particiones[[columna_run]] == "Train"
    es_test  <- df_particiones[[columna_run]] == "Test"
    
    X_train_run <- X_completo[es_train, , drop = FALSE]
    y_train_run <- y_completo[es_train]
    X_test_run  <- X_completo[es_test, , drop = FALSE]
    y_test_run  <- y_completo[es_test]
    
    # EVALUACIÓN DIRECTA (Método Analítico sin Grid Search interno)
    tryCatch({
      modelo_final         <- MASS::lda(x = X_train_run, grouping = y_train_run)
      predicciones_finales <- predict(modelo_final, X_test_run)$class
      cm                   <- caret::confusionMatrix(predicciones_finales, y_test_run)
      
      # BLINDAJE UNIVERSAL DE MÉTRICAS (Asegura compatibilidad estructural)
      metricas_clase <- as.matrix(t(cm$byClass))
      if (!(nrow(metricas_clase) == 1 && ncol(metricas_clase) != 11)) {
        metricas_clase <- cm$byClass
      }
      
      macro_f1   <- mean(metricas_clase[, "F1"], na.rm = TRUE)
      macro_rec  <- mean(metricas_clase[, "Sensitivity"], na.rm = TRUE)
      macro_prec <- mean(metricas_clase[, "Precision"], na.rm = TRUE)
      accuracy   <- cm$overall["Accuracy"]
      kappa      <- cm$overall["Kappa"]
    }, error = function(e) {
      accuracy   <<- 0; kappa <<- 0; macro_f1 <<- 0; macro_rec <<- 0; macro_prec <<- 0
    })
    
    # -> DETENCIÓN DEL CRONÓMETRO Y CÓMPUTO DEL TIEMPO TRANSCURRIDO
    tiempo_total    <- proc.time() - tiempo_inicio
    tiempo_segundos <- as.numeric(tiempo_total["elapsed"])
    
    # Retornar el registro consolidado de la corrida
    data.frame(
      Corrida         = run,
      Accuracy        = accuracy,
      Kappa           = kappa,
      Macro_F1        = macro_f1,
      Macro_Rec       = macro_rec,
      Macro_Prec      = macro_prec,
      Tiempo_Segundos = tiempo_segundos,
      stringsAsFactors = FALSE
    )
    
  }, future.seed = TRUE)
  
  # 3. Consolidar la lista devuelta por future_lapply en un solo DataFrame
  df_res_instancia <- do.call(rbind, resultados_lista)
  write.csv(df_res_instancia, 
            file = file.path(ruta_resultados, paste0(nombre_base, "_res_lda.csv")), 
            row.names = FALSE)
  
  cat("Resultados consolidados guardados con éxito para:", nombre_base, "\n")
}

# 4. Cerrar el plan paralelo volviendo al flujo secuencial
plan(sequential)
cat("\n==================================================================\n")
cat("Proceso completado. Clúster de 'future' liberado para LDA.\n")
cat("==================================================================\n")