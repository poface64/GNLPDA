# ==============================================================================
# FRAMEWORK KFDA MANUAL PARALELIZADO CON EL ENFOQUE FUTURE (R)
# ==============================================================================

# 1. Librerías básicas y del ecosistema future
library(kernlab)
library(caret)
library(future)
library(future.apply)

# Cargar funciones base de KFDA
source("C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS_KFDA\\KFDA MANUAL.R")

# CONFIGURACIÓN DE RUTAS
ruta_datasets    <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS/"
ruta_particiones <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS\\SEMILLAS GENERADAS/"
ruta_resultados  <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS_KFDA/"

if(!dir.exists(ruta_resultados)) dir.create(ruta_resultados, recursive = TRUE)

# 2. Configurar el Plan de Paralelización (Estrategia Multisession)
# Se detectan los núcleos y se reserva uno para el sistema operativo
num_cores <- max(1, availableCores() - 1)
plan(multisession, workers = num_cores)

cat("Ecosistema 'future' inicializado exitosamente con:", num_cores, "hilos de procesamiento.\n")

# Obtener los nombres de los datasets originales
archivos_csv <- list.files(path = ruta_datasets, pattern = "\\.csv$", full.names = TRUE)
archivos_csv <- archivos_csv[!grepl("_partitions", archivos_csv)]

# BUCLE PRINCIPAL: Procesar Instancia por Instancia (Secuencial)
for (archivo in archivos_csv) {
  
  nombre_base  <- tools::file_path_sans_ext(basename(archivo))
  archivo_part <- file.path(ruta_particiones, paste0(nombre_base, "_partitions.csv"))
  
  if (!file.exists(archivo_part)) {
    cat("Saltando:", nombre_base, "(No se encontró su archivo de particiones)\n")
    next
  }
  
  cat("\n==================================================================\n")
  cat("Iniciando Fine-Tuning con future_lapply para:", nombre_base, "\n")
  cat("==================================================================\n")
  
  # Cargar datos originales y su matriz de índices compartida
  df_datos       <- na.omit(read.csv(archivo))
  df_particiones <- read.csv(archivo_part)
  
  ncol_datos <- ncol(df_datos)
  X_completo <- as.matrix(df_datos[, -ncol_datos])
  y_completo <- as.factor(df_datos[, ncol_datos])
  
  n_corridas <- 33
  
  # Estimar un Sigma base usando la heurística del Median Trick sobre todo el set
  sigma_base <- mean(sigest(X_completo, na.action = na.omit)[2])
  grid_sigma <- sigma_base * c(0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 1, 2, 4, 10)
  
  # ----------------------------------------------------------------------------
  # LAZO PARALELO: future_lapply ejecuta asíncronamente las 33 corridas
  # ----------------------------------------------------------------------------
  # future.seed = TRUE es obligatorio para que los hilos generen números pseudoaleatorios
  # reproducibles de forma segura en entornos distribuidos (L'Ecuyer-CMRG)
  resultados_lista <- future_lapply(1:n_corridas, function(run) {
    
    columna_run <- paste0("Run_", run)
    
    # Máscaras booleanas indexadas por la partición estricta de R
    es_train <- df_particiones[[columna_run]] == "Train"
    es_test  <- df_particiones[[columna_run]] == "Test"
    
    X_train_run <- X_completo[es_train, , drop = FALSE]
    y_train_run <- y_completo[es_train]
    X_test_run  <- X_completo[es_test, , drop = FALSE]
    y_test_run  <- y_completo[es_test]
    
    # TUNING INTERNO: 5-Fold Cross-Validation sobre el 70% de Entrenamiento
    set.seed(run)
    pliegues_internos <- caret::createFolds(y_train_run, k = 5, list = TRUE)
    
    acc_parametro <- numeric(length(grid_sigma))
    
    for (s in seq_along(grid_sigma)) {
      sigma_candidato <- grid_sigma[s]
      rbf_cand        <- kernlab::rbfdot(sigma = sigma_candidato)
      acc_pliegues    <- numeric(5)
      
      for (f in 1:5) {
        idx_val_interno <- pliegues_internos[[f]]
        
        X_t_interno <- X_train_run[-idx_val_interno, , drop = FALSE]
        y_t_interno <- y_train_run[-idx_val_interno]
        X_v_interno <- X_train_run[idx_val_interno, , drop = FALSE]
        y_v_interno <- y_train_run[idx_val_interno]
        
        K_t_interno <- kernlab::kernelMatrix(rbf_cand, X_t_interno)
        K_v_interno <- kernlab::kernelMatrix(rbf_cand, X_v_interno, X_t_interno)
        
        tryCatch({
          mod_interno     <- kfda_entrenar(K_t_interno, y_t_interno)
          preds_v_interno <- kfda_predecir(mod_interno, K_v_interno)$preds
          acc_pliegues[f] <- sum(preds_v_interno == y_v_interno) / length(y_v_interno)
        }, error = function(e) { acc_pliegues[f] <- 0 })
      }
      acc_parametro[s] <- mean(acc_pliegues)
    }
    
    # Seleccionar el mejor Sigma de la validación cruzada interna
    sigma_optimo <- grid_sigma[which.max(acc_parametro)]
    
    # EVALUACIÓN EXTERNA DEFINITIVA SOBRE EL 30% TEST OCULTO
    rbf_optimo    <- kernlab::rbfdot(sigma = sigma_optimo)
    K_train_final <- kernlab::kernelMatrix(rbf_optimo, X_train_run)
    K_test_final  <- kernlab::kernelMatrix(rbf_optimo, X_test_run, X_train_run)
    
    modelo_final         <- kfda_entrenar(K_train_final, y_train_run)
    predicciones_finales <- kfda_predecir(modelo_final, K_test_final)$preds
    
    cm <- caret::confusionMatrix(predicciones_finales, y_test_run)

    if ( length(unique(y_train_run))  == 2) {
      
      resultado <- data.frame(
        Corrida = run,
        Sigma_Optimo = sigma_optimo,
        Accuracy= cm$overall["Accuracy"],
        Kappa= cm$overall["Kappa"],
        Macro_F1= cm$byClass["F1"],
        Macro_Rec = cm$byClass["Sensitivity"],
        Macro_Prec = cm$byClass["Precision"],
        stringsAsFactors = FALSE)
      
    } else {
      
      resultado <- data.frame(
        Corrida      = run,
        Sigma_Optimo = sigma_optimo,
        Accuracy     = cm$overall["Accuracy"],
        Kappa        = cm$overall["Kappa"],
        Macro_F1     = mean(cm$byClass[, "F1"], na.rm = TRUE),
        Macro_Rec    = mean(cm$byClass[, "Sensitivity"], na.rm = TRUE),
        Macro_Prec   = mean(cm$byClass[, "Precision"], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
    }
    
    resultado

      }, future.seed = TRUE) # Indispensable para reproducibilidad en clusters paralelos
  
  # 3. Consolidar la lista devuelta por future_lapply en un solo DataFrame
  df_res_instancia <- do.call(rbind, resultados_lista)
  write.csv(df_res_instancia, 
            file = file.path(ruta_resultados, paste0(nombre_base, "_res_kfda.csv")), 
            row.names = FALSE)
  
  cat("Resultados consolidados guardados con éxito para:", nombre_base, "\n")
}

# 4. Cerrar explícitamente el plan de fondo volviendo al flujo secuencial
plan(sequential)
cat("\n==================================================================\n")
cat("Proceso completado. Clúster de 'future' liberado.\n")
cat("==================================================================\n")

