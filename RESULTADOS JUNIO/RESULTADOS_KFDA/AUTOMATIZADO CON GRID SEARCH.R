
# Librerias necesarias #
library(kernlab)
library(caret)

# Funciones hechas #

# CONFIGURACIÓN DE RUTAS (Modificar según el entorno de Ubuntu)
ruta_datasets   <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS/"
ruta_particiones <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS\\SEMILLAS GENERADAS/"
ruta_resultados  <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS_KFDA/"


# Obtener los nombres de los datasets originales
archivos_csv <- list.files(path = ruta_datasets, pattern = "\\.csv$", full.names = TRUE)

archivo = archivos_csv[4]
# BUCLE PRINCIPAL: Procesar Instancia por Instancia
for (archivo in archivos_csv) {
  # Cargar las particiones #
  nombre_base <- tools::file_path_sans_ext(basename(archivo))
  archivo_part <- file.path(ruta_particiones, paste0(nombre_base, "_partitions.csv"))
  
  if (!file.exists(archivo_part)) {
    cat("Saltando:", nombre_base, "(No se encontró su archivo de particiones)\n")
    next
  }
  
  cat("\n==================================================================\n")
  cat("Iniciando Fine-Tuning y Evaluación para:", nombre_base, "\n")
  cat("==================================================================\n")
  
  # Cargar datos originales y su matriz de índices compartida
  df_datos <- na.omit(read.csv(archivo))
  df_particiones <- read.csv(archivo_part)
  # Cargar los datos y separar
  ncol_datos <- ncol(df_datos)
  # Separar por X y por Y
  X_completo <- as.matrix(df_datos[, -ncol_datos])
  y_completo <- as.factor(df_datos[, ncol_datos])
  # Definir el numero de corridas
  n_corridas <- 33
  resultados_corridas <- vector("list", n_corridas)
  
  # Estimar un Sigma base usando la heurística del Median Trick sobre todo el set
  sigma_base <- mean(sigest(X_completo, na.action = na.omit)[2])
  # Definir espacio de búsqueda (Gama de búsqueda alrededor de la mediana)
  grid_sigma <- sigma_base * c(0.01,0.025,0.05,0.075,0.1, 0.25, 0.5, 1, 2, 4, 10)
  
  # Iterar sobre las 33 corridas estadísticas
  for (run in 1:n_corridas) {
    columna_run <- paste0("Run_", run)
    
    # Máscaras booleanas indexadas por la partición estricta de R
    es_train <- df_particiones[[columna_run]] == "Train"
    es_test  <- df_particiones[[columna_run]] == "Test"
    # Aplicar las divisiones en X y etiquetas Y
    X_train_run <- X_completo[es_train, , drop = FALSE]
    y_train_run <- y_completo[es_train]
    X_test_run  <- X_completo[es_test, , drop = FALSE]
    y_test_run  <- y_completo[es_test]
    
    # --------------------------------------------------------------------------
    # TUNING INTERNO: 5-Fold Cross-Validation sobre el 70% de Entrenamiento
    # --------------------------------------------------------------------------
    set.seed(run) # Semilla de control para los pliegues internos de la corrida
    pliegues_internos <- createFolds(y_train_run, k = 5, list = TRUE)
    
    acc_parametro <- numeric(length(grid_sigma))
    
    for (s in seq_along(grid_sigma)) {
      sigma_candidato <- grid_sigma[s]
      rbf_cand <- rbfdot(sigma = sigma_candidato)
      acc_pliegues <- numeric(5)
      
      for (f in 1:5) {
        idx_val_interno <- pliegues_internos[[f]]
        
        # Segmentación interna del pliegue
        X_t_interno <- X_train_run[-idx_val_interno, , drop = FALSE]
        y_t_interno <- y_train_run[-idx_val_interno]
        X_v_interno <- X_train_run[idx_val_interno, , drop = FALSE]
        y_v_interno <- y_train_run[idx_val_interno]
        
        # Calcular matrices de kernel internas para el pliegue
        K_t_interno <- kernelMatrix(rbf_cand, X_t_interno)
        K_v_interno <- kernelMatrix(rbf_cand, X_v_interno, X_t_interno)
        
        # Evaluar el modelo interno analítico
        tryCatch({
          mod_interno <- kfda_entrenar(K_t_interno, y_t_interno)
          preds_v_interno <- kfda_predecir(mod_interno, K_v_interno)$preds
          acc_pliegues[f] <- sum(preds_v_interno == y_v_interno) / length(y_v_interno)
        }, error = function(e) { acc_pliegues[f] <- 0 })
      }
      acc_parametro[s] <- mean(acc_pliegues)
    }
    
    # Seleccionar el mejor Sigma de la validación cruzada interna
    sigma_optimo <- grid_sigma[which.max(acc_parametro)]
    
    # --------------------------------------------------------------------------
    # EVALUACIÓN EXTERNA DEFINITIVA SOBRE EL 30% TEST OCULTO
    # --------------------------------------------------------------------------
    rbf_optimo <- rbfdot(sigma = sigma_optimo)
    K_train_final <- kernelMatrix(rbf_optimo, X_train_run)
    K_test_final  <- kernelMatrix(rbf_optimo, X_test_run, X_train_run)
    
    modelo_final <- kfda_entrenar(K_train_final, y_train_run)
    predicciones_finales <- kfda_predecir(modelo_final, K_test_final)$preds
    
    # Calcular matriz de confusión y métricas multiclase vía caret
    cm <- confusionMatrix(predicciones_finales, y_test_run)
    
    # Guardar métricas de la corrida actual
    resultados_corridas[[run]] <- data.frame(
      Corrida      = run,
      Sigma_Optimo = sigma_optimo,
      Accuracy     = cm$overall["Accuracy"],
      Kappa        = cm$overall["Kappa"],
      Macro_F1     = mean(cm$byClass[, "F1"], na.rm = TRUE),
      Macro_Rec    = mean(cm$byClass[, "Sensitivity"], na.rm = TRUE),
      Macro_Prec   = mean(cm$byClass[, "Precision"], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    
    cat("Corrida", run, "/33 completada. Sigma Óptimo =", round(sigma_optimo, 4), 
        "| Accuracy de Test =", round(cm$overall["Accuracy"], 4), "\n")
  }
  
  # 3. Consolidar y guardar resultados de la instancia
  df_res_instancia <- do.call(rbind, resultados_corridas)
  write.csv(df_res_instancia, 
            file = file.path(ruta_resultados, paste0(nombre_base, "_res_kfda.csv")), 
            row.names = FALSE)
  
  cat("\nResultados guardados con éxito para:", nombre_base, "\n")
}
