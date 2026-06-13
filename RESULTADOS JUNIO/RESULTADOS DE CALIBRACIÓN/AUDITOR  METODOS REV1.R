rm(list=ls())

# ==============================================================================
# SCRIPT DE AUDITORÍA Y VERIFICACIÓN DE REPLICABILIDAD EXPERIMENTAL (R)
# ==============================================================================

library(e1071)
library(kernlab)
library(caret)

# ── Configuración de Rutas Estrictas ──────────────────────────────────────────
ruta_base        <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO"
ruta_datasets    <- file.path(ruta_base, "DATASETS")
ruta_particiones <- file.path(ruta_datasets, "SEMILLAS GENERADAS")
ruta_calibracion <- file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN")

# Cargar el motor analítico de KFDA para la reconstrucción manual
source(file.path(ruta_calibracion, "KFDA MANUAL.R"))

# Definición de los 4 métodos y sus respectivas subcarpetas
metodos <- list(
  KFDA     = list(carpeta = "RESULTADOS_KFDA",      sufijo = "_res_kfda.csv"),
  SVM_RBF  = list(carpeta = "RESULTADOS_SVM_RBF",   sufijo = "_res_svm_rbf.csv"),
  SVM_POLY2 = list(carpeta = "RESULTADOS_SVM_POLY2", sufijo = "_res_svm_poly2.csv"),
  SVM_POLY3 = list(carpeta = "RESULTADOS_SVM_POLY3", sufijo = "_res_svm_poly3.csv")
)

# Obtener los nombres de los datasets filtrados (mismo criterio del framework)
archivos_csv <- list.files(path = ruta_datasets, pattern = "\\.csv$", full.names = TRUE)
archivos_csv <- archivos_csv[!grepl("_partitions", archivos_csv)][-c(2,9,10)]

cat("==================================================================\n")
cat("            INICIANDO AUDITORÍA GENERAL DE REPLICABILIDAD        \n")
cat("==================================================================\n")

# BUCLE GENERAL: Iterar sobre cada uno de los 4 métodos de clasificación
nombre_metodo = names(metodos)[1]
for (nombre_metodo in names(metodos)) {
  # Información del metodo
  info_metodo     <- metodos[[nombre_metodo]]
  path_resultados <- file.path(ruta_calibracion, info_metodo$carpeta)
  
  cat("\n------------------------------------------------------------------\n")
  cat(" Auditanodo Método:", nombre_metodo, "\n")
  cat("------------------------------------------------------------------\n")
  
  if (!dir.exists(path_resultados)) {
    cat(" Saltando: La carpeta", info_metodo$carpeta, "no existe.\n")
    next
  }
  # Hacer el reporte por método
  reporte_metodo <- list()
  
  # BUCLE INTERMEDIO: Procesar cada uno de los datasets filtrados
  for (archivo in archivos_csv) {
    nombre_base  <- tools::file_path_sans_ext(basename(archivo))
    archivo_part <- file.path(ruta_particiones, paste0(nombre_base, "_partitions.csv"))
    archivo_res  <- file.path(path_resultados, paste0(nombre_base, info_metodo$sufijo))
    
    if (!file.exists(archivo_res)) {
      cat("  [!] Sin registros para:", nombre_base, "en este método.\n")
      next
    }
    
    # Cargar datos originales, particiones y la bitácora de calibración histórica
    df_datos       <- na.omit(read.csv(archivo))
    df_particiones <- read.csv(archivo_part)
    df_bitacora    <- read.csv(archivo_res)
    
    ncol_datos <- ncol(df_datos)
    X_completo <- as.matrix(df_datos[, -ncol_datos])
    y_completo <- as.factor(df_datos[, ncol_datos])
    
    conteo_verificadas <- 0
    tolerancia <- 1e-6
    
    # BUCLE INTERNO: Reconstrucción secuencial de las 33 corridas estadísticas
    for (run in 1:nrow(df_bitacora)) {
      columna_run <- paste0("Run_", run)
      
      es_train <- df_particiones[[columna_run]] == "Train"
      es_test  <- df_particiones[[columna_run]] == "Test"
      
      X_train_run <- X_completo[es_train, , drop = FALSE]
      y_train_run <- y_completo[es_train]
      X_test_run  <- X_completo[es_test, , drop = FALSE]
      y_test_run  <- y_completo[es_test]
      
      # Extracción de la métrica histórica reportada para contrastar
      acc_historico <- df_bitacora$Accuracy[df_bitacora$Corrida == run]
      acc_recalculado <- NA
      
      # -> RECONSTRUCCIÓN ESPECÍFICA SEGÚN EL MODELO
      if (nombre_metodo == "KFDA") {
        sigma_optimo  <- df_bitacora$Sigma_Optimo[df_bitacora$Corrida == run]
        rbf_optimo    <- kernlab::rbfdot(sigma = sigma_optimo)
        K_train_final <- kernlab::kernelMatrix(rbf_optimo, X_train_run)
        K_test_final  <- kernlab::kernelMatrix(rbf_optimo, X_test_run, X_train_run)
        
        modelo_final         <- kfda_entrenar(K_train_final, y_train_run)
        predicciones_finales <- kfda_predecir(modelo_final, K_test_final)$preds
        acc_recalculado      <- sum(predicciones_finales == y_test_run) / length(y_test_run)
        
      } else if (nombre_metodo == "SVM_RBF") {
        c_opt     <- df_bitacora$Cost_Optimo[df_bitacora$Corrida == run]
        gamma_opt <- df_bitacora$Gamma_Optimo[df_bitacora$Corrida == run]
        
        modelo_final    <- e1071::svm(x = X_train_run, y = y_train_run, kernel = "radial", cost = c_opt, gamma = gamma_opt)
        preds_finales   <- predict(modelo_final, X_test_run)
        acc_recalculado <- sum(preds_finales == y_test_run) / length(y_test_run)
        
      } else if (nombre_metodo == "SVM_POLY2") {
        c_opt     <- df_bitacora$Cost_Optimo[df_bitacora$Corrida == run]
        gamma_opt <- df_bitacora$Gamma_Optimo[df_bitacora$Corrida == run]
        
        modelo_final    <- e1071::svm(x = X_train_run, y = y_train_run, kernel = "polynomial", degree = 2, coef0 = 1, cost = c_opt, gamma = gamma_opt)
        preds_finales   <- predict(modelo_final, X_test_run)
        acc_recalculado <- sum(preds_finales == y_test_run) / length(y_test_run)
        
      } else if (nombre_metodo == "SVM_POLY3") {
        c_opt     <- df_bitacora$Cost_Optimo[df_bitacora$Corrida == run]
        gamma_opt <- df_bitacora$Gamma_Optimo[df_bitacora$Corrida == run]
        
        modelo_final    <- e1071::svm(x = X_train_run, y = y_train_run, kernel = "polynomial", degree = 3, coef0 = 1, cost = c_opt, gamma = gamma_opt)
        preds_finales   <- predict(modelo_final, X_test_run)
        acc_recalculado <- sum(preds_finales == y_test_run) / length(y_test_run)
      }
      
      # Comparación matemática estricta contra la bitácora histórica
      if (abs(acc_recalculado - acc_historico) < tolerancia) {
        conteo_verificadas <- conteo_verificadas + 1
      }
    }
    
    estatus_final <- ifelse(conteo_verificadas == nrow(df_bitacora), "EXITOSA (100% Replicable)", "FALLIDA (Discrepancia detectada)")
    cat("  Instance:", sprintf("%-20s", nombre_base), "-> Corridas verificadas:", conteo_verificadas, "/", nrow(df_bitacora), "[", estatus_final, "]\n")
    
    reporte_metodo[[nombre_base]] <- data.frame(
      Dataset            = nombre_base,
      Corridas_Totales   = nrow(df_bitacora),
      Corridas_Validas   = conteo_verificadas,
      Estatus            = estatus_final,
      stringsAsFactors   = FALSE
    )
  }
  
  # Guardar reporte resumido de auditoría por método en la raíz de calibración
  df_reporte_metodo <- do.call(rbind, reporte_metodo)
  write.csv(df_reporte_metodo, 
            file = file.path(ruta_calibracion, paste0("AUDITORIA_", nombre_metodo, ".csv")), 
            row.names = FALSE)
}

cat("\n==================================================================\n")
cat(" Auditoría finalizada. Los reportes individuales han sido guardados.\n")
cat("==================================================================\n")