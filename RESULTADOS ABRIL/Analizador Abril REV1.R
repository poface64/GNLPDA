rm(list=ls())

### Cargar las librerias necesarias ###
library(caret)
library(JuliaCall)
library(MASS)

# 1.- Cargar las funciones auxiliares 
source("POLEXP.R")

# 2.- Definir los objetos donde estan las rutas
### Rutas de los datos ###
rutaDS <- "/home/angel/Desktop/GNLPDA REV1/RESULTADOS ABRIL/DATASETS/"
rutasDS <- list.files(
  path = rutaDS,
  full.names = TRUE)
### Ruta de los resultados ###
rutaRS <- "/home/angel/Desktop/GNLPDA REV1/RESULTADOS ABRIL/CORRIDAS DE 30 NORMALIZADAS/"
rutasRS <- list.files(
  path = rutaRS,
  pattern = "^Resultados_Detalle_",
  full.names = TRUE)

# Ciclo principal para automatizar las corridas

# n.- Definir una semilla maestra
# Configuración de Cross-Validation (10-fold estratificado)
semilla_maestra <- 1019
set.seed(semilla_maestra)

# Iterar por cada Dataset disponible
for (i in seq_along(rutasDS)) {
  
  # 1. Carga de archivos correspondientes
  nombre_ds <- basename(rutasDS[i])
  message(">>> Procesando Dataset: ", nombre_ds)
  datosDS <- read.csv(rutasDS[i])
  # Buscamos el reporte que coincida con el nombre del dataset (o por índice)
  datosRS <- read.csv(rutasRS[i]) 
  
  # 2. Preprocesamiento y Expansión Polinomial Única
  # Extraemos TODAS las variables que alguna vez se usaron en E1 para este DS
  vars_e1_all <- sort(unique(trimws(unlist(strsplit(datosRS$Nombres_E1, ",")))))
  # Normalizar con media 0 y varianza 1
  datosEST <- as.data.frame(scale(datosDS[, -ncol(datosDS)]))
  # Separar la variable objetivo
  target   <- as.factor(datosDS[, ncol(datosDS)])
  
  # Pasamos a Julia solo lo necesario para expandir una sola vez
  datosFIL  <- datosEST[, vars_e1_all]
  df_expandido <- polexpj(datosFIL, 2)
  
  # 3. Contenedor para los resultados de las 30 corridas de este Dataset
  resultados_30_corridas <- data.frame(matrix(0,nrow=30,ncol = 4))
  names(resultados_30_corridas) = c("Accuracy","Precision","Recall","F1" ) 
  # 4. Ciclo de las 30 Corridas (Basado en el reporte RS)
  for (j in 1:nrow(datosRS)) {
    
    # Extraer y normalizar variables de la Fase 2 (E2)
    vars_e2_raw <- gsub(" ", "", unlist(strsplit(datosRS$Nombres_E2[j], ",")))
    vars_e2_ready <- normalizar_nombres(vars_e2_raw)
    
    # Filtrar el dataset expandido
    df_eval <- df_expandido[, vars_e2_ready, drop = FALSE]
    df_eval$Clase <- target # Garantizar que sea un factor
    
    # Configurar el control de entrenamiento (CV Estratificado)
    # Usamos la misma semilla en cada corrida para que los folds sean idénticos
    set.seed(semilla_maestra)
    folds <- createFolds(df_eval$Clase, k = 10, 
                         list = TRUE, returnTrain = F)
    
    # Crear el objeto para extraer las metricas #
    metrics_results <- data.frame()
    
    # Aplicar el bucle para evaluar los kfolds #
    for(f in 1:10) {
      # Separar la data de prueba de la data de entrenamiento
      train_data <- df_eval[-folds[[f]], ]
      test_data  <- df_eval[folds[[f]], ]
      #dim(train_data);dim(test_data)
      
      # Ajustar LDA (MASS)
      # Se usa la fórmula Clase ~ . para incluir todas las variables seleccionadas
      fit_lda <- lda(Clase ~ ., data = train_data)
      # Predicciones
      preds <- predict(fit_lda, test_data)$class
      # Métricas detalladas
      cm <- confusionMatrix(preds, test_data$Clase, mode = "everything")
      byClass <- as.data.frame(t(cm$byClass))
      
      # Extracción Macro-Average
      if(length(unique(target))==2){
        metrics_results <- rbind(metrics_results, data.frame(
          Accuracy  = cm$overall["Accuracy"],
          Precision = cm$byClass["Precision"],
          Recall    = cm$byClass["Recall"],
          F1        = cm$byClass["F1"]))
      }else{
        metrics_results <- rbind(metrics_results, data.frame(
          Accuracy  = cm$overall["Accuracy"],
          Precision = mean(cm$byClass[, "Precision"], na.rm = TRUE),
          Recall    = mean(cm$byClass[, "Recall"], na.rm = TRUE),
          F1        = mean(cm$byClass[, "F1"], na.rm = TRUE)
        ))
        
      }
    }
    # Obtener las medias de los resultados por corrida
    res_corrida_j <- colMeans(metrics_results)
    # Guardar los 30 resultados de las 30 corridas
    resultados_30_corridas[j,] = res_corrida_j
  }
  
  # 5. Cálculo de Estadísticos Finales para el Dataset i
  res_final_ds <- data.frame(
    Dataset = nombre_ds,
    Mean_Accuracy  = mean(resultados_30_corridas$Accuracy),
    Std_Accuracy   = sd(resultados_30_corridas$Accuracy),
    Mean_F1        = mean(resultados_30_corridas$F1),
    Std_F1         = sd(resultados_30_corridas$F1),
    Mean_Precision = mean(resultados_30_corridas$Precision),
    Std_precision  = sd(resultados_30_corridas$Precision),
    Mean_Recall    = mean(resultados_30_corridas$Recall),
    Std_Recall     = sd(resultados_30_corridas$Recall)
  )
  
  # Guardar resultados parciales en disco
  write.csv(res_final_ds, paste0(rutaRS, "Resumen_30R_", nombre_ds), row.names = FALSE)
  write.csv(res_final_ds, paste0(rutaRS, "Resumen_FinalR_", nombre_ds), row.names = FALSE)
  print(res_final_ds)
}

######## PIPE LINE REDEFINIDO CON: CV -- Scale -- expand -- evaluar train/test


# --- BLOQUE DE COMPETIDORES DIRECTOS ---
# Este bloque se ejecuta una vez por Dataset (dentro del bucle de i)

rivales <- c("LDA_Simple", "QDA", "SVM_Radial", "RF")
res_rivales_ds <- data.frame()

for (metodo in rivales) {
  message("  > Evaluando competidor: ", metodo)
  metrics_rival <- data.frame()
  
  # Usamos los mismos folds que creamos con la semilla_maestra
  for(f in 1:10) {
    train_rival <- datosEST[-folds[[f]], ]
    test_rival  <- datosEST[folds[[f]], ]
    train_rival$Clase <- target[-folds[[f]]]
    test_rival$Clase  <- target[folds[[f]]]
    
    # --- Ajuste del Modelo según el método ---
    model_fit <- NULL
    
    if(metodo == "LDA_Simple") {
      model_fit <- lda(Clase ~ ., data = train_rival)
      preds_rival <- predict(model_fit, test_rival)$class
      
    } else if(metodo == "QDA") {
      # El QDA puede fallar si hay colinealidad o pocas muestras
      model_fit <- tryCatch(qda(Clase ~ ., data = train_rival), error = function(e) NULL)
      preds_rival <- if(!is.null(model_fit)) predict(model_fit, test_rival)$class else rep(NA, nrow(test_rival))
      
    } else if(metodo == "SVM_Radial") {
      # Usamos caret::train para que el SVM se auto-calibre internamente (Grid Search)
      # pero solo con la data de entrenamiento del fold actual
      ctrl_interno <- trainControl(method = "none") # No CV interno para ahorrar tiempo
      model_fit <- train(Clase ~ ., data = train_rival, method = "svmRadial", 
                         trControl = ctrl_interno, tuneGrid = data.frame(sigma=0.1, C=1))
      preds_rival <- predict(model_fit, test_rival)
      
    } else if(metodo == "RF") {
      model_fit <- train(Clase ~ ., data = train_rival, method = "rf", 
                         trControl = trainControl(method = "none"), tuneGrid = data.frame(mtry=sqrt(ncol(datosEST))))
      preds_rival <- predict(model_fit, test_rival)
    }
    
    # --- Cálculo de Métricas (Misma lógica que tu pipeline) ---
    if(!any(is.na(preds_rival))) {
      cm_r <- confusionMatrix(preds_rival, test_rival$Clase, mode = "everything")
      
      if(length(unique(target)) == 2) {
        metrics_rival <- rbind(metrics_rival, data.frame(
          Accuracy = cm_r$overall["Accuracy"], Precision = cm_r$byClass["Precision"],
          Recall = cm_r$byClass["Recall"], F1 = cm_r$byClass["F1"]))
      } else {
        metrics_rival <- rbind(metrics_rival, data.frame(
          Accuracy = cm_r$overall["Accuracy"],
          Precision = mean(cm_r$byClass[, "Precision"], na.rm = TRUE),
          Recall = mean(cm_r$byClass[, "Recall"], na.rm = TRUE),
          F1 = mean(cm_r$byClass[, "F1"], na.rm = TRUE)))
      }
    }
  }
  
  # Promediar los 10 folds para el competidor
  res_rivales_ds <- rbind(res_rivales_ds, cbind(Metodo = metodo, as.data.frame(t(colMeans(metrics_rival)))))
}

# Guardar los resultados de los competidores para este dataset
write.csv(res_rivales_ds, paste0(rutaRS, "Competidores_", nombre_ds), row.names = FALSE)


#### Competidores ####
rm(list=ls())
library(caret)
library(MASS)
library(e1071)
library(randomForest)

# 1. Configuración de Rutas
rutaDS <- "/home/angel/Desktop/GNLPDA REV1/RESULTADOS ABRIL/DATASETS/"
rutaRS <- "/home/angel/Desktop/GNLPDA REV1/RESULTADOS ABRIL/CORRIDAS DE 30 NORMALIZADAS/"
rutasDS <- list.files(path = rutaDS, full.names = TRUE)

# 2. Semilla Maestra (DEBE SER LA MISMA QUE EN TU OTRO SCRIPT)
semilla_maestra <- 1019

# 3. Definición de Competidores
# Agregamos SVM con Kernels Polinomiales
metodos <- c("LDA_Simple", "QDA", "SVM_Radial", "SVM_Poly2", "SVM_Poly3", "RF")
for (i in seq_along(rutasDS)) {
  # Cargar el nombre de la ruta
  nombre_ds <- basename(rutasDS[i])
  message(">>> Evaluando Competidores para: ", nombre_ds)
  
  # Carga y Preprocesamiento básico
  datosDS <- read.csv(rutasDS[i])
  if(nombre_ds=="4.2.Ionosphere.csv"){
    datosDS = datosDS[,-2]
  }
  target  <- as.factor(paste0("G",datosDS[, ncol(datosDS)]))
  datosEST <- as.data.frame(scale(datosDS[, -ncol(datosDS)]))
  datosEST$Clase <- target
  
  # Contenedor de resultados para este Dataset
  res_competidores <- data.frame()
  
  for (m in metodos) {
    message("  > Ejecutando: ", m)
    metrics_folds <- data.frame()
    
    # Replicar exactamente los mismos Folds
    set.seed(semilla_maestra)
    folds <- createFolds(datosEST$Clase, k = 10, list = TRUE, returnTrain = FALSE)
    
    for (f in 1:10) {
      train_data <- datosEST[-folds[[f]], ]
      test_data  <- datosEST[folds[[f]], ]
      
      model_fit <- NULL
      preds <- NULL
      
      # Lógica de entrenamiento por método con manejo de errores
      try({
        if (m == "LDA_Simple") {
          model_fit <- lda(Clase ~ ., data = train_data)
          preds <- predict(model_fit, test_data)$class
        } else if (m == "QDA") {
          model_fit <- qda(Clase ~ ., data = train_data)
          preds <- predict(model_fit, test_data)$class
        } else if (m == "SVM_Radial") {
          model_fit <- svm(Clase ~ ., data = train_data, kernel = "radial", cost = 10, gamma = 1/ncol(datosEST))
          preds <- predict(model_fit, test_data)
        } else if (m == "SVM_Poly2") {
          model_fit <- svm(Clase ~ ., data = train_data, kernel = "polynomial", degree = 2, cost = 10)
          preds <- predict(model_fit, test_data)
        } else if (m == "SVM_Poly3") {
          model_fit <- svm(Clase ~ ., data = train_data, kernel = "polynomial", degree = 3, cost = 10)
          preds <- predict(model_fit, test_data)
        } else if (m == "RF") {
          model_fit <- randomForest(Clase ~ ., data = train_data, ntree = 500)
          preds <- predict(model_fit, test_data)
        }
      }, silent = TRUE)
      
      # Cálculo de métricas si hubo éxito en el entrenamiento
      if (!is.null(preds)) {
        # Validación extra: Verificar que las predicciones no sean puras NAs
        if(!all(is.na(preds))){
          cm <- confusionMatrix(preds, test_data$Clase, mode = "everything")
          
          if (length(unique(target)) == 2) {
            fold_res <- data.frame(
              Acc = cm$overall["Accuracy"], 
              Pre = cm$byClass["Precision"], 
              Rec = cm$byClass["Recall"], 
              F1  = cm$byClass["F1"]
            )
          } else {
            fold_res <- data.frame(
              Acc = cm$overall["Accuracy"],
              Pre = mean(cm$byClass[, "Precision"], na.rm = TRUE),
              Rec = mean(cm$byClass[, "Recall"], na.rm = TRUE),
              F1  = mean(cm$byClass[, "F1"], na.rm = TRUE)
            )
          }
          metrics_folds <- rbind(metrics_folds, fold_res)
        }
      }
    }
    

    # Blindaje: Si metrics_folds está vacío (el método falló en los 10 pliegues)
    if (nrow(metrics_folds) > 0) {
      means_metodo <- colMeans(metrics_folds, na.rm = TRUE)
      sds_metodo   <- apply(metrics_folds, 2, sd, na.rm = TRUE)
    } else {
      # Asignamos NA o 0 para indicar que el modelo no fue factible
      means_metodo <- c(Acc = NA, Pre = NA, Rec = NA, F1 = NA)
      sds_metodo   <- c(Acc = NA, Pre = NA, Rec = NA, F1 = NA)
    }
    
    # Guardar en el contenedor de competidores
    res_competidores <- rbind(res_competidores, data.frame(
      Metodo    = m,
      Accuracy  = means_metodo["Acc"],
      Std_Acc   = sds_metodo["Acc"],
      Precision = means_metodo["Pre"],
      Std_Pre   = sds_metodo["Pre"],
      Recall    = means_metodo["Rec"],
      Std_Rec   = sds_metodo["Rec"],
      F1        = means_metodo["F1"],
      Std_F1    = sds_metodo["F1"]
    ))
  }  
  # Guardar resultados de competidores para este dataset
  archivo_salida <- paste0(rutaRS, "Competidores_Final_", nombre_ds)
  write.csv(res_competidores, archivo_salida, row.names = FALSE)
  print(res_competidores)
}

rutasDS[7]
datos = read.csv(rutasDS[7])
LDA1 = lda(datos$Diagnosis~.,datos)






