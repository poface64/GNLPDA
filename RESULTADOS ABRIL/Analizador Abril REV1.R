rm(list=ls())

### Cargar las librerias necesarias ###
library(caret)
library(JuliaCall)
library(MASS)

# 1.- Cargar las funciones auxiliares 
source("POLEXP.R")

# 2.- Definir los objetos donde estan las rutas
### Rutas de los datos ###
rutaDS = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS ABRIL\\DATASETS\\" # Windows
#rutaDS <- "~//Desktop/GNLPDA REV1/RESULTADOS ABRIL/DATASETS/" # UBUNTU
rutasDS <- list.files(
  path = rutaDS,
  full.names = TRUE)
### Ruta de los resultados ###
rutaRS = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS ABRIL\\CORRIDAS DE 30 NORMALIZADAS/" # Windows
#rutaRS <- "~//Desktop/GNLPDA REV1/RESULTADOS ABRIL/CORRIDAS DE 30 NORMALIZADAS/"
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
  
  #### Aquí puede funcionar el gráficador ####
  # 5. Preparar los datos y gráficar los mejores por dataset
  
  # Separar la corrida que mejores resultados da #
  mejorcorrida = order(resultados_30_corridas$Accuracy,decreasing = T)[2]
  
  ## Separar los datos que necesito ##
  # Extraer y normalizar variables de la Fase 2 (E2)
  vars_e2_raw <- gsub(" ", "", unlist(strsplit(datosRS$Nombres_E2[mejorcorrida], ",")))
  vars_e2_ready <- organizar_variables_por_tipo(normalizar_nombres(vars_e2_raw))$Ordenado

  mejor_fila = data.frame(Accuracy = resultados_30_corridas$Accuracy[mejorcorrida],
                          VarsE2 = datosRS$Nombres_E2[mejorcorrida])
  str(mejor_fila)
  
  # Filtrar el dataset expandido
  df_eval <- df_expandido[, vars_e2_ready, drop = FALSE]
  ylab <- target # Garantizar que sea un factor
  
  # 1. Definir y asegurar la carpeta de destino
  # Creamos una subcarpeta dentro de tu ruta de resultados
  carpeta_graficos <- file.path(rutaRS, "graficos")
  
  # Si la carpeta no existe, R la creará por ti
  if (!dir.exists(carpeta_graficos)) {
    dir.create(carpeta_graficos)
  }
  
  # 2. Invocar la función de graficación
  p_final <- graficar_proyeccion_tesis(
    X_final = df_eval[, vars_e2_ready], 
    y_labels = ylab, 
    nombre_ds = sub("\\.csv$", "", nombre_ds), 
    mejor_fila = mejor_fila
  )
  
  # 3. Guardar el gráfico en la subcarpeta específica
  file_name <- paste0("Proyeccion_GNLPDA_", sub("\\.csv$", "", nombre_ds), ".png")
  
  ggsave(
    filename = file_name,
    plot = p_final,
    path = carpeta_graficos, # <--- Ahora apunta a la subcarpeta "graficos"
    width = 10, 
    height = 7, 
    dpi = 300,
    bg = "white"
  )
  
  message(">>> Gráfico exportado con éxito a: ", file.path("graficos", file_name))
  
  ###########################################
  
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
  #write.csv(res_final_ds, paste0(rutaRS, "Resumen_30R_", nombre_ds), row.names = FALSE)
  #write.csv(res_final_ds, paste0(rutaRS, "Resumen_FinalR_", nombre_ds), row.names = FALSE)
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

#### Bloque para explorar estadísticas inferenciales ####
# Rutas de tus resultados
archivos_comp <- list.files(rutaRS, pattern = "^Competidores_Final_", full.names = TRUE)
archivos_gnlp <- list.files(rutaRS, pattern = "^Resumen_FinalR_", full.names = TRUE)

# Crear Tabla Maestra de Accuracy
library(tidyverse)
tabla_comparativa <- data.frame()
for(i in seq_along(archivos_comp)) {
  # Cargar competidores y GNLPDA
  df_c <- read.csv(archivos_comp[i])
  df_g <- read.csv(archivos_gnlp[i])
  # Extraer solo el Accuracy de cada método
  # Competidores
  temp_c <- df_c %>% select(Metodo, Accuracy) %>% spread(Metodo, Accuracy)
  # Tu método (tomamos la media de las 30 corridas)
  temp_c$GNLPDA <- df_g$Mean_Accuracy
  # Nombre del dataset limpio
  temp_c$Dataset <- sub("Competidores_Final_", "", basename(archivos_comp[i]))
  tabla_comparativa <- rbind(tabla_comparativa, temp_c)
}

# Preparar matriz para Friedman (Filas = Datasets, Columnas = Métodos)
matriz_stats <- as.matrix(tabla_comparativa %>% select(-Dataset))
friedman_res <- friedman.test(matriz_stats)
print(friedman_res)

# Ejemplo: GNLPDA vs SVM_Radial
test_svm <- wilcox.test(tabla_comparativa$GNLPDA, 
                        tabla_comparativa$SVM_Radial, 
                        paired = TRUE, 
                        alternative = "greater")

message("P-value GNLPDA vs SVM Radial: ", test_svm$p.value)
hist(resu30$Acc)
shapiro.test(resu30$Acc)

t.test(resu30$Acc,mu = tabla_comparativa$SVM_Radial[8])

########
library(tidyverse)

# 1. Configuración de rutas (Asegúrate de que rutaRS sea correcta)
archivos_30 <- list.files(
  path = rutaRS,
  pattern = "^Resumen_30R_", # Ajusta el patrón según el nombre de tus archivos de 30 corridas
  full.names = TRUE
)

resumen_estadistico_gnlpda <- data.frame()

for (i in seq_along(rutasRS)) {
  # 2. Cargar datos de las 30 corridas
  df_30 <- read.csv(rutasRS[i])
  dataset_name <- sub("Resumen_30R_", "", basename(rutasDS[i]))
  dataset_name <- sub("\\.csv$", "", dataset_name)
  
  # Extraer vector de Accuracy (Asegúrate de que la columna se llame Accuracy o Acc)
  # Según tu script anterior es "Accuracy"
  acc_vector <- df_30$Acc
  
  # 3. Prueba de Normalidad (Shapiro-Wilk)
  # H0: La muestra proviene de una distribución normal
  test_shapiro <- shapiro.test(acc_vector)
  p_val_shapiro <- test_shapiro$p.value
  es_normal <- ifelse(p_val_shapiro > 0.05, "Sí", "No")
  
  # 4. Cálculo de Intervalos de Confianza (95%)
  n <- length(acc_vector)
  media <- mean(acc_vector)
  desv_std <- sd(acc_vector)
  error_std <- desv_std / sqrt(n)
  
  # Usamos la distribución t de Student (ideal para n=30)
  alfa <- 0.05
  valor_t <- qt(1 - alfa/2, df = n - 1)
  
  margen_error <- valor_t * error_std
  ic_inferior <- media - margen_error
  ic_superior <- media + margen_error
  
  # 5. Acumular resultados
  fila_resumen <- data.frame(
    Dataset = dataset_name,
    Media = round(media, 4),
    P_Value_Shapiro = round(p_val_shapiro, 5),
    Distribucion_Normal = es_normal,
    IC_Inferior = round(ic_inferior, 4),
    IC_Superior = round(ic_superior, 4),
    IC_Texto = paste0("[", round(ic_inferior, 3), ", ", round(ic_superior, 3), "]")
  )
  
  resumen_estadistico_gnlpda <- rbind(resumen_estadistico_gnlpda, fila_resumen)
}

# 6. Mostrar y guardar el reporte
print(resumen_estadistico_gnlpda)
write.csv(resumen_estadistico_gnlpda, 
          file.path(rutaRS, "Validacion_Estadistica_GNLPDA.csv"), 
          row.names = FALSE)

##########

# Excluimos 'GNLPDA' y 'Dataset' para quedarnos solo con los competidores
competidores_nombres <- setdiff(names(tabla_comparativa), c("GNLPDA", "Dataset"))

resumen_significancia_detalle <- data.frame()
i = 1

for (i in seq_along(rutasRS)) {
  
  # A. Cargar tus 30 corridas actuales
  df_30 <- read.csv(rutasRS[i])
  acc_vector <- df_30$Acc
  
  # B. Identificar el nombre del dataset para buscarlo en la tabla_comparativa
  # Usamos basename para asegurar coincidencia exacta con la columna Dataset
  ds_nombre_target <- basename(rutasDS[i])
  
  # Extraer la fila correspondiente de la tabla comparativa
  fila_ref <- tabla_comparativa[tabla_comparativa$Dataset == ds_nombre_target, ]
  
  # C. Crear una fila para los p-values de este dataset
  # Empezamos con el nombre del dataset
  fila_p_values <- data.frame(Dataset = sub("\\.csv$", "", ds_nombre_target))
  
  for (metodo in competidores_nombres) {
    valor_mu <- fila_ref[[metodo]]
    
    # Si el valor es NA (porque el método falló), ponemos NA en el p-value
    if (is.na(valor_mu)) {
      fila_p_values[[metodo]] <- NA
    } else {
      # PRUEBA DE WILCOXON DE UNA MUESTRA
      # HA: GNLPDA > valor_mu
      test_w <- wilcox.test(acc_vector, 
                            mu = valor_mu, 
                            alternative = "two.sided")
      
      # Guardamos el p-value formateado
      fila_p_values[[metodo]] <- round(test_w$p.value, 4)
    }
  }
  
  resumen_significancia_detalle <- rbind(resumen_significancia_detalle, fila_p_values)
  round(resumen_significancia_detalle[,-1],4)
}

#############

# --- SCRIPT AJUSTADO AL ENFOQUE DE EXPLICABILIDAD ---

resumen_interpretado <- data.frame()
for (i in seq_along(rutasRS)) {
  df_30 <- read.csv(rutasRS[i])
  acc_vector <- df_30$Acc
  ds_nombre_target <- basename(rutasDS[i])
  fila_ref <- tabla_comparativa[tabla_comparativa$Dataset == ds_nombre_target, ]
  
  fila_resumen <- data.frame(Dataset = sub("\\.csv$", "", ds_nombre_target))
  
  for (metodo in competidores_nombres) {
    valor_mu <- fila_ref[[metodo]]
    
    if (is.na(valor_mu)) {
      fila_resumen[[metodo]] <- "Fallo Met."
    } else {
      # 1. Prueba de DOS COLAS (Busca diferencia, no solo superioridad)
      test_w <- wilcox.test(acc_vector, mu = valor_mu, alternative = "two.sided")
      p_val <- test_w$p.value
      media_propuesta <- mean(acc_vector)
      
      # 2. Lógica de Veredicto basada en tu investigación
      if (p_val > 0.05) {
        # No hay evidencia de diferencia: ÉXITO (Equivalencia)
        fila_resumen[[metodo]] <- "Equivalente (≈)"
      } else {
        # Hay diferencia: ¿Quién es mejor?
        if (media_propuesta > valor_mu) {
          fila_resumen[[metodo]] <- "Superior (+)"
        } else {
          fila_resumen[[metodo]] <- "Inferior (-)"
        }
      }
    }
  }
  resumen_interpretado <- rbind(resumen_interpretado, fila_resumen)
}

print(resumen_interpretado)


#############


# --- PRESENTACIÓN DE RESULTADOS ---

# 2. Crear una versión "estética" para la tesis (con asteriscos)
resumen_tesis_estético <- resumen_significancia_detalle
for (metodo in competidores_nombres) {
  resumen_tesis_estético[[metodo]] <- ifelse(
    resumen_significancia_detalle[[metodo]] < 0.05, 
    paste0(resumen_significancia_detalle[[metodo]], " *"), 
    as.character(resumen_significancia_detalle[[metodo]])
  )
}

# Mostrar tabla de p-values puros
print("Tabla de P-Values (Wilcoxon One-Sample):")
print(resumen_significancia_detalle)

# Guardar
write.csv(resumen_significancia_detalle, 
          file.path(rutaRS, "P_Values_Detalle_Vs_Competidores.csv"), 
          row.names = FALSE)

########





#rutasDS[7]
#datos = read.csv(rutasDS[7])
#LDA1 = lda(datos$Diagnosis~.,datos)


#### Aquí empieza el bloque del reportador de resultados ####
rm(list=ls())
rutaDS <- "~/Desktop/GNLPDA/RESULTADOS ABRIL/DATASETS/"
rutaRS <- "~/Desktop/GNLPDA REV1/RESULTADOS ABRIL/CORRIDAS DE 30 NORMALIZADAS/"
rutasDS <- list.files(path = rutaDS, full.names = TRUE);rutasDS
rutasRS <- list.files(path = rutaRS, full.names = TRUE);rutasRS

# Nombres limpios
nombresdf <- rutasDS |>
  basename() |>
  sub("\\.csv$", "", x = _) |>
  sub("^[0-9]+\\.[0-9]+\\.", "", x = _)

# Función para extraer info
extraer_info <- function(path, nombre) {
  # Cargar la ruta
  dtemp <- read.csv(path)
  # Hacer el dataframe de resumen
  data.frame(
    Nombre = nombre,
    Obs = nrow(dtemp),
    Vars = ncol(dtemp) - 1,
    Clases = length(unique(dtemp[[ncol(dtemp)]]))
  )
}

# Aplicar a todos
resudf <- do.call(rbind,mapply(extraer_info, rutasDS, nombresdf, SIMPLIFY = FALSE))
row.names(resudf) = NULL

#### Revisar para extraer la cantidad de variables que deja fuera ####
# Resumen del dropoff de variables
resudropvariables = data.frame(Nombre = "",
                               Vars_E1p = 0,
                               Vars_E1des =0,
                               Vars_E2p = 0,
                               Vars_E2des =0)
i = 1

for(i in 1:length(rutasDS)){
  # Buscar solo esa ruta
  resu30 = read.csv(rutasRS[i])
  # Cuantas variables esta dejando fuera
  E1pm = round(mean(1 - resu30$Vars_E1/resudf$Vars[i])*100,2)
  E1pdes = round(sd(1 - resu30$Vars_E1/resudf$Vars[i])*100,2)
  # Cuantos terminos expandidos deja fuera
  comb2 = choose(resu30$Vars_E1+2,2)-1
  E2pm = round(mean(1 - resu30$Vars_E2/comb2)*100,2)
  E2pdes = round(sd(1 - resu30$Vars_E2/comb2)*100,2)
  # Armar la salida
  resudropvariables[i,] = data.frame(Nombre = resudf$Nombre[i],
                                     Vars_E1p = E1pm,
                                     Vars_E1des =E1pdes,
                                     Vars_E2p = E2pm,
                                     Vars_E2des =E2pdes)
}
# Aquí queda lo de cuantas variables dropea por detectar que NO son relevantes 
resudropvariables 


############

# Aquí hago un bloque para la proyección #






############

#### Cargar las rutas ####

# Ajustar para linux
library(tidyverse)
#rutaRES = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS ABRIL\\CORRIDAS DE 30 NORMALIZADAS\\"
rutaRES = "/home/angeal/Desktop/GNLPDA/RESULTADOS ABRIL/CORRIDAS DE 30 NORMALIZADAS/"

# Separar las rutas de los resultados en julia y los de R
archivos  <- list.files(path = rutaRES, pattern = "^Resumen_FinalR_", full.names = TRUE)
archivos2 <- list.files(path = rutaRES, pattern = "^Resumen_Estadistico_", full.names = TRUE)

resumen1 = data.frame()
nombres_dataset <- sub(".*Resumen_FinalR_", "", archivos)

for(i in 1:length(archivos)){
  # 1. Cargar ambos reportes
  resR   <- read.csv(archivos[i])
  resjul <- read.csv(archivos2[i])
  
  # 2. Procesar reporte de Julia (Vars y Tiempo)
  julia_temp <- resjul %>%
    mutate(Valor = paste0(format(round(Media, 2), nsmall = 2), " ± ", format(round(Std_Dev, 2), nsmall = 2))) %>%
    select(Metrica, Valor) %>%
    pivot_wider(names_from = Metrica, values_from = Valor)
  
  # 3. Procesar reporte de R (Métricas de validación)
  # Agregamos Accuracy a la cadena de procesamiento
  r_temp <- resR %>%
    mutate(
      Dataset   = sub("\\.csv$", "", nombres_dataset[i]),
      # Formateo con 2 decimales fijos (nsmall asegura que salga 0.90 y no 0.9)
      Accuracy  = paste0(format(round(Mean_Accuracy, 2), nsmall = 2), " ± ", format(round(Std_Accuracy, 2), nsmall = 2)),
      Precision = paste0(format(round(Mean_Precision, 2), nsmall = 2), " ± ", format(round(Std_precision, 2), nsmall = 2)),
      Recall    = paste0(format(round(Mean_Recall, 2), nsmall = 2), " ± ", format(round(Std_Recall, 2), nsmall = 2)),
      F1Score   = paste0(format(round(Mean_F1, 2), nsmall = 2), " ± ", format(round(Std_F1, 2), nsmall = 2))
    ) %>%
    select(Dataset, Accuracy, Precision, Recall, F1Score)
  
  # 4. Extraer No. Vars (Entero de la media de Vars_E1)
  no_vars_val <- floor(resjul$Media[resjul$Metrica == "Vars_E1"]) 
  
  # 5. Ensamblar la fila completa para el Dataset i
  fila_completa <- data.frame(
    Dataset   = r_temp$Dataset,
    No_Vars   = no_vars_val,
    Vars_E1   = julia_temp$Vars_E1,
    Vars_E2   = julia_temp$Vars_E2,
    Accuracy  = r_temp$Accuracy,   # <-- Nueva columna agregada
    Precision = r_temp$Precision,
    Recall    = r_temp$Recall,
    F1Score   = r_temp$F1Score,
    Time_S    = julia_temp$Tiempo
  )
  
  # 6. Acumular
  resumen1 = rbind(resumen1, fila_completa)
}

# Limpieza final de nombres de dataset (quitar 1.2., etc)
resumen1$Dataset <- sub("^[0-9]+\\.[0-9]+\\.", "", resumen1$Dataset)

# Mostrar tabla final
print(resumen1)





