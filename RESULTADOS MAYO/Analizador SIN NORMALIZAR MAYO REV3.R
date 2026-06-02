rm(list=ls())

### Cargar las librerias necesarias ###
library(caret)
library(JuliaCall)
library(MASS)

# 1.- Cargar las funciones auxiliares 
source("POLEXP.R")

# 2.- Definir los objetos donde estan las rutas
### Rutas de los datos ###
rutaDS = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS MAYO\\DATASETS\\" # Windows
#rutaDS <- "~//Desktop/GNLPDA REV1/RESULTADOS ABRIL/DATASETS/" # UBUNTU
rutasDS <- list.files(
  path = rutaDS,
  full.names = TRUE)
### Ruta de los resultados ###
rutaRS = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS MAYO\\CORRIDAS DE 30/" # Windows
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
i = 1
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
  #datosEST <- as.data.frame(scale(datosDS[, -ncol(datosDS)]))
  # Separar la variable objetivo
  target   <- as.factor(datosDS[, ncol(datosDS)])
  
  # Pasamos a Julia solo lo necesario para expandir una sola vez
  datosFIL  <- datosDS[, vars_e1_all]
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
      #### Solamente para el concjunto de cancer Wisconsin ####
      if(i == 7){
        # Separar la data de prueba de la data de entrenamiento
        train_data <- df_eval[-folds[[f]],-22] # Solo para wisconsin
        test_data  <- df_eval[folds[[f]],-22]
        #dim(train_data);dim(test_data)
      }else{
        # Separar la data de prueba de la data de entrenamiento
        train_data <- df_eval[-folds[[f]], ]
        test_data  <- df_eval[folds[[f]], ]
        #dim(train_data);dim(test_data)
      }
      
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
  mejorcorrida = order(resultados_30_corridas$Accuracy,decreasing = T)[1]
  
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
  write.csv(res_final_ds, paste0(rutaRS, "Resumen_Final_MAYO_R_", nombre_ds), row.names = FALSE)
  print(res_final_ds)
}

#### Ajustador de resultados del primer método ####
library(dplyr)
library(purrr)

# Listado de las rutas #
rutas_gpres <- list.files(path = rutaRS, 
                          pattern = paste0("^Resumen_Final_MAYO_R_"), 
                          full.names = TRUE)
rutas_respol = list.files(path = rutaRS, 
                          pattern = paste0("Resultados_Detalle_"), 
                          full.names = TRUE)

# Lista para almacenar los resultados de cada iteración

lista_resultados <- list()

# Función auxiliar para formatear Media ± Desviación
format_stat <- function(mean_val, sd_val) {
  sprintf("%.2f ± %.2f", mean_val, sd_val)
}
## Recorrer sobre todos los conjuntos ##
for (i in 1:length(rutas_gpres)) {
  
  # Lectura de los archivos correspondientes al índice i
  m_actual  <- read.csv(rutas_gpres[i])
  km_actual <- read.csv(rutas_respol[i])
  
  # 1. Procesar km (Detalle de 30 observaciones)
  km_stats <- km_actual %>%
    summarise(
      VarsE1_fmt = format_stat(mean(Vars_E1, na.rm = TRUE), sd(Vars_E1, na.rm = TRUE)),
      VarsE2_fmt = format_stat(mean(Vars_E2, na.rm = TRUE), sd(Vars_E2, na.rm = TRUE)),
      Time_fmt   = format_stat(mean(Tiempo, na.rm = TRUE), sd(Tiempo, na.rm = TRUE)),
      # Guardamos el promedio de Vars_E1 solo para el conteo entero solicitado
      No_Vars    = round(mean(Vars_E1, na.rm = TRUE), 0)
    )
  
  # 2. Procesar m (Resumen de métricas) y combinar
  resumen_dataset <- m_actual %>%
    mutate(
      Accuracy_fmt  = format_stat(Mean_Accuracy, Std_Accuracy),
      Precision_fmt = format_stat(Mean_Precision, Std_precision),
      Recall_fmt    = format_stat(Mean_Recall, Std_Recall),
      F1_fmt        = format_stat(Mean_F1, Std_F1),
      # Limpiar nombre del dataset (quita prefijos y extensiones si existen)
      Dataset_Clean = gsub("^\\d+\\.|\\.csv$", "", Dataset)
    ) %>%
    bind_cols(km_stats) %>%
    select(
      Dataset = Dataset_Clean,
      `No. Vars` = No_Vars,
      `Vars E1`  = VarsE1_fmt,
      `Vars E2`  = VarsE2_fmt,
      Accuracy   = Accuracy_fmt,
      Precision  = Precision_fmt,
      Recall     = Recall_fmt,
      F1Score    = F1_fmt,
      `Time (S)` = Time_fmt
    )
  
  lista_resultados[[i]] <- resumen_dataset
}

# Consolidar todos los datasets en una sola tabla final
tabla_final_GLP <- bind_rows(lista_resultados)

# Visualizar la tabla resultante
print(tabla_final_GLP)
# Escribir para el excel #
write.csv(tabla_final_GLP,"VALIDACION MAYO RENDIMIENTO.csv",row.names = F)

#############


#### PipeLine para aplicar los kfolds a todos los clasificadores ####

#### Competidores ####
#rm(list=ls())

library(caret)
library(MASS)
library(e1071)
library(randomForest)

# 1. Configuración de Rutas
#rutaDS <- "/home/angel/Desktop/GNLPDA REV/RESULTADOS MAYO/DATASETS/" #Linux
#rutaRS <- "/home/angel/Desktop/GNLPDA REV/RESULTADOS MAYO/CORRIDAS DE 30/"
rutaDS = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS MAYO\\DATASETS\\" # Windows
rutasDS <- list.files(path = rutaDS, full.names = TRUE)
### Ruta de los resultados ###
rutaRS = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS MAYO\\CORRIDAS DE 30/" # Windows
#rutaRS <- "~//Desktop/GNLPDA REV1/RESULTADOS ABRIL/CORRIDAS DE 30 NORMALIZADAS/"
rutasRS <- list.files(
  path = rutaRS,
  pattern = "^Resultados_Detalle_",
  full.names = TRUE)


# 2. Semilla Maestra (DEBE SER LA MISMA QUE EN TU OTRO SCRIPT)
semilla_maestra <- 1019

# 3. Definición de Competidores
# Agregamos SVM con Kernels Polinomiales
metodos <- c("LDA_Simple", "QDA", "SVM_Radial",
             "SVM_Poly2", "SVM_Poly3", "RF")
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
  datosEST <- as.data.frame(datosDS[, -ncol(datosDS)])
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
      # Separar en prueba y entrenamiento
      train_data <- datosEST[-folds[[f]], ]
      test_data  <- datosEST[folds[[f]], ]
      # Guardar el ajuste del modelo y sus predicciones
      model_fit <- NULL
      preds <- NULL
      
      # Lógica de entrenamiento por método con manejo de errores
      try({
        if (m == "LDA_Simple") {
          # Armar y predecir el LDA
          model_fit <- lda(Clase ~ ., data = train_data)
          preds <- predict(model_fit, test_data)$class
        } else if (m == "QDA") {
          # Armar y predecir el QDA
          model_fit <- qda(Clase ~ ., data = train_data)
          preds <- predict(model_fit, test_data)$class
          # Armar y predecir el kernel radial
        } else if (m == "SVM_Radial") {
          model_fit <- svm(Clase ~ ., data = train_data, kernel = "radial", cost = 10, gamma = 1/ncol(datosEST))
          preds <- predict(model_fit, test_data)
          # Armar y predecir el kernel polinomial 2
        } else if (m == "SVM_Poly2") {
          model_fit <- svm(Clase ~ ., data = train_data, kernel = "polynomial", degree = 2, cost = 10)
          preds <- predict(model_fit, test_data)
          # Armar y predecir el kernel polinomial 3
        } else if (m == "SVM_Poly3") {
          model_fit <- svm(Clase ~ ., data = train_data, kernel = "polynomial", degree = 3, cost = 10)
          preds <- predict(model_fit, test_data)
          # Armar y hacer los RF
        } else if (m == "RF") {
          model_fit <- randomForest(Clase ~ ., data = train_data, ntree = 500)
          preds <- predict(model_fit, test_data)
        }
      }, silent = TRUE)
      
      # Cálculo de métricas si hubo éxito en el entrenamiento
      if (!is.null(preds)) {
        # Validación extra: Verificar que las predicciones no sean puras NAs
        if(!all(is.na(preds))){
          # Armar la matriz de confusión
          cm <- confusionMatrix(preds, test_data$Clase, mode = "everything")
          # Implementar logica por si son 1 o 2 clases
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
  archivo_salida <- paste0(rutaRS, "Competidores_Final_MAYO", nombre_ds)
  write.csv(res_competidores, archivo_salida, row.names = FALSE)
  print(res_competidores)
}

#### Resumir las 2 tablas en una sola ####

library(tidyverse)

# Función auxiliar para asegurar el formato de 3 decimales
format_stat <- function(m, s) sprintf("%.2f ± %.2f", m, s)

tabla_comparativa <- data.frame()

for(i in seq_along(archivos_comp)) {
  # 1. Cargar archivo de competidores
  df_c <- read.csv(archivos_comp[i]) 
  
  # 2. Crear la cadena "Media ± SD" y pivotar
  # Se asume que df_c tiene las columnas: Metodo, Accuracy, Std_Acc
  temp_metodos <- df_c %>%
    mutate(Fmt = format_stat(Accuracy, Std_Acc)) %>%
    select(Metodo, Fmt) %>%
    spread(Metodo, Fmt)
  
  # 3. Limpiar nombre del dataset para la fila actual
  nombre_base <- basename(archivos_comp[i])
  temp_metodos$Dataset <- gsub("Competidores_Final_MAYO_|.csv", "", nombre_base)
  
  # Reorganizar: Dataset primero, luego los métodos
  temp_metodos <- temp_metodos %>% select(Dataset, everything())
  
  # Acumular en la tabla maestra
  tabla_comparativa <- rbind(tabla_comparativa, temp_metodos)
}

# Resultado final listo para concatenar GNLPDA después
print(tabla_comparativa)
write.csv(tabla_comparativa,"Rendimiento de los competidores MAYO.csv",row.names = F)

#### Reportar los resultados significativos de GLPNDA

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
  ds_nombre_target <- basename(tabla_comparativa$Dataset[i])
  
  # Extraer la fila correspondiente de la tabla comparativa
  fila_ref <- tabla_comparativa[tabla_comparativa$Dataset == ds_nombre_target, ]
  
  # C. Crear una fila para los p-values de este dataset
  # Empezamos con el nombre del dataset
  fila_p_values <- data.frame(Dataset = sub("\\.csv$", "", ds_nombre_target))
  
  for (metodo in competidores_nombres) {
    # Fijar como valor de la media
    valor_mu <- as.numeric(sub(" ±.*", "", fila_ref[[metodo]]))
    
    # Si el valor es NA (porque el método falló), ponemos NA en el p-value
    if (is.na(valor_mu)) {
      fila_p_values[[metodo]] <- NA
    } else {
      # PRUEBA DE WILCOXON DE UNA MUESTRA
      # HA: GNLPDA != valor_mu
      test_w <- wilcox.test(acc_vector, 
                            mu = valor_mu, 
                            alternative = "greater")
      
      # Guardamos el p-value formateado
      fila_p_values[[metodo]] <- round(test_w$p.value, 4)
    }
  }
  # Revisar las significancias
  resumen_significancia_detalle <- rbind(resumen_significancia_detalle, fila_p_values)
  round(resumen_significancia_detalle[,-1],4)
}

resumen_significancia_detalle

#### Aquí empieza el bloque del reportador de resultados ####
rm(list=ls())
#rutaDS <- "~/Desktop/GNLPDA/RESULTADOS MAYO/DATASETS/"
#rutaRS <- "~/Desktop/GNLPDA REV1/RESULTADOS MAYO/CORRIDAS DE 30/"
### Ruta de los resultados ###
rutaDS = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS MAYO\\DATASETS\\" # Windows
rutaRS = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS MAYO\\CORRIDAS DE 30/" # Windows

rutasDS <- list.files(path = rutaDS, full.names = TRUE);rutasDS
rutasRS <- list.files(
  path = rutaRS,
  pattern = "^Resultados_Detalle_",
  full.names = TRUE);rutasRS



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


library(dplyr)

# Función para formatear Media ± Desviación
# Se usa %.2f por tratarse de porcentajes, pero puede usar %.3f si requiere más precisión
format_pct <- function(m, s) sprintf("%.2f ± %.2f", m, s)

resudrop_simplificada <- resudropvariables %>%
  mutate(
    `Vars E1 (%)` = format_pct(Vars_E1p, Vars_E1des),
    `Vars E2 (%)` = format_pct(Vars_E2p, Vars_E2des)
  ) %>%
  select(Nombre, `Vars E1 (%)`, `Vars E2 (%)`)

# Visualizar el resultado
print(resudrop_simplificada)

write.csv(resudrop_simplificada,"Resultados Explicabilidad.csv",row.names = F)

############


#### Resumir que show con el cancer de wisconsin ###

ruta = "DATASETS/4.1.CancerWisconsin.csv"
df1 = read.csv(ruta)

plot(df1[,1:5],pch = 16,
     col = as.factor(df1$Diagnosis))

plot(df1[,6:10],pch = 16,
     col = as.factor(df1$Diagnosis))


png("plot1.png", width = 1600, height = 900, res = 150)

plot(df1[,1:5], pch = 16,
     col = as.factor(df1$Diagnosis))

dev.off()


png("plot2.png", width = 1600, height = 900, res = 150)

plot(df1[,6:10], pch = 16,
     col = as.factor(df1$Diagnosis))

dev.off()


A1 =princomp(df1[,-ncol(df1)])

plot(A1$scores[,1:2],
     col = as.factor(df1$Diagnosis),
     pch = 16)

#### Gráfico rapido de la IONOSPHERA ####

df1 = read.csv(rutasDS[8])

png("plotIONOSPHERA.png", width = 1600, height = 900, res = 150)

plot(df1[,3:8], pch = 16,
     col = as.factor(df1$Class))

dev.off()
