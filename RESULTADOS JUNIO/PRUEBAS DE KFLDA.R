rm(list=ls())

#### Cargar las librerias para el KFDA ####
#install.packages("lfda") #Local Fisher Discriminant Analysis
library(lfda)

library(lfda)

k <- kmatrixGauss(iris[, -5], sigma = 0.9)
y  <- iris[, 5]
n  <- nrow(iris)   # 150 observaciones

# ── KLFDA original (con localidad) ──────────────────────────────
A_local <- klfda(k, y, r = 2, metric = "plain", knn = 10)

# ── KLFDA sin localidad ≈ KFDA puro ─────────────────────────────
# knn = n hace que TODOS los puntos sean "vecinos" de todos
# → pesos de afinidad uniformes → no hay estructura local
A_global <- klfda(k, y, r = 2, metric = "plain", knn = 49)


k <- kmatrixGauss(iris[, -5],sigma = 0.9)
y <- iris[, 5]
r <- 3
A = klfda(k, y, r=2, metric = "plain",knn = 10)
A$T
A$Z
A$T |> dim()

plot(A$Z[,1:2],pch = 3,
     col = iris$Species)



### Cargar las librerias necesarias ###
library(caret)
library(JuliaCall)
library(MASS)
library(kfda)
library(e1071)
library(tidyr)
library(dplyr)
library(ggplot2)
library(PMCMRplus)
library(tsutils)

# 1.- Cargar las funciones auxiliares modulares
source("POLEXP.R")

# 2.- Definir los objetos donde están las rutas de entrada
rutaDS <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS/"
rutasDS <- list.files(path = rutaDS, full.names = TRUE)

rutaRS <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\CORRIDAS DE 30/"
rutasRS <- list.files(path = rutaRS, pattern = "^Resultados_Detalle_", full.names = TRUE)

# 3.- Definir y crear la carpeta raíz para las salidas
ruta_salidas_raiz <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\ANALISIS_ESTADISTICO_SALIDAS/"
if (!dir.exists(ruta_salidas_raiz)) dir.create(ruta_salidas_raiz, recursive = TRUE)

# Fijar vector global de semillas para reproducibilidad inter-dataset
set.seed(1234)
semillas <- sample(1:10000,30,replace = F)

# =========================================================================
# CICLO MAESTRO: PROCESAMIENTO DE LOS 8 DATASETS
# =========================================================================

for (i in 1:length(rutasDS)) {
  
  # 1. Extraer el nombre limpio del dataset para subcarpetas y títulos
  nombre_dataset <- tools::file_path_sans_ext(basename(rutasDS[i]))
  
  # 2. Crear subcarpeta física exclusiva para el dataset actual
  carpeta_destino <- paste0(ruta_salidas_raiz, nombre_dataset, "/")
  if (!dir.exists(carpeta_destino)) dir.create(carpeta_destino)
  
  cat("=========================================================================\n")
  cat("INICIANDO PIPELINE: ", nombre_dataset, " (Iteración ", i, " de ", length(rutasDS), ")\n", sep="")
  cat("=========================================================================\n")
  
  # 3. Cargar base de datos original y estandarizar la variable objetivo
  df_original <- read.csv(rutasDS[i])
  df_original[, ncol(df_original)] <- factor(df_original[, ncol(df_original)])
  names(df_original)[ncol(df_original)] <- "Clase"
  
  # 4. Cargar el detalle de las 30 soluciones del Algoritmo Genético
  res_gnlpda <- read.csv(rutasRS[i])
  
  # 5. Ejecutar la expansión polinomial en Julia a través del Wrapper
  df_expandido_completo <- data.frame(
    polexpj(df_original[, -ncol(df_original)]),
    Clase = df_original$Clase
  )
  
  # 6. Inicializar contenedor para guardar las 30 precisiones por semilla
  resultados_particiones <- data.frame(
    Semilla    = 1:30,
    LDA_MASS   = NA,
    KFDA       = NA,
    QDA        = NA,
    SVM_Radial = NA,
    SVM_Poly2  = NA,
    SVM_Poly3  = NA,
    GNLPDA     = NA
  )
  
  # -------------------------------------------------------------------------
  # CICLO INTERNO: LAS 30 SEMILLAS
  # -------------------------------------------------------------------------
  for (s in 1:30) {
    
    # Construir el dataset GNLPDA específico (Solución del AG en la fila s)
    df_gnlpda_optimo <- obtener_dataset_gnlpda(df_expandido_completo, res_gnlpda, idx_fila = s)
    names(df_gnlpda_optimo)
    # Partición balanceada 70/30 estratificada
    set.seed(semillas[s])
    indices_train <- createDataPartition(df_original$Clase, p = 0.70, list = FALSE)
    
    # Conjuntos de entrenamiento y prueba (Originales y Optimizados)
    train_orig <- df_original[indices_train, ]
    test_orig  <- df_original[-indices_train, ]
    
    train_gnlpda <- df_gnlpda_optimo[indices_train, ]
    test_gnlpda  <- df_gnlpda_optimo[-indices_train, ]
    
    # --- Evaluación 1: LDA MASS ---
    modelo_lda <- lda(Clase ~ ., data = train_orig)
    pred_lda   <- predict(modelo_lda, newdata = test_orig)$class
    resultados_particiones$LDA_MASS[s] <- mean(pred_lda == test_orig$Clase)
    
    # --- Evaluación 2: KFDA ---
    modelo_kfda <- kfda(train_orig, kernel.name = "rbfdot", kpar.sigma = 0.0001)
    pred_kfda   <- kfda.predict(modelo_kfda, test_orig)
    resultados_particiones$KFDA[s] <- mean(pred_kfda$class == test_orig$Clase)
    
    # =========================================================================
    # EVALUACIÓN 3: QDA (Protección Absoluta con tryCatch)
    # =========================================================================
    
    resultados_particiones$QDA[s] <- tryCatch({
      # 1. Validar primero la condición básica de dimensiones
      num_features <- ncol(train_orig) - 1
      min_obs_clase <- min(table(train_orig$Clase))
      
      if (num_features >= min_obs_clase) {
        stop("Condición matemática básica incumplida (p >= n_clase)")
      }
      
      # 2. Intentar ajustar el modelo y predecir si pasa el filtro previo
      modelo_qda <- qda(Clase ~ ., data = train_orig)
      pred_qda   <- predict(modelo_qda, newdata = test_orig)$class
      
      # Retornar el cálculo de la precisión
      mean(pred_qda == test_orig$Clase)
      
    }, error = function(e) {
      # Si ocurre rank deficiency o cualquier otro error, imprime un aviso discreto y retorna NA
      # cat("  [Aviso QDA Semilla", s, "]:", e$message, "\n") # Descomentar para debuggear
      return(NA)
    })

    # --- Evaluación 4: SVM Radial ---
    g_gamma <- 1 / (ncol(train_orig) - 1)
    modelo_svm_rad <- svm(Clase ~ ., data = train_orig, kernel = "radial", cost = 10, gamma = g_gamma)
    pred_svm_rad   <- predict(modelo_svm_rad, newdata = test_orig)
    resultados_particiones$SVM_Radial[s] <- mean(pred_svm_rad == test_orig$Clase)
    
    # --- Evaluación 5: SVM Poly 2 ---
    modelo_svm_p2 <- svm(Clase ~ ., data = train_orig, kernel = "polynomial", degree = 2, cost = 10)
    pred_svm_p2   <- predict(modelo_svm_p2, newdata = test_orig)
    resultados_particiones$SVM_Poly2[s] <- mean(pred_svm_p2 == test_orig$Clase)
    
    # --- Evaluación 6: SVM Poly 3 ---
    modelo_svm_p3 <- svm(Clase ~ ., data = train_orig, kernel = "polynomial", degree = 3, cost = 10)
    pred_svm_p3   <- predict(modelo_svm_p3, newdata = test_orig)
    resultados_particiones$SVM_Poly3[s] <- mean(pred_svm_p3 == test_orig$Clase)
    
    # --- Evaluación 7: GNLPDA (Método Propuesto) ---
    resultados_particiones$GNLPDA[s] <- tryCatch({
      modelo_gnlpda <- lda(Clase ~ ., data = train_gnlpda)
      pred_gnlpda   <- predict(modelo_gnlpda, newdata = test_gnlpda)$class
      mean(pred_gnlpda == test_gnlpda$Clase)
     }, error = function(e) {
      # Almacena NA si el muestreo genera una variable constante o colinealidad extrema
      return(NA)
    })
  }
  # Rellenar especificamente para hacer funcional para el caso 7 
  #resultados_particiones$GNLPDA[is.na(resultados_particiones$GNLPDA)] = mean(resultados_particiones$GNLPDA,na.rm = T )
  # -------------------------------------------------------------------------
  # EXPORTACIÓN DEL ENTREGABLE 1: Matriz de Precisiones Crudas
  # -------------------------------------------------------------------------
  write.csv(resultados_particiones, file = paste0(carpeta_destino, nombre_dataset, "_precisiones_30.csv"), row.names = FALSE)
  
  # -------------------------------------------------------------------------
  # EXPORTACIÓN DEL ENTREGABLE 2: Boxplot Comparativo (ggplot2)
  # -------------------------------------------------------------------------
  df_long <- resultados_particiones %>% 
    pivot_longer(cols = -Semilla, names_to = "Metodo", values_to = "Precision") %>% 
    mutate(Metodo = as.factor(Metodo), Semilla = as.factor(Semilla)) %>% 
    filter(!is.na(Precision)) %>% 
    as.data.frame()
  # Refactorizar para evitar problemas en el caso que se desecha todo
  df_long$Metodo = factor(df_long$Metodo)
  
  g_box <- ggplot(df_long, aes(x = Metodo, y = Precision, fill = Metodo)) +
    geom_boxplot(alpha = 0.7) + 
    labs(title = paste0("Distribución de Precisión: ", nombre_dataset), x = "Clasificador", y = "Acc") +
    theme_bw() + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "none")
  
  ggsave(filename = paste0(carpeta_destino, nombre_dataset, "_boxplot.png"), plot = g_box, width = 6, height = 4.5, dpi = 300)
  
  # -------------------------------------------------------------------------
  # EXPORTACIÓN DEL ENTREGABLE 3: Cálculos Estadísticos Avanzados
  # -------------------------------------------------------------------------
  prueba_friedman <- friedman.test(Precision ~ Metodo | Semilla, data = df_long)
  prueba_nemenyi <- NULL
  
  if (prueba_friedman$p.value < 0.05) {
    prueba_nemenyi <- frdAllPairsNemenyiTest(y = df_long$Precision, groups = df_long$Metodo, blocks = df_long$Semilla)
  }
  
  exportar_reportes_hipotesis(prueba_friedman, prueba_nemenyi, carpeta_destino, nombre_dataset)
  
  # -------------------------------------------------------------------------
  # EXPORTACIÓN DEL ENTREGABLE 4: Critical Difference Diagram (Nemenyi / tsutils)
  # -------------------------------------------------------------------------
  matriz_precisiones <- resultados_particiones %>% 
    select(-Semilla) %>% 
    select(where(~ !all(is.na(.)))) %>% 
    as.matrix()
  
  png(filename = paste0(carpeta_destino, nombre_dataset, "_cd_diagram.png"), width = 7, height = 4, units = "in", res = 300)
  
  # La función dibuja las barras y calcula los rangos medios en base a Demšar
  nemenyi(matriz_precisiones, conf.level = 0.95, plottype = "mcb")
  
  dev.off()
  
  cat("¡FINALIZADO CON ÉXITO!: Archivos respaldados en subcarpeta /", nombre_dataset, "/\n\n", sep="")
}

cat("=========================================================================\n")
cat("PROCESS COMPLETED: Todas las bases de datos fueron evaluadas con éxito.\n")
cat("=========================================================================\n")

