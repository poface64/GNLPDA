
##### Cargar las funciones de Julia ####
#library(JuliaCall)
#julia_setup()
#julia
#julia_source("POLEXP REV1.jl")

#### Extraer la mejor corrida de cada método por cada dataset ####
library(dplyr)

obtener_mejor_corrida <- function(ruta_carpeta, nombre_dataset, metodo_tag) {
  
  # 1. Localizar el archivo específico en la carpeta mediante concordancia de patrones
  # Se busca que inicie con el nombre exacto del dataset y contenga el tag del método
  patron <- paste0("^", regexp_quote(nombre_dataset), ".*_", metodo_tag, "\\..*$")
  archivo_encontrado <- list.files(path = ruta_carpeta, pattern = patron, full.names = TRUE)
  
  # Control preventivo en caso de discrepancias en nombres de archivos
  if (length(archivo_encontrado) == 0) {
    warning(sprintf("[!] No se encontró bitácora para %s en %s", nombre_dataset, metodo_tag))
    return(NULL)
  }
  
  # Si encuentra múltiples coincidencias por error del sistema, se toma la primera
  df_log <- read.csv(archivo_encontrado[1], stringsAsFactors = FALSE)
  
  # 2. Identificar el renglón con el Accuracy de prueba más alto
  # Se asume que la columna de rendimiento se llama 'Accuracy' de forma estándar
  # Nota: Se desempata tomando la primera ocurrencia en caso de rendimientos idénticos
  mejor_renglon <- df_log %>% 
    arrange(desc(Accuracy)) %>% 
    slice(1)
  
  # Añadir un identificador explícito del método para la trazabilidad futura
  mejor_renglon$Metodo_Origen <- metodo_tag
  
  return(mejor_renglon)
}

# Función auxiliar interna para escapar caracteres especiales de rutas (puntos) en expresiones regulares
regexp_quote <- function(kv) {
  gsub("([\\.\\+\\*\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", kv)
}


#### Reconstruir partición Ganadora por dataset y por método ####

# =========================================================================
# COMPONENTE 3: RECONSTRUCTOR DE PARTICIONES BASADO EN MATRIZ DE ASIGNACIÓN
# =========================================================================

reconstruir_particion_ganadora <- function(ruta_datasets, ruta_semillas, nombre_dataset, id_mejor_corrida) {
  
  # 1. Cargar el dataset original completo
  # Buscamos el archivo CSV que inicie exactamente con el prefijo del dataset
  archivo_ds <- list.files(path = ruta_datasets, pattern = paste0("^", regexp_quote(nombre_dataset), "\\.csv$"), full.names = TRUE)
  
  if (length(archivo_ds) == 0) {
    stop(sprintf("[CRÍTICO] No se encontró el dataset original para: %s", nombre_dataset))
  }
  
  df_original <- read.csv(archivo_ds[1], stringsAsFactors = FALSE)
  
  # 2. Cargar la matriz de asignación de semillas/particiones
  # Buscamos el archivo de asignación en la carpeta de semillas generadas
  archivo_semilla <- list.files(path = ruta_semillas, pattern = paste0("^", regexp_quote(nombre_dataset), ".*\\.csv$"), full.names = TRUE)
  
  if (length(archivo_semilla) == 0) {
    stop(sprintf("[CRÍTICO] No se encontró el archivo de particiones para: %s", nombre_dataset))
  }
  
  df_particiones <- read.csv(archivo_semilla[1], stringsAsFactors = FALSE)
  
  # 3. Construir dinámicamente el nombre de la columna (e.g., "Run_12")
  columna_run <- paste0("Run_", id_mejor_corrida)
  
  # Validación de seguridad por si las columnas en las bitácoras no coinciden numéricamente
  if (!columna_run %in% names(df_particiones)) {
    stop(sprintf("[CRÍTICO] La columna %s no existe en la matriz de particiones de %s", columna_run, nombre_dataset))
  }
  
  # 4. Segmentar el dataset original en vectores/matrices de Entrenamiento y Prueba
  # Usamos la columna de asignación como pivote de indexación condicional
  es_train <- df_particiones[[columna_run]] == "Train"
  es_test  <- df_particiones[[columna_run]] == "Test"
  
  #### Contar cuantas columnas tiene el dataset ####
  ncolumns = ncol(df_original)
  
  # Separar variables predictoras (X) y etiquetas (y) asumiendo que 'class' es la última columna
  X_total <- df_original[, -ncolumns]
  y_total <- as.factor(df_original[,ncolumns])
  
  # Empaquetado final estructurado en una lista para el consumo de los modelos
  return(list(
    X_train = X_total[es_train, , drop = FALSE],
    y_train = y_total[es_train],
    X_test  = X_total[es_test, , drop = FALSE],
    y_test  = y_total[es_test],
    clases_niveles = levels(y_total)
  ))
}

#### Proyector de LDA MASS ####

proyectar_lda_mass <- function(particion_mass) {
  # 1. Expandir polinomialmente entrenamiento y prueba vía Julia
  cat("   [MASS] Ejecutando LDA MASS")
  X_train_exp <- particion_mass$X_train
  X_test_exp  <- particion_mass$X_test
  
  # 2. Ajustar el modelo con el set de entrenamiento expandido
  modelo_mass <- lda(X_train_exp, grouping = particion_mass$y_train)
  
  # 3. Proyectar el set de prueba
  proyeccion_test <- predict(modelo_mass, newdata = X_test_exp)$x
  
  return(as.data.frame(proyeccion_test))
}

#### Proyector de KFDA ####

# =========================================================================
# COMPONENTE 4.2: PROYECTOR DE PRUEBA - KFDA MANUAL
# =========================================================================

proyectar_kfda <- function(particion_kfda, sigma_ganador) {
  cat(sprintf("   [KFDA] Evaluando con Sigma RBF optimizado: %s\n", round(sigma_ganador, 4)))
  
  X_tr <- as.matrix(particion_kfda$X_train)
  X_te <- as.matrix(particion_kfda$X_test)
  
  # 1. Construir la función de Kernel con el sigma guardado en la bitácora
  funcion_rbf <- rbfdot(sigma = sigma_ganador)
  
  # 2. Calcular matrices de Kernel de entrenamiento y prueba
  K_train <- kernelMatrix(funcion_rbf, X_tr)
  K_test  <- kernelMatrix(funcion_rbf, X_te, X_tr) # Mapeo cruzado Test vs Train
  
  # 3. Entrenar y predecir/proyectar
  modelo_kfda <- kfda_entrenar(K_train, particion_kfda$y_train, r = NULL, reg = 1e-6)
  resultados_test <- kfda_predecir(modelo_kfda, K_test)
  
  return(as.data.frame(resultados_test$proyecciones))
}

#### Proyector de GNLPDA ####

# =========================================================================
# COMPONENTE 4.3: PROYECTOR DE PRUEBA - GNLPDA (MÉTODO PROPIO)
# =========================================================================

proyectar_gnlpda <- function(particion_gnlpda, cadena_variables) {
  # 1. Parsear la cadena de texto de la bitácora para aislar los nombres de las variables
  vars_seleccionadas <- unlist(strsplit(gsub(" ", "", cadena_variables), ","))
  cat(sprintf("   [GNLPDA] Subespacio optimizado detectado: %d características seleccionadas.\n", length(vars_seleccionadas)))
  
  # 2. Generar la expansión polinomial completa en Julia para poder buscar los términos
  X_train_exp <- polexpj(as.data.frame(particion_gnlpda$X_train), grado = 2)
  X_test_exp  <- polexpj(as.data.frame(particion_gnlpda$X_test),  grado = 2)
  
  # 3. Filtrar estrictamente las columnas seleccionadas por el Algoritmo Genético
  # Control preventivo por si existieran discrepancias de codificación de caracteres en los nombres
  columnas_validas <- intersect(vars_seleccionadas, names(X_train_exp))
  
  X_train_filtrado <- X_train_exp[, columnas_validas, drop = FALSE]
  X_test_filtrado  <- X_test_exp[, columnas_validas, drop = FALSE]
  
  # 4. Entrenar el clasificador sobre el espacio filtrado usando MASS
  # Nota: Se migra a lda() de MASS por su alta estabilidad numérica basada en SVD
  modelo_gnlpda_mass <- lda(X_train_filtrado, grouping = particion_gnlpda$y_train)
  
  # 5. Proyección matemática exacta sobre el conjunto de prueba
  # Extracción estricta de la matriz de transformación angular (scaling) de MASS
  matriz_coeficientes <- modelo_gnlpda_mass$scaling

  # Multiplicación matricial: (Muestras_Test x Vars_Filtradas) %*% (Vars_Filtradas x Dimensiones)
  proyeccion_test <- as.matrix(X_test_filtrado) %*% matriz_coeficientes  
  return(as.data.frame(proyeccion_test))
}

#### catalogo de archivos ####

# Listar los archivos usando como pivote la carpeta de KFDA (o cualquier otra de resultados)
archivos_logs_pivote <- list.files(path = ruta_logs_kfda, pattern = "\\.csv$")

# Extraer el prefijo limpio: Todo lo que esté antes del primer símbolo "_"
datasets_maestros <- unique(gsub("(^[^_]+)_.*", "\\1", archivos_logs_pivote))

cat("Catálogo de conjuntos de datos reales identificados para el análisis:\n")
print(datasets_maestros)

#### Organizador de variables ####


#### Organizador de variables ####

organizar_variables_por_tipo <- function(vector_vars) {
  
  # 1. Identificar Términos Lineales (No tienen ^ ni *)
  
  lineales <- vector_vars[!grepl("\\^|\\*", vector_vars)]
  
  
  
  # 2. Identificar Términos Cuadráticos (Tienen ^2)
  
  cuadraticos <- vector_vars[grepl("\\^2", vector_vars)]
  
  
  
  # 3. Identificar Interacciones (Tienen * pero NO son cuadráticos)
  
  # Nota: Una variable como V1^2*V2 entraría aquí si existiera, 
  
  # pero en grado 2 puro son solo V1*V2
  
  interacciones <- vector_vars[grepl("\\*", vector_vars) & !grepl("\\^", vector_vars)]
  
  
  
  # 4. Otros (Por si existiera algo fuera de grado 2)
  
  otros <- setdiff(vector_vars, c(lineales, cuadraticos, interacciones))
  
  
  
  # Retornar lista organizada o vector concatenado
  
  return(list(
    
    Lineales = sort(lineales),
    
    Cuadraticos = sort(cuadraticos),
    
    Interacciones = sort(interacciones),
    
    Ordenado = c(sort(lineales), sort(cuadraticos), sort(interacciones), sort(otros))
    
  ))
  
}



#### Organizador gráfico ####

library(stringr)

organizar_variables_gráficas <- function(vector_vars) {
  
  # 1. Lineales
  
  lineales <- vector_vars[!grepl("\\^|\\*", vector_vars)]
  
  
  
  # 2. Cuadráticos
  
  cuadraticos_raw <- vector_vars[grepl("\\^2", vector_vars)]
  
  cuadraticos_formateados <- str_replace_all(cuadraticos_raw, "\\^2", "²")
  
  
  
  # 3. Interacciones
  
  interacciones_raw <- vector_vars[grepl("\\*", vector_vars) & !grepl("\\^", vector_vars)]
  
  interacciones_formateadas <- interacciones_raw  # ya están tipo X1*X2
  
  
  
  # 4. Otros
  
  otros_raw <- setdiff(vector_vars, c(lineales, cuadraticos_raw, interacciones_raw))
  
  otros_formateados <- str_replace_all(otros_raw, "\\^2", "²")
  
  
  
  return(list(
    
    # Conteos
    
    Lineales_Count = length(lineales),
    
    Cuadraticos_Count = length(cuadraticos_raw),
    
    Interacciones_Count = length(interacciones_raw),
    
    
    
    # NUEVO: vectores por tipo (YA ORDENADOS Y FORMATEADOS)
    
    Lineales = sort(lineales),
    
    Cuadraticos = sort(cuadraticos_formateados),
    
    Interacciones = sort(interacciones_formateadas),
    
    Otros = sort(otros_formateados),
    
    
    
    # Compatibilidad con lo que ya usabas
    
    Leyenda_Formateada = c(sort(lineales), 
                           
                           sort(cuadraticos_formateados), 
                           
                           sort(interacciones_formateadas), 
                           
                           sort(otros_formateados))
    
  ))
  
}

