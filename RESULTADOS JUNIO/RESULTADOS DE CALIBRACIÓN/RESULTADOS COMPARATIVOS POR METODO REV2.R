rm(list=ls())

# ==============================================================================
# SCRIPT UNIFICADOR DE RESULTADOS A FORMATO LARGO MULTIVARIABLE (ACC + F1)
# ==============================================================================

# CONFIGURACIÓN DE RUTAS DE LA TESIS
ruta_base        <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN/"
ruta_datasets    <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS/"
ruta_consolidado <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"

if(!dir.exists(ruta_consolidado)) dir.create(ruta_consolidado, recursive = TRUE)

# Definición de los 7 métodos y sus respectivas subcarpetas y sufijos
metodos <- list(
  GNLPDA    = list(carpeta = "RESULTADOS_GNLPDA",    sufijo = "_res_gnlpda.csv"),
  KFDA      = list(carpeta = "RESULTADOS_KFDA",      sufijo = "_res_kfda.csv"),
  SVM_RBF   = list(carpeta = "RESULTADOS_SVM_RBF",   sufijo = "_res_svm_rbf.csv"),
  SVM_POLY2 = list(carpeta = "RESULTADOS_SVM_POLY2", sufijo = "_res_svm_poly2.csv"),
  SVM_POLY3 = list(carpeta = "RESULTADOS_SVM_POLY3", sufijo = "_res_svm_poly3.csv"),
  LDA       = list(carpeta = "RESULTADOS_LDA",       sufijo = "_res_lda.csv"),
  QDA       = list(carpeta = "RESULTADOS_QDA",       sufijo = "_res_qda.csv")
)

# Obtener los nombres de los datasets mapeados (mismo filtro del framework)
archivos_csv <- list.files(path = ruta_datasets, pattern = "\\.csv$", full.names = TRUE)
archivos_csv <- archivos_csv[!grepl("_partitions", archivos_csv)]

cat("==================================================================\n")
cat("       INICIANDO CONSOLIDACIÓN MULTIVARIABLE (ACCURACY + F1)      \n")
cat("==================================================================\n")

# BUCLE PRINCIPAL: Procesar dataset por dataset
for (archivo in archivos_csv) {
  
  nombre_base <- tools::file_path_sans_ext(basename(archivo))
  cat("\nConsolidando información para el dataset:", nombre_base, "\n")
  
  # Lista temporal para almacenar los data.frames en formato largo de cada método
  lista_long_dataset <- list()
  
  # DEFINICIÓN DEL FILTRO DE EXCLUSIÓN HOMOGÉNEO (30 Bloques de Friedman)
  # Omitimos la corrida 11, 32 y 33 de las bitácoras que contienen las 33 originales
  corridas_validas <- c(1:10, 12:31)
  
  # BUCLE INTERMEDIO: Extraer datos de cada método para el dataset I-esimo
  for (nombre_metodo in names(metodos)) {
    info_metodo <- metodos[[nombre_metodo]]
    path_archivo_res <- file.path(ruta_base, info_metodo$carpeta, paste0(nombre_base, info_metodo$sufijo))
    
    # Control de seguridad por si un método colapsó o no tiene el archivo generado
    if (!file.exists(path_archivo_res)) {
      cat("  [!] Advertencia: Archivo no encontrado para el método", nombre_metodo, "\n")
      next
    }
    
    # Leer el csv histórico del competidor
    df_res_metodo <- read.csv(path_archivo_res)
    
    # ── BLOQUE UNIFICADO DE EXTRACCIÓN HISTÓRICA (ACCURACY + MACRO_F1) ────────
    # Dado que todos los CSV de calibración contienen las 33 corridas originales,
    # filtramos los renglones válidos y re-indexamos secuencialmente del 1 al 30.
    
    # Filtrar el dataframe crudo basándonos en el vector estricto de exclusión
    df_recortado <- df_res_metodo[df_res_metodo$Corrida %in% corridas_validas, ]

    # Armar la estructura formal en formato largo unificado
    df_long_temp <- data.frame(
      Bloque   = df_recortado[,1] , # Mapeo limpio (1 al 30) indispensable para Friedman
      Metodo   = nombre_metodo,
      Accuracy = df_recortado$Accuracy,
      Macro_F1 = df_recortado$Macro_F1,
      stringsAsFactors = FALSE
    )
    
    # Guardar en la lista temporal de consolidación
    lista_long_dataset[[nombre_metodo]] <- df_long_temp
    
  }
  
  # Combinar los registros de todos los métodos disponibles para este dataset
  if (length(lista_long_dataset) > 0) {
    df_final_long <- do.call(rbind, lista_long_dataset)
    
    # Guardar el archivo final de formato largo listo para ggplot2 u otras métricas
    write.csv(df_final_long, 
              file = file.path(ruta_consolidado, paste0(nombre_base, "_long_graficos.csv")), 
              row.names = FALSE)
    
    cat("  -> Archivo largo generado exitosamente: ", paste0(nombre_base, "_long_graficos.csv"), " (", nrow(df_final_long), " observaciones)\n")
  }
}

cat("\n==================================================================\n")
cat(" Proceso concluido. Todos los archivos de formato largo están listos.\n")
cat("==================================================================\n")

#### BOXPLOTS DINAMICOS ####

rm(list=ls())

# ==============================================================================
# SCRIPT DE GENERACIÓN DE BOXPLOTS CONFIGURABLE Y MULTIVARIABLE (ggplot2)
# ==============================================================================

library(ggplot2)
library(RColorBrewer)
library(dplyr)

# ── CONFIGURACIÓN DE PARÁMETROS DEL PIPELINE GRÁFICO ──────────────────────────
METRICA_OBJETIVO <- c("Accuracy","Macro_F1")[1]    # Opciones válidas: "Accuracy" o "Macro_F1"
MODO_CAMPEONES   <- T          # TRUE: Grafica solo campeones | FALSE: Grafica benchmark completo
METODOS_TOP      <- c("GNLPDA", "KFDA", "SVM_RBF", "SVM_POLY2", "SVM_POLY3")

# ── CONFIGURACIÓN DE RUTAS DE LA TESIS ────────────────────────────────────────
ruta_consolidado <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
ruta_figuras     <- file.path(ruta_consolidado, "GRAFICOS_R")

if (!dir.exists(ruta_consolidado)) stop("La ruta base de consolidados no existe.")
if (!dir.exists(ruta_figuras)) dir.create(ruta_figuras, recursive = TRUE)

# Encontrar los archivos consolidados unificados de 30 observaciones
archivos_long <- list.files(path = ruta_consolidado, pattern = "_long_graficos\\.csv$", full.names = TRUE)

# Diccionario maestro global para el mapeo dinámico de etiquetas en el eje X
diccionario_etiquetas <- c(
  "GNLPDA"    = "GNLPDA",
  "LDA"       = "LDA",
  "QDA"       = "QDA",
  "KFDA"      = "KFDA",
  "SVM_POLY2" = "SVM(POL2)",
  "SVM_POLY3" = "SVM(POL3)",
  "SVM_RBF"= "SVM(Gauss)"
)

# Definición de componentes de texto e indicadores dinámicos basados en la métrica
nombre_metrica <- ifelse(METRICA_OBJETIVO == "Accuracy", "Accuracy", "Macro_F1")
etiqueta_eje_y <- ifelse(METRICA_OBJETIVO == "Accuracy", "Accuracy (Precisión Global)", "Macro F1-Score")
prefijo_archivo <- ifelse(MODO_CAMPEONES, "Champ_", "")

cat("==================================================================\n")
cat("          INICIANDO RENDERIZADO AUTOMÁTICO DE BOXPLOTS            \n")
cat("  Métrica seleccionada:", nombre_metrica, "\n")
cat("  Modo Campeones activo:", MODO_CAMPEONES, "\n")
cat("==================================================================\n")

# BUCLE PRINCIPAL: Procesar cada archivo largo y generar su gráfico de cajas
archivo = archivos_long[1]
for (archivo in archivos_long) {
  
  nombre_archivo <- basename(archivo)
  nombre_base    <- gsub("_long_graficos\\.csv$", "", nombre_archivo)
  
  cat("Renderizando gráfico para:", nombre_base, "... ")
  
  # 1. Cargar el dataframe unificado (Ya viene limpio a 30 observaciones)
  df_crudo <- read.csv(archivo)
  
  # 2. Asignar dinámicamente la columna seleccionada a una variable genérica 'Valor'
  df_crudo$Valor <- df_crudo[[METRICA_OBJETIVO]]
  
  # Remover registros vacíos o NA's (Previene colapsos como el del QDA en Ionosphere)
  df_limpio <- df_crudo %>% filter(!is.na(Valor))
  
  # 3. Aplicar el filtro de inclusión de clasificadores si el interruptor está activo
  if (MODO_CAMPEONES) {
    df_grafico <- df_limpio %>% filter(Metodo %in% METODOS_TOP)
  } else {
    df_grafico <- df_limpio
  }
  
  # 4. RE-FACTORIZACIÓN DINÁMICA DE EJES: Mapear solo los métodos presentes
  metodos_presentes <- unique(df_grafico$Metodo)
  
  # Filtrar el orden jerárquico y las etiquetas que corresponden únicamente a lo que se va a graficar
  orden_niveles     <- intersect(names(diccionario_etiquetas), metodos_presentes)
  etiquetas_limpias <- diccionario_etiquetas[orden_niveles]
  
  df_grafico$Metodo <- factor(df_grafico$Metodo, levels = orden_niveles, labels = etiquetas_limpias)
  
  # ============================================================================
  # 5. CONSTRUCCIÓN DE LA ARQUITECTURA GRÁFICA CON GGPLOT2
  # ============================================================================
  p <- ggplot(df_grafico, aes(x = Metodo, y = Valor, fill = Metodo)) +
    
    # Capa de Cajas: Oculta outliers duplicados ya que se pintarán con el jitter
    geom_boxplot(outlier.shape = NA, alpha = 0.75, color = "#2c3e50", width = 0.6) +
    
    # Capa de Puntos Jitter: Distribución real de las 30 muestras
    geom_jitter(position = position_jitter(width = 0.2, height = 0), 
                color = "#34495e", size = 1.2, alpha = 0.5) +
    
    # Paleta de colores académica discreta
    scale_fill_brewer(palette = "Set2") +
    
    # Etiquetas semánticas dinámicas
    labs(
      title    = paste("Distribución de Rendimiento -", nombre_base),
      subtitle = paste("Métrica:", nombre_metrica, "| Comparativa basada en 30 bloques de evaluación independientes"),
      x        = "Algoritmo de Clasificación / Competidor",
      y        = etiqueta_eje_y
    ) +
    
    # Estética Minimalista
    theme_minimal(base_size = 12) +
    
    theme(
      plot.title         = element_text(face = "bold", size = 14, color = "#2c3e50", hjust = 0),
      plot.subtitle      = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      axis.title.x       = element_text(face = "bold", size = 11, color = "#2c3e50", margin = margin(t = 12)),
      axis.title.y       = element_text(face = "bold", size = 11, color = "#2c3e50", margin = margin(r = 12)),
      axis.text.x        = element_text(size = 10, color = "#34495e"),
      axis.text.y        = element_text(size = 10, color = "#34495e"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#ecf0f1", size = 0.5),
      legend.position    = "none"
    )
  
  # 6. EXPORTACIÓN DIRECCIONADA Y NOMENCLATURA COHERENTE
  nombre_salida_png <- file.path(ruta_figuras, paste0(prefijo_archivo, "Boxplot_", nombre_base, "_", nombre_metrica, ".png"))
  nombre_salida_pdf <- file.path(ruta_figuras, paste0(prefijo_archivo, "Boxplot_", nombre_base, "_", nombre_metrica, ".pdf"))
  
  ggsave(filename = nombre_salida_png, plot = p, width = 8.5, height = 5.5, dpi = 300)
  ggsave(filename = nombre_salida_pdf, plot = p, width = 8.5, height = 5.5)
  
  cat("[GUARDADO EN GRAFICOS_R]\n")
}

cat("\n==================================================================\n")
cat(" Proceso concluido. Catálogo de figuras unificado exitosamente.\n")
cat("==================================================================\n")


#### DIAGNOSTICO DE MEDIDAS REPETIDAS ####
rm(list = ls())
# ==============================================================================
# DIAGNÓSTICO DE SUPUESTOS PARAMÉTRICOS DINÁMICO Y MULTIVARIABLE (R)
# ==============================================================================

library(rstatix)
library(dplyr)

# ── CONFIGURACIÓN DE PARÁMETROS DEL PIPELINE DE DIAGNÓSTICO ───────────────────
METRICA_OBJETIVO <- c("Accuracy","Macro_F1")[2]    # Opciones válidas: "Accuracy" o "Macro_F1"
MODO_CAMPEONES   <- F          # TRUE: Diagnóstico solo para campeones | FALSE: Todo el benchmark
METODOS_TOP      <- c("GNLPDA", "KFDA", "SVM_RBF", "SVM_POLY2", "SVM_POLY3")

# ── CONFIGURACIÓN DE RUTAS DE LA TESIS ────────────────────────────────────────
ruta_consolidados <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
ruta_salidas_raiz <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS_ESTADISTICO_INFERENCIALES/"

if (!dir.exists(ruta_salidas_raiz)) dir.create(ruta_salidas_raiz, recursive = TRUE)

# Encontrar los archivos consolidados unificados de 30 observaciones
archivos_long <- list.files(path = ruta_consolidados, pattern = "_long_graficos\\.csv$", full.names = TRUE)

# Variables auxiliares para la automatización de nombres y etiquetas
prefijo_archivo <- ifelse(MODO_CAMPEONES, "Champ_", "")
nombre_metrica  <- ifelse(METRICA_OBJETIVO == "Accuracy", "Accuracy", "Macro_F1")

# Lista contenedora para los reportes iterativos
reporte_list <- list()

cat("==================================================================\n")
cat("          INICIANDO EVALUACIÓN DE SUPUESTOS PARAMÉTRICOS          \n")
cat("  Métrica evaluada:", nombre_metrica, "\n")
cat("  Modo Campeones activo:", MODO_CAMPEONES, "\n")
cat("==================================================================\n")

for (i in seq_along(archivos_long)) {
  
  archivo <- archivos_long[i]
  if (!file.exists(archivo)) next
  
  # 1. Cargar el dataframe (Ya viene pre-filtrado a 30 observaciones de bloque)
  datos <- read.csv(archivo)
  
  # Asignar dinámicamente la métrica a evaluar a la columna común 'Valor'
  datos$Valor <- datos[[METRICA_OBJETIVO]]
  
  # Remover registros vacíos o nulos preventivamente
  df_limpio <- datos %>% filter(!is.na(Valor))
  
  # 2. FILTRADO REACTIVO: Detectar y remover métodos con rendimiento constante (Varianza 0)
  metodos_validos <- df_limpio %>%
    group_by(Metodo) %>%
    summarise(v_acc = var(Valor, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(v_acc) & v_acc > 0) %>%
    pull(Metodo)
  
  # 3. Aplicar filtro secundario según la selección de clasificadores (Inclusión estricta)
  if (MODO_CAMPEONES) {
    df_analisis <- df_limpio %>% filter(Metodo %in% METODOS_TOP & Metodo %in% metodos_validos)
  } else {
    df_analisis <- df_limpio %>% filter(Metodo %in% metodos_validos)
  }
  
  # Forzar estructuras factoriales limpias para los modelos lineales
  df_analisis$Bloque <- factor(df_analisis$Bloque)
  df_analisis$Metodo <- factor(df_analisis$Metodo)
  
  # Extraer el nombre formal del dataset limpio de prefijos o sufijos extensos
  nombre_dataset <- tools::file_path_sans_ext(basename(archivo))
  nombre_dataset <- sub("^\\d+\\.\\d+\\.", "", nombre_dataset)
  nombre_dataset <- sub("_long_graficos$", "", nombre_dataset)
  
  # Validar que existan al menos 2 métodos para estructurar un ANOVA
  if (length(unique(df_analisis$Metodo)) < 2) {
    cat("  [!] Dataset:", nombre_dataset, "omitido por falta de variabilidad inter-clase.\n")
    next
  }
  
  # ============================================================================
  # SUPUESTO 1: AJUSTE DEL MODELO Y EXTRACCIÓN DE RESIDUOS (SHAPIRO-WILK)
  # ============================================================================
  modelo <- tryCatch({
    aov(Valor ~ Metodo + Error(Bloque/Metodo), data = df_analisis)
  }, error = function(e) NULL)
  
  W_shapiro  <- NA
  p_shapiro  <- NA
  normalidad <- "Incomputable"
  
  if (!is.null(modelo) && !is.null(modelo$`Bloque:Metodo`)) {
    residuos <- modelo$`Bloque:Metodo`$residuals
    sh       <- tryCatch(shapiro.test(residuos), error = function(e) NULL)
    
    if (!is.null(sh)) {
      W_shapiro  <- round(as.numeric(sh$statistic), 4)
      p_shapiro  <- round(as.numeric(sh$p.value), 4)
      normalidad <- ifelse(p_shapiro >= 0.05, "Si", "No")
    }
  }
  
  # ============================================================================
  # SUPUESTO 2: PRUEBA DE ESFERICIDAD DE MAUCHLY (rstatix)
  # ============================================================================
  anova_rm <- tryCatch({
    anova_test(data = df_analisis, dv = Valor, wid = Bloque, within = Metodo)
  }, error = function(e) NULL)
  
  W_mauchly   <- NA
  p_mauchly   <- NA
  esfericidad <- "Incomputable"
  
  if (!is.null(anova_rm)) {
    mauchly_attr <- anova_rm$`Mauchly's Test for Sphericity`
    
    if (!is.null(mauchly_attr)) {
      W_mauchly   <- round(as.numeric(mauchly_attr$W), 4)
      p_mauchly   <- round(as.numeric(mauchly_attr$p), 4)
      esfericidad <- ifelse(p_mauchly >= 0.05, "Si", "No")
    } else {
      # Si rstatix no reporta Mauchly es porque k=2 métodos (esfericidad trivialmente cumplida)
      W_mauchly   <- 1.0000
      p_mauchly   <- 1.0000
      esfericidad <- "Si (Trivial)"
    }
  }
  
  # ============================================================================
  # COMPILACIÓN DEL REGISTRO ACTUAL
  # ============================================================================
  reporte_list[[i]] <- data.frame(
    Dataset     = nombre_dataset,
    W_Shapiro   = W_shapiro,
    P_Shapiro   = p_shapiro,
    Normalidad  = normalidad,
    W_Mauchly   = W_mauchly,
    P_Mauchly   = p_mauchly,
    Esfericidad = esfericidad,
    stringsAsFactors = FALSE
  )
  
  cat("  -> Completado diagnóstico para:", nombre_dataset, "\n")
}

# ==============================================================================
# CONSOLIDACIÓN Y EXPORTACIÓN DEL ARCHIVO DE SALIDA
# ==============================================================================
reporte_final <- bind_rows(reporte_list)

nombre_archivo_salida <- paste0(prefijo_archivo, "Supuestos_Parametricos_", nombre_metrica, ".csv")
path_salida_csv       <- file.path(ruta_salidas_raiz, nombre_archivo_salida)

write.csv(reporte_final, file = path_salida_csv, row.names = FALSE)

cat("\n==================================================================\n")
cat(" Proceso concluido. Reporte de supuestos guardado en:\n")
cat(" ", path_salida_csv, "\n")
cat("==================================================================\n")
print(reporte_final, row.names = FALSE)


##### HACER LAS PRUEBAS DE BLOQUES DE FRIEDMAN ####

# ==============================================================================
# PIPELINE INFERENCIAL MAESTRO: FRIEDMAN, NEMENYI Y DIAGRAMAS DE DEMŠAR (R)
# ==============================================================================

rm(list=ls())

library(dplyr)
library(tidyr)
library(PMCMRplus)
library(tsutils)

# ── CONFIGURACIÓN DE PARÁMETROS INTERRUPTORES DEL PIPELINE ────────────────────
METRICA_OBJETIVO <- c("Accuracy","Macro_F1")[1]    # Opciones válidas: "Accuracy" o "Macro_F1"
MODO_CAMPEONES   <- T       # TRUE: Filtra y evalúa solo campeones | FALSE: Evalúa todo el benchmark
METODOS_TOP      <- c("GNLPDA", "KFDA", "SVM_RBF", "SVM_POLY2", "SVM_POLY3")

# ── CONFIGURACIÓN DE RUTAS DEL PROYECTO ───────────────────────────────────────
ruta_consolidados <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
ruta_salidas_raiz <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS_ESTADISTICO_INFERENCIALES/"

# Definición del prefijo y sufijo dinámico para organización de archivos
prefijo        <- ifelse(MODO_CAMPEONES, "Champ_", "")
nombre_metrica <- ifelse(METRICA_OBJETIVO == "Accuracy", "Accuracy", "Macro_F1")

# Creación de la estructura física de directorios (Solo 2 carpetas unificadas)
ruta_diagramas_cd  <- file.path(ruta_salidas_raiz, "DIAGRAMAS_CD_DEMSAR")
ruta_matrices_nem  <- file.path(ruta_salidas_raiz, "MATRICES_NEMENYI")

if (!dir.exists(ruta_salidas_raiz)) dir.create(ruta_salidas_raiz, recursive = TRUE)
if (!dir.exists(ruta_diagramas_cd))  dir.create(ruta_diagramas_cd, recursive = TRUE)
if (!dir.exists(ruta_matrices_nem))  dir.create(ruta_matrices_nem, recursive = TRUE)

# Buscar los archivos de origen en formato largo (Ya vienen limpios a 30 bloques)
archivos_long <- list.files(path = ruta_consolidados, pattern = "_long_graficos\\.csv$", full.names = TRUE)

# Inicializar el dataframe contenedor para el concentrado único de Friedman
concentrado_friedman <- data.frame(
  Dataset          = character(),
  Metodo_Prueba    = character(),
  Estadistico_Chi2 = numeric(),
  Grados_Libertad  = integer(),
  P_Valor          = numeric(),
  Significativo    = character(),
  stringsAsFactors = FALSE
)

cat("=========================================================================\n")
cat("          INICIANDO PIPELINE INFERENCIAL MAESTRO UNIFICADO               \n")
cat("  Métrica objetivo seleccionada:", nombre_metrica, "\n")
cat("  Modo Campeones seleccionado: ", MODO_CAMPEONES, "\n")
cat("=========================================================================\n")

# ==============================================================================
# CICLO MAESTRO: PROCESAMIENTO INFERENCIAL POR DATASET
# ==============================================================================

for (i in seq_along(archivos_long)) {
  
  archivo <- archivos_long[i]
  if (!file.exists(archivo)) next
  
  # Extraer el nombre base del dataset libre de sufijos de gráficos
  nombre_archivo <- basename(archivo)
  nombre_base    <- gsub("_long_graficos\\.csv$", "", nombre_archivo)
  
  cat("Procesando dataset: ", nombre_base, " (", i, " de ", length(archivos_long), ")\n", sep="")
  
  # 1. Cargar el dataframe en formato largo
  df_long <- read.csv(archivo)
  
  # Mapear dinámicamente la columna seleccionada a una variable genérica 'Valor'
  df_long$Valor <- df_long[[METRICA_OBJETIVO]]
  
  # 2. Exclusión homogénea estricta de las 3 corridas con ruido inductivo
  df_limpio <- df_long[!(df_long$Bloque %in% c(11, 32, 33)), ]
  
  # 3. FILTRADO REACTIVO: Detectar clasificadores colapsados o con varianza cero
  metodos_validos <- df_limpio %>%
    group_by(Metodo) %>%
    summarise(v_met = var(Valor, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(v_met) & v_met > 0) %>%
    pull(Metodo)
  
  # 4. APLICACIÓN DEL INTERRUPTOR (Filtro base o Torneo de Campeones)
  if (MODO_CAMPEONES) {
    df_filtrado     <- df_limpio %>% filter(Metodo %in% METODOS_TOP)
    metodos_finales <- intersect(metodos_validos, METODOS_TOP)
  } else {
    df_filtrado     <- df_limpio %>% filter(Metodo %in% metodos_validos)
    metodos_finales <- metodos_validos
  }
  
  # Asegurar la correcta naturaleza factorial para R
  df_filtrado$Bloque <- factor(df_filtrado$Bloque)
  df_filtrado$Metodo <- factor(df_filtrado$Metodo)
  
  # Validar que existan suficientes métodos operativos para ejecutar la comparación
  if (length(unique(df_filtrado$Metodo)) < 2) {
    cat("  [!] Error: Elementos insuficientes para efectuar el test en este escenario.\n")
    next
  }
  
  # ============================================================================
  # EVALUACIÓN 1: TEST DE FRIEDMAN POR RANGOS GLOBAL
  # ============================================================================
  prueba_friedman <- friedman.test(Valor ~ Metodo | Bloque, data = df_filtrado)
  
  # Guardar los estadísticos en el concentrado único global
  fila_friedman <- data.frame(
    Dataset          = nombre_base,
    Metodo_Prueba    = prueba_friedman$method,
    Estadistico_Chi2 = round(as.numeric(prueba_friedman$statistic), 4),
    Grados_Libertad  = as.integer(prueba_friedman$parameter),
    P_Valor          = round(as.numeric(prueba_friedman$p.value), 6),
    Significativo    = ifelse(prueba_friedman$p.value < 0.05, "Si", "No"),
    stringsAsFactors = FALSE
  )
  concentrado_friedman <- rbind(concentrado_friedman, fila_friedman)
  
  cat("  -> P-Valor de Friedman Global:", round(prueba_friedman$p.value, 6), "\n")
  
  # ============================================================================
  # EVALUACIÓN 2: POST-HOC DE NEMENYI Y RECOLECCIÓN DE GRÁFICOS
  # ============================================================================
  if (prueba_friedman$p.value < 0.05) {
    cat("  -> Friedman Significativo (p < 0.05). Ejecutando Post-Hoc de Nemenyi...\n")
    
    # Cálculo formal de comparaciones múltiples por parejas
    prueba_nemenyi <- frdAllPairsNemenyiTest(
      y      = df_filtrado$Valor, 
      groups = df_filtrado$Metodo, 
      blocks = df_filtrado$Bloque
    )
    
    # Extracción y formateo limpio de la matriz de p-valores a 4 decimales
    matriz_p        <- as.data.frame(prueba_nemenyi$p.value)
    matriz_p_pulida <- round(matriz_p, 4)
    
    # Exportación tabular directa a la carpeta unificada con nomenclatura limpia
    nombre_nemenyi_csv <- paste0(prefijo, nombre_base, "_matriz_nemenyi_", nombre_metrica, ".csv")
    write.csv(matriz_p_pulida, file = file.path(ruta_matrices_nem, nombre_nemenyi_csv), row.names = TRUE)
    
    # --------------------------------------------------------------------------
    # GENERACIÓN DEL DIAGRAMA DE DIFERENCIA CRÍTICA (DEMŠAR)
    # --------------------------------------------------------------------------
    # Re-transformar temporalmente a formato ancho para el motor gráfico de tsutils
    df_wide_temp <- df_filtrado %>%
      pivot_wider(id_cols = Bloque, names_from = Metodo, values_from = Valor)
    
    matriz_precisiones <- df_wide_temp %>%
      select(all_of(metodos_finales)) %>%
      as.matrix()
    
    nombre_diagrama_png <- paste0(prefijo, nombre_base, "_cd_diagram_", nombre_metrica, ".png")
    path_diagrama       <- file.path(ruta_diagramas_cd, nombre_diagrama_png)
    
    png(filename = path_diagrama, width = 7, height = 3.5, units = "in", res = 300)
    
    # Dibujar rangos y marcar las líneas horizontales de indiferencia de Demšar
    tsutils::nemenyi(matriz_precisiones, conf.level = 0.95, plottype = "mcb")
    
    dev.off()
    cat("  -> Diagrama de Diferencia Crítica generado con éxito.\n")
    
  } else {
    cat("  -> Friedman No Significativo (p >= 0.05). Se omite la fase de comparaciones locales.\n")
  }
  
  cat("  -> Análisis del dataset concluido de forma exitosa.\n\n")
}

# ==============================================================================
# EXPORTACIÓN DEL CONCENTRADO MAESTRO ÚNICO DE FRIEDMAN
# ==============================================================================
nombre_concentrado_friedman <- paste0(prefijo, "Resumen_Global_Friedman_", nombre_metrica, ".csv")
write.csv(concentrado_friedman, file = file.path(ruta_salidas_raiz, nombre_concentrado_friedman), row.names = FALSE)

cat("=========================================================================\n")
cat(" PIPELINE EJECUTADO CON ÉXITO: Archivos inferenciales unificados.\n")
cat(" Archivo Maestro de Friedman y subcarpetas actualizados correctamente.\n")
cat("=========================================================================\n")