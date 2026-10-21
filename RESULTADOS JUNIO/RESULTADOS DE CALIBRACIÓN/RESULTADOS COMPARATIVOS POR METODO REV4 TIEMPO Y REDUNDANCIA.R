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


# ==============================================================================
# SCRIPT DE GENERACIÓN AUTOMÁTICA DE BOXPLOTS (TOTAL + CAMPEONES)
# ==============================================================================


rm(list=ls())
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# ── CONFIGURACIÓN DE PARÁMETROS DEL PIPELINE GRÁFICO ──────────────────────────
METRICA_OBJETIVO <- "Accuracy"    # Opciones válidas: "Accuracy" o "Macro_F1"
METODOS_TOP      <- c("GNLPDA", "KFDA", "SVM_RBF", "SVM_POLY2", "SVM_POLY3")

# ── CONFIGURACIÓN DE RUTAS BASE DE LA TESIS ───────────────────────────────────
ruta_consolidado <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
ruta_figuras_base <- file.path(ruta_consolidado, "GRAFICOS_R")

if (!dir.exists(ruta_consolidado)) stop("La ruta base de consolidados no existe.")

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
  "SVM_RBF"   = "SVM(Gauss)"
)

# Definición de componentes de texto basados en la métrica seleccionada
nombre_metrica <- ifelse(METRICA_OBJETIVO == "Accuracy", "Accuracy", "Macro_F1")
etiqueta_eje_y <- ifelse(METRICA_OBJETIVO == "Accuracy", "Accuracy (Precisión Global)", "Macro F1-Score")

# ==============================================================================
# BUCLE SECUNDARIO DE CONTROL: ITERACIÓN AUTOMÁTICA DE ENFOQUES
# ==============================================================================
for (MODO_CAMPEONES in c(FALSE, TRUE)) {
  
  # Asignación reactiva de la carpeta destino según el modo de evaluación
  nombre_carpeta_destino <- ifelse(MODO_CAMPEONES, "Boxplots Campeones", "Boxplots Total")
  ruta_salida_especifica <- file.path(ruta_figuras_base, nombre_carpeta_destino)
  
  # Asegurar la existencia física del directorio correspondiente en el disco
  if (!dir.exists(ruta_salida_especifica)) {
    dir.create(ruta_salida_especifica, recursive = TRUE)
  }
  
  cat("==================================================================\n")
  cat(" INICIANDO PROCESAMIENTO EN CARPETA:", nombre_carpeta_destino, "\n")
  cat(" Métrica objetivo:", nombre_metrica, "\n")
  cat("==================================================================\n")
  
  # BUCLE PRINCIPAL: Procesar cada archivo largo y generar su gráfico de cajas
  for (archivo in archivos_long) {
    
    nombre_archivo <- basename(archivo)
    nombre_base    <- gsub("_long_graficos\\.csv$", "", nombre_archivo)
    nombre_subase = sub("^[^.]*\\.[^.]*\\.", "", nombre_base)
    cat("  Renderizando gráfico para:", nombre_base, "... ")
    
    # 1. Cargar el dataframe unificado (Limpio a 30 observaciones)
    df_crudo <- read.csv(archivo)
    
    # 2. Asignar la columna seleccionada a la variable genérica 'Valor'
    df_crudo$Valor <- df_crudo[[METRICA_OBJETIVO]]
    
    # Remover registros vacíos o NA's preventivamente
    df_limpio <- df_crudo %>% filter(!is.na(Valor))
    
    # 3. Filtrar clasificadores según el enfoque del lazo actual
    if (MODO_CAMPEONES) {
      df_grafico <- df_limpio %>% filter(Metodo %in% METODOS_TOP)
    } else {
      df_grafico <- df_limpio
    }
    
    # 4. RE-FACTORIZACIÓN DINÁMICA DE EJES: Mapear solo los métodos presentes
    metodos_presentes <- unique(df_grafico$Metodo)
    orden_niveles     <- intersect(names(diccionario_etiquetas), metodos_presentes)
    etiquetas_limpias <- diccionario_etiquetas[orden_niveles]
    
    df_grafico$Metodo <- factor(df_grafico$Metodo, levels = orden_niveles, labels = etiquetas_limpias)
    
    ### Definir los colores unicos de los métodos ###
    colores_metodos <- setNames(
      rep("#043B7B", length(levels(df_grafico$Metodo))),
      levels(df_grafico$Metodo)
    )
    
    colores_metodos["GNLPDA"] <- "#02963E"

        # ============================================================================
    # 5. CONSTRUCCIÓN DE LA ARQUITECTURA GRÁFICA CON GGPLOT2
    # ============================================================================
    p <- ggplot(df_grafico, aes(x = Metodo, y = Valor, fill = Metodo)) +
      
      # Capa de Cajas: Oculta outliers duplicados ya que se pintarán con el jitter
      geom_boxplot(outlier.shape = NA, alpha = 0.95, color = "black", width = 0.6) +
      
      # Capa de Puntos Jitter: Distribución real de las 30 muestras
      geom_jitter(position = position_jitter(width = 0.2, height = 0), 
                  color = "black", size = 1, alpha = 0.6) +
      
      # Paleta de colores académica discreta
      scale_fill_manual(values = colores_metodos) +
      
      # Etiquetas semánticas dinámicas
      labs(
        title = paste("Comparativa de algoritmos -", nombre_subase),
        x = "Algoritmo",
        y        = "Exactitud (Accuracy)"
      ) +
      
      # Fijar límites estables para visualización comparativa homogénea
      # Estética Minimalista
      theme_minimal(base_size = 12) +
      
      theme(
        plot.title    = element_text(face = "bold", size = 13, color = "black",
                                     hjust = 0.5),
        axis.title.x  = element_text(face = "bold", size = 11, color = "black",
                                     hjust = 0.5),
        axis.title.y  = element_text(face = "bold", size = 11, color = "black",
                                     hjust = 0.5),
        axis.text.x   = element_text(size = 11, color = "black"),
        axis.text.y   = element_text(size = 11, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "#c8ccd1", size = 0.5),
        legend.position    = "none"
      )
    
    # 6. EXPORTACIÓN AISLADA A SU SUBDIR DEDICADO
    nombre_salida_png <- file.path(ruta_salida_especifica, paste0("Boxplot_", nombre_base, "_", nombre_metrica, ".png"))
    nombre_salida_pdf <- file.path(ruta_salida_especifica, paste0("Boxplot_", nombre_base, "_", nombre_metrica, ".pdf"))
    
    ggsave(filename = nombre_salida_png, plot = p, width = 8.5, height = 5.5, dpi = 300)
    #ggsave(filename = nombre_salida_pdf, plot = p, width = 8.5, height = 5.5)
    
    cat("[COMPLETO]\n")
  }
  cat("\n")
}

cat("==================================================================\n")
cat(" Proceso concluido. Las imágenes han sido organizadas por carpetas.\n")
cat("==================================================================\n")

#### DIAGNOSTICO DE MEDIDAS REPETIDAS ####
rm(list = ls())

# ==============================================================================
# DIAGNÓSTICO DE SUPUESTOS PARAMÉTRICOS AUTOMATIZADO (TOTAL + CAMPEONES)
# ==============================================================================

library(rstatix)
library(dplyr)

# ── CONFIGURACIÓN DE PARÁMETROS DEL PIPELINE DE DIAGNÓSTICO ───────────────────
METRICA_OBJETIVO <- "Accuracy"    # Opciones válidas: "Accuracy" o "Macro_F1"
METODOS_TOP      <- c("GNLPDA", "KFDA", "SVM_RBF", "SVM_POLY2", "SVM_POLY3")

# ── CONFIGURACIÓN DE RUTAS DE LA TESIS ────────────────────────────────────────
ruta_consolidados <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
ruta_salidas_raiz <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS_ESTADISTICO_INFERENCIALES/"

if (!dir.exists(ruta_salidas_raiz)) dir.create(ruta_salidas_raiz, recursive = TRUE)

# Encontrar los archivos consolidados unificados de 30 observaciones
archivos_long <- list.files(path = ruta_consolidados, pattern = "_long_graficos\\.csv$", full.names = TRUE)

# Variable auxiliar para la automatización de nombres y etiquetas
nombre_metrica <- ifelse(METRICA_OBJETIVO == "Accuracy", "Accuracy", "Macro_F1")

# ── FUNCIÓN AUXILIAR DE REDONDEO ACADÉMICO ESTRICTO ───────────────────────────
# Aplica la regla: si el valor < 0.001 devuelve "< 0.001", de lo contrario redondea a 3 decimales.
formatear_valor_tesis <- function(valor) {
  if (is.na(valor) || is.character(valor)) return(valor)
  valor_num <- as.numeric(valor)
  if (valor_num < 0.001) {
    return(0.001) # Formato de cadena estándar para publicaciones indexadas
  } else {
    return((round(valor_num, 3)))
  }
}

# ==============================================================================
# BUCLE SECUNDARIO DE CONTROL: ITERACIÓN AUTOMÁTICA DE ENFOQUES
# ==============================================================================
for (MODO_CAMPEONES in c(FALSE, TRUE)) {
  
  # Definición del prefijo dinámico basado en el modo actual
  prefijo_archivo <- ifelse(MODO_CAMPEONES, "Champ_", "")
  
  cat("==================================================================\n")
  cat(" INICIANDO EVALUACIÓN DE SUPUESTOS PARAMÉTRICOS\n")
  cat(" Modo:", ifelse(MODO_CAMPEONES, "Campeones", "Total"), "\n")
  cat(" Métrica evaluada:", nombre_metrica, "\n")
  cat("==================================================================\n")
  
  # Lista contenedora para los reportes iterativos de este bloque
  reporte_list <- list()
  
  for (i in seq_along(archivos_long)) {
    
    archivo <- archivos_long[i]
    if (!file.exists(archivo)) next
    
    # 1. Cargar el dataframe
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
        W_shapiro  <- as.numeric(sh$statistic)
        p_shapiro  <- as.numeric(sh$p.value)
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
        W_mauchly   <- as.numeric(mauchly_attr$W)
        p_mauchly   <- as.numeric(mauchly_attr$p)
        esfericidad <- ifelse(p_mauchly >= 0.05, "Si", "No")
      } else {
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
  # CONSOLIDACIÓN, FORMATEO DE DECIMALES Y EXPORTACIÓN
  # ==============================================================================
  reporte_final <- bind_rows(reporte_list)
  
  # Aplicar la regla de formateo estricto a las columnas especificadas: c(2, 3, 5, 6)
  # W_Shapiro (2), P_Shapiro (3), W_Mauchly (5), P_Mauchly (6)
  columnas_formato <- c(2, 3, 5, 6)
  for (col in columnas_formato) {
    reporte_final[[col]] <- sapply(reporte_final[[col]], formatear_valor_tesis)
  }
  
  nombre_archivo_salida <- paste0(prefijo_archivo, "Supuestos_Parametricos_", nombre_metrica, ".csv")
  path_salida_csv       <- file.path(ruta_salidas_raiz, nombre_archivo_salida)
  
  write.csv(reporte_final, file = path_salida_csv, row.names = FALSE)
  
  cat(" Proceso concluido para este bloque. Reporte guardado en:\n")
  cat(" ", path_salida_csv, "\n\n")
  print(reporte_final, row.names = FALSE)
}



##### HACER LAS PRUEBAS DE BLOQUES DE FRIEDMAN ####

rm(list=ls())

# ==============================================================================
# PIPELINE INFERENCIAL MAESTRO MULTIVARIABLE Y SEPARADO POR CARPETAS (R)
# ==============================================================================

library(dplyr)
library(tidyr)
library(PMCMRplus)
library(tsutils)

# ── CONFIGURACIÓN DE PARÁMETROS DEL PIPELINE ──────────────────────────────────
METRICA_OBJETIVO <- "Accuracy"    # Opciones válidas: "Accuracy" o "Macro_F1"
METODOS_TOP      <- c("GNLPDA", "KFDA", "SVM_RBF", "SVM_POLY2", "SVM_POLY3")

# ── CONFIGURACIÓN DE RUTAS DE LA TESIS ────────────────────────────────────────
ruta_consolidados <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
ruta_salidas_raiz <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS_ESTADISTICO_INFERENCIALES/"

if (!dir.exists(ruta_salidas_raiz)) dir.create(ruta_salidas_raiz, recursive = TRUE)

# Encontrar los archivos consolidados unificados de 30 observaciones
archivos_long <- list.files(path = ruta_consolidados, pattern = "_long_graficos\\.csv$", full.names = TRUE)

# Variables de texto dinámicas basadas en la métrica objetivo
nombre_metrica <- ifelse(METRICA_OBJETIVO == "Accuracy", "Accuracy", "Macro_F1")

# ── FUNCIÓN AUXILIAR DE REDONDEO ESTRICTO (ESTÁNDAR TESIS) ───────────────────
formatear_p_value <- function(p_val) {
  if (is.na(p_val)) return(NA)
  if (p_val < 0.001) {
    return("0.001") # Se fuerza numéricamente como texto para evitar notación científica
  } else {
    return(as.character(round(p_val, 3)))
  }
}

# ==============================================================================
# BUCLE SECUNDARIO DE CONTROL: ITERACIÓN AUTOMÁTICA DE ENFOQUES
# ==============================================================================
for (MODO_CAMPEONES in c(FALSE, TRUE)) {
  
  # Asignación reactiva de la subcarpeta destino y prefijo según el enfoque del lazo
  nombre_carpeta_destino <- ifelse(MODO_CAMPEONES, "Resultados Campeones", "Resultados Globales")
  prefijo                <- ifelse(MODO_CAMPEONES, "Champ_", "")
  
  # Estructurar las rutas físicas dedicadas para aislar las salidas
  ruta_carpeta_enfoque <- file.path(ruta_salidas_raiz, nombre_carpeta_destino)
  ruta_diagramas_cd    <- file.path(ruta_carpeta_enfoque, "DIAGRAMAS_CD_DEMSAR")
  
  if (!dir.exists(ruta_carpeta_enfoque)) dir.create(ruta_carpeta_enfoque, recursive = TRUE)
  if (!dir.exists(ruta_diagramas_cd))    dir.create(ruta_diagramas_cd, recursive = TRUE)
  
  cat("=========================================================================\n")
  cat(" INICIANDO PIPELINE INFERENCIAL EN CARPETA:", nombre_carpeta_destino, "\n")
  cat(" Métrica objetivo seleccionada:", nombre_metrica, "\n")
  cat("=========================================================================\n")
  
  # Inicializar el dataframe contenedor para el concentrado maestro único de Friedman
  concentrado_friedman <- data.frame(
    Dataset          = character(),
    Metodo_Prueba    = character(),
    Estadistico_Chi2 = numeric(),
    Grados_Libertad  = integer(),
    P_Valor          = character(),
    Significativo    = character(),
    stringsAsFactors = FALSE
  )
  
  # Inicializar la estructura base para la compresión unificada de Nemenyi
  maestro_nemenyi <- data.frame(Comparativa = character(), stringsAsFactors = FALSE)
  
  # ============================================================================
  # CICLO MAESTRO: PROCESAMIENTO INFERENCIAL POR DATASET
  # ============================================================================
  for (i in seq_along(archivos_long)) {
    
    archivo <- archivos_long[i]
    if (!file.exists(archivo)) next
    
    # Extraer el nombre base del dataset libre de sufijos
    nombre_archivo <- basename(archivo)
    nombre_base    <- gsub("_long_graficos\\.csv$", "", nombre_archivo)
    
    cat("  Procesando dataset: ", nombre_base, "\n", sep="")
    
    # 1. Cargar el dataframe en formato largo (30 observaciones fijas)
    df_long <- read.csv(archivo)
    
    # Mapear la columna seleccionada a la variable genérica 'Valor'
    df_long$Valor <- df_long[[METRICA_OBJETIVO]]
    
    # 2. Exclusión homogénea estricta de seguridad
    df_limpio <- df_long[!(df_long$Bloque %in% c(11, 32, 33)), ]
    
    # 3. FILTRADO REACTIVO: Detectar clasificadores colapsados o con varianza cero
    metodos_validos <- df_limpio %>%
      group_by(Metodo) %>%
      summarise(v_met = var(Valor, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(v_met) & v_met > 0) %>%
      pull(Metodo)
    
    # 4. Inclusión de clasificadores según el enfoque activo
    if (MODO_CAMPEONES) {
      df_filtrado     <- df_limpio %>% filter(Metodo %in% METODOS_TOP)
      metodos_finales <- intersect(metodos_validos, METODOS_TOP)
    } else {
      df_filtrado     <- df_limpio %>% filter(Metodo %in% metodos_validos)
      metodos_finales <- metodos_validos
    }
    
    df_filtrado$Bloque <- factor(df_filtrado$Bloque)
    df_filtrado$Metodo <- factor(df_filtrado$Metodo)
    
    if (length(unique(df_filtrado$Metodo)) < 2) {
      cat("    [!] Error: Elementos insuficientes para efectuar pruebas en este dataset.\n")
      next
    }
    
    # ==========================================================================
    # EVALUACIÓN 1: ANOVA DE FRIEDMAN POR RANGOS GLOBAL
    # ==========================================================================
    prueba_friedman <- friedman.test(Valor ~ Metodo | Bloque, data = df_filtrado)
    
    fila_friedman <- data.frame(
      Dataset          = nombre_base,
      Metodo_Prueba    = prueba_friedman$method,
      Estadistico_Chi2 = round(as.numeric(prueba_friedman$statistic), 3),
      Grados_Libertad  = as.integer(prueba_friedman$parameter),
      P_Valor          = formatear_p_value(as.numeric(prueba_friedman$p.value)),
      Significativo    = ifelse(prueba_friedman$p.value < 0.05, "Si", "No"),
      stringsAsFactors = FALSE
    )
    concentrado_friedman <- rbind(concentrado_friedman, fila_friedman)
    
    # ==========================================================================
    # EVALUACIÓN 2: COMPRESIÓN DE NEMENYI PAIRWISE Y RENDERIZADO CD
    # ==========================================================================
    if (prueba_friedman$p.value < 0.05) {
      
      prueba_nemenyi <- frdAllPairsNemenyiTest(
        y      = df_filtrado$Valor, 
        groups = df_filtrado$Metodo, 
        blocks = df_filtrado$Bloque
      )
      
      # Transformar la matriz simétrica de Nemenyi en un dataframe plano por filas
      matriz_p <- prueba_nemenyi$p.value
      df_parejas_lista <- list()
      idx_count <- 1
      
      for (row_idx in 1:nrow(matriz_p)) {
        for (col_idx in 1:ncol(matriz_p)) {
          if (row_idx >= col_idx) { # Extraer únicamente el triángulo inferior libre de redundancias
            nombre_pareja <- paste0(rownames(matriz_p)[row_idx], " - ", colnames(matriz_p)[col_idx])
            p_val_crudo   <- matriz_p[row_idx, col_idx]
            
            df_parejas_lista[[idx_count]] <- data.frame(
              Comparativa = nombre_pareja,
              P_Value_Tmp = formatear_p_value(p_val_crudo),
              stringsAsFactors = FALSE
            )
            idx_count <- idx_count + 1
          }
        }
      }
      
      df_parejas_dataset <- do.call(rbind, df_parejas_lista)
      colnames(df_parejas_dataset)[2] <- nombre_base # Renombrar la columna con el nombre real del dataset
      
      # Fusionar de forma relacional segura en la matriz maestra unificada
      if (nrow(maestro_nemenyi) == 0) {
        maestro_nemenyi <- df_parejas_dataset
      } else {
        maestro_nemenyi <- full_join(maestro_nemenyi, df_parejas_dataset, by = "Comparativa")
      }
      
      # ------------------------------------------------------------------------
      # GENERACIÓN DEL DIAGRAMA DE DIFERENCIA CRÍTICA (DEMŠAR)
      # ------------------------------------------------------------------------
      df_wide_temp <- df_filtrado %>%
        pivot_wider(id_cols = Bloque, names_from = Metodo, values_from = Valor)
      
      matriz_precisiones <- df_wide_temp %>%
        select(all_of(metodos_finales)) %>%
        as.matrix()
      
      nombre_diagrama_png <- paste0("CD_Diagram_", nombre_base, "_", nombre_metrica, ".png")
      path_diagrama       <- file.path(ruta_diagramas_cd, nombre_diagrama_png)
      
      png(filename = path_diagrama, width = 7, height = 3.5, units = "in", res = 300)
      tsutils::nemenyi(matriz_precisiones, conf.level = 0.95, plottype = "mcb")
      dev.off()
    }
  }
  
  # ==============================================================================
  # EXPORTACIÓN DE LOS ENTREGABLES CONCENTRADOS POR CARPETA
  # ==============================================================================
  nombre_csv_friedman <- paste0(prefijo, "Resumen_Global_Friedman_", nombre_metrica, ".csv")
  write.csv(concentrado_friedman, file = file.path(ruta_carpeta_enfoque, nombre_csv_friedman), row.names = FALSE)
  
  if (nrow(maestro_nemenyi) > 0) {
    # Ordenar alfabéticamente las filas de comparación para simetría visual en el reporte
    maestro_nemenyi <- maestro_nemenyi %>% arrange(Comparativa)
    
    nombre_csv_nemenyi <- paste0(prefijo, "Resumen_Global_Nemenyi_", nombre_metrica, ".csv")
    write.csv(maestro_nemenyi, file = file.path(ruta_carpeta_enfoque, nombre_csv_nemenyi), row.names = FALSE)
    cat("  -> Archivo Maestro de Nemenyi comprimido generado con éxito.\n")
  }
  
  cat(" Finalizado procesamiento para el bloque:", nombre_carpeta_destino, "\n\n")
}

cat("=========================================================================\n")
cat(" PROCESS COMPLETED: Pipeline inferencial finalizado y unificado.\n")
cat("=========================================================================\n")

