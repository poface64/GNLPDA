rm(list=ls())

# ==============================================================================
# SCRIPT UNIFICADOR DE RESULTADOS A FORMATO LARGO POR DATASET (R)
# ==============================================================================

# CONFIGURACIÓN DE RUTAS DE LA TESIS
ruta_base        <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN/"
ruta_datasets    <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS/"
ruta_consolidado <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"

if(!dir.exists(ruta_consolidado)) dir.create(ruta_consolidado, recursive = TRUE)

# Definición de los 6 métodos y sus respectivas subcarpetas y sufijos
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
cat("          INICIANDO CONSOLIDACIÓN PARA GRÁFICOS DE CAJAS          \n")
cat("==================================================================\n")

# BUCLE PRINCIPAL: Procesar dataset por dataset
for (archivo in archivos_csv) {
  
  nombre_base <- tools::file_path_sans_ext(basename(archivo))
  cat("\nConsolidando información para el dataset:", nombre_base, "\n")
  
  # Lista temporal para almacenar los data.frames en formato largo de cada método
  lista_long_dataset <- list()
  
  # DEFINICIÓN DEL FILTRO DE EXCLUSIÓN HOMOGÉNEO (30 Bloques de Friedman)
  # Omitimos la corrida 11, 32 y 33 de las bitácoras que contienen las 33 originales
  corridas_validas <- c(1:33)
  
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
    
    # CASO 1: Si es tu propuesta GNLPDA
    if (nombre_metodo == "GNLPDA") {
      df_long_temp <- data.frame(
        Bloque   = df_res_metodo$Bloque_Friedman,
        Accuracy = df_res_metodo$Accuracy,
        Metodo   = nombre_metodo,
        stringsAsFactors = FALSE
      )
    } else {
      # CASO 2: Para los demás competidores que tienen las 33 corridas originales
      # Extraemos únicamente las filas correspondientes a las corridas seleccionadas
      df_recortado <- df_res_metodo[df_res_metodo$Corrida %in% corridas_validas, ]
      
      df_long_temp <- data.frame(
        Bloque   = 1:nrow(df_recortado), # Mapeo ordenado del 1 al 30 para emparejar
        Accuracy = df_recortado$Accuracy,
        Metodo   = nombre_metodo,
        stringsAsFactors = FALSE
      )
    }
    
    # Guardar en la lista temporal
    lista_long_dataset[[nombre_metodo]] <- df_long_temp
  }
  
  # Combinar los registros de todos los métodos disponibles para este dataset
  if (length(lista_long_dataset) > 0) {
    df_final_long <- do.call(rbind, lista_long_dataset)
    
    # Guardar el archivo final de formato largo listo para ggplot2
    write.csv(df_final_long, 
              file = file.path(ruta_consolidado, paste0(nombre_base, "_long_graficos.csv")), 
              row.names = FALSE)
    
    cat("  -> Archivo largo generado exitosamente: ", paste0(nombre_base, "_long_graficos.csv"), " (", nrow(df_final_long), " observaciones)\n")
  }
}

cat("\n==================================================================\n")
cat(" Proceso concluido. Todos los archivos de formato largo están listos.\n")
cat("==================================================================\n")


#### Script para hacer los boxplots a partir de los datos en formato largo ####
### Pendiente corregir el estilo
### Reordenar los métodos (GNLPDA, LDA, QDA, KFDA, SVM)
### Aquí NO hay problema con la aparición del QDA
### Recordar que corta las observaciones 11, 33 y 32

rm(list=ls())

# ==============================================================================
# SCRIPT DE GENERACIÓN DE BOXPLOTS CON CARPETA DE SALIDA DEDICADA (ggplot2)
# ==============================================================================

library(ggplot2)
library(RColorBrewer)

# CONFIGURACIÓN DE RUTAS DE LA TESIS
ruta_consolidado <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
# Subcarpeta exclusiva para las imágenes de salida
ruta_figuras     <- file.path(ruta_consolidado, "GRAFICOS_R")

# Asegurar la existencia de las rutas físicas en el disco
if (!dir.exists(ruta_consolidado)) {
  stop("La ruta base de consolidados no existe. Verifica la configuración previa.")
}
if (!dir.exists(ruta_figuras)) {
  dir.create(ruta_figuras, recursive = TRUE)
  cat("Carpeta dedicada creada exitosamente en:", ruta_figuras, "\n")
}

# Obtener la lista de archivos en formato largo generados previamente
archivos_long <- list.files(path = ruta_consolidado, pattern = "_long_graficos\\.csv$", full.names = TRUE)

if (length(archivos_long) == 0) {
  cat("No se encontraron archivos con el patrón '_long_graficos.csv' en la carpeta.\n")
} else {
  cat("==================================================================\n")
  cat("          INICIANDO RENDERIZADO AUTOMÁTICO DE BOXPLOTS            \n")
  cat("==================================================================\n")
}

# BUCLE PRINCIPAL: Procesar cada archivo largo y generar su gráfico de cajas
archivo = archivos_long[8]
for (archivo in archivos_long) {
  
  # Extraer el nombre base del dataset para títulos y archivos de guardado
  nombre_archivo <- basename(archivo)
  nombre_base    <- gsub("_long_graficos\\.csv$", "", nombre_archivo)
  
  cat("Renderizando gráfico para:", nombre_base, "... ")
  
  # 1. Cargar el dataframe en formato largo
  df_grafico <- read.csv(archivo)
  if(nombre_archivo=="4.2.Ionosphere_long_graficos.csv"){
    # Cambiar los valores del QDA por NA's
    df_grafico[df_grafico$Metodo=="QDA",2] = NA
  }
  
  # Forzar a que la columna de Método sea un Factor para el correcto mapeo estético
  df_grafico$Metodo <- factor(df_grafico$Metodo,
                              levels = c("GNLPDA","LDA","QDA","KFDA",
                                         "SVM_POLY2","SVM_POLY3","SVM_RBF"),
                              labels = c("GNLPDA","LDA","QDA","KFDA",
                                         "SVM(POL2)","SVM(POL3)","SVM(Gauss)"))
  
  # Ordenar los métodos
  # Forzar a quitar las corridas 11,33 y 32
  condicion1 = (df_grafico$Bloque == 11 | df_grafico$Bloque == 32 | df_grafico$Bloque == 33)
  df_grafico = df_grafico[!condicion1,]
  
  # 2. Construcción de la Arquitectura Gráfica con ggplot2
  p <- ggplot(df_grafico, aes(x = Metodo, y = Accuracy, fill = Metodo)) +
    
    # Capa de Cajas: Bordes finos gris oscuro, paleta sobria y outliers ocultos
    geom_boxplot(outlier.shape = NA, alpha = 0.75, color = "#2c3e50", width = 0.6) +
    
    # Capa de Puntos Jitter: Muestra las 30 observaciones reales con dispersión
    geom_jitter(position = position_jitter(width = 0.2, height = 0), 
                color = "#34495e", size = 1.2, alpha = 0.5) +
    
    # Configuración de Paleta de Colores Académica
    scale_fill_brewer(palette = "Set2") +
    
    # Títulos, Etiquetas y Subtítulos formales de Tesis
    labs(
      title    = paste("Distribución de Rendimiento -", nombre_base),
      subtitle = "Comparativa robusta basada en 30 bloques de evaluación independientes",
      x        = "Algoritmo de Clasificación / Competidor",
      y        = "Accuracy (Precisión Global)"
    ) +
    
    # Restricción del eje Y para centrar la atención en la zona de fricción (0 a 1)
    
    # Línea base de Tema Limpio (Minimalista)
    theme_minimal(base_size = 12) +
    
    # Ajustes finos de tipografía y visualización del panel
    theme(
      plot.title         = element_text(face = "bold", size = 14, color = "#2c3e50", hjust = 0),
      plot.subtitle      = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      axis.title.x       = element_text(face = "bold", size = 11, color = "#2c3e50", margin = margin(t = 12)),
      axis.title.y       = element_text(face = "bold", size = 11, color = "#2c3e50", margin = margin(r = 12)),
      axis.text.x        = element_text(size = 10, color = "#34495e", angle = 0),
      axis.text.y        = element_text(size = 10, color = "#34495e"),
      panel.grid.major.x = element_blank(), # Eliminar rejillas verticales
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#ecf0f1", size = 0.5), # Líneas horizontales tenues
      legend.position    = "none" # Ocultar leyenda redundante
    )
  
  # 3. Exportar el gráfico direccionado a la nueva subcarpeta dedicada
  nombre_salida_png <- file.path(ruta_figuras, paste0("Boxplot_", nombre_base, ".png"))
  nombre_salida_pdf <- file.path(ruta_figuras, paste0("Boxplot_", nombre_base, ".pdf"))
  
  # Guardar PNG (300 DPI) para inserción rápida
  ggsave(filename = nombre_salida_png, plot = p, width = 8.5, height = 5.5, dpi = 300)
  
  # Guardar PDF vectorial para compilar en LaTeX con escalabilidad perfecta
  ggsave(filename = nombre_salida_pdf, plot = p, width = 8.5, height = 5.5)
  
  cat("[GUARDADO EN GRAFICOS_R]\n")
}

cat("\n==================================================================\n")
cat(" Proceso concluido. Las figuras quedaron organizadas de forma aislada.\n")
cat("==================================================================\n")

#### Diagnostico de supuestos sobre los anovas de medidas repetidas ####

#### JUSTIFICACIÓN DE FRIEDMAN ####

library(rstatix)
library(dplyr)

# Lista para guardar resultados
reporte_list <- list()

for (i in 1:10) {
  # Cargar el archivo
  archivo <- archivos_long[i]
  
  if (!file.exists(archivo)) next
  
  datos <- read.csv(archivo)
  
  # Filtrado correcto (arreglado condicion1 -> condicion)
  condicion <- datos$Bloque %in% c(11, 32, 33)
  datos1 <- datos[!condicion, ]
  ## Filtrado preventivo para QDA en el dataset 8
  if(i == 8){
    # Retirar QDA por reportar solo NA's
    datos1 = datos1[!datos1$Metodo=="QDA",]}
  
  datos1$Bloque <- factor(datos1$Bloque)
  datos1$Metodo <- factor(datos1$Metodo)
  
  nombre_dataset <- tools::file_path_sans_ext(basename(archivo))
  nombre_dataset <- sub("^\\d+\\.\\d+\\.([^_]+).*", "\\1", nombre_dataset)
  
  # =========================================================
  # 1. MODELO ANOVA (medidas repetidas)
  # =========================================================
  modelo <- aov(
    Accuracy ~ Metodo + Error(Bloque/Metodo),
    data = datos1
  )
  
  residuos <- modelo$`Bloque:Metodo`$residuals
  
  # =========================================================
  # 2. SHAPIRO SOBRE RESIDUOS
  # =========================================================
  sh <- tryCatch(
    shapiro.test(residuos),
    error = function(e) NULL
  )
  
  W_shapiro <- if (!is.null(sh)) round(as.numeric(sh$statistic), 4) else NA
  p_shapiro <- if (!is.null(sh)) round(sh$p.value, 4) else NA
  
  normalidad <- ifelse(!is.na(p_shapiro) & p_shapiro >= 0.05,
                       "Si", "No")
  
  # =========================================================
  # 3. MAUCHLY (ESFERICIDAD)
  # =========================================================
  anova_rm <- tryCatch(
    anova_test(
      data = datos1,
      dv = Accuracy,
      wid = Bloque,
      within = Metodo
    ),
    error = function(e) NULL
  )
  
  mauchly <- if (!is.null(anova_rm)) {
    as.data.frame(anova_rm$`Mauchly's Test for Sphericity`)[1, ]
  } else {
    NULL
  }
  
  W_mauchly <- if (!is.null(mauchly)) round(mauchly$W, 4) else NA
  p_mauchly <- if (!is.null(mauchly)) round(mauchly$p, 4) else NA
  
  esfericidad <- ifelse(!is.na(p_mauchly) & p_mauchly >= 0.05,
                        "Si", "No")
  
  # =========================================================
  # 4. ARMAZÓN DEL REPORTE
  # =========================================================
  reporte_list[[i]] <- data.frame(
    Dataset = nombre_dataset,
    W_Shapiro = W_shapiro,
    P_Shapiro = p_shapiro,
    Normalidad = normalidad,
    W_Mauchly = W_mauchly,
    P_Mauchly = p_mauchly,
    Esfericidad = esfericidad,
    stringsAsFactors = FALSE
  )
}

# =========================================================
# 5. TABLA FINAL
# =========================================================
reporte_final <- bind_rows(reporte_list)

print(reporte_final)

##### HACER LAS PRUEBAS DE BLOQUES DE FRIEDMAN ####

# ==============================================================================
# PIPELINE INFERENCIAL MAESTRO: FRIEDMAN, NEMENYI Y DIAGRAMAS DE DEMŠAR (R)
# ==============================================================================

rm(list=ls())

library(dplyr)
library(tidyr)
library(PMCMRplus)
library(tsutils)

# ── CONFIGURACIÓN DE PARÁMETROS DEL TORNEO DE CAMPEONES ───────────────────────
MODO_CAMPEONES <- TRUE    # TRUE: Filtra y evalúa solo campeones | FALSE: Evalúa todo el benchmark
METODOS_TOP    <- c("GNLPDA", "KFDA", "SVM_POLY2",
                    "SVM_POLY3", "SVM_RBF")

  
# ── CONFIGURACIÓN DE RUTAS DEL PROYECTO ───────────────────────────────────────
ruta_consolidados <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
ruta_salidas_raiz  <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS_ESTADISTICO_INFERENCIALES/"

# Definición del prefijo dinámico para organización de archivos
prefijo <- ifelse(MODO_CAMPEONES, "Champ_", "")

# Creación de la estructura física de directorios (Solo 2 carpetas especializadas)
ruta_diagramas_cd  <- file.path(ruta_salidas_raiz, "DIAGRAMAS_CD_DEMSAR")
ruta_matrices_nem  <- file.path(ruta_salidas_raiz, "MATRICES_NEMENYI")

if (!dir.exists(ruta_salidas_raiz)) dir.create(ruta_salidas_raiz, recursive = TRUE)
if (!dir.exists(ruta_diagramas_cd))  dir.create(ruta_diagramas_cd, recursive = TRUE)
if (!dir.exists(ruta_matrices_nem))  dir.create(ruta_matrices_nem, recursive = TRUE)

# Buscar los archivos de origen en formato largo
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

# ==============================================================================
# CICLO MAESTRO: PROCESAMIENTO INFERENCIAL POR DATASET
# ==============================================================================

for (i in seq_along(archivos_long)) {
  # Cargar el archivo i-esimo
  archivo <- archivos_long[i]
  if (!file.exists(archivo)) next
  
  # Extraer el nombre base del dataset libre de sufijos de gráficos
  nombre_archivo <- basename(archivo)
  nombre_base    <- gsub("_long_graficos\\.csv$", "", nombre_archivo)
  
  cat("=========================================================================\n")
  cat("INICIANDO ANÁLISIS INFERENCIAL: ", nombre_base, " (", i, " de ", length(archivos_long), ")\n", sep="")
  if (MODO_CAMPEONES) cat("  [MODO ACTIVADO]: Torneo de Campeones Seleccionados\n")
  cat("=========================================================================\n")
  
  # 1. Cargar el dataframe en formato largo
  df_long <- read.csv(archivo)
  
  # 2. Exclusión homogénea estricta de las 3 corridas con ruido inductivo
  df_limpio <- df_long[!(df_long$Bloque %in% c(11, 32, 33)), ]
  # 3. FILTRADO REACTIVO: Detectar clasificadores colapsados o con varianza cero
  metodos_validos <- df_limpio %>%
    group_by(Metodo) %>%
    summarise(v_acc = var(Accuracy, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(v_acc) & v_acc > 0) %>%
    pull(Metodo)
  
  # 4. APLICACIÓN DEL INTERRUPTOR (Filtro base o Torneo de Campeones)
  if (MODO_CAMPEONES) {
    # Filtrado por lista de inclusión estricta
    df_filtrado <- df_limpio %>% filter(Metodo %in% METODOS_TOP)
    metodos_finales <- intersect(metodos_validos, METODOS_TOP)
  } else {
    # Conservar todos los métodos que pasaron el filtro de varianza
    df_filtrado <- df_limpio %>% filter(Metodo %in% metodos_validos)
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
  prueba_friedman <- friedman.test(Accuracy ~ Metodo | Bloque, data = df_filtrado)
  
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
      y      = df_filtrado$Accuracy, 
      groups = df_filtrado$Metodo, 
      blocks = df_filtrado$Bloque
    )
    
    # Extracción y formateo limpio de la matriz de p-valores a 4 decimales
    matriz_p <- as.data.frame(prueba_nemenyi$p.value)
    matriz_p_pulida <- round(matriz_p, 4)
    
    # Exportación tabular directa a la carpeta unificada con nomenclatura limpia
    nombre_nemenyi_csv <- paste0(prefijo, nombre_base, "_matriz_nemenyi.csv")
    write.csv(matriz_p_pulida, file = file.path(ruta_matrices_nem, nombre_nemenyi_csv), row.names = TRUE)
    
    # --------------------------------------------------------------------------
    # GENERACIÓN DEL DIAGRAMA DE DIFERENCIA CRÍTICA (DEMŠAR)
    # --------------------------------------------------------------------------
    # Re-transformar temporalmente a formato ancho para el motor gráfico de tsutils
    df_wide_temp <- df_filtrado %>%
      pivot_wider(id_cols = Bloque, names_from = Metodo, values_from = Accuracy)
    
    matriz_precisiones <- df_wide_temp %>%
      select(all_of(metodos_finales)) %>%
      as.matrix()
    
    nombre_diagrama_png <- paste0(prefijo, nombre_base, "_cd_diagram.png")
    path_diagrama <- file.path(ruta_diagramas_cd, nombre_diagrama_png)
    
    png(filename = path_diagrama, width = 7, height = 3.5, units = "in", res = 300)
    
    # Dibujar rangos y marcar las líneas horizontales de indiferencia de Demšar
    tsutils::nemenyi(matriz_precisiones, conf.level = 0.95, plottype = "mcb")
    
    dev.off()
    cat("  -> Diagrama de Diferencia Crítica generado en alta definición.\n")
    
  } else {
    cat("  -> Friedman No Significativo (p >= 0.05). Se omite la fase de comparaciones locales.\n")
  }
  
  cat("  -> Análisis del dataset concluido de forma exitosa.\n\n")
}

# ==============================================================================
# EXPORTACIÓN DEL CONCENTRADO MAESTRO ÚNICO DE FRIEDMAN
# ==============================================================================
nombre_concentrado_friedman <- paste0(prefijo, "Resumen_Global_Friedman.csv")
write.csv(concentrado_friedman, file = file.path(ruta_salidas_raiz, nombre_concentrado_friedman), row.names = FALSE)

cat("=========================================================================\n")
cat(" PIPELINE EJECUTADO CON ÉXITO: Espacio inferencial unificado y ordenado.\n")
cat(" Archivo Maestro de Friedman y subcarpetas actualizadas correctamente.\n")
cat("=========================================================================\n")

