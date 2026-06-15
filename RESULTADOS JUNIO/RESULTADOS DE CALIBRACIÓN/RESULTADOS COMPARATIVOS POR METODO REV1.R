rm(list=ls())

# ==============================================================================
# SCRIPT UNIFICADOR DE RESULTADOS A FORMATO LARGO POR DATASET (R)
# ==============================================================================

# CONFIGURACIÓN DE RUTAS DE LA TESIS
ruta_base        <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN/"
ruta_datasets    <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS/"
ruta_consolidado <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"

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
    
    # CASO 1: Si es tu propuesta GNLPDA, el archivo ya viene recortado a 30 filas
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


##########################################

rm(list=ls())

# ==============================================================================
# SCRIPT DE GENERACIÓN DE BOXPLOTS CON CARPETA DE SALIDA DEDICADA (ggplot2)
# ==============================================================================

library(ggplot2)
library(RColorBrewer)

# CONFIGURACIÓN DE RUTAS DE LA TESIS
ruta_consolidado <- "C:\\Users\\Angel\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\CONSOLIDADO_GRAFICOS/"
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
  df_grafico$Metodo <- as.factor(df_grafico$Metodo)
  
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


##### Armar el script para comparar los métodos con ayuda de Friedman y de Nemenyi ####

generalnorm = data.frame(Metodo="",Wvalor=0,Pvalor=0,Normalidad="")[-1,]
for(j in 9:10){
  archivo = archivos_long[j]
  # 1. Cargar el dataframe en formato largo
  df_grafico <- read.csv(archivo)
  condicion1 = (df_grafico$Bloque == 11 | df_grafico$Bloque == 32 | df_grafico$Bloque == 33)
  df_grafico <- read.csv(archivo)[-condicion1,]

    G1 = tapply(df_grafico$Accuracy,df_grafico$Metodo,
              shapiro.test)
  resumenNORM = data.frame(Wvalor = 0,Pvalor = 0)
  for(i in 1:length(G1)){
    resumenNORM[i,] = round(c(Wvalor = G1[[i]]$statistic,Pvalor = G1[[i]]$p.value ),4)
  }
  resumenNORM$Metodo = names(G1)
  resumenNORM = resumenNORM[,c(3,1,2)]
  resumenNORM$Normalidad = ifelse(resumenNORM$Pvalor>=0.05,"Normal","")
  resumenNORM$Dataset = basename(archivo)
  # Concatenar con el nuevo archivo
  generalnorm = rbind.data.frame(generalnorm,resumenNORM)
  
}



generalnorm
archivos_long
