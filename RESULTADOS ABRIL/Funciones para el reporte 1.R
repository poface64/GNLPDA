
#### Función para la expansión ####
polexp = function(X,grado){
  # Entra la matriz de datos en forma de dataframe 
  datos_expandidos <- poly(as.matrix(X), degree = grado, raw = T)
  # Se procesan los datos
  # Obtenemos los nombres de las variables originales
  nombres_originales <- colnames(X)
  # Obtenemos los nombres generados por poly()
  nombres_cripticos <- colnames(datos_expandidos)
  # Usamos sapply() para iterar sobre cada nombre críptico y traducirlo
  nuevos_nombres <- sapply(nombres_cripticos, function(nombre) {
    # Separamos los exponentes (ej. "1.1.0" se convierte en un vector numérico c(1, 1, 0))
    poderes <- as.integer(strsplit(nombre, "\\.")[[1]])
    # Creamos un vector para guardar las partes del nuevo nombre (ej. "X1", "X2")
    terminos <- c()
    # Recorremos cada exponente junto con su variable correspondiente
    for (i in seq_along(poderes)) {
      if (poderes[i] == 1) {
        # Si el exponente es 1, solo añadimos el nombre de la variable
        terminos <- c(terminos, nombres_originales[i])
      } else if (poderes[i] > 1) {
        # Si el exponente es > 1, añadimos "Variable^Exponente"
        terminos <- c(terminos, paste0(nombres_originales[i], "^", poderes[i]))
      }
      # Si el exponente es 0, no hacemos nada, ya que Var^0 = 1
    }
    # Unimos las partes con un "*" para indicar la multiplicación/interacción
    paste(terminos, collapse = "*")
  })
  # --- 3. Asignamos los nuevos nombres a nuestra matriz ---
  colnames(datos_expandidos) <- nuevos_nombres
  # Devolver las salidas #
  resu = as.data.frame(datos_expandidos)
  return(resu)
}

#### Gráficar los resultados ####
library(ggplot2)
library(MASS)
library(dplyr)

graficar_proyeccion_tesis <- function(X_final, y_labels, nombre_ds, mejor_fila) {
  
  # 1. Ajustar el modelo LDA
  modelo_lda <- lda(X_final, grouping = y_labels)
  proyeccion <- predict(modelo_lda)$x %>% as.data.frame()
  
  # 2. Preparar el dataframe para ggplot
  df_plot <- proyeccion
  df_plot$Clase <- as.factor(y_labels)
  num_clases <- length(unique(y_labels))
  
  # --- LÓGICA DE GRAFICACIÓN SIMPLIFICADA ---
  
  if (num_clases == 2) {
    # Si es binario, solo existe LD1. Creamos LD2 lleno de ceros para proyectar sobre el eje X.
    df_plot$LD2 <- 0
    
    p <- ggplot(df_plot, aes(x = LD1, y = LD2, color = Clase)) +
      geom_hline(yintercept = 0, color = "gray80", size = 0.5) + # Línea base
      geom_point(size = 3, alpha = 0.7) +
      # Nota: stat_ellipse no se puede calcular con varianza cero en Y, 
      # así que para el caso binario lineal no la incluimos.
      labs(x = "Discriminante Lineal 1 (LD1)", y = "") +
      theme(axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    
  } else {
    # CASO MULTICLASE: 2D (LD1 vs LD2) con elipses
    p <- ggplot(df_plot, aes(x = LD1, y = LD2, color = Clase, fill = Clase)) +
      geom_point(size = 3, alpha = 0.7) +
      stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95) +
      labs(x = "Discriminante Lineal 1 (LD1)", y = "Discriminante Lineal 2 (LD2)")
  }
  
  # 3. Estética común (Academy Style)
  p <- p + theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = paste("Proyección GPN-LDA:", nombre_ds),
      subtitle = paste("Mejor Corrida (Acc:", round(mejor_fila$Acc, 4), ")"),
      caption = paste("Términos óptimos:", mejor_fila$Nombres_E2)
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "darkslategrey"),
      plot.caption = element_text(size = 8, face = "italic", color = "blue")
    )
  
  return(p)
}
###################

# Funcion para tablas bonitas
# X es la tabla en formato dataframe #
tb = function(X){autofit(theme_box(flextable(X)))}

#### Extractor de CSV's Actualizado ####
obtener_csv <- function(ruta_carpeta) {
  # 1. Obtener las rutas completas de los archivos .csv
  rutas <- list.files(
    path = ruta_carpeta,
    pattern = "\\.csv",
    full.names = TRUE
  )
  
  # 2. Extraer solo el nombre del archivo (sin extensión ni ruta)
  # Esto sirve para identificar el dataset, ej: "1.1.iris"
  nombres <- tools::file_path_sans_ext(basename(rutas))
  
  # 3. Quitar el prefijo "Resultados_Detalle_" si es que existe (para dfr30)
  nombres <- gsub("Resultados_Detalle_", "", nombres)
  
  # 4. Devolver un Data Frame organizado
  return(data.frame(
    nombre = nombres,
    ruta = rutas,
    stringsAsFactors = FALSE
  ))
}


#### Funcion para normalizar los nombres ####

normalizar_nombres <- function(vec) {
  sapply(vec, function(x) {
    partes <- strsplit(x, "\\*")[[1]]
    paste(sort(partes), collapse="*")
  })
}

#### Función para resumir los datasets ####

resumendf <- function(ruta){
  info_archivos <- obtener_csv(ruta)
  resultados <- lapply(1:nrow(info_archivos), function(i){
    df <- read.csv(info_archivos$ruta[i])
    muestras <- nrow(df)
    variables <- ncol(df) - 1
    clases <- length(unique(df[, ncol(df)]))
    # Quitar prefijos tipo 1.1. , 2.3. , etc.
    nombre_limpio <- gsub("^[0-9]+\\.[0-9]+\\.", "", info_archivos$nombre[i])
    data.frame(
      Dataset = nombre_limpio,
      Muestras = muestras,
      Variables = variables,
      Clases = clases,
      stringsAsFactors = FALSE
    )
  })
  resumen_df <- do.call(rbind, resultados)
  return(resumen_df)
}

#### Función para resumir los resultados de las corridas ####

#### Función para Generar la Tabla Maestra de Tesis ####
#### Función para Generar la Tabla Maestra (Versión Blindada) ####
library(dplyr)
library(purrr)

resumencorridas <- function(df_rutas) {
  
  # Procesamos cada fila del dataframe de rutas que generó obtener_csv
  tabla_final <- map_df(1:nrow(df_rutas), function(i) {
    
    # Intentar leer el archivo
    df_temp <- tryCatch({
      read.csv(df_rutas$ruta[i])
    }, error = function(e) return(NULL))
    
    # VALIDACIÓN: Si el archivo no tiene la columna 'Acc', no es de resultados
    if (is.null(df_temp) || !"Acc" %in% names(df_temp)) {
      return(NULL)
    }
    
    # Funciones auxiliares para el formato "Media ± Std"
    fmt_m <- function(x) paste0(round(mean(x, na.rm = TRUE), 4), " ± ", round(sd(x, na.rm = TRUE), 4))
    fmt_t <- function(x) paste0(round(mean(x, na.rm = TRUE), 2), " ± ", round(sd(x, na.rm = TRUE), 2))
    
    # Generar el resumen por dataset
    df_temp %>%
      summarise(
        Dataset    = df_rutas$nombre[i],
        Accuracy   = fmt_m(Acc),
        Precision  = fmt_m(Prec),
        Recall     = fmt_m(Rec),
        F1_Score   = fmt_m(F1),
        Vars_E1    = fmt_t(Vars_E1),
        Vars_E2    = fmt_t(Vars_E2),
        Tiempo_Seg = fmt_t(Tiempo),
        Ahorro_Mem = paste0(round(mean(Ahorro, na.rm = TRUE) * 100, 2), "%")
      )
  })
  
  return(tabla_final)
}

#### CebollaND ####

library(tidyverse)

cebollaND <- function(n_samples = 300, n_dims = 2, n_classes = 3, noise = 0.1) {
  points_per_class <- floor(n_samples / n_classes)
  dataset <- data.frame()
  
  for (i in 1:n_classes) {
    # Definir radios para la "capa" actual
    # Clase 1: r entre 0 y 1.5
    # Clase 2: r entre 2 y 3.5
    # Clase 3: r entre 4 y 5.5
    inner_radius <- (i - 1) * 2.0
    outer_radius <- inner_radius + 1.5
    
    # 1. Generar puntos en una hiperesfera unitaria (distribución normal)
    raw_points <- matrix(rnorm(points_per_class * n_dims), ncol = n_dims)
    
    # 2. Normalizar para que tengan norma 1 (estén en la superficie)
    norms <- sqrt(rowSums(raw_points^2))
    unit_points <- raw_points / norms
    
    # 3. Escalar aleatoriamente entre inner_radius y outer_radius
    r <- inner_radius + runif(points_per_class) * (outer_radius - inner_radius)
    scaled_points <- unit_points * r
    
    # 4. Agregar el ruido Gaussiano
    final_points <- scaled_points + matrix(rnorm(points_per_class * n_dims, sd = noise), ncol = n_dims)
    
    # Estructurar en DataFrame
    df_class <- as.data.frame(final_points)
    colnames(df_class) <- paste0("V", 1:n_dims)
    df_class$class <- as.factor(paste0("G_", i))
    
    dataset <- rbind(dataset, df_class)
  }
  
  return(dataset)
}

# Ejemplo de uso y visualización rápida (si es 2D)
cebolla3D = cebollaND(n_samples = 300, n_dims = 3, n_classes = 3, noise = 0.1)
cebolla10D = cebollaND(n_samples = 300, n_dims = 10, n_classes = 3, noise = 0.1)
cebolla20D = cebollaND(n_samples = 300, n_dims = 20, n_classes = 3, noise = 0.1)


write.csv(cebolla3D,"cebolla3D.csv",row.names = F)
write.csv(cebolla10D,"cebolla10D.csv",row.names = F)
write.csv(cebolla20D,"cebolla20D.csv",row.names = F)


