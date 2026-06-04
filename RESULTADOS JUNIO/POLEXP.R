#### 2. Normalizador de nombres en R (Adaptado al prefijo X2 de R) ####
normalizar_nombres <- function(vector_nombres) {
  sapply(vector_nombres, function(nom) {
    # 1. Reemplazar asteriscos (*) y gorritos (^) por puntos (.)
    nom_con_puntos <- gsub("\\*", ".", nom)
    nom_con_puntos <- gsub("\\^", ".", nom_con_puntos)
    
    # 2. Separar por el punto
    partes <- unlist(strsplit(nom_con_puntos, "\\."))
    
    # 3. Detectar si es un término cuadrático (contiene un "2")
    if ("2" %in% partes) {
      # Quitar el "2" para quedarnos solo con el nombre de la variable original
      partes_sin_dos <- partes[partes != "2"]
      partes_ordenadas <- sort(partes_sin_dos)
      
      # Forzar el prefijo "X2." que R autogenera por sintaxis
      nombre_final <- paste0("X2.", paste(partes_ordenadas, collapse = "."))
      return(nombre_final)
    }
    
    # 4. Si es una interacción común (ej. B.A), se ordena alfabéticamente (A.B)
    partes_ordenadas <- sort(partes)
    nombre_final <- paste(partes_ordenadas, collapse = ".")
    
    return(nombre_final)
  }, USE.NAMES = FALSE)
}

#### 1. Constructor de subconjunto óptimo para GNLPDA ####
obtener_dataset_gnlpda <- function(df_expandido, res_gnlpda, idx_fila) {
  # Extraer, limpiar espacios y separar por comas
  vars_crudas <- gsub(" ", "", unlist(strsplit(res_gnlpda$Nombres_E2[idx_fila], ",")))
  
  # Normalizar usando la regla sintáctica de la 'X2.'
  variables_seleccionadas <- normalizar_nombres(vars_crudas)
  # Cambiar explicitamente la cosa esa para ponerle el X2 #
  variables_seleccionadas <- gsub("^2\\.", "X2.", variables_seleccionadas)
  
  # Control de seguridad: Filtrar únicamente las que coincidan con las columnas reales
  variables_validas <- variables_seleccionadas[variables_seleccionadas %in% colnames(df_expandido)]
  
  # Alerta preventiva en consola si llega a faltar alguna correspondencia
  if (length(variables_validas) < length(variables_seleccionadas)) {
    vars_faltantes <- variables_seleccionadas[!variables_seleccionadas %in% colnames(df_expandido)]
    warning(paste0("En la fila ", idx_fila, " no se encontraron las columnas: ", 
                   paste(vars_faltantes, collapse = ", "), ". Se procedió solo con las existentes."))
  }
  
  # Filtrar columnas y añadir la variable objetivo
  df_subconjunto <- data.frame(
    df_expandido[, variables_validas, drop = FALSE],
    Clase = df_expandido$Clase
  )
  
  return(df_subconjunto)
}

#### 3. Exportador de Reportes de Hipótesis (Friedman y Nemenyi) ####
exportar_reportes_hipotesis <- function(prueba_friedman, prueba_nemenyi, ruta_salida, nombre_base) {
  # Guardar reporte de Friedman
  df_friedman <- data.frame(
    Metodo_Prueba = prueba_friedman$method,
    Estadistico_Chi2 = as.numeric(prueba_friedman$statistic),
    Grados_Libertad = as.numeric(prueba_friedman$parameter),
    P_Valor = as.numeric(prueba_friedman$p.value)
  )
  write.csv(df_friedman, file = paste0(ruta_salida, nombre_base, "_test_friedman.csv"), row.names = FALSE)
  
  # Guardar reporte de Nemenyi (Matriz de p-valores por parejas)
  if (!is.null(prueba_nemenyi)) {
    matriz_p <- as.data.frame(prueba_nemenyi$p.value)
    write.csv(matriz_p, file = paste0(ruta_salida, nombre_base, "_matriz_nemenyi.csv"), row.names = TRUE)
  }
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




#### Funciónes necesarias para invocar a Julia ####
library(JuliaCall)
julia_setup()
julia_command('include("POLEXP.jl")')
#### Wrapper del polexp en Julia a R ####
polexpj <- function(df_nombre_en_R, grado = 2) {
  # 1. Pasamos el objeto al entorno de Julia con un nombre fijo
  julia_assign("df_temp_in", df_nombre_en_R)
  # 2. Ejecutamos la expansión dentro de Julia (el ';' evita que Julia intente imprimir el resultado en la consola de R)
  julia_command(sprintf("df_temp_out = polexp(df_temp_in, grado = %d);", grado))
  # 3. Traemos el resultado de vuelta de forma explícita
  res <- julia_eval("df_temp_out")
  # 4. Limpieza opcional en Julia para liberar RAM
  julia_command("df_temp_in = nothing; df_temp_out = nothing; GC.gc();")
  names(res) = normalizar_nombres(names(res))
  return(res)
}

#### Normalizador de nombres en R (Corregido para manejar puntos) ####

#### Normalizador de nombres en R (Reemplazo directo por puntos) ####
normalizar_nombres <- function(vector_nombres) {
  sapply(vector_nombres, function(nom) {
    # 1. Reemplazar directamente asteriscos (*) y gorritos (^) por puntos (.)
    nom_con_puntos <- gsub("\\*", ".", nom)
    nom_con_puntos <- gsub("\\^", ".", nom_con_puntos)
    
    # 2. Separar por el punto para poder ordenar las partes
    partes <- unlist(strsplit(nom_con_puntos, "\\."))
    
    # 3. Ordenar alfabéticamente
    # Las letras se ordenarán primero y el "2" se mantendrá al final (ej. "A", "2")
    partes_ordenadas <- sort(partes)
    
    # 4. Volver a pegar con puntos
    paste(partes_ordenadas, collapse = ".")
  }, USE.NAMES = FALSE)
}

#### Funcion para calcular las metricas ####

# Función para calcular métricas multiclase
obtener_metricas <- function(conf_matrix) {
  # Extraer métricas globales y por clase (promedio macro)
  acc <- conf_matrix$overall["Accuracy"]
  prec <- mean(conf_matrix$byClass[, "Precision"], na.rm = TRUE)
  rec  <- mean(conf_matrix$byClass[, "Recall"], na.rm = TRUE)
  f1   <- mean(conf_matrix$byClass[, "F1"], na.rm = TRUE)
  return(c(Accuracy = acc, Precision = prec, Recall = rec, F1 = f1))
}

############3

library(stringr) # Necesaria para el wrap de texto
library(ggplot2)
library(MASS)
library(dplyr)

# df_eval <- df_expandido[, vars_e2_ready, drop = FALSE]
# ylab <- target # Garantizar que sea un factor
# X_final = df_eval
# y_labels = ylab
# nombre_ds = "Cebolla10D"
# mejor_fila = mejor_fila


graficar_proyeccion_tesis <- function(X_final, y_labels, nombre_ds, mejor_fila) {
  
  # 1. Ajustar el modelo LDA
  modelo_lda <- lda(X_final, grouping = y_labels)
  proyeccion <- predict(modelo_lda)$x %>% as.data.frame()
  
  # 2. Preparar el dataframe para ggplot
  df_plot <- proyeccion
  df_plot$Clase <- as.factor(y_labels)
  num_clases <- length(unique(y_labels))
  
  
  # 1. Obtienes el vector de variables de la mejor corrida
  vars_vec <- unlist(strsplit(gsub(" ", "", mejor_fila$VarsE2), ","))
  
  # 2. Usas la NUEVA función de formato gráfico
  clasificadas <- organizar_variables_gráficas(vars_vec)
  
  # 3. Creas el string de la leyenda (Caption) con los conteos y el vector formateado
  # Separar por tipo (asumiendo que Leyenda_Formateada ya viene limpia tipo X1, X1², X1*X2)
  lin <- clasificadas$Lineales
  quad <- clasificadas$Cuadraticos
  inter <- clasificadas$Interacciones
  
  bloques <- c()
  
  if (length(lin) > 0) {
    bloques <- c(
      bloques,
      paste0("Lin ", length(lin), ": ", paste(lin, collapse = ", "))
    )
  }
  
  if (length(quad) > 0) {
    bloques <- c(
      bloques,
      paste0("Quad ", length(quad), ": ", paste(quad, collapse = ", "))
    )
  }
  
  if (length(inter) > 0) {
    bloques <- c(
      bloques,
      paste0("Interact ", length(inter), ": ", paste(inter, collapse = ", "))
    )
  }
  
  # opcional: incluir "Otros"
  if (length(clasificadas$Otros) > 0) {
    bloques <- c(
      bloques,
      paste0("Otros ", length(clasificadas$Otros), ": ", 
             paste(clasificadas$Otros, collapse = ", "))
    )
  }
  
  resumen_vars <- paste(bloques, collapse = "\n")
  
  
  # --- LÓGICA DE GRAFICACIÓN ---
  if (num_clases == 2) {
    df_plot$LD2 <- 0
    p <- ggplot(df_plot, aes(x = LD1, fill = Clase, color = Clase)) +
      # Curvas de densidad suaves
      geom_density(alpha = 0.3) +
      # Los puntos en la base para ver cada observación real
      geom_rug(sides = "b", size = 1) + 
      labs(x = "Discriminante Lineal 1 (LD1)", y = "Densidad")
    
  } else {
    p <- ggplot(df_plot, aes(x = LD1, y = LD2, color = Clase, fill = Clase)) +
      geom_point(size = 3, alpha = 0.7) +
      stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95) +
      labs(x = "Discriminante Lineal 1 (LD1)", y = "Discriminante Lineal 2 (LD2)")
  }
  
  # 3. Estética Final
  p <- p + theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = paste("GPN-LDA Projection:", nombre_ds),
      subtitle = paste0("Accuracy: ", round(mejor_fila$Accuracy, 3), 
                        " | Total Vars: ", length(vars_vec)),
      caption = resumen_vars
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, family = "mono"),
      plot.caption = element_text(size = 9, face = "italic", color = "darkblue", hjust = 0)
    )
  p
  return(p)
}

