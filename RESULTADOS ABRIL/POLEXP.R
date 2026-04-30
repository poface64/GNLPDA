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

#### Normalizador de nombres en R ####
normalizar_nombres <- function(vector_nombres) {
  sapply(vector_nombres, function(nom) {
    # 1. Separar por el asterisco
    partes <- unlist(strsplit(nom, "\\*"))
    # 2. Ordenar alfabéticamente
    partes_ordenadas <- sort(partes)
    # 3. Volver a pegar
    paste(partes_ordenadas, collapse = "*")
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
    p <- ggplot(df_plot, aes(x = LD1, y = LD2, color = Clase)) +
      geom_hline(yintercept = 0, color = "gray80", size = 0.5) +
      geom_point(size = 3, alpha = 0.7) +
      labs(x = "Discriminante Lineal 1 (LD1)", y = "") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
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

