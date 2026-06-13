
# Inicializar entorno de Julia y cargar scripts de expansión polinomial
library(stringr)
library(JuliaCall)

julia_setup()
julia_command('include("POLEXP R1.jl")')

# ── FUNCIONES DE SOPORTE SINTÁCTICO (POLEXP R1.r) ─────────────────────────────

normalizar_nombres <- function(vector_nombres) {
  sapply(vector_nombres, function(nom) {
    nom_con_puntos <- gsub("\\*", ".", nom)
    nom_con_puntos <- gsub("\\^", ".", nom_con_puntos)
    partes <- unlist(strsplit(nom_con_puntos, "\\."))
    partes_ordenadas <- sort(partes)
    paste(partes_ordenadas, collapse = ".")
  }, USE.NAMES = FALSE)
}

obtener_dataset_gnlpda <- function(df_expandido, res_gnlpda, idx_fila) {
  vars_crudas <- gsub(" ", "", unlist(strsplit(res_gnlpda$Nombres_E2[idx_fila], ",")))
  variables_seleccionadas <- normalizar_nombres(vars_crudas)
  variables_seleccionadas <- gsub("^2\\.", "X2.", variables_seleccionadas)
  
  # Control de equivalencia para tolerar el prefijo 'X2.' autogenerado por R
  colnames(df_expandido) <- gsub("^2\\.", "X2.", colnames(df_expandido))
  
  variables_validas <- variables_seleccionadas[variables_seleccionadas %in% colnames(df_expandido)]
  
  if (length(variables_validas) < length(variables_seleccionadas)) {
    vars_faltantes <- variables_seleccionadas[!variables_seleccionadas %in% colnames(df_expandido)]
    warning(paste0("En la fila ", idx_fila, " no se encontraron las columnas: ", 
                   paste(vars_faltantes, collapse = ", "), ". Se procedió solo con las existentes."))
  }
  
  df_subconjunto <- data.frame(
    df_expandido[, variables_validas, drop = FALSE],
    Clase = df_expandido$Clase
  )
  return(df_subconjunto)
}

polexpj <- function(df_nombre_en_R, grado = 2) {
  julia_assign("df_temp_in", df_nombre_en_R)
  julia_command(sprintf("df_temp_out = polexp(df_temp_in, grado = %d);", grado))
  res <- julia_eval("df_temp_out")
  julia_command("df_temp_in = nothing; df_temp_out = nothing; GC.gc();")
  names(res) <- normalizar_nombres(names(res))
  return(res)
}






#### Cargar los gráficos ####

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

