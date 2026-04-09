
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


