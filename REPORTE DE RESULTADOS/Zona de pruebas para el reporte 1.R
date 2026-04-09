rm(list=ls())

# Cargar las funciones para el reporte
source("C:\\Users\\Angel\\Desktop\\RESULTADOS GPLDA LIMPIOS\\REPORTE DE RESULTADOS\\Funciones para el reporte 1.R")

# Cargar la ruta donde estan los dataframes
ruta1 = "C:\\Users\\Angel\\Desktop\\RESULTADOS GPLDA LIMPIOS\\FLUJO UNIFICADO\\DATASETS\\"
# Cargar la ruta donde estan los resultados de las 30 corridas por dataframe
ruta2 = "C:\\Users\\Angel\\Desktop\\RESULTADOS GPLDA LIMPIOS\\GPLDA CORRIDAS EN BASH\\30 corridas\\"


file.choose()
############
file.choose()
# --- PASO 1: Obtener rutas ---
dfr <- obtener_csv(ruta1)
dfr30 <- obtener_csv(ruta2)


# 2. Generar la tabla consolidada
tabla_maestra_tesis <- resumencorridas(dfr30)
tabla_maestra_tesis

library(flextable)
tb(tabla_maestra_tesis)




# --- PASO 2: Procesar el primer dataset ---
idx <- 3
nombre_actual <- dfr$nombre[idx]
df_orig <- read.csv(dfr$ruta[idx])
res_30  <- read.csv(dfr30$ruta[dfr30$nombre == nombre_actual])
names(df_orig)
# Mejor corrida
mejor_corrida <- res_30[which.max(res_30$Acc), ]

# --- PASO 3: Expansión inteligente (Lo que ya tienes) ---
target_col <- names(df_orig)[ncol(df_orig)]
vars_e1 <- unlist(strsplit(as.character(mejor_corrida$Nombres_E1), ", "))
X_reducido <- df_orig[, vars_e1, drop = FALSE]
y_labels <- df_orig[[target_col]]

X_expandido <- polexp(X_reducido, grado = 2)
terminos_e2 <- unlist(strsplit(as.character(mejor_corrida$Nombres_E2), ", "))
terminos_e2 = normalizar_nombres(terminos_e2)
names(X_expandido)  = normalizar_nombres(names(X_expandido))
normalizar_nombres(names(X_expandido))
X_final <- X_expandido[, terminos_e2, drop = FALSE]
terminos_e2

# --- PASO 4: Generar Gráfico ---
mi_grafico <- graficar_proyeccion_tesis(X_final, y_labels, nombre_actual, mejor_corrida)

# Visualizar
print(mi_grafico)

# Guardar para la tesis
ggsave(paste0("Proyeccion_LDA_", nombre_actual, ".png"), plot = mi_grafico, width = 10, height = 7, dpi = 300)

getwd()



#### Mini extractor de caracteristicas ####

# Crear un Data frame que contenga 
# Dataset: Nombre del dataset
# Muestras: Numero de observaciones
# Variables: Numero de variables
# Clases: Numero de clases
ruta1 = "C:\\Users\\Angel\\Desktop\\GPNLDA  EN JULIA\\DATASETS\\"
dfr <- obtener_csv(ruta1)
obtener_csv <- function(ruta_carpeta) {
  # 1. Obtener las rutas completas de los archivos .csv
  rutas <- list.files(
    path = ruta_carpeta,
    pattern = "\\.csv$",
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

resumen <- function(ruta){
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
resumencorridas(ruta2)

resu1 = resumen(ruta1)
tb(resu1)



