rm(list=ls())

# Cargar las funciones para el reporte
ruta0 = "C:\\Users\\Angel\\Desktop\\RESULTADOS GPLDA LIMPIOS\\REPORTE DE RESULTADOS\\Funciones para el reporte 1.R"
source(ruta0)
# Cargar la ruta donde estan los dataframes
ruta1 = "C:\\Users\\Angel\\Desktop\\RESULTADOS GPLDA LIMPIOS\\FLUJO UNIFICADO\\DATASETS\\"
# Cargar la ruta donde estan los resultados de las 30 corridas por dataframe
ruta2 = "C:\\Users\\Angel\\Desktop\\RESULTADOS GPLDA LIMPIOS\\GPLDA CORRIDAS EN BASH\\30 corridas\\"

############

#### PASO 1: Obtener rutas ####
dfr <- obtener_csv(ruta1)
dfr30 <- obtener_csv(ruta2)


### Hacer el resumen ###
resumendf(ruta1)

write.csv(resumendf(ruta1),"RESUMEN DATASETS.csv",row.names = F)

#### PASO 2: Generar la tabla maestra por dataset
library(flextable)
tabla_maestra_tesis <- resumencorridas(dfr30)
tb(tabla_maestra_tesis)

#### Paso 3: Procesar para gráficar los datasets ####
# Hacerlo como ciclo for para escalarlo

# Gráficador de resusltados con datos crudos

for(i in 1:nrow(dfr) ){
  # Cargar el dataset i-esimo y sus 30 corridas
  nombre_actual = dfr$nombre[i]
  df_orig <- read.csv(dfr$ruta[i])
  res_30  <- read.csv(dfr30$ruta[dfr30$nombre == nombre_actual])
  # Extraer la mejor corrida
  mejor_corrida <- res_30[which.max(res_30$Acc), ]
  ## Extraer de la etapa 1 ##
  target_col <- names(df_orig)[ncol(df_orig)]
  vars_e1 <- unlist(strsplit(as.character(mejor_corrida$Nombres_E1), ", "))
  X_reducido <- df_orig[, vars_e1, drop = FALSE]
  y_labels <- df_orig[[target_col]]
  ## Extraer lo de la etapa 2 normalizando las variables ##
  X_expandido <- polexp(X_reducido, grado = 2)
  # Normalizar los nombres de la corrida
  terminos_e2 = unlist(strsplit(as.character(mejor_corrida$Nombres_E2),
                                ", ")) |> normalizar_nombres()
  # Normalizar los nombres del dataset
  names(X_expandido)  = normalizar_nombres(names(X_expandido))
  # Extraer el DF final 
  X_final <- X_expandido[, terminos_e2, drop = FALSE]
  # Gráficar el resultado de la mejor corrida
  # --- PASO 4: Generar Gráfico ---
  mi_grafico <- graficar_proyeccion_tesis(X_final, y_labels, nombre_actual, mejor_corrida)
  # Guardar el gráfico 
  # Guardar para la tesis
  ggsave(paste0("1.LDA-RAW", nombre_actual, ".png"), plot = mi_grafico, width = 10, height = 7, dpi = 300)
  
}

# Graficador de los datos pero normalizados #
i = 1
for(i in 1:nrow(dfr) ){
  # Cargar el dataset i-esimo y sus 30 corridas
  nombre_actual = dfr$nombre[i]
  df_orig <- read.csv(dfr$ruta[i])
  res_30  <- read.csv(dfr30$ruta[dfr30$nombre == nombre_actual])
  # Extraer la mejor corrida
  mejor_corrida <- res_30[which.max(res_30$Acc), ]
  ## Extraer de la etapa 1 ##
  target_col <- names(df_orig)[ncol(df_orig)]
  vars_e1 <- unlist(strsplit(as.character(mejor_corrida$Nombres_E1), ", "))
  X_reducido <- as.data.frame(scale(df_orig[, vars_e1, drop = FALSE]))
  y_labels <- df_orig[[target_col]]
  ## Extraer lo de la etapa 2 normalizando las variables ##
  X_expandido <- polexp(X_reducido, grado = 2)
  # Normalizar los nombres de la corrida
  terminos_e2 = unlist(strsplit(as.character(mejor_corrida$Nombres_E2),
                                ", ")) |> normalizar_nombres()
  # Normalizar los nombres del dataset
  names(X_expandido)  = normalizar_nombres(names(X_expandido))
  # Extraer el DF final 
  X_final <- X_expandido[, terminos_e2, drop = FALSE]
  # Gráficar el resultado de la mejor corrida
  # --- PASO 4: Generar Gráfico ---
  mi_grafico <- graficar_proyeccion_tesis(X_final, y_labels, nombre_actual, mejor_corrida)
  # Guardar el gráfico 
  # Guardar para la tesis
  ggsave(paste0("2.LDA-STD", nombre_actual, ".png"), plot = mi_grafico, width = 10, height = 7, dpi = 300)
  
}

### Intentar con la estandarización minimax ####
i = 1
library(scales)
for(i in 1:nrow(dfr) ){
  # Cargar el dataset i-esimo y sus 30 corridas
  nombre_actual = dfr$nombre[i]
  df_orig <- read.csv(dfr$ruta[i])
  res_30  <- read.csv(dfr30$ruta[dfr30$nombre == nombre_actual])
  # Extraer la mejor corrida
  mejor_corrida <- res_30[which.max(res_30$Acc), ]
  ## Extraer de la etapa 1 ##
  target_col <- names(df_orig)[ncol(df_orig)]
  vars_e1 <- unlist(strsplit(as.character(mejor_corrida$Nombres_E1), ", "))
  X_reducido <-as.data.frame(apply(df_orig[, vars_e1, drop = FALSE],2,rescale))
  y_labels <- df_orig[[target_col]]
  ## Extraer lo de la etapa 2 normalizando las variables ##
  X_expandido <- polexp(X_reducido, grado = 2)
  # Normalizar los nombres de la corrida
  terminos_e2 = unlist(strsplit(as.character(mejor_corrida$Nombres_E2),
                                ", ")) |> normalizar_nombres()
  # Normalizar los nombres del dataset
  names(X_expandido)  = normalizar_nombres(names(X_expandido))
  # Extraer el DF final 
  X_final <- X_expandido[, terminos_e2, drop = FALSE]
  # Gráficar el resultado de la mejor corrida
  # --- PASO 4: Generar Gráfico ---
  mi_grafico <- graficar_proyeccion_tesis(X_final, y_labels, nombre_actual, mejor_corrida)
  # Guardar el gráfico 
  # Guardar para la tesis
  ggsave(paste0("3.LDA-MINMAX", nombre_actual, ".png"), plot = mi_grafico, width = 10, height = 7, dpi = 300)
  
}


#### De aquí para abajo, se puede intentar replicar lo de arriba pero con los kernells ####



