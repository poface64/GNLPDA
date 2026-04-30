
#### Cargar las rutas ####
rutaRES = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS ABRIL\\CORRIDAS DE 30 NORMALIZADAS\\"
# Buscar archivos que comiencen con "Competidores_Final_"
archivos <- list.files(
  path = rutaRES,
  pattern = "^Competidores_Final_",
  full.names = TRUE
)

# Guardar los resultados 
resumen1 = data.frame()
nombres_dataset <- sub(".*Competidores_Final_", "", archivos)
for(i in 1: length(archivos)){
  # Cargar el dataset
  restemp = read.csv(archivos[i])
  restemp[,-1] = round(restemp[,-1],3)
  restemp$Dataset = nombres_dataset[i]
  resumen1 = rbind.data.frame(resumen1,restemp)
  
}

write.csv(resumen1,"Resultados por métodos.csv",row.names = F)

#### Tabla bonita ####

library(tidyverse)

# 1. Crear el formato "Media ± SD" y limpiar nombres
resumen_formateado <- resumen1 %>%
  # Creamos la columna combinada
  mutate(Valor = paste0(round(Accuracy, 2), " ± ", round(Std_Acc, 2)),
         # Limpiamos el nombre del dataset para que se vea como en la imagen
         Dataset = sub("\\.csv$", "", Dataset),
         Dataset = sub("^[0-9]+\\.[0-9]+\\.", "", Dataset)) %>%
  # Nos quedamos solo con lo necesario
  select(Dataset, Metodo, Valor)

# 2. Pivotar la tabla para que los métodos sean columnas
tabla_final <- resumen_formateado %>%
  pivot_wider(names_from = Metodo, values_from = Valor)

# 3. Ver el resultado
print(tabla_final)

# Opcional: Guardar a CSV listo para Excel o LaTeX
write.csv(tabla_final, "resultados por metodo estilizados.csv", row.names = FALSE)



### RESULTADOS DEL GNLPDA SOLO ###

#### Cargar las rutas ####
rutaRES = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS ABRIL\\CORRIDAS DE 30 NORMALIZADAS\\"

# Buscar archivos que comiencen con "Competidores_Final_"
archivos <- list.files(
  path = rutaRES,
  pattern = "^Resumen_FinalR_",
  full.names = TRUE
)
archivos2<- list.files(
  path = rutaRES,
  pattern = "^Resumen_Estadistico_",
  full.names = TRUE
)


# Guardar los resultados 
resumen1 = data.frame()
nombres_dataset <- sub(".*Resumen_FinalR_", "", archivos)
for(i in 1: length(archivos)){
  # Cargar el dataset
  resR = read.csv(archivos[i])
  resjul = read.csv(archivos2[i])
  #----------
  
}


##################################


library(tidyverse)

#### Cargar las rutas ####
rutaRES = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS ABRIL\\CORRIDAS DE 30 NORMALIZADAS\\"

archivos  <- list.files(path = rutaRES, pattern = "^Resumen_FinalR_", full.names = TRUE)
archivos2 <- list.files(path = rutaRES, pattern = "^Resumen_Estadistico_", full.names = TRUE)

resumen1 = data.frame()
nombres_dataset <- sub(".*Resumen_FinalR_", "", archivos)

for(i in 1:length(archivos)){
  # 1. Cargar ambos reportes
  resR   <- read.csv(archivos[i])
  resjul <- read.csv(archivos2[i])
  
  # 2. Procesar reporte de Julia (Vars y Tiempo)
  julia_temp <- resjul %>%
    mutate(Valor = paste0(format(round(Media, 2), nsmall = 2), " ± ", format(round(Std_Dev, 2), nsmall = 2))) %>%
    select(Metrica, Valor) %>%
    pivot_wider(names_from = Metrica, values_from = Valor)
  
  # 3. Procesar reporte de R (Métricas de validación)
  # Agregamos Accuracy a la cadena de procesamiento
  r_temp <- resR %>%
    mutate(
      Dataset   = sub("\\.csv$", "", nombres_dataset[i]),
      # Formateo con 2 decimales fijos (nsmall asegura que salga 0.90 y no 0.9)
      Accuracy  = paste0(format(round(Mean_Accuracy, 2), nsmall = 2), " ± ", format(round(Std_Accuracy, 2), nsmall = 2)),
      Precision = paste0(format(round(Mean_Precision, 2), nsmall = 2), " ± ", format(round(Std_precision, 2), nsmall = 2)),
      Recall    = paste0(format(round(Mean_Recall, 2), nsmall = 2), " ± ", format(round(Std_Recall, 2), nsmall = 2)),
      F1Score   = paste0(format(round(Mean_F1, 2), nsmall = 2), " ± ", format(round(Std_F1, 2), nsmall = 2))
    ) %>%
    select(Dataset, Accuracy, Precision, Recall, F1Score)
  
  # 4. Extraer No. Vars (Entero de la media de Vars_E1)
  no_vars_val <- floor(resjul$Media[resjul$Metrica == "Vars_E1"]) 
  
  # 5. Ensamblar la fila completa para el Dataset i
  fila_completa <- data.frame(
    Dataset   = r_temp$Dataset,
    No_Vars   = no_vars_val,
    Vars_E1   = julia_temp$Vars_E1,
    Vars_E2   = julia_temp$Vars_E2,
    Accuracy  = r_temp$Accuracy,   # <-- Nueva columna agregada
    Precision = r_temp$Precision,
    Recall    = r_temp$Recall,
    F1Score   = r_temp$F1Score,
    Time_S    = julia_temp$Tiempo
  )
  
  # 6. Acumular
  resumen1 = rbind(resumen1, fila_completa)
}

# Limpieza final de nombres de dataset (quitar 1.2., etc)
resumen1$Dataset <- sub("^[0-9]+\\.[0-9]+\\.", "", resumen1$Dataset)

# Mostrar tabla final
print(resumen1)

write.csv(resumen1,"Resultados del algoritmo interno.csv",row.names = F)
