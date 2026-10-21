rm(list = ls())

# 1. Cargar las librerías necesarias
library(dplyr)
library(readr)
library(purrr)

# 2. Definir rutas de acceso
ruta_directorio <- "C:/Users/Angeal/Desktop/GNLPDA/RESULTADOS JUNIO/RESULTADOS DE CALIBRACIÓN/RESULTADOS_GNLPDA"
mdruta          <- "C:/Users/Angeal/Desktop/GNLPDA/RESULTADOS JUNIO/RESULTADOS DE CALIBRACIÓN/METADATOS DATOS.csv"

# 3. Listar archivos CSV que inicien con el patrón "Número.Número."
patron_filtro <- "^[0-9]+\\.[0-9]+\\..*\\.csv$"

archivos <- list.files(
  path = ruta_directorio, 
  pattern = patron_filtro, 
  full.names = TRUE
)

# 4. Cargar metadatos
metadatos <- read.csv(mdruta)

# 5. Función de lectura y preprocesamiento individual
procesar_archivo <- function(ruta) {
  df <- read_csv(ruta, show_col_types = FALSE)
  
  df <- df %>% 
    mutate(Dataset = basename(ruta)) %>% 
    mutate(Corrida = row_number())
  
  return(df)
}

# 6. Consolidar datos, asociar metadatos y calcular métricas de reducción
datos_completos <- archivos %>% 
  map_df(~ procesar_archivo(.x)) %>% 
  # Filtrar corridas excluidas
  filter(!Corrida %in% c(11, 32, 33)) %>% 
  # Unir metadatos utilizando la columna Referencia
  left_join(metadatos, by = c("Dataset" = "Referencia")) %>% 
  # Cálculo de reducciones por fila (Etapa 1 y Etapa 2)
  mutate(
    # Reducción Etapa 1
    Reduccion_E1 = round((1 - (Vars_E1 / Variables))*100,3),
    
    # Espacio expandido cuadrático: choose(k + 2, 2) - 1
    Total_Expandido_E2 = choose(Vars_E1 + 2, 2) - 1,
    
    # Reducción Etapa 2
    Reduccion_E2 = round((1 - Vars_E2 / Total_Expandido_E2)*100,3)
  )

# 7. Generar la tabla de resumen estadístico por Dataset
tabla_resumen_reduccion <- datos_completos %>% 
  group_by(Dataset) %>% 
  summarise(
    # Estadísticos para Etapa 1
    Red_E1_Min    = min(Reduccion_E1, na.rm = TRUE),
    Red_E1_Max    = max(Reduccion_E1, na.rm = TRUE),
    Red_E1_Mean   = mean(Reduccion_E1, na.rm = TRUE),
    Red_E1_Median = median(Reduccion_E1, na.rm = TRUE),
    Red_E1_Std    = sd(Reduccion_E1, na.rm = TRUE),
    
    # Estadísticos para Etapa 2
    Red_E2_Min    = min(Reduccion_E2, na.rm = TRUE),
    Red_E2_Max    = max(Reduccion_E2, na.rm = TRUE),
    Red_E2_Mean   = mean(Reduccion_E2, na.rm = TRUE),
    Red_E2_Median = median(Reduccion_E2, na.rm = TRUE),
    Red_E2_Std    = sd(Reduccion_E2, na.rm = TRUE)
  )

# 8. Redondear valores numéricos a 2 decimales y visualizar
tabla_resumen_reduccion[,-1] <- round(tabla_resumen_reduccion[,-1], digits = 3)
tabla_resumen_reduccion$Variables = metadatos$Variables
print(tabla_resumen_reduccion)

# 9. Exportar resultados a CSV
write_csv(tabla_resumen_reduccion, file.path(ruta_directorio, "Resumen_Reduccion_GNLPDA.csv"))
