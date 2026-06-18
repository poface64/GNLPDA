rm(list=ls())

#### Cargar las funciones necesarias ####
source("C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO\\FUNCIONES SEMAFORO REV1.R")

#### Generar los datos tipo cebolla ####

# 1. Configuración del Entorno y Rutas
# Nota: Se duplican las barras invertidas para que R interprete la ruta de Windows correctamente.
ruta_salida <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO"

if (!dir.exists(ruta_salida)) {
  dir.create(ruta_salida, recursive = TRUE)
}

# 2. Definición de la Tabla de Metadatos (6 Casos)
tabla_casos <- data.frame(
  Caso    = 1:6,
  Obs     = c(150, 150, 15, 15, 15, 15),
  Vars    = c(3, 10, 3, 10, 15, 17),
  Atr_Pol = c(9, 65, 9, 65, 135, 170)
)
## Generar los datos experimentales ##
cat("Iniciando la generación de los 6 casos experimentales...\n")
cat("---------------------------------------------------------\n")

for (i in 1:nrow(tabla_casos)) {
  
  # Aislamiento metodológico: Semilla única secuencial por caso (1 a 6)
  id_caso <- tabla_casos$Caso[i]
  set.seed(id_caso)
  
  # Generación de la estructura geométrica base
  datos_base <- cebollaND(
    n_samples = tabla_casos$Obs[i],
    n_dims    = tabla_casos$Vars[i],
    n_classes = 3,
    noise     = 0.1
  )
  
  # Construcción del nombre del archivo dinámico
  nombre_archivo <- sprintf("caso_%d_N%d_D%d.csv", 
                            id_caso, 
                            tabla_casos$Obs[i], 
                            tabla_casos$Vars[i])
  
  ruta_completa <- file.path(ruta_salida, nombre_archivo)
  
  # Exportación a disco (sin nombres de renglones para optimizar la lectura posterior)
  write.csv(datos_base, file = ruta_completa, row.names = FALSE)
  
  cat(sprintf("✓ Caso %d exportado exitosamente: %s (N=%d, D=%d)\n", 
              id_caso, nombre_archivo, tabla_casos$Obs[i], tabla_casos$Vars[i]))
}


#### Aquí empieza la parte gráfica pesada ####
rm(list=ls())

# Carga de librerías requeridas
library(MASS)
library(ggplot2)
library(gridExtra) # Para emparejar las gráficas visualmente
library(JuliaCall)
source("C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO\\FUNCIONES SEMAFORO REV1.R")

# 1. Configuración de Entornos y Rutas
ruta_carpeta <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO"

# Carga de funciones externas (Asegurar que las correcciones de Sw estén hechas en este archivo)
source(file.path(ruta_carpeta, "FUNCIONES SEMAFORO REV1.R"))

# Listar de forma ordenada los archivos CSV generados previamente
archivos_csv <- list.files(path = ruta_carpeta, pattern = "^caso_.*\\.csv$", full.names = TRUE)

# Opcional: Asegurar orden numérico de los archivos por si el OS los desordena
archivos_csv <- archivos_csv[order(gsub(".*caso_([0-9]+)_.*", "\\1", archivos_csv))]

# 2. Ciclo Principal del Experimento
for (i in 1:length(archivos_csv)) {
  
  # --- PASO 1: Carga del caso i-ésimo ---
  ruta_actual <- archivos_csv[i]
  nombre_base <- basename(ruta_actual)
  cat(sprintf("\n=========================================================\n"))
  cat(sprintf("PROCESANDO: %s\n", nombre_base))
  cat(sprintf("=========================================================\n"))
  
  df_vivos <- read.csv(ruta_actual, stringsAsFactors = FALSE)
  df_vivos$class <- as.factor(df_vivos$class)
  
  # --- PASO 2: Separación de Variables (X) y Etiquetas (Y) ---
  X_original <- df_vivos[, names(df_vivos) != "class"]
  Y <- df_vivos$class
  
  # --- PASO 3: Expansión Polinomial Grado 2 vía Julia ---
  cat("Ejecutando expansión polinomial en Julia...\n")
  X_expandido <- polexpj(X_original, grado = 2)
  
  # Almacén temporal para los plots de esta iteración
  plot_lda_puro <- NULL
  plot_lda_mass <- NULL
  
  # =======================================================================
  # --- PASO 4: Modelado y Proyección con LDA PURO ---
  # =======================================================================
  cat("Evaluando LDA Puro...\n")
  resultado_puro <- LDAM(X_expandido, Y)
  
  if (resultado_puro$estado != "OK") {
    cat(sprintf("[!] LDA Puro no computable en este caso. Razón: %s\n", resultado_puro$estado))
    
    # Generar una gráfica vacía con la notificación del error numérico
    plot_lda_puro <- ggplot() + 
      annotate("text", x = 4, y = 4, label = sprintf("INCOMPUTABLE\n(%s)", resultado_puro$estado), 
               color = "darkred", size = 5, fontface = "bold") +
      theme_void() +
      labs(title = paste("LDA Puro -", nombre_base)) +
      theme(plot.title = element_text(hjust = 0.5, color = "darkred"))
  } else {
    # Si es computable, estructurar las proyecciones (Se asumen nr-1 = 2 dimensiones proyectadas)
    df_proy_pura <- as.data.frame(resultado_puro$proyecciones)
    df_proy_pura$Clase <- Y
    
    plot_lda_puro <- ggplot(df_proy_pura, aes(x = ND1, y = ND2, color = Clase)) +
      geom_point(size = 2.5, alpha = 0.8) +
      labs(title = paste("LDA Puro -", nombre_base), x = "Discriminante 1", y = "Discriminante 2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # =======================================================================
  # --- PASO 5: Modelado y Proyección con LDA MASS ---
  # =======================================================================
  cat("Evaluando LDA MASS...\n")
  
  # Encapsular en tryCatch por extrema seguridad, aunque SVD es altamente resiliente
  plot_lda_mass <- tryCatch({
    modelo_mass <- lda(X_expandido, grouping = Y)
    proyeccion_mass <- predict(modelo_mass, X_expandido)$x
    
    df_proy_mass <- as.data.frame(proyeccion_mass)
    df_proy_mass$Clase <- Y
    
    ggplot(df_proy_mass, aes(x = LD1, y = LD2, color = Clase)) +
      geom_point(size = 2.5, alpha = 0.8) +
      labs(title = paste("LDA MASS -", nombre_base), x = "LD 1", y = "LD 2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }, error = function(e) {
    ggplot() + 
      annotate("text", x = 4, y = 4, label = "MASS COLAPSO (Inesperado)", color = "red", size = 5) +
      theme_void() +
      labs(title = paste("LDA MASS -", nombre_base))
  })
  
  # =======================================================================
  # --- PASO 6: Visualización Emparejada (Lado a Lado) y Guardado ---
  # =======================================================================
  # Combinar los dos gráficos en un solo lienzo comparativo
  grafica_comparativa <- grid.arrange(plot_lda_puro, plot_lda_mass, ncol = 2)
  
  # Construcción del nombre del gráfico de salida
  nombre_grafico <- sprintf("comparativa_caso_%d.png", i)
  ruta_grafico <- file.path(ruta_carpeta, nombre_grafico)
  
  # Guardar a disco en alta resolución para la tesis (300 DPI)
  ggsave(ruta_grafico, plot = grafica_comparativa, width = 12, height = 5, dpi = 300)
  cat(sprintf("✓ Gráfica comparativa guardada: %s\n", nombre_grafico))
}

cat("\n---------------------------------------------------------\n")
cat("Experimento finalizado. Las proyecciones visuales están listas para análisis.\n")






