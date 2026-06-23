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


# =========================================================================
# SCRIPT UNIFICADO: GENERACIÓN DUAL (TRÍPTICOS E INDIVIDUALES) - VERSIÓN TESIS
# =========================================================================

rm(list=ls())

library(MASS)
library(kernlab)
library(ggplot2)
library(gridExtra)
library(JuliaCall)

# 1. Configuración de Entornos y Rutas Absolutas
ruta_base        <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO"
ruta_tripticos   <- file.path(ruta_base, "Gráficos Tripticos")
ruta_individuales <- file.path(ruta_base, "Gráficos Individuales")

# Crear directorios estructurados si no existen
if (!dir.exists(ruta_tripticos))   dir.create(ruta_tripticos, recursive = TRUE)
if (!dir.exists(ruta_individuales)) dir.create(ruta_individuales, recursive = TRUE)

# Carga de funciones externas de soporte
source(file.path(ruta_base, "FUNCIONES SEMAFORO REV1.R"))
source(file.path(ruta_base, "KFDA MANUAL.r"))

# Tabla de metadatos oficial del experimento
tabla_meta <- data.frame(
  Caso    = 1:6,
  Obs     = c(150, 150, 15, 15, 15, 15),
  Vars    = c(3, 10, 3, 10, 15, 17),
  Atr_Pol = c(9, 65, 9, 65, 135, 170)
)

archivos_csv <- list.files(path = ruta_base, pattern = "^caso_.*\\.csv$", full.names = TRUE)
archivos_csv <- archivos_csv[order(gsub(".*caso_([0-9]+)_.*", "\\1", archivos_csv))]

# 2. Definición Estricta de la Paleta de Color Institucional UV y Comentarios Corregidos
paleta_uv <- c("G_1" = "#B22222",   # Rojo Alizarina (Capa Interna)
               "G_2" = "#02963E",   # Verde UV (Capa Media)
               "G_3" = "#043B7B")   # Azul UV (Capa Externa)

# Configuración del Estilo Cohesivo para la Tesis (Sin leyendas)
estilo_base <- theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle   = element_text(size = 9.5, color = "gray25", hjust = 0.5),
    axis.title      = element_text(size = 10, face = "bold"),
    axis.text       = element_text(size = 9),
    legend.position = "none" # Eliminación absoluta de leyendas en el entorno global
  )

# Parámetros estandarizados para alta nitidez y presencia de puntos
ajuste_puntos <- list(
  geom_point(shape = 21, size = 3, stroke = 0.4, color = "#FFFFFF", alpha = 1.0),
  scale_fill_manual(values = paleta_uv),
  scale_color_manual(values = paleta_uv)
)

# 3. Ciclo de Ejecución y Modelado
for (i in 1:length(archivos_csv)) {
  
  num_caso <- i
  meta <- tabla_meta[tabla_meta$Caso == num_caso, ]
  sub_titulo_lineal <- paste0("Obs: ", meta$Obs, " | Vars: ", meta$Vars, " | Terms: ", meta$Atr_Pol)
  
  cat(sprintf("\nPROCESANDO CASO %d (N=%d, D=%d) -> Flujo Dual Simplificado\n", num_caso, meta$Obs, meta$Vars))
  
  df_vivos <- read.csv(archivos_csv[i], stringsAsFactors = FALSE)
  X_original <- as.matrix(df_vivos[, names(df_vivos) != "class"])
  y <- as.factor(df_vivos$class)
  
  # --- Expansión Polinomial Explícita vía Julia (Exclusiva para aproximaciones lineales) ---
  X_expandido <- polexpj(as.data.frame(X_original), grado = 2)
  
  # -------------------------------------------------------------------------
  # MODELO 1: LDA CLÁSICO
  # -------------------------------------------------------------------------
  if (num_caso >= 4) {
    g1 <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "INCOMPUTABLE\n(Problema SSS: D >> N)", size = 3.5, color = "#B22222", fontface = "bold") +
      theme_void() + labs(title = "LDA Clásico", subtitle = sub_titulo_lineal) + estilo_base
  } else {
    md1E <- LDAM(X_expandido, y)
    if (md1E$estado == "Sw singular") {
      g1 <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "INCOMPUTABLE\n(Sw Singular)", size = 3.5, color = "#B22222", fontface = "bold") +
        theme_void() + labs(title = "LDA Clásico", subtitle = sub_titulo_lineal) + estilo_base
    } else {
      df_manual <- data.frame(Re(md1E$proyecciones[, 1:2]))
      colnames(df_manual) <- c("LD1", "LD2")
      df_manual$class <- y
      
      g1 <- ggplot(df_manual, aes(x = LD1, y = LD2, fill = class, color = class)) +
        ajuste_puntos + 
        #coord_equal() + # Preservación de la relación de aspecto geométrica interna
        labs(title = "LDA Clásico", subtitle = sub_titulo_lineal, x = "Dimensión Discriminante 1", y = "Dimensión Discriminante 2") + 
        estilo_base
    }
  }
  
  # -------------------------------------------------------------------------
  # MODELO 2: LDA MASS (SVD)
  # -------------------------------------------------------------------------
  g2 <- tryCatch({
    mmd1E <- lda(X_expandido, y)
    m1 <- predict(mmd1E)
    df_mass <- data.frame(m1$x[, 1:2])
    colnames(df_mass) <- c("LD1", "LD2")
    df_mass$class <- y
    
    ggplot(df_mass, aes(x = LD1, y = LD2, fill = class, color = class)) +
      ajuste_puntos + 
      #coord_equal() + # Preservación de la relación de aspecto geométrica interna
      labs(title = "LDA MASS", subtitle = sub_titulo_lineal, x = "Dimensión Discriminante 1", y = "Dimensión Discriminante 2") + 
      estilo_base
  }, error = function(e) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "MASS Colapso Numérico", size = 3.5, color = "#B22222") +
      theme_void() + labs(title = "LDA MASS", subtitle = sub_titulo_lineal) + estilo_base
  })
  
  # -------------------------------------------------------------------------
  # MODELO 3: KFDA (KERNEL GAUSSIANO + HEURÍSTICA DE LA MEDIANA CORREGIDA)
  # -------------------------------------------------------------------------
  matriz_distancias <- as.matrix(dist(X_original))
  distancias_vivas <- matriz_distancias[lower.tri(matriz_distancias)]
  mediana_cuadrados <- median(distancias_vivas^2)
  
  # Corrección de la variable huérfana para mitigar indeterminaciones por sigmas infinitos
  if (mediana_cuadrados == 0) { 
    mediana_cuadrados <- 1e-5 
  }
  sigma_heuristico <- 1 / mediana_cuadrados
  
  funcion_rbf <- rbfdot(sigma = sigma_heuristico)
  K_train <- kernelMatrix(funcion_rbf, X_original)
  
  g3 <- tryCatch({
    modelo_kfda <- kfda_entrenar(K_train, y, r = NULL, reg = 1e-6)
    df_proy_kfda <- as.data.frame(modelo_kfda$Z_train)
    colnames(df_proy_kfda) <- c("KFDA1", "KFDA2")
    df_proy_kfda$class <- y
    
    sub_titulo_kfda <- paste0("Obs: ", meta$Obs, " | Vars: ", meta$Vars, " | σ: ", round(sigma_heuristico, 3))
    
    ggplot(df_proy_kfda, aes(x = KFDA1, y = KFDA2, fill = class, color = class)) +
      ajuste_puntos + 
      #coord_equal() + # Preservación de la relación de aspecto geométrica interna
      labs(title = "KFDA (Kernel Gaussiano)", subtitle = sub_titulo_kfda, x = "Dimensión Discriminante 1 (ψ₁)", y = "Dimensión Discriminante 2 (ψ₂)") + 
      estilo_base
  }, error = function(e) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "KFDA Colapso Numérico", size = 3.5, color = "darkorange") +
      theme_void() + labs(title = "KFDA (Kernel)", subtitle = sub_titulo_lineal) + estilo_base
  })
  
  # =========================================================================
  # FLUJO DE EXPORTACIÓN 1: GRÁFICOS INDIVIDUALES (SIN LEYENDA)
  # =========================================================================
  ggsave(file.path(ruta_individuales, sprintf("caso_%d_1_lda_clasico.png", num_caso)), plot = g1, width = 5.5, height = 5, dpi = 300)
  ggsave(file.path(ruta_individuales, sprintf("caso_%d_2_lda_mass.png", num_caso)),    plot = g2, width = 5.5, height = 5, dpi = 300)
  ggsave(file.path(ruta_individuales, sprintf("caso_%d_3_kfda.png", num_caso)),        plot = g3, width = 5.5, height = 5, dpi = 300)
  
  # =========================================================================
  # FLUJO DE EXPORTACIÓN 2: TRÍPTICO SIMPLIFICADO (SIN CÓDIGO MUERTO DE LEYENDAS)
  # =========================================================================
  # Ensamble directo en una sola fila simétrica limpia (1x3)
  triptico_final <- arrangeGrob(g1, g2, g3, ncol = 3)
  
  # Guardar tríptico conservando dimensiones e inyección estricta a disco
  ggsave(file.path(ruta_tripticos, sprintf("triptico_caso_%d.png", num_caso)), plot = triptico_final, width = 15, height = 4.8, dpi = 300)
  
  cat(sprintf("✓ Exportación dual simplificada del Caso %d completada.\n", num_caso))
}

cat("\n=========================================================\n")
cat("PROCESO TERMINADO: Gráficos de tesis generados con éxito.\n")
cat("=========================================================\n")
