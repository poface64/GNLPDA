rm(list=ls())
# =========================================================================
# ORQUESTADOR PRINCIPAL: ANÁLISIS DEL MEJOR ESCENARIO POSIBLE (BEST-CASE)
# =========================================================================

library(MASS) # Para el LDA
library(kernlab) # Para la matriz Kernell
library(ggplot2) # Para gráficar
library(gridExtra) # Para juntar los gráficos
library(JuliaCall) # Para usar Julia polexp
library(dplyr) # Para manipular y ordenar datos

# 1. CONFIGURACIÓN CENTRAL DE RUTAS ABSOLUTAS
ruta_base             <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO"
ruta_datasets         <- file.path(ruta_base, "NUEVAS CORRIDAS DE 30 JUNIO\\DATASETS")
ruta_semillas         <- file.path(ruta_base, "NUEVAS CORRIDAS DE 30 JUNIO\\SEMILLAS GENERADAS")

# Directorios de origen para los logs de calibración (30 corridas)
ruta_logs_mass        <- file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\RESULTADOS_LDA")
ruta_logs_kfda        <- file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\RESULTADOS_KFDA")
ruta_logs_gnlpda      <- file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\RESULTADOS_GNLPDA")

# Carpetas de exportación final exclusivas para proyecciones reales
ruta_salida_tripticos <- file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO\\Gráficos proyecciones triptico")
ruta_salida_indiv     <- file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO\\Gráficos proyecciones individual")

# Creación automática de los directorios en disco si no existen
if (!dir.exists(ruta_salida_tripticos)) dir.create(ruta_salida_tripticos, recursive = TRUE)
if (!dir.exists(ruta_salida_indiv))     dir.create(ruta_salida_indiv, recursive = TRUE)

# 2. CARGA DE LA INFRAESTRUCTURA DE FUNCIONES MODULARES
# Nota: Asegurar que este archivo contenga: obtener_mejor_corrida(), 
# reconstruir_particion_ganadora(), proyectar_lda_mass(), proyectar_kfda(), 
# proyectar_gnlpda() y organizar_variables_gráficas().
source(file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO\\FUNCIONES PARA GRAFICADOR REV1.R"))
source(file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO\\KFDA MANUAL.r"))
source(file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO\\FUNCIONES SEMAFORO REV1.r"))


# 3. PALETA INSTITUCIONAL UV Y CONFIGURACIÓN ESTÉTICA BASE (Estática)
colores_institucionales_uv <- c("#043B7B", "#02963E", "#B22222",
                                "#FF8C00", "#9400D3")

estilo_base <- theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle   = element_text(size = 9.5, color = "gray25", hjust = 0.5),
    axis.title      = element_text(size = 10, face = "bold"),
    axis.text       = element_text(size = 9),
    legend.position = "none"
  )

# 4. ESCANEO DINÁMICO DEL CATÁLOGO DE DATASETS
archivos_logs_pivote <- list.files(path = ruta_logs_kfda, pattern = "\\.csv$")
datasets_maestros    <- unique(gsub("(^[^_]+)_.*", "\\1", archivos_logs_pivote))

cat(sprintf("[ENTORNO] Inicializado con éxito. Detectados %d datasets reales para evaluar.\n", length(datasets_maestros)))

# =========================================================================
# 5. BUCLE PRINCIPAL DE PROCESAMIENTO Y CRUCE EXPERIMENTAL
# =========================================================================
for (ds in datasets_maestros) {
  
  cat(sprintf("\n=========================================================\n"))
  cat(sprintf("PROCESANDO DATASET MAESTRO: %s\n", ds))
  cat(sprintf("=========================================================\n"))
  
  # --- PASO 1: Identificación independiente de la mejor corrida por método ---
  log_mass   <- obtener_mejor_corrida(ruta_logs_mass, ds, "lda")
  log_kfda   <- obtener_mejor_corrida(ruta_logs_kfda, ds, "kfda")
  log_gnlpda <- obtener_mejor_corrida(ruta_logs_gnlpda, ds, "gnlpda")
  
  if (is.null(log_mass) | is.null(log_kfda) | is.null(log_gnlpda)) {
    cat(sprintf("[!] Saltando %s debido a falta de bitácoras de calibración.\n", ds))
    next
  }
  
  # --- PASO 2: Reconstrucción e inyección de particiones óptimas ---
  id_run_mass   <- log_mass$Corrida[1]
  id_run_kfda   <- log_kfda$Corrida[1]
  id_run_gnlpda <- log_gnlpda$Corrida_Original[1]
  
  particion_mass   <- reconstruir_particion_ganadora(ruta_datasets, ruta_semillas, ds, id_run_mass)
  particion_kfda   <- reconstruir_particion_ganadora(ruta_datasets, ruta_semillas, ds, id_run_kfda)
  particion_gnlpda <- reconstruir_particion_ganadora(ruta_datasets, ruta_semillas, ds, id_run_gnlpda)
  
  num_clases <- length(particion_mass$clases_niveles)
  
  # --- PASO 3: Modelado y extracción de proyecciones de prueba (Test) ---
  coor_mass   <- proyectar_lda_mass(particion_mass)
  coor_kfda   <- proyectar_kfda(particion_kfda, log_kfda$Sigma_Optimo[1]) 
  coor_gnlpda <- proyectar_gnlpda(particion_gnlpda, log_gnlpda$Nombres_E2[1])
  
  # --- PASO 4: Construcción Adaptativa de Entornos Gráficos ---
  # Homogeneizar estructuras para el proyector
  df_g1 <- data.frame(Dim1 = coor_mass[,1],   class = particion_mass$y_test)
  df_g2 <- data.frame(Dim1 = coor_kfda[,1],   class = particion_kfda$y_test)
  df_g3 <- data.frame(Dim1 = coor_gnlpda[,1], class = particion_gnlpda$y_test)
  
  if (num_clases >= 3) {
    df_g1$Dim2 <- coor_mass[,2]
    df_g2$Dim2 <- coor_kfda[,2]
    df_g3$Dim2 <- coor_gnlpda[,2]
  }
  
  # MODIFICACIÓN CRÍTICA: Construcción de la paleta dinámica en tiempo de ejecución (Dentro del bucle)
  niveles_actuales <- levels(df_g1$class)
  num_niveles      <- length(niveles_actuales)
  
  paleta_dinamica  <- colores_institucionales_uv[1:num_niveles]
  names(paleta_dinamica) <- niveles_actuales
  
  ajuste_puntos <- list(
    geom_point(shape = 21, size = 3, stroke = 0.4, color = "#FFFFFF", alpha = 1.0),
    scale_fill_manual(values = paleta_dinamica),
    scale_color_manual(values = paleta_dinamica)
  )
  
  # NUEVO: Ajuste exclusivo para curvas de densidad (Solo escalas cromáticas)
  ajuste_densidades <- list(
    scale_fill_manual(values = paleta_dinamica),
    scale_color_manual(values = paleta_dinamica)
  )
  
  # BIFURCACIÓN GEOMÉTRICA DE ACUERDO AL NÚMERO DE CLASES (1D vs 2D Truncado)
  if (num_clases == 2) {
    # --- Geometría 1D: Curvas de Densidad Suaves + Alfombra ---
    g1 <- ggplot(df_g1, aes(x = Dim1, fill = class, color = class)) + geom_density(alpha = 0.35) + geom_rug(sides = "b", size = 0.8) + labs(x = "Dimensión Discriminante 1 ", y = "Densidad")  + ajuste_densidades
    g2 <- ggplot(df_g2, aes(x = Dim1, fill = class, color = class)) + geom_density(alpha = 0.35) + geom_rug(sides = "b", size = 0.8) + labs(x = "Dimensión Discriminante 1 (ψ₁)", y = "Densidad")  + ajuste_densidades
    g3 <- ggplot(df_g3, aes(x = Dim1, fill = class, color = class)) + geom_density(alpha = 0.35) + geom_rug(sides = "b", size = 0.8) + labs(x = "Dimensión Discriminante 1 ", y = "Densidad")  + ajuste_densidades
  } else {
    # --- Geometría 2D: Mapas de Dispersión Isométricos por Método ---
    g1 <- ggplot(df_g1, aes(x = Dim1, y = Dim2, fill = class, color = class)) + ajuste_puntos + labs(x = "Dimensión Discriminante 1", y = "Dimensión Discriminante 2")
    g2 <- ggplot(df_g2, aes(x = Dim1, y = Dim2, fill = class, color = class)) + ajuste_puntos + labs(x = "Dimensión Discriminante 1 (ψ₁)", y = "Dimensión Discriminante 2 (ψ₂)")
    g3 <- ggplot(df_g3, aes(x = Dim1, y = Dim2, fill = class, color = class)) + ajuste_puntos + labs(x = "Dimensión Discriminante 1", y = "Dimensión Discriminante 2")
  }
  
  # Sincronización estricta de la columna del log para el cálculo de Sigma
  sigma_valor <- log_kfda$Sigma_Optimo[1]
  sigma_texto <- if(sigma_valor < 0.001) format(sigma_valor, scientific = TRUE, digits = 3) else round(sigma_valor, 4)
  
  g1 <- g1 + labs(title = "LDA MASS", subtitle = sprintf("Test Accuracy: %s", round(log_mass$Accuracy[1], 3))) + estilo_base
  g2 <- g2 + labs(title = "KFDA (Kernel Gauss)", subtitle = sprintf("Test Accuracy: %s | σ: %s", round(log_kfda$Accuracy[1], 3), sigma_texto)) + estilo_base
  
  # =========================================================================
  # CONSTRUCCIÓN DEL CAPTION POLINOMIAL CON AJUSTE DE LÍNEA AUTOMÁTICO (GNLPDA)
  # =========================================================================
  vars_vec     <- unlist(strsplit(gsub(" ", "", log_gnlpda$Nombres_E2[1]), ","))
  clasificadas <- organizar_variables_gráficas(vars_vec)
  bloques      <- c()
  
  # Umbral de caracteres para forzar el salto de línea (Ideal para 5.5 in de ancho)
  ancho_maximo_texto <- 80
  
  # 1. Ajustar bloque de Lineales
  if (length(clasificadas$Lineales) > 0) {
    texto_lin <- paste0("Lin: ", paste(clasificadas$Lineales, collapse = ", "))
    # str_wrap segmenta el texto largo y indent_sub inserta espacios si salta de línea
    texto_lin_ajustado <- stringr::str_wrap(texto_lin, width = ancho_maximo_texto, indent = 0, exdent = 5)
    bloques <- c(bloques, texto_lin_ajustado)
  }
  
  # 2. Ajustar bloque de Cuadráticos
  if (length(clasificadas$Cuadraticos) > 0) {
    texto_quad <- paste0("Quad: ", paste(clasificadas$Cuadraticos, collapse = ", "))
    texto_quad_ajustado <- stringr::str_wrap(texto_quad, width = ancho_maximo_texto, indent = 0, exdent = 6)
    bloques <- c(bloques, texto_quad_ajustado)
  }
  
  # 3. Ajustar bloque de Interacciones
  if (length(clasificadas$Interacciones) > 0) {
    texto_inter <- paste0("Interact: ", paste(clasificadas$Interacciones, collapse = ", "))
    texto_inter_ajustado <- stringr::str_wrap(texto_inter, width = ancho_maximo_texto, indent = 0, exdent = 10)
    bloques <- c(bloques, texto_inter_ajustado)
  }
  
  # Unificar los bloques respetando las jerarquías verticales originales
  caption_gnlpda <- paste(bloques, collapse = "\n")
  
  # Inyección en el gráfico ggplot2 de GNLPDA
  g3 <- g3 + labs(
    title = "GNLPDA (Propuesto)", 
    subtitle = sprintf("Test Accuracy: %s | Total Vars: %d", round(log_gnlpda$Accuracy[1], 3), length(vars_vec)), 
    caption = caption_gnlpda
  ) + 
    estilo_base + 
    theme(plot.caption = element_text(size = 8.5, face = "italic", color = "#043B7B", hjust = 0, lineheight = 1.1))
  
  # =========================================================================
  # PASO 5: FLUJO DE EXPORTACIÓN GENERALIZADO Y COMPATIBLE (1D, 2D Y 3D)
  # =========================================================================
  
  # 5.1 Exportación Estándar (Curvas 1D para C=2, Planos 2D para C>=3)
  # Se inyecta el sufijo "_2d" para asegurar la coexistencia armónica de archivos
  ggsave(file.path(ruta_salida_indiv, sprintf("mejor_escenario_%s_1_mass_2d.png", ds)),   plot = g1, width = 5.5, height = 5, dpi = 300)
  ggsave(file.path(ruta_salida_indiv, sprintf("mejor_escenario_%s_2_kfda_2d.png", ds)),   plot = g2, width = 5.5, height = 5, dpi = 300)
  ggsave(file.path(ruta_salida_indiv, sprintf("mejor_escenario_%s_3_gnlpda_2d.png", ds)), plot = g3, width = 5.5, height = 5, dpi = 300)
  
  triptico_final_2d <- arrangeGrob(g1, g2, g3, ncol = 3)
  ggsave(file.path(ruta_salida_tripticos, sprintf("triptico_mejor_escenario_%s_2d.png", ds)), plot = triptico_final_2d, width = 15, height = 4.8, dpi = 300)
  
  # 5.2 Bloque de Activación para Espacios Multidimensionales (Modificado a >= 3 Clases)
  if (num_clases > 3) {
    
    # Instanciación local y dinámica de los vectores de color independientes por método
    colores_sujetos_mass   <- paleta_dinamica[as.character(particion_mass$y_test)]
    colores_sujetos_kfda   <- paleta_dinamica[as.character(particion_kfda$y_test)]
    colores_sujetos_gnlpda <- paleta_dinamica[as.character(particion_gnlpda$y_test)]
    
    # Parámetros estéticos fijos heredados de la calibración 3D
    angulo_rotacion <- 75
    tipo_punto      <- 16
    tamano_punto    <- 1.1
    
    # Guardado de Archivos Individuales en 3D (Sufijo explícito "_3d")
    # Panel 1: MASS 3D
    png(filename = file.path(ruta_salida_indiv, sprintf("mejor_escenario_%s_1_mass_3d.png", ds)), width = 5.5, height = 5, units = "in", res = 300)
    scatterplot3d(x = coor_mass[, 1], y = coor_mass[, 2], z = coor_mass[, 3], 
                  color = colores_sujetos_mass, pch = tipo_punto, cex.symbols = tamano_punto, angle = angulo_rotacion, 
                  grid = TRUE, box = FALSE, main = "LDA MASS (Espacio 3D)", 
                  xlab = "Dimensión Discriminante 1 (LD1)", ylab = "Dimensión Discriminante 2 (LD2)", zlab = "Dimensión Discriminante 3 (LD3)",
                  col.axis = "gray30", col.grid = "gray90", cex.main = 1.2, font.main = 2)
    dev.off()
    
    # Panel 2: KFDA 3D
    png(filename = file.path(ruta_salida_indiv, sprintf("mejor_escenario_%s_2_kfda_3d.png", ds)), width = 5.5, height = 5, units = "in", res = 300)
    scatterplot3d(x = coor_kfda[, 1], y = coor_kfda[, 2], z = coor_kfda[, 3], 
                  color = colores_sujetos_kfda, pch = tipo_punto, cex.symbols = tamano_punto, angle = angulo_rotacion, 
                  grid = TRUE, box = FALSE, main = "KFDA (Kernel Gauss)", 
                  xlab = expression(paste("Dimensión Discriminante 1 (", psi[1], ")")),
                  ylab = expression(paste("Dimensión Discriminante 2 (", psi[2], ")")), 
                  zlab = expression(paste("Dimensión Discriminante 3 (", psi[3], ")")),
                  col.axis = "gray30", col.grid = "gray90", cex.main = 1.2, font.main = 2)
    dev.off()
    
    # Panel 3: GNLPDA 3D
    png(filename = file.path(ruta_salida_indiv, sprintf("mejor_escenario_%s_3_gnlpda_3d.png", ds)), width = 5.5, height = 5, units = "in", res = 300)
    scatterplot3d(x = coor_gnlpda[, 1], y = coor_gnlpda[, 2], z = coor_gnlpda[, 3],
                  color = colores_sujetos_gnlpda, pch = tipo_punto, cex.symbols = tamano_punto, angle = angulo_rotacion, 
                  grid = TRUE, box = FALSE, main = "GNLPDA (Propuesto)", 
                  xlab = "Dimensión Discriminante 1 (GPN1)", ylab = "Dimensión Discriminante 2 (GPN2)", zlab = "Dimensión Discriminante 3 (GPN3)",
                  col.axis = "gray30", col.grid = "gray90", cex.main = 1.2, font.main = 2)
    dev.off()
    
    # Ensamble y Guardado del Tríptico Unificado 3D (Sufijo explícito "_3d")
    png(filename = file.path(ruta_salida_tripticos, sprintf("triptico_mejor_escenario_%s_3d.png", ds)), width = 15, height = 4.8, units = "in", res = 300)
    par(mfrow = c(1, 3), mar = c(4, 4.5, 3, 1), oma = c(0, 0, 0, 0))
    
    scatterplot3d(x = coor_mass[, 1], y = coor_mass[, 2], z = coor_mass[, 3], color = colores_sujetos_mass, pch = tipo_punto, cex.symbols = tamano_punto, angle = angulo_rotacion, grid = TRUE, box = FALSE, main = "LDA MASS (Espacio 3D)", xlab = "Dimensión Discriminante 1 (LD1)", ylab = "Dimensión Discriminante 2 (LD2)", zlab = "Dimensión Discriminante 3 (LD3)", col.axis = "gray30", col.grid = "gray90", cex.main = 1.2, font.main = 2)
    scatterplot3d(x = coor_kfda[, 1], y = coor_kfda[, 2], z = coor_kfda[, 3], color = colores_sujetos_kfda, pch = tipo_punto, cex.symbols = tamano_punto, angle = angulo_rotacion, grid = TRUE, box = FALSE, main = "KFDA (Kernel Gauss)", xlab = expression(paste("Dimensión Discriminante 1 (", psi[1], ")")), ylab = expression(paste("Dimensión Discriminante 2 (", psi[2], ")")), zlab = expression(paste("Dimensión Discriminante 3 (", psi[3], ")")), col.axis = "gray30", col.grid = "gray90", cex.main = 1.2, font.main = 2)
    scatterplot3d(x = coor_gnlpda[, 1], y = coor_gnlpda[, 2], z = coor_gnlpda[, 3], color = colores_sujetos_gnlpda, pch = tipo_punto, cex.symbols = tamano_punto, angle = angulo_rotacion, grid = TRUE, box = FALSE, main = "GNLPDA (Propuesto)", xlab = "Dimensión Discriminante 1 (GPN1)", ylab = "Dimensión Discriminante 2 (GPN2)", zlab = "Dimensión Discriminante 3 (GPN3)", col.axis = "gray30", col.grid = "gray90", cex.main = 1.2, font.main = 2)
    dev.off()
  }
  
  cat(sprintf("✓ Procesamiento y exportación dual completada para: %s\n", ds))

  
}

cat("\n=========================================================\n")
cat("EXPERIMENTO FINALIZADO: Las proyecciones óptimas están listas para análisis.\n")
cat("=========================================================\n")


#### GRÁFICOS DESCRIPTIVOS ####

# =========================================================================
# PIPELINE INDEPENDIENTE: GENERACIÓN DE ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# =========================================================================

library(GGally)   # Para la matriz de dispersión cruzada
library(ggplot2)  # Para la estilización del lienzo
library(stringr)  # Para la manipulación de cadenas de texto

# 1. Configuración Exclusiva de Rutas para el Entorno EDA
ruta_base                <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO"
ruta_datasets            <- file.path(ruta_base, "NUEVAS CORRIDAS DE 30 JUNIO\\DATASETS")
ruta_semillas            <- file.path(ruta_base, "NUEVAS CORRIDAS DE 30 JUNIO\\SEMILLAS GENERADAS")
ruta_logs_gnlpda         <- file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\RESULTADOS_GNLPDA")
ruta_salida_descriptivos <- file.path(ruta_base, "RESULTADOS DE CALIBRACIÓN\\ANALISIS SEMAFORO\\Gráficos descriptivos")

# Creación automática del directorio de destino si no existe
if (!dir.exists(ruta_salida_descriptivos)) dir.create(ruta_salida_descriptivos, recursive = TRUE)

# 2. Paleta Institucional UV Base
colores_institucionales_uv <- c("#043B7B", "#02963E", "#B22222", "#FF8C00", "#9400D3")

# 3. Escaneo del Catálogo de Datasets (Sincronizado con tus archivos de bitácora)
archivos_logs_pivote <- list.files(path = ruta_logs_gnlpda, pattern = "\\.csv$")
datasets_maestros    <- unique(gsub("(^[^_]+)_.*", "\\1", archivos_logs_pivote))

cat(sprintf("[EDA-ENTORNO] Inicializado. Detectados %d datasets para análisis en crudo.\n", length(datasets_maestros)))

# =========================================================================
# 4. BUCLE SECUNDARIO DE PROCESAMIENTO EXPLORATORIO EXCLUSIVO
# =========================================================================
for (ds in datasets_maestros) {
  
  cat(sprintf("\n---------------------------------------------------------\n"))
  cat(sprintf("[EDA] PROCESANDO DATASET: %s\n", ds))
  cat(sprintf("---------------------------------------------------------\n"))
  
  # --- PASO 1: Carga de la bitácora ganadora de GNLPDA ---
  log_gnlpda <- obtener_mejor_corrida(ruta_logs_gnlpda, ds, "gnlpda")
  
  if (is.null(log_gnlpda)) {
    cat(sprintf("[!] Saltando EDA para %s debido a falta de bitácora de calibración.\n", ds))
    next
  }
  
  # --- PASO 2: Reconstrucción de la partición de prueba óptima ---
  id_run_gnlpda  <- log_gnlpda$Corrida_Original[1]
  particion_gnlpda <- reconstruir_particion_ganadora(ruta_datasets, ruta_semillas, ds, id_run_gnlpda)
  
  # --- PASO 3: Extracción y Filtrado Adaptativo de Variables (Nombres_E1) ---
  vars_crudas <- unlist(strsplit(gsub(" ", "", log_gnlpda$Nombres_E1[1]), ","))
  total_vars_etapa1 <- length(vars_crudas)
  
  # Control estricto de saturación geométrica (A acotar a máximo 5 dimensiones)
  if (length(vars_crudas) > 5) {
    vars_crudas <- vars_crudas[1:5]
  }
  
  # --- PASO 4: Construcción del Dataframe en Crudo (Set de Prueba) ---
  df_crudo <- as.data.frame(particion_gnlpda$X_test[, vars_crudas, drop = FALSE])
  df_crudo$class <- as.factor(particion_gnlpda$y_test)
  
  # --- PASO 5: Sincronización de la Paleta Dinámica según Niveles Reales ---
  niveles_actuales <- levels(df_crudo$class)
  num_niveles      <- length(niveles_actuales)
  paleta_eda       <- colores_institucionales_uv[1:num_niveles]
  names(paleta_eda) <- niveles_actuales
  
  # --- PASO 6: Limpieza Dinámica del Nombre del Dataset ---
  # Se eliminan los primeros 4 caracteres para suprimir prefijos de control (ej. "5.4.")
  nombre_ds_limpio <- substr(ds, 5, nchar(ds))
  
  # --- PASO 7: Renderizado de la Matriz Combinada (Diagonal + Triángulo Inferior) ---
  pairplot_final <- ggpairs(
    df_crudo,
    columns = 1:length(vars_crudas),
    mapping = aes(color = class, fill = class),
    upper = list(continuous = "blank"),          # <--- ELIMINA EL RUIDO NUMÉRICO DEL TRIÁNGULO SUPERIOR
    diag  = list(continuous = wrap("densityDiag", alpha = 0.35)), # <--- CONSERVA LAS DENSIDADES EN LA DIAGONAL
    lower = list(continuous = wrap("points", shape = 21, size = 1.5, stroke = 0.3, color = "#FFFFFF", alpha = 0.75))
  ) + 
    scale_fill_manual(values = paleta_eda) +
    scale_color_manual(values = paleta_eda)
  
  # --- PASO 8: Inyección del Tema BW y Estilización Tipográfica Discreta ---
  pairplot_final <- pairplot_final + 
    theme_bw() + # <--- ADOPCIÓN DEL ENTORNO DE BORDES DEFINIDOS
    theme(
      plot.title         = element_text(face = "bold", size = 11, hjust = 0.5), # Título más compacto
      plot.subtitle      = element_blank(),                                     # SE ELIMINA EL SUBTÍTULO
      strip.text         = element_text(face = "bold", size = 7.2, color = "#222222"), # 20% más pequeño y negro neutral
      strip.background   = element_rect(fill = "gray97", color = "gray85"),     # Fondo sutil para las cabeceras
      panel.grid.major   = element_line(color = "gray93"),
      panel.grid.minor   = element_blank(),
      legend.position    = "none"
    ) +
    labs(
      title = sprintf("Distribución de %s", nombre_ds_limpio), # Título directo y limpio
      x = NULL, y = NULL
    )
  
  # --- PASO 9: Exportación Física en Formato Cuadrado ---
  dimension_lienzo <- 1.2 * length(vars_crudas) + 1.2
  
  ggsave(
    file.path(ruta_salida_descriptivos, sprintf("eda_crudo_%s.png", ds)), 
    plot = pairplot_final, 
    width = dimension_lienzo, 
    height = dimension_lienzo, 
    dpi = 300
  )
}

cat("\n=========================================================\n")
cat("PROCESO EDA COMPLETADO: Todos los pairplots en crudo están listos.\n")
cat("=========================================================\n")







