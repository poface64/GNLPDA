rm(list=ls())

###

# Cargar librerías necesarias
library(ggplot2)
library(MASS)

# 1. Simulación de datos originales
set.seed(123)
mu <- c(5, 5)
sigma <- matrix(c(4, 2.5, 2.5, 4), 2)
df_pca <- as.data.frame(mvrnorm(40, mu, sigma))
colnames(df_pca) <- c("x1", "x2")

# 2. Filtrado de puntos mediante distancia de Mahalanobis
centro_datos <- colMeans(df_pca)
matriz_cov <- cov(df_pca)
distancias <- mahalanobis(df_pca, centro_datos, matriz_cov)

# Valor crítico para 90% de confianza con 2 grados de libertad
limite <- qchisq(0.90, df = 2) 

# Nuevo dataframe excluyendo los puntos exteriores
df_pca_dentro <- df_pca[distancias <= limite, ]

# 3. Cálculo de Componentes Principales (PCA) con los datos originales
pca <- prcomp(df_pca, center = TRUE)
centro <- pca$center
v1 <- pca$rotation[,1] * pca$sdev[1] * 2.5
v2 <- pca$rotation[,2] * pca$sdev[2] * 2.5

# 4. Generación del gráfico
PCA_G = ggplot() +
  # Elipse calculada con el total de los datos para no alterar su forma
  stat_ellipse(data = df_pca, aes(x = x1, y = x2), geom = "polygon", 
               fill = "#c5d9f1", alpha = 0.6, color = NA, level = 0.90) +
  
  # Puntos graficados ÚNICAMENTE con los datos filtrados (interiores)
  geom_point(data = df_pca_dentro, aes(x = x1, y = x2), 
             shape = 4, color = "#e74c3c", size = 2.5, stroke = 1) +
  
  # Vectores PC1 y PC2
  annotate("segment", x = centro[1], y = centro[2], 
           xend = centro[1] + v1[1]-1 , yend = centro[2] + v1[2]-1,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"), size = 1) +
  annotate("segment", x = centro[1], y = centro[2], 
           xend = centro[1] + v2[1]+0.5, yend = centro[2] + v2[2]-0.5,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"), size = 1) +
  
  # Etiquetas de los vectores
  annotate("text", x = centro[1] + v1[1], y = centro[2] + v1[2] -0.8, 
           label = "PC1", fontface = "bold") +
  annotate("text", x = centro[1] + v2[1], y = centro[2] + v2[2], 
           label = "PC2", fontface = "bold") +
  
  # Ejes manuales con flechas
  annotate("segment", x = 0, y = 0, xend = 10, yend = 0, 
           arrow = arrow(length = unit(0.3, "cm")), size = 0.8) +
  annotate("segment", x = 0, y = 0, xend = 0, yend = 10, 
           arrow = arrow(length = unit(0.3, "cm")), size = 0.8) +
  
  # Nomenclatura de ejes
  annotate("text", x = 5, y = -0.6, label = "x[1]", parse = TRUE, 
           fontface = "bold", size = 5) +
  annotate("text", x = -0.6, y = 5, label = "x[2]", parse = TRUE, 
           fontface = "bold", size = 5) +
  
  # Estética y diseño
  labs(title = "Análisis de componentes principales (PCA)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  coord_fixed(ratio = 1, xlim = c(-1, 11), ylim = c(-1, 11))


#### LDA ####

library(ggplot2)

# 1. Simulación base y clonación desplazada
set.seed(42)
# Generación del grupo 2 (verde) con el comportamiento de óvalo recto
df_lda2 <- data.frame(x = rnorm(40, 9, 0.8), y = rnorm(40, 5, 2.0))

# Clonación del grupo 1 (azul) desplazando exactamente 5 unidades a la izquierda
df_lda1 <- df_lda2
df_lda1$x <- df_lda1$x - 5

# 2. Filtrado de puntos fuera de las elipses (Distancia de Mahalanobis)
nivel_conf <- 0.95
limite <- qchisq(nivel_conf, df = 2)

df_lda1_dentro <- df_lda1[mahalanobis(df_lda1, colMeans(df_lda1), cov(df_lda1)) <= limite, ]
df_lda2_dentro <- df_lda2[mahalanobis(df_lda2, colMeans(df_lda2), cov(df_lda2)) <= limite, ]

# 3. Coordenadas para las curvas de densidad marginales (Ejes ampliados a 13 y 12)
x_seq <- seq(0, 13, length.out = 200)
y_seq <- seq(0, 12, length.out = 200)

# Las medias teóricas para la densidad ahora son 4 (9 - 5) y 9 en X
dens_x1 <- data.frame(x = x_seq, y = -dnorm(x_seq, 4, 0.8) * 3)
dens_x2 <- data.frame(x = x_seq, y = -dnorm(x_seq, 9, 0.8) * 3)
dens_y  <- data.frame(x = -dnorm(y_seq, 5, 2.0) * 8, y = y_seq)

# 4. Generación del gráfico
LDA_G = ggplot() +
  # Elipses
  stat_ellipse(data = df_lda1, aes(x, y), geom = "polygon", 
               fill = "#c5d9f1", alpha = 0.6, color = NA, level = nivel_conf) +
  stat_ellipse(data = df_lda2, aes(x, y), geom = "polygon", 
               fill = "#d7e3bc", alpha = 0.6, color = NA, level = nivel_conf) +
  
  # Puntos limitados a la región interior
  geom_point(data = df_lda1_dentro, aes(x, y), 
             shape = 4, color = "black", size = 4, stroke = 1.2) +
  geom_point(data = df_lda2_dentro, aes(x, y), 
             shape = 16, color = "#e74c3c", size = 4) +
  
  # Curvas de Densidad (solo contornos)
  geom_path(data = dens_x1, aes(x, y), color = "#5b9bd5", size = 1.5) +
  geom_path(data = dens_x2, aes(x, y), color = "#70ad47", size = 1.5) +
  geom_path(data = dens_y, aes(x, y), color = "gray50", size = 1.5) +
  
  # Ejes manuales ampliados para alojar los datos hasta X=13 e Y=12
  annotate("segment", x = 0, y = 0, xend = 13, yend = 0, 
           arrow = arrow(length = unit(0.4, "cm")), size = 1) +
  annotate("segment", x = 0, y = 0, xend = 0, yend = 12, 
           arrow = arrow(length = unit(0.4, "cm")), size = 1) +
  
  # Textos de los discriminantes ajustados a las nuevas dimensiones
  annotate("text", x = 6.5, y = -1.5, label = "LD 1", fontface = "italic", size = 6) +
  annotate("text", x = -0.8, y = 5, label = "LD 2", fontface = "italic", size = 6) +
  
  # Diseño y proporciones generales del lienzo
  labs(title = "Análisis discriminante lineal (LDA)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(b = 15))) +
  coord_fixed(ratio = 1, xlim = c(-2.5, 13.5), ylim = c(-2.5, 12.5))




PCA_G
LDA_G


# Instalar el paquete si no se encuentra en el entorno
# install.packages("patchwork")

# Cargar la librería
library(patchwork)

# Unir los gráficos lado a lado
grafico_final <- PCA_G | LDA_G

# Opcional: Se puede agregar un título general que abarque ambos gráficos
#grafico_final <- grafico_final + 
#  plot_annotation(
#    title = "Comparación entre PCA y LDA",
#    theme = theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))
#  )

# Desplegar el gráfico combinado
print(grafico_final)

# Guardar el gráfico combinado en alta resolución
ggsave(filename = "PCALDA.png", 
       plot = grafico_final, 
       width = 14,     # Ancho en pulgadas (amplio para acomodar dos columnas)
       height = 6,     # Alto en pulgadas
       dpi = 300,      # Alta resolución (puntos por pulgada)
       bg = "white")   # Fondo blanco sólido

