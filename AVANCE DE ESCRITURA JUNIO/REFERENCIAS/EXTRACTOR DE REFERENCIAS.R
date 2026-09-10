rm(list=ls())

library(bib2df) # Manipulador del bibtex
library(dplyr) # Herramientas en general
library(writexl) # Escribir el archivo en un xlsx
library(RefManageR) #Manejador de referencias en R
# 1. Definir la ruta del archivo .bib
ruta_bib <- "C:\\Users\\Angeal\\Desktop\\GNLPDA\\CORRECCIONES TESIS AGOSTO 2026\\tesis\\bibliografia\\referenciaslimpias.bib"

# 2. Extraer metadatos estructurados con bib2df
df_datos <- bib2df(ruta_bib)

# 3. Cargar la bibliografía con RefManageR para formatear a APA
bib_ref <- ReadBib(ruta_bib, check = FALSE)



