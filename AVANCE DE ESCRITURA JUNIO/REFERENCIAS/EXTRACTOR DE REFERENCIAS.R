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


tabla_refs <- df_datos %>%
  select(BIBTEXKEY, CAPITULO, RESUMEN)

# 5. Exportar a Excel si lo deseas
write_xlsx(tabla_refs, "referencias_tabla1.xlsx")


