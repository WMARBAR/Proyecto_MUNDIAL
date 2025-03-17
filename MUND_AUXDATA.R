## CODIGO ENCARGADO DE AYUDAR A CREAR LAS BASES DE DATOS AUXILIARES.
## SE TOTALIZA CADA ENTRENADOR Y CADA SELECCION POR AÑO - PAIS.

library(readxl)
library(writexl)
library(stringr)
library(dplyr)


archivo <- "Mundial_Historic_RawData.xlsx" 
MUND_DF <- read_excel(archivo)

# Crear la tabla totalizada con Año, Selección y Entrenador
ENTRENADOR_DF <- MUND_DF %>%
  select(MYEAR, Seleccion, COACH) %>%  # Seleccionar las columnas necesarias
  distinct()  # Eliminar duplicados

# Arreglar los nombres
# Limpiar la columna "Nombre del Entrenador" eliminando los paréntesis y su contenido
ENTRENADOR_DF <- ENTRENADOR_DF %>%
  mutate(COACH = str_trim(str_remove(COACH, "\\(.*?\\)")))


# Guardar la tabla en Excel
# Definir la ruta donde se guardará el archivo (dentro del proyecto)
ruta_guardado <- "entrenadores_totalizados.xlsx"
write_xlsx(ENTRENADOR_DF, ruta_guardado)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado, "\n")

# Crear la tabla totalizada con Año y Selección
SELECCIONES_DF <- MUND_DF %>%
  select(MYEAR, Seleccion) %>%  # Seleccionar solo las columnas necesarias
  distinct()  # Eliminar duplicados

# Guardar la tabla en Excel
ruta_guardado_selecciones <- "selecciones_totalizadas.xlsx"
write_xlsx(SELECCIONES_DF, ruta_guardado_selecciones)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado_selecciones, "\n")

