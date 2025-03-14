# Cargar la data en un dataframe llamado MUND_DF
library(readxl)
library(writexl)
library(stringr)
library(dplyr)


archivo <- "Mundial_Historic_RawData.xlsx"  # Reemplaza con la ruta real
MUND_DF <- read_excel(archivo)

# Crear la tabla totalizada con Año, Selección y Entrenador


ENTRENADOR_DF <- MUND_DF %>%
  select(Año, Selección, `Nombre del Entrenador`) %>%  # Seleccionar las columnas necesarias
  distinct()  # Eliminar duplicados

# Arreglar los nombres

# Limpiar la columna "Nombre del Entrenador" eliminando los paréntesis y su contenido
ENTRENADOR_DF <- ENTRENADOR_DF %>%
  mutate(`Nombre del Entrenador` = str_trim(str_remove(`Nombre del Entrenador`, "\\(.*?\\)")))



# Guardar la tabla en Excel
# Definir la ruta donde se guardará el archivo (dentro del proyecto)
ruta_guardado <- "entrenadores_totalizados.xlsx"
write_xlsx(ENTRENADOR_DF, ruta_guardado)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado, "\n")


# Crear la tabla totalizada con Año y Selección
SELECCIONES_DF <- MUND_DF %>%
  select(Año, Selección) %>%  # Seleccionar solo las columnas necesarias
  distinct()  # Eliminar duplicados

# Guardar la tabla en Excel
ruta_guardado_selecciones <- "selecciones_totalizadas.xlsx"
write_xlsx(SELECCIONES_DF, ruta_guardado_selecciones)

# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado_selecciones, "\n")

