# Cargar la data en un dataframe llamado MUND_DF
install.packages("readxl")  # Si no lo tienes instalado
library(readxl)
archivo <- "Mundial_Historic_RawData.xlsx"  # Reemplaza con la ruta real
MUND_DF <- read_excel(archivo)

# Crear la tabla totalizada con Año, Selección y Entrenador
install.packages("dplyr")  # Si no lo tienes instalado
library(dplyr)

ENTRENADOR_DF <- MUND_DF %>%
  select(Año, Selección, `Nombre del Entrenador`) %>%  # Seleccionar las columnas necesarias
  distinct()  # Eliminar duplicados

# Arreglar los nombres
library(stringr)
# Limpiar la columna "Nombre del Entrenador" eliminando los paréntesis y su contenido
ENTRENADOR_DF <- ENTRENADOR_DF %>%
  mutate(`Nombre del Entrenador` = str_trim(str_remove(`Nombre del Entrenador`, "\\(.*?\\)")))

# Ver el resultado
print(ENTRENADOR_DF)

# Guardar la tabla en Excel
library(writexl)
# Definir la ruta donde se guardará el archivo (dentro del proyecto)
ruta_guardado <- "entrenadores_totalizados.xlsx"
write_xlsx(ENTRENADOR_DF, ruta_guardado)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado, "\n")




