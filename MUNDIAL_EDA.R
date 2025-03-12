# Instalar y cargar librerías necesarias
install.packages("readxl")  # Si no lo tienes instalado
library(readxl)

# Definir la ruta del archivo (cambia esto según donde esté tu archivo)
archivo <- "Mundial_Historic_RawData.xlsx"  # Reemplaza con la ruta real

# Cargar la data en un dataframe llamado MUND_DF
MUND_DF <- read_excel(archivo)

# Ver las primeras filas para asegurarnos de que se cargó correctamente
head(MUND_DF)