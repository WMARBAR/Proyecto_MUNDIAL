library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(writexl)

#----------DATA MUNDIAL FINAL---------------------------------------------
MUNDIALES_DF <- "Players_Mundial_RAWdata.xlsx"
DF_MUNDIALES <- read_excel(MUNDIALES_DF)

colnames(DF_MUNDIALES)

# Reemplazar los valores vacíos en edad_Player con el promedio de la columna
DF_MUNDIALES <- DF_MUNDIALES %>%
  mutate(edad_Player = if_else(is.na(edad_Player), 
                               mean(edad_Player, na.rm = TRUE), 
                               edad_Player))

#----------DATA PUESTOS 1---------------------------------------------
# Filtrar los datos para obtener solo los puestos 1
DF_MUNDIALES_champs <- DF_MUNDIALES %>%
  filter(Puesto_obtenido == 1)

# Verificar el resultado
print(DF_MUNDIALES_champs)

colnames(DF_MUNDIALES_champs)


#----------EXPORT TO EXCEL---------------------------------------------
ruta_guardado_selecciones <- "test_analisis.xlsx"
write_xlsx(DF_MUNDIALES, ruta_guardado_selecciones)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado_selecciones, "\n")