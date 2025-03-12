library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

# Leer el archivo Excel (sin forzar col_types)
MUND_players <- "Mundial_Historic_RawData.xlsx"  
MUND_DF <- read_excel(MUND_players)

# Función para extraer el año correctamente
convertir_año <- function(fecha) {
  if (is.na(fecha) | fecha == "") {
    return(0)  # Si está vacío o es NA, poner 0
  } else if (is.numeric(fecha)) {
    # Si es un número (serial de Excel), convertirlo a fecha y extraer el año
    return(year(as.Date(fecha, origin = "1899-12-30")))
  } else {
    # Si es texto, extraer el año con regex
    year_extraido <- str_extract(fecha, "\\d{4}") %>% as.numeric()
    if (is.na(year_extraido) | year_extraido < 1800 | year_extraido > 2100) {
      return(0)  # Si el año es inválido, devolver 0
    } else {
      return(year_extraido)
    }
  }
}

# Aplicar la función a la columna "Fecha de Nacimiento"
MUND_DF <- MUND_DF %>%
  mutate(
    birth_year = sapply(`Fecha de Nacimiento`, convertir_año),
    edad_enMundial = ifelse(birth_year > 0, Año - birth_year, NA), # Calcular edad, si birth_year es 0 poner NA
    years_expMundial = ifelse(birth_year > 0, Año - as.numeric(`Año de Debut`), NA)
  )
# Mostrar resultado
print(MUND_DF)

# TECNICOS
MUND_tecnicos <- "Aux_Tecnicos.xlsx"
TECNICOS_DF <- read_excel(MUND_tecnicos)
TECNICOS_DF
