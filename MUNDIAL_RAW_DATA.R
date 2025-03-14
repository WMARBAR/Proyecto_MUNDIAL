library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(writexl)

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

MUND_DF <- MUND_DF %>%
  mutate(
    birth_year = sapply(`Fecha de Nacimiento`, convertir_año),
    edad_Player = ifelse(birth_year > 0, Año - birth_year, NA), # Calcular edad, si birth_year es 0 poner NA
    years_expMundial = ifelse(birth_year > 0, Año - as.numeric(`Año de Debut`), NA),
    edad_debut = ifelse(birth_year > 0 & !is.na(`Año de Debut`), as.numeric(`Año de Debut`) - birth_year, NA)
  )
print(MUND_DF)

#----------DATA TECNICOS---------------------------------------------
MUND_tecnicos <- "Aux_Tecnicos.xlsx"
TECNICOS_DF <- read_excel(MUND_tecnicos)

#----------DATA POSICIONES---------------------------------------------
archivo_excel <- "Mundial_Aux_Data.xlsx"
nombres_hojas <- excel_sheets(archivo_excel)
# Leer una hoja específica (ejemplo: la primera hoja)
nombre_hoja <- nombres_hojas[3]  # Puedes cambiar el índice para leer otra hoja

# Leer los datos de la hoja seleccionada
MUND_POSICIONES <- read_excel(archivo_excel, sheet = nombre_hoja)
MUND_POSICIONES


#----------JOIN MEGA BASE---------------------------------------------
# Seleccionar las columnas que queremos traer de TECNICOS_DF
TECNICOS_DF_JOIN <- TECNICOS_DF %>%
  select(Año, Selección, edad_enMundial, tecnico_campeon, tecnico_anfitrion, Tecnico_outsider,
         `País Anfitrión`, Campeón, Nacionalidad, dias_durados, `Nombre del Entrenador`)

# Hacer el JOIN entre MUND_DF y TECNICOS_DF usando Año y Selección como claves
MUND_DF <- MUND_DF %>%
  left_join(TECNICOS_DF_JOIN, by = c("Año", "Selección"))
MUND_DF <- MUND_DF %>%
  mutate(
equipo_local = ifelse(`País Anfitrión` == Selección, 1, 0)
)
MUND_DF <- MUND_DF %>%
  left_join(MUND_POSICIONES %>% select(Año, Selección, Puesto_obtenido), by = c("Año", "Selección"))

# Verificar la estructura después del join
colnames(MUND_DF)
colnames(MUND_POSICIONES)

#----------EXPORT TO EXCEL---------------------------------------------
ruta_guardado_selecciones <- "Players_Mundial_RAWdata.xlsx"
write_xlsx(MUND_DF, ruta_guardado_selecciones)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado_selecciones, "\n")
