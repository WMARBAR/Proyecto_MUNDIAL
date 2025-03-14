library(readxl)
library(lubridate)
library(dplyr)
library(stringr)


#----------MESES EN NUMEROS---------------------------------------------
meses_dict <- c(
  "enero" = 1, "febrero" = 2, "marzo" = 3, "abril" = 4, "mayo" = 5, "junio" = 6, 
  "julio" = 7, "agosto" = 8, "septiembre" = 9, "octubre" = 10, "noviembre" = 11, "diciembre" = 12
)
archivo_excel <- "Mundial_Aux_Data.xlsx"
nombres_hojas <- excel_sheets(archivo_excel)
#----------DATA HISTORICA---------------------------------------------
# Leer una hoja específica (ejemplo: la primera hoja)
nombre_hoja <- nombres_hojas[1]  # Puedes cambiar el índice para leer otra hoja

# Leer los datos de la hoja seleccionada
df_historica <- read_excel(archivo_excel, sheet = nombre_hoja)

df_historica <- df_historica %>%
  mutate(
    num_mes_init = meses_dict[Mes_init], 
    num_mes_end = meses_dict[Mes_end],
    init_date = make_date(Año, num_mes_init, Day_init),
    end_date = make_date(Año, num_mes_end, Day_end),  
    dias_durados = as.integer(end_date - init_date)
  )
#----------DATA TECNICOS-----------------------------------------------------------

nombre_hoja <- nombres_hojas[2]  
df_tecnicos <- read_excel(archivo_excel, sheet = nombre_hoja)

df_tecnicos <- df_tecnicos %>%
  left_join(df_historica %>% select(Año, init_date, `País Anfitrión`, Campeón, dias_durados), by = "Año")

df_tecnicos <- df_tecnicos %>%
  mutate(
    num_mes = meses_dict[Mes],  
    birth_date = make_date(Year, num_mes, Day),  
    tecnico_campeon = ifelse(Selección == Campeón, 1, 0),
    Tecnico_outsider = ifelse(Nacionalidad == Selección, 0, 1),
    tecnico_anfitrion = ifelse(Selección == `País Anfitrión`, 1, 0),
    edad_enMundial = as.numeric(interval(birth_date, init_date) / years(1))
        )

df_tecnicos <- df_tecnicos %>%
  mutate(birth_date = format(birth_date, "%Y-%m-%d"))  # Guardar como texto

# Guardar la tabla en Excel
library(writexl)
ruta_guardado <- "Aux_Tecnicos.xlsx"
write_xlsx(df_tecnicos, ruta_guardado)
cat("Archivo guardado en:", ruta_guardado, "\n")
