# 📌 Cargar librerías necesarias
library(readxl)   # Para leer archivos Excel
library(dplyr)    # Para manipulación de datos
library(writexl)  # Para escribir archivos Excel
library(ggplot2)  # Para visualización de datos

#  Cargar y limpiar la base de datos
archivo <- "Players_Mundial_RAWdata_1_vf.xlsx"
df <- read_excel(archivo, sheet = "Sheet1")  # Leer los datos desde Excel

colnames(df)

# Verificar valores únicos en la columna clave
unique(df$Grupo_De_La_Muerte)

#  Convertir valores vacíos y texto "NA" en valores NA reales
df <- df %>%
  mutate(Grupo_De_La_Muerte = na_if(Grupo_De_La_Muerte, "NA")) %>%
  mutate(Grupo_De_La_Muerte = na_if(Grupo_De_La_Muerte, ""))

#  Eliminar filas con NA en la columna 'Grupo_De_La_Muerte'
df_limpio <- df %>% filter(!is.na(Grupo_De_La_Muerte))

#  Verificar que no haya valores NA
sum(is.na(df_limpio$Grupo_De_La_Muerte))  # Debe devolver 0

#  Guardar el dataframe limpio en un nuevo archivo Excel
write_xlsx(df_limpio, "Players_Mundial_Limpio.xlsx")

#  Convertir la variable `Grupo_De_La_Muerte` en factor
df_limpio$Grupo_De_La_Muerte <- as.factor(df_limpio$Grupo_De_La_Muerte)

#  TABLA DE FRECUENCIAS ABSOLUTAS Y RELATIVAS
tabla_frec <- table(df_limpio$Grupo_De_La_Muerte)  # Frecuencia absoluta
tabla_frec_rel <- prop.table(tabla_frec)  # Frecuencia relativa

# Mostrar las tablas
print("Tabla de Frecuencias Absolutas:")
print(tabla_frec)

print("Tabla de Frecuencias Relativas:")
print(tabla_frec_rel)

#  VISUALIZACIÓN: GRÁFICO DE BARRAS
ggplot(df_limpio, aes(x = Grupo_De_La_Muerte, fill = Grupo_De_La_Muerte)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribución de Grupo_De_La_Muerte", x = "Grupo", y = "Frecuencia") +
  scale_fill_manual(values = c("red", "blue"))



# Calcular los porcentajes
df_pie <- df_limpio %>%
  count(Grupo_De_La_Muerte) %>%
  mutate(percentage = round(n / sum(n) * 100, 1),
         label = paste0(percentage, "%"))

# Crear el gráfico de pastel con etiquetas más pequeñas
ggplot(df_pie, aes(x = "", y = n, fill = Grupo_De_La_Muerte)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Distribución de Grupo_De_La_Muerte", x = NULL, y = NULL) +
  scale_fill_manual(values = c("#FF6961", "#80CEE1")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +  # Tamaño más pequeño
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())



# CONVERTIR 'CAMPEÓN' EN VARIABLE BINARIA USANDO 'FaseAlcanzada'
df_limpio <- df_limpio %>%
  mutate(Campeón_Binario = ifelse(FaseAlcanzada == "Campeón", 1, 0)) %>%
  mutate(Campeón_Binario = factor(Campeón_Binario, levels = c(0,1)))

#⃣ ANÁLISIS: ¿EL CAMPEÓN PROVIENE DEL GRUPO DE LA MUERTE?
tabla_campeon <- table(df_limpio$Grupo_De_La_Muerte, df_limpio$Campeón_Binario)
print(tabla_campeon)  # Mostrar la tabla

#⃣ CONVERTIR 'Eliminados en fase de grupos' EN VARIABLE BINARIA
df_limpio <- df_limpio %>%
  mutate(Eliminado_Binario = ifelse(FaseAlcanzada == "Eliminados en fase de grupos", 1, 0)) %>%
  mutate(Eliminado_Binario = factor(Eliminado_Binario, levels = c(0,1)))

# ⃣ ANALIZAR SI LOS ELIMINADOS PROVIENEN DEL GRUPO DE LA MUERTE
tabla_eliminados <- table(df_limpio$Grupo_De_La_Muerte, df_limpio$Eliminado_Binario)
print(tabla_eliminados)

#  Normalizar la variable `FaseAlcanzada`
df_limpio <- df_limpio %>%
  mutate(FaseAlcanzada = tolower(FaseAlcanzada)) %>%
  mutate(FaseAlcanzada = case_when(
    FaseAlcanzada %in% c("fase de grupos", "eliminado en fase de grupos") ~ "eliminado en fase de grupos",
    FaseAlcanzada %in% c("cuartos de final", "cuartos de final ") ~ "cuartos de final",
    FaseAlcanzada %in% c("octavos de final", "octavos de final ") ~ "octavos de final",
    TRUE ~ FaseAlcanzada
  ))

# ⃣ Estadísticas descriptivas de `Edad_Player`
df_limpio %>%
  group_by(Grupo_De_La_Muerte) %>%
  summarise(
    Edad_Promedio = mean(edad_Player, na.rm = TRUE),
    Edad_Mediana = median(edad_Player, na.rm = TRUE),
    Edad_Min = min(edad_Player, na.rm = TRUE),
    Edad_Max = max(edad_Player, na.rm = TRUE),
    Cantidad = n()
  )


# Boxplot de edades según Grupo de la Muerte
ggplot(df_limpio, aes(x = Grupo_De_La_Muerte, y = edad_Player, fill = Grupo_De_La_Muerte)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de Edad según Grupo de la Muerte",
       x = "Grupo de la Muerte", 
       y = "Edad del Jugador") +
  scale_fill_manual(values = c("red", "blue"))

# ⃣ Resumen estadístico de la experiencia en mundiales
summary_exp <- df_limpio %>%
  group_by(Grupo_De_La_Muerte) %>%
  summarise(
    Promedio_Experiencia = mean(years_expMundial, na.rm = TRUE),
    Mediana_Experiencia = median(years_expMundial, na.rm = TRUE),
    Desviacion_Estandar = sd(years_expMundial, na.rm = TRUE),
    Min_Experiencia = min(years_expMundial, na.rm = TRUE),
    Max_Experiencia = max(years_expMundial, na.rm = TRUE),
    Total_Jugadores = n()
  )

print(summary_exp)

# Boxplot para visualizar la distribución de experiencia en mundiales
ggplot(df_limpio, aes(x = factor(Grupo_De_La_Muerte), y = years_expMundial, fill = factor(Grupo_De_La_Muerte))) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Experiencia en Mundiales según Grupo de la Muerte",
       x = "Grupo de la Muerte",
       y = "Años de Experiencia en Mundiales",
       fill = "Grupo de la Muerte") +
  scale_fill_manual(values = c("red", "blue"))


#  Resumen estadístico de `Goles Marcados(mundial)`
summary_goles <- df_limpio %>%
  group_by(Grupo_De_La_Muerte) %>%
  summarise(
    Promedio_Goles = mean(`Goles Marcados(mundial)`, na.rm = TRUE),
    Mediana_Goles = median(`Goles Marcados(mundial)`, na.rm = TRUE),
    Desviacion_Estandar = sd(`Goles Marcados(mundial)`, na.rm = TRUE),
    Min_Goles = min(`Goles Marcados(mundial)`, na.rm = TRUE),
    Max_Goles = max(`Goles Marcados(mundial)`, na.rm = TRUE),
    Total_Jugadores = n()
  )

print(summary_goles)

# Histograma de goles marcados por `Grupo_De_La_Muerte`
ggplot(df_limpio, aes(x = `Goles Marcados(mundial)`, fill = factor(Grupo_De_La_Muerte))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  theme_minimal() +
  labs(title = "Distribución de Goles Marcados en Mundiales",
       x = "Goles Marcados",
       y = "Frecuencia",
       fill = "Grupo de la Muerte") +
  scale_fill_manual(values = c("red", "blue"))


# Agrupar posiciones en categorías más generales
df_limpio$Posicion_Agrupada <- case_when(
  grepl("Defensa", df_limpio$Posicion) ~ "Defensor",
  grepl("Mediocampista", df_limpio$Posicion) ~ "Mediocampista",
  grepl("Delantero", df_limpio$Posicion) ~ "Delantero",
  grepl("Portero", df_limpio$Posicion) ~ "Portero",
  TRUE ~ "Otro"
)



# Eliminar la categoría "Otro" si no es útil
df_limpio <- df_limpio %>% filter(Posicion_Agrupada != "Otro")

# Crear tabla de contingencia
tabla_posicion_agrupada <- table(df_limpio$Posicion_Agrupada, df_limpio$Grupo_De_La_Muerte)


# Graficar la distribución de posiciones agrupadas
ggplot(df_limpio, aes(x = reorder(Posicion_Agrupada, -table(Posicion_Agrupada)[Posicion_Agrupada]), 
                      fill = factor(Grupo_De_La_Muerte))) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +  # Agregar etiquetas
  theme_minimal() +
  labs(title = "Distribución de Posiciones Agrupadas según Grupo de la Muerte",
       x = "Posición Agrupada en el Campo",
       y = "Cantidad de Jugadores",
       fill = "Grupo de la Muerte") +
  scale_fill_manual(values = c("red", "blue")) +  # Cambia los colores si lo deseas
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 📌 ANALIZAR SI LOS EQUIPOS DEL GRUPO DE LA MUERTE AVANZAN MÁS EN EL TORNEO

# Crear una variable ordinal para 'FaseAlcanzada' basada en la etapa alcanzada
df_limpio <- df_limpio %>%
  mutate(Fase_Ordinal = case_when(
    FaseAlcanzada == "eliminado en fase de grupos" ~ 1,
    FaseAlcanzada == "octavos de final" ~ 2,
    FaseAlcanzada == "cuartos de final" ~ 3,
    FaseAlcanzada == "semifinales" ~ 4,
    FaseAlcanzada == "finalista" ~ 5,
    FaseAlcanzada == "campeón" ~ 6,
    TRUE ~ NA_real_
  ))

# Convertir a factor ordenado para modelar la progresión en el torneo
df_limpio$Fase_Ordinal <- factor(df_limpio$Fase_Ordinal, ordered = TRUE)


# Gráfico de barras apiladas
ggplot(df_limpio, aes(x = FaseAlcanzada, fill = Grupo_De_La_Muerte)) +
  geom_bar(position = "fill") +  # Normaliza las alturas para comparar proporciones
  theme_minimal() +
  labs(title = "Distribución de Equipos por Fase Alcanzada",
       x = "Fase Alcanzada",
       y = "Proporción",
       fill = "Grupo de la Muerte") +
  scale_fill_manual(values = c("red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de densidad para visualizar la probabilidad de avanzar
ggplot(df_limpio, aes(x = Fase_Ordinal, fill = Grupo_De_La_Muerte)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribución de Probabilidades de Avance en el Torneo",
       x = "Fase Alcanzada (Ordinal)",
       y = "Densidad",
       fill = "Grupo de la Muerte") +
  scale_fill_manual(values = c("red", "blue"))

