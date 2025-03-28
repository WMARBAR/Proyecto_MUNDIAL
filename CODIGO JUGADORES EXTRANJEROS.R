
library(readxl)

library(lubridate)
library(dplyr)
library(stringr)

library(writexl)
library(ggplot2)
library(tidyr)

library(readr)
#---------------------------------------------------------------------
#----------[DATA MUNDIAL PARA ANALIZAR]-------------------------------
#---------------------------------------------------------------------
MUNDIALES_DF <- "Players_Mundial_RAWdata.xlsx"
DF_MUNDIALES <- read_excel(MUNDIALES_DF)

colnames(DF_MUNDIALES)

# Reemplazar los valores vacíos en edad_Player con el promedio de la columna
DF_MUNDIALES <- DF_MUNDIALES %>%
  mutate(edad_Player = if_else(is.na(edad_Player), 
                               mean(edad_Player, na.rm = TRUE), 
                               edad_Player))

# Continentes
DF_MUNDIALES <- DF_MUNDIALES %>%
  mutate(confederacion = case_when(
    Seleccion %in% c("Alemania Federal", "Alemania Oriental", "Austria", "Bélgica", "Bosnia y Herzegovina",
                     "Bulgaria", "Checoslovaquia", "Croacia", "Dinamarca", "Eslovaquia", "Eslovenia", 
                     "España", "Francia", "Gales", "Grecia", "Hungría", "Inglaterra", "Irlanda", 
                     "Irlanda del Norte", "Italia", "Países Bajos", "Polonia", "Portugal", 
                     "República Checa", "Rumania", "Rusia", "Escocia", "Serbia", "Suecia", "Suiza", 
                     "Turquía", "Ucrania", "Unión Soviética", "Yugoslavia") ~ "UEFA",
    
    Seleccion %in% c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Ecuador", "Paraguay", 
                     "Perú", "Uruguay", "Venezuela") ~ "CONMEBOL",
    
    Seleccion %in% c("México", "Estados Unidos", "Canadá", "Costa Rica", "Cuba", "El Salvador", 
                     "Haití", "Honduras", "Jamaica", "Trinidad y Tobago") ~ "CONCACAF",
    
    Seleccion %in% c("Argelia", "Camerún", "Costa de Marfil", "Egipto", "Ghana", "Marruecos", "Nigeria", 
                     "Senegal", "Sudáfrica", "Togo", "Túnez", "Zaire") ~ "CAF",
    
    Seleccion %in% c("Arabia Saudita", "China", "Corea del Norte", "Corea del Sur", "Emiratos Árabes", 
                     "Irak", "Irán", "Israel", "Japón", "Kuwait", "Indias Orientales Neerlandesas", "Australia") ~ "AFC",
    
    Seleccion %in% c("Nueva Zelanda") ~ "OFC",
    
    TRUE ~ "Otro"
  ))

#---------------------------------------------------------------------
#----------[PREPROCESAMIENTO]-----------------------------------------
#---------------------------------------------------------------------
#---1. Crear la columna [etapa_general] según la clasificación de [FaseAlcanzada]
#---------------------------------------------------------------------
DF_MUNDIALES <- DF_MUNDIALES %>%
  mutate(etapa_general = case_when(
    FaseAlcanzada == "Campeón" ~ "campeon",
    FaseAlcanzada == "Subcampeón" ~ "subcampeon",
    FaseAlcanzada %in% c("Semifinalista", 
                         "Semifinalista (3° Lugar)", 
                         "Semifinalista (4° Lugar)", 
                         "Cuarto lugar", 
                         "Cuarto Lugar (Semifinalista)", 
                         "Tercer lugar", 
                         "Tercer Lugar (Semifinalista)") ~ "semifinalista",
    TRUE ~ "eliminado_temprano"
  ))
#---------------------------------------------------------------------
#---2. DUMMYS DE [etapa_general]
#---------------------------------------------------------------------
DF_MUNDIALES <- DF_MUNDIALES %>%
  group_by(MYEAR) %>%  # Agrupas por edición del Mundial
  mutate(max_goles = max("Goles Marcados(mundial)", na.rm = TRUE),  # Máximo de goles en la edición
         dummy_goleador = if_else("Goles Marcados(mundial)" == max_goles & max_goles > 0, 1, 0)) %>%
  ungroup() %>%  # Sacas el agrupamiento
  mutate(dummy_campeon = if_else(etapa_general == "campeon", 1, 0),
         dummy_subcampeon = if_else(etapa_general == "subcampeon", 1, 0),
         dummy_semifinalista = if_else(etapa_general == "semifinalista", 1, 0),
         dummy_eliminados_tempranos = if_else(etapa_general == "eliminado_temprano", 1, 0),
         dummy_finalistas = if_else(dummy_campeon == 1 | dummy_subcampeon == 1, 1, 0),
         player_ones = 1,
         player_anotador = if_else("Goles Marcados(mundial)" > 0, 1, 0),
         categ_posicion = case_when(
           str_detect(tolower(Posicion), "arquero|portero|guardameta") ~ "Arquero",
           str_detect(tolower(Posicion), "izquierdo|derecho|lateral") ~ "Lateral",
           str_detect(tolower(Posicion), "centrocampista|mediocampista|medio") ~ "Centrocampista",
           str_detect(tolower(Posicion), "^defensa$|^defensor$|defensa central") ~ "Defensa",
           str_detect(tolower(Posicion), "^centrocampista/") ~ "Centrocampista",
           str_detect(tolower(Posicion), "^mediocampista/") ~ "Centrocampista",
           str_detect(tolower(Posicion), "^delantero/") ~ "Delantero",
           str_detect(tolower(Posicion), "delantero|extremo|dleantero") ~ "Delantero",
           TRUE ~ "Otros"
         )
  ) %>%
  select(-max_goles)



# crear nueva columna; Si el jugador juega fuera de su pais de su seleccion

DF_MUNDIALES <- DF_MUNDIALES %>%
 mutate(jugador_extranjero = ifelse(player_club_pais != Seleccion, 1,0) )

#---------------------------------------------------------------------
#----------[DATOS FILTRADOS DE INTERES]-------------------------------
#---------------------------------------------------------------------
DF_CAMPEONES <- DF_MUNDIALES %>% filter(dummy_campeon == 1)
DF_SUBCAMPEONES <- DF_MUNDIALES %>% filter(dummy_subcampeon == 1)
DF_ELIMINADOS <- DF_MUNDIALES %>% filter(dummy_eliminados_tempranos == 1)
DF_SEMIFINALISTAS <- DF_MUNDIALES %>%
  filter(etapa_general %in% c("campeon", "subcampeon", "semifinalista"))

DF_SUB_SEMIS <- DF_MUNDIALES %>%
  filter(etapa_general %in% c("subcampeon", "semifinalista"))

DF_ANOTADORES <- DF_MUNDIALES %>% filter(player_anotador == 1)

#-------------------GRAFICOS DE BARRAS-------------------

DF_CAMPEONES_PROPORCION <- DF_CAMPEONES %>%
  group_by(MYEAR, Seleccion) %>%
  summarise(
    total_jugadores_extranjeros = sum(jugador_extranjero, na.rm = TRUE),
    total_jugadores = n()  # Cuenta total de jugadores en la selección ese año
  ) %>%
  ungroup() %>%
  mutate(proporcion_extranjeros = (total_jugadores_extranjeros / total_jugadores) * 100)  # Convertir a porcentaje

# Crear gráfico de barras en proporción con ajustes solicitados
barras_ext_prop <- ggplot(DF_CAMPEONES_PROPORCION, aes(x = factor(MYEAR), y = proporcion_extranjeros, fill = Seleccion)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 1.2) +  # Barras agrupadas con borde negro
  labs(title = "Proporción Jugadores Extranjeros\n en Equipos Campeones",
       x = "Año del Mundial",
       y = "Proporción de Jugadores Extranjeros (%)",
       fill = "Selección") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 24, hjust = 0.5, color = "black"),  # Aumentado
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, family = "Consolas", color = "black"),      # Aumentado
    axis.text.y = element_text(size = 22, family = "Consolas", color = "black"),                             # Aumentado
    axis.title.x = element_text(size = 22, family = "Consolas", face = "bold", color = "black"),
    axis.title.y = element_text(size = 22, family = "Consolas", face = "bold", color = "black"),
    legend.title = element_text(size = 22, family = "Consolas", face = "bold", color = "black"),
    legend.text = element_text(size = 20, family = "Consolas", color = "black"),
    panel.grid.major.y = element_line(color = "gray70", size = 0.8, linetype = "dotted")
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Mostrar en porcentaje

# Mostrar el gráfico
print(barras_ext_prop)
# Guardar el gráfico como imagen PNG
ggsave(filename = "proporcion_jugadores extranjeros.png",
       plot = barras_ext_prop,
       width = 10, height = 8, dpi = 300)

#--------CORRELACCION DE PORCENTAJES DE JUGADORES EXTRANJEROS CON BASE A FASES ALCANZADAS-----------
# Calcular el porcentaje de jugadores extranjeros en cada categoría

calcular_porcentaje_extranjeros <- function(df) {
  sum(df$jugador_extranjero, na.rm = TRUE) / nrow(df) * 100
}

porcentaje_campeones <- calcular_porcentaje_extranjeros(DF_CAMPEONES)
porcentaje_subcampeones <- calcular_porcentaje_extranjeros(DF_SUBCAMPEONES)
porcentaje_semifinalistas <- calcular_porcentaje_extranjeros(DF_SEMIFINALISTAS)
porcentaje_eliminados <- calcular_porcentaje_extranjeros(DF_ELIMINADOS)

# Crear un dataframe con los resultados
df_porcentajes <- data.frame(
  Categoria = c("Campeones", "Subcampeones", "Semifinalistas", "Eliminados"),
  Porcentaje = c(porcentaje_campeones, porcentaje_subcampeones, porcentaje_semifinalistas, porcentaje_eliminados)
)

# Gráfica de barras con líneas de guía y sin etiquetas
Ext_por_etapa <- ggplot(df_porcentajes, aes(x = Categoria, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "purple4", color = "black", size = 1.2) +  # Todo purple4 con borde negro
  
  # Líneas horizontales de referencia gruesas y punteadas
  geom_hline(yintercept = seq(0, max(df_porcentajes$Porcentaje) + 10, by = 10), 
             color = "gray40", linetype = "dotted", size = 1.5) +
  
  labs(title = "Porcentaje de Jugadores Extranjeros por Categoría",
       x = "Fase Alcanzada",
       y = "Porcentaje de Jugadores Extranjeros") +
  
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 24, hjust = 0.5, color = "black"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 22, face = "bold", family = "Consolas", color = "black"),
    axis.text.y = element_text(size = 22, face = "bold", family = "Consolas", color = "black"),
    axis.title.x = element_text(size = 22, face = "bold", family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 22, face = "bold", family = "Consolas", color = "black"),
    panel.grid.major.x = element_blank()
  )

# Mostrar gráfico
print(Ext_por_etapa)

# Guardar gráfico
ggsave(filename = "Extranjero_porcentaje_por_fase.png",
       plot = Ext_por_etapa,
       width = 10, height = 8, dpi = 300)
#------------- CORRELACCION DE LIGAS Y EQUIPOS CON MAS JUGADORES EXTRANJEROS DE LOS CAMPEONES------

#---------------------------------------------------------------

DF_CAMPEONES$total_player_ones <- sum(DF_CAMPEONES$player_ones, na.rm = TRUE)
DF_CAMPEONES$total_jugador_extranjero <- sum(DF_CAMPEONES$jugador_extranjero, na.rm = TRUE)

# Mostrar el dataframe actualizado
print(DF_CAMPEONES)




# Análisis de las ligas con más jugadores extranjeros en selecciones campeonas

# Agrupar y sumar los valores por liga_pais
DF_RESULTADO <- DF_CAMPEONES %>%
  group_by(player_club_pais) %>%
  summarise(
        total_jugador_extranjero = sum(jugador_extranjero, na.rm = TRUE),
  ) %>%
  rename(liga_pais = player_club_pais)

# Ver el resultado
print(DF_RESULTADO)

#Proporcion de ligas con mas jugadores extranjeros -----------

DF_CAMPEONES <- DF_CAMPEONES %>%
  mutate(player_club_pais = ifelse(player_club_pais %in% c("FRA", "Francia"), "Francia", player_club_pais))

# Agrupar y sumar los valores por liga_pais
DF_RESULTADO <- DF_CAMPEONES %>%
  group_by(player_club_pais) %>%
  summarise(
    total_jugador_extranjero = sum(jugador_extranjero, na.rm = TRUE)
  ) %>%
  rename(liga_pais = player_club_pais)

# Calcular el total general
total_general <- sum(DF_RESULTADO$total_jugador_extranjero, na.rm = TRUE)

# Agregar columna con porcentaje
DF_RESULTADO <- DF_RESULTADO %>%
  mutate(porcentaje = (total_jugador_extranjero / total_general) * 100)

# Ver el resultado
print(DF_RESULTADO)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
###result
DF_RESULTADO <- data.frame(
  liga_pais = c("Alemania Federal", "Argentina", "Brasil", "Colombia", "España", 
                "Francia", "Inglaterra", "Italia", "Japón", "Portugal", "Uruguay"),
  total_jugador_extranjero = c(5, 0, 0, 1, 11, 5, 7, 11, 1, 1, 0),
  porcentaje = c(11.9, 0, 0, 2.38, 26.2, 11.9, 16.7, 26.2, 2.38, 2.38, 0)
)

# Crear el gráfico de barras con líneas de guía y sin etiquetas
Ligas_pais <- ggplot(DF_RESULTADO, aes(x = reorder(liga_pais, -porcentaje), y = porcentaje)) +
  geom_bar(stat = "identity", fill = "purple4", color = "black", size = 1.2) +  # Barras púrpura con borde negro
  
  # Líneas horizontales de referencia gruesas y punteadas
  geom_hline(yintercept = seq(0, max(DF_RESULTADO$porcentaje) + 5, by = 5),
             color = "gray40", linetype = "dotted", size = 1.5) +
  
  theme_minimal(base_family = "Consolas") +  # Fuente Consolas
  
  labs(title = "Proporción de Jugadores Extranjeros por País", 
       x = "Liga País", 
       y = "Porcentaje (%)") +
  
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20, face = "bold", color = "black"),
    axis.text.y = element_text(size = 20, face = "bold", color = "black"),
    axis.title.x = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(Ligas_pais)

# Guardar el gráfico
ggsave(filename = "proporcion_jugadores_extranjeros_liga.png",
       plot = Ligas_pais,
       width = 12, height = 8, dpi = 300)

