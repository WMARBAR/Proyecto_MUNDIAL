library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(writexl)
library(ggplot2)
library(tidyr)
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
  mutate(max_goles = max(`Goles Marcados(mundial)`, na.rm = TRUE),  # Máximo de goles en la edición
         dummy_goleador = if_else(`Goles Marcados(mundial)` == max_goles & max_goles > 0, 1, 0)) %>%
  ungroup() %>%  # Sacas el agrupamiento
  mutate(dummy_campeon = if_else(etapa_general == "campeon", 1, 0),
         dummy_subcampeon = if_else(etapa_general == "subcampeon", 1, 0),
         dummy_semifinalista = if_else(etapa_general == "semifinalista", 1, 0),
         dummy_eliminados_tempranos = if_else(etapa_general == "eliminado_temprano", 1, 0),
         dummy_finalistas = if_else(dummy_campeon == 1 | dummy_subcampeon == 1, 1, 0),
         player_ones = 1,
         player_anotador = if_else(`Goles Marcados(mundial)` > 0, 1, 0)) %>%
  select(-max_goles)  # Eliminas la columna auxiliar si no la necesitas
#---------------------------------------------------------------------
# ---------[ANALISIS GENERAL]-----------------------------------------
#---------------------------------------------------------------------
#-** ESTADISTICOS POR EDAD **
#---------------------------------------------------------------------
# Función para calcular la moda
moda <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

DF_MEDAD_stats <- data.frame(
  promedio = mean(DF_MUNDIALES$edad_Player, na.rm = TRUE),
  mediana = median(DF_MUNDIALES$edad_Player, na.rm = TRUE),
  moda = moda(DF_MUNDIALES$edad_Player),
  desviacion_estandar = sd(DF_MUNDIALES$edad_Player, na.rm = TRUE),
  varianza = var(DF_MUNDIALES$edad_Player, na.rm = TRUE),
  Q1 = quantile(DF_MUNDIALES$edad_Player, 0.25, na.rm = TRUE),
  Q2 = quantile(DF_MUNDIALES$edad_Player, 0.50, na.rm = TRUE),  # Es la mediana
  Q3 = quantile(DF_MUNDIALES$edad_Player, 0.75, na.rm = TRUE),
  Q4 = quantile(DF_MUNDIALES$edad_Player, 1, na.rm = TRUE)      # Máximo
)

print(DF_MEDAD_stats)

#---------------------------------------------------------------------
# ESTADISTICOS POR GOLES 
#---------------------------------------------------------------------
# Función para calcular la moda
moda <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
#---------------------------------------------------------------------
# JUGADORES QUE ANOTARON GOL
#---------------------------------------------------------------------
DF_Anotadores <- DF_MUNDIALES %>%
  filter(player_anotador == 1)

# Calculas las estadísticas solo sobre los que hicieron goles
DF_MGOLES_stats <- data.frame(
  promedio = mean(DF_Anotadores$`Goles Marcados(mundial)`, na.rm = TRUE),
  mediana = median(DF_Anotadores$`Goles Marcados(mundial)`, na.rm = TRUE),
  moda = moda(DF_Anotadores$`Goles Marcados(mundial)`),
  desviacion_estandar = sd(DF_Anotadores$`Goles Marcados(mundial)`, na.rm = TRUE),
  varianza = var(DF_Anotadores$`Goles Marcados(mundial)`, na.rm = TRUE),
  Q1 = quantile(DF_Anotadores$`Goles Marcados(mundial)`, 0.25, na.rm = TRUE),
  Q2 = quantile(DF_Anotadores$`Goles Marcados(mundial)`, 0.50, na.rm = TRUE),  # Es la mediana
  Q3 = quantile(DF_Anotadores$`Goles Marcados(mundial)`, 0.75, na.rm = TRUE),
  Q4 = quantile(DF_Anotadores$`Goles Marcados(mundial)`, 1, na.rm = TRUE)      # Máximo
)
print(DF_MGOLES_stats)
ggplot(DF_Anotadores, aes(x = `Goles Marcados(mundial)`)) +
  geom_density(fill = "#1b9e77", alpha = 0.5) +
  labs(
    title = "Distribución de Goles de los Anotadores en el Mundial",
    x = "Goles Marcados en el Mundial",
    y = "Densidad"
  ) +
  theme_minimal()

#---------------------------------------------------------------------
# POSICIONES ANOTADORAS :
#---------------------------------------------------------------------
DF_Resumen_Posicion <- DF_Anotadores %>%
  group_by(Posicion) %>%
  summarise(total_anotadores = sum(player_ones, na.rm = TRUE)) %>%
  arrange(desc(total_anotadores))
print(DF_Resumen_Posicion)
#---------------------------------------------------------------------
# CONTINENTES ANOTADORES :
#---------------------------------------------------------------------
DF_Resumen_Continente <- DF_Anotadores %>%
  filter(!is.na(confederacion) & confederacion != "") %>%
  group_by(confederacion) %>%
  summarise(total_anotadores = sum(player_ones, na.rm = TRUE)) %>%
  arrange(desc(total_anotadores))
print(DF_Resumen_Continente)
#---------------------------------------------------------------------
# PAISES ANOTADORES :
#---------------------------------------------------------------------
DF_Resumen_Seleccion <- DF_Anotadores %>%
  filter(!is.na(Seleccion) & Seleccion != "") %>%
  group_by(Seleccion) %>%
  summarise(total_anotadores = sum(player_ones, na.rm = TRUE)) %>%
  arrange(desc(total_anotadores))
print(DF_Resumen_Seleccion)

#---------------------------------------------------------------------
# POSICION Y SELECCION :
#---------------------------------------------------------------------
DF_Resumen_Seleccion_Posicion <- DF_Anotadores %>%
  filter(!is.na(Seleccion) & Seleccion != "",
         !is.na(Posicion) & Posicion != "") %>%
  group_by(Seleccion, Posicion) %>%
  summarise(total_anotadores = sum(player_ones, na.rm = TRUE)) %>%
  arrange(Seleccion, desc(total_anotadores))

print(DF_Resumen_Seleccion_Posicion)

#---------------------------------------------------------------------
# ---------[VOLUMETRIA GENERAL]---------------------------------------
#---------------------------------------------------------------------
# PORCENTAJE JUGADORES GANADORES
#---------------------------------------------------------------------
DF_PORCENTAJE <- DF_MUNDIALES %>%
  mutate(categoria = if_else(dummy_campeon == 1, "Campeones", "No Campeones")) %>%
  group_by(categoria) %>%
  summarise(Jugadores = n()) %>%
  mutate(Porcentaje = round((Jugadores / sum(Jugadores)) * 100, 1))

print(DF_PORCENTAJE)

# TORTA
library(ggplot2)

library(ggplot2)

# Guardar el gráfico en un objeto

grafico_campeones <- ggplot(DF_PORCENTAJE, aes(x = "", y = Porcentaje, fill = categoria)) +
  geom_col(width = 1, color = "black", size = 1) +  # Contorno negro marcado
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("Campeones" = "#fdfd96",    # Dorado - Victoria
                               "No Campeones" = "#FEA1B6")) +  # Rojo ladrillo - Derrota
  geom_text(aes(label = paste0(Porcentaje, "%"),
                y = cumsum(Porcentaje) - (Porcentaje / 2)),
            size = 12,
            family = "Consolas",
            color = "black",
            vjust = -0.5) +
  labs(title = "Proporción de los futbolistas campeones\nsobre el total de jugadores",
       fill = "Categoría") +
  theme_minimal(base_family = "serif") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Arial Black", face = "bold", size = 25, hjust = 0.5),  # Título más grande
    legend.title = element_text(family = "Consolas", face = "bold", size = 18),
    legend.text = element_text(family = "Consolas", size = 18),
    legend.position = "top",                # Leyenda arriba
    legend.justification = "center"         # Centrada horizontalmente
  )

grafico_campeones

# Guardar el gráfico como imagen PNG
ggsave(filename = "GRAPHS/proporcion_campeones.png",
       plot = grafico_campeones,
       width = 10, height = 8, dpi = 300)

#---------------------------------------------------------------------
# PORCENTAJE JUGADORES FINALSTAS :
#---------------------------------------------------------------------
DF_PORCENTAJE_FINALISTAS <- DF_MUNDIALES %>%
  mutate(categoria = if_else(dummy_finalistas == 1, "Finalistas", "Otros")) %>%
  group_by(categoria) %>%
  summarise(Jugadores = n()) %>%
  mutate(Porcentaje = round((Jugadores / sum(Jugadores)) * 100, 1))

print(DF_PORCENTAJE_FINALISTAS)

# Gráfico
ggplot(DF_PORCENTAJE_FINALISTAS, aes(x = "", y = Porcentaje, fill = categoria)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Finalistas" = "#1b9e77", "Otros" = "gray50")) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 6, family = "Consolas") +
  labs(title = "% Finalistas sobre el total de jugadores",
       fill = "Categoría") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "Consolas", face = "bold"))
#---------------------------------------------------------------------
# PORCENTAJE JUGADORES SEMIFINALSTAS (CON LOS FINALISTAS): 
#---------------------------------------------------------------------
# Crear la categoría combinada de Semifinalistas y Finalistas
# Calcular posición personalizada para el texto
DF_PORCENTAJE_FINAL <- DF_PORCENTAJE_FINAL %>%
  arrange(desc(categoria)) %>%
  mutate(y_pos = cumsum(Porcentaje) - (Porcentaje / 2))

# Gráfico con ajuste de la proporción pequeña
grafico_semiftas <- ggplot(DF_PORCENTAJE_FINAL, aes(x = "", y = Porcentaje, fill = categoria)) +
  geom_col(width = 1, color = "black", size = 1) +  # Contorno negro marcado
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("Semifinalistas y Finalistas" = "#fdfd96",  # Amarillo
                               "Perdedores" = "#FEA1B6")) +                 # Rosado
  geom_text(aes(label = paste0(Porcentaje, "%"),
                y = y_pos,
                size = if_else(categoria == "Semifinalistas y Finalistas", 10, 14)),  # Más chico si es Semifinalista
            family = "Consolas",
            color = "black",
            vjust = -0.5,
            show.legend = FALSE) +
  scale_size_identity() +  # Para que respete el size dinámico
  labs(title = "Proporción de Semifinalistas\nsobre el total de jugadores",
       fill = "Categoría") +
  theme_minimal(base_family = "serif") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Arial Black", face = "bold", size = 25, hjust = 0.5),
    legend.title = element_text(family = "Consolas", face = "bold", size = 18),
    legend.text = element_text(family = "Consolas", size = 18),
    legend.position = "top",                # Leyenda arriba
    legend.justification = "center"         # Centrada horizontalmente
  )

grafico_semiftas


# Guardar el gráfico como imagen PNG
ggsave(filename = "GRAPHS/proporcion_semifinalistas.png",
       plot = grafico_semiftas,
       width = 10, height = 8, dpi = 300)

#---------------------------------------------------------------------
# PORCENTAJE JUGADORES QUE ANOTARON: 
#---------------------------------------------------------------------
# Crear la categoría combinada de Semifinalistas y Finalistas
DF_PORCENTAJE_ANOTADORES <- DF_MUNDIALES %>%
  mutate(categoria = if_else(player_anotador == 1, "Anotadores", "No Anotadores")) %>%
  group_by(categoria) %>%
  summarise(Jugadores = sum(player_ones, na.rm = TRUE)) %>%
  mutate(Porcentaje = round((Jugadores / sum(Jugadores)) * 100, 1))

print(DF_PORCENTAJE_ANOTADORES)

ggplot(DF_PORCENTAJE_ANOTADORES, aes(x = "", y = Porcentaje, fill = categoria)) +
  geom_col(width = 1, color = "white") +       # La torta
  coord_polar(theta = "y") +                   # La convierte en circular
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 6, family = "Consolas") +
  scale_fill_manual(values = c("Anotadores" = "#1b9e77", "No Anotadores" = "gray50")) +
  theme_void() +                               # Quita todo menos la torta
  labs(title = "% de Jugadores Anotadores vs No Anotadores",
       fill = "Categoría")


#---------------------------------------------------------------------
#----------[DATOS FILTRADOS DE INTERES]-------------------------------
#---------------------------------------------------------------------
DF_CAMPEONES <- DF_MUNDIALES %>% filter(dummy_campeon == 1)
DF_SUBCAMPEONES <- DF_MUNDIALES %>% filter(dummy_subcampeon == 1)
DF_ELIMINADOS <- DF_MUNDIALES %>% filter(dummy_eliminados_tempranos == 1)
DF_SEMIFINALISTAS <- DF_MUNDIALES %>%
  filter(etapa_general %in% c("campeon", "subcampeon", "semifinalista"))

#---------------------------------------------------------------------
#----------[ANALISIS DE LOS CAMPEONES]--------------------------------
#---------------------------------------------------------------------
#---** ESTADISTICOS EDAD**------------------------------------------------
#---------------------------------------------------------------------
DF_CAMPEDAD_stats <- data.frame(
  promedio = mean(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  mediana = median(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  desviacion_estandar = sd(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  varianza = var(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  Q1 = quantile(DF_CAMPEONES$edad_Player, 0.25, na.rm = TRUE),
  Q2 = quantile(DF_CAMPEONES$edad_Player, 0.50, na.rm = TRUE),  # Es la mediana
  Q3 = quantile(DF_CAMPEONES$edad_Player, 0.75, na.rm = TRUE),
  Q4 = quantile(DF_CAMPEONES$edad_Player, 1, na.rm = TRUE)      # Máximo
)

# Visualiza la tabla
print(DF_CAMPEDAD_stats)
#---------------------------------------------------------------------
#---** ESTADISTICOS GOLES**------------------------------------------------
#---------------------------------------------------------------------
# Función para calcular la moda
moda <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Filtras solo los campeones que hayan anotado goles
DF_CAMPEONES_GOLEADORES <- DF_CAMPEONES %>%
  filter(player_anotador == 1)

# Calculas las estadísticas solo sobre los campeones que hicieron goles
DF_CAMPGOLES_stats <- data.frame(
  promedio = mean(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`, na.rm = TRUE),
  mediana = median(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`, na.rm = TRUE),
  moda = moda(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`),
  desviacion_estandar = sd(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`, na.rm = TRUE),
  varianza = var(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`, na.rm = TRUE),
  Q1 = quantile(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`, 0.25, na.rm = TRUE),
  Q2 = quantile(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`, 0.50, na.rm = TRUE),  # Es la mediana
  Q3 = quantile(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`, 0.75, na.rm = TRUE),
  Q4 = quantile(DF_CAMPEONES_GOLEADORES$`Goles Marcados(mundial)`, 1, na.rm = TRUE)      # Máximo
)

# Visualizas la tabla solo con los goleadores campeones
print(DF_CAMPGOLES_stats)
# Gráfico de la distribución de goles de los campeones goleadores
ggplot(DF_CAMPEONES_GOLEADORES, aes(x = `Goles Marcados(mundial)`)) +
  geom_density(fill = "#1b9e77", alpha = 0.5) +
  labs(
    title = "Distribución de Goles de los Campeones Goleadores",
    x = "Goles Marcados en el Mundial",
    y = "Densidad"
  ) +
  theme_minimal()
#---------------------------------------------------------------------
#-----------[GRAFICOS DE LOS CAMPEONES]-------------------------------
#---------------------------------------------------------------------
#---** GANADORES POR PAÍS **
#---------------------------------------------------------------------
DF_TITULOS_PAISES <- DF_CAMPEONES %>% 
  select(Seleccion, MYEAR) %>%
  distinct() %>%
  group_by(Seleccion) %>%
  summarise(Titulos = n()) %>%
  arrange(desc(Titulos))

ggplot(DF_TITULOS_PAISES, aes(x = reorder(Seleccion, Titulos), y = Titulos)) +
  geom_col(fill = "goldenrod") +
  coord_flip() +
  labs(title = "Selecciones ganadoras del mundial",
       x = "Selección",
       y = "Cantidad de Títulos") +
  theme_minimal()
#---------------------------------------------------------------------
#---** GANADORES POR CONTINENTE **
#---------------------------------------------------------------------
DF_TITULOS_CONTINENTE <- DF_CAMPEONES %>% 
  select(confederacion, MYEAR) %>%
  distinct() %>%
  group_by(confederacion) %>%
  summarise(Titulos = n()) %>%
  arrange(desc(Titulos))

# Calculamos los porcentajes
DF_TITULOS_CONTINENTE <- DF_TITULOS_CONTINENTE %>%
  mutate(porcentaje = round((Titulos / sum(Titulos)) * 100, 1),
         label = paste0(porcentaje, "%"))

# Gráfico de torta con porcentajes
ggplot(DF_TITULOS_CONTINENTE, aes(x = "", y = Titulos, fill = confederacion)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("UEFA" = "#228B22", "CONMEBOL" = "black")) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            color = "white", 
            family = "Consolas",
            size = 5) +
  labs(title = "Mundiales Ganados por Confederación",
       fill = "Confederación") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "Consolas", face = "bold"))

#---------------------------------------------------------------------
#---** BATALLA POR CONTINENTE **
#---------------------------------------------------------------------
DF_POR_ANIO <- DF_CAMPEONES %>%
  select(MYEAR, confederacion) %>%
  distinct() %>%
  arrange(MYEAR)

# Crear columnas de acumulado por confederación
DF_ACUMULADO <- DF_POR_ANIO %>%
  mutate(uefa_gana = ifelse(confederacion == "UEFA", 1, 0),
         conmebol_gana = ifelse(confederacion == "CONMEBOL", 1, 0)) %>%
  mutate(uefa_acum = cumsum(uefa_gana),
         conmebol_acum = cumsum(conmebol_gana))

# Armamos la data
DF_GRAFICO <- data.frame(
  MYEAR = rep(DF_ACUMULADO$MYEAR, 2),
  Confederacion = c(rep("UEFA", nrow(DF_ACUMULADO)), rep("CONMEBOL", nrow(DF_ACUMULADO))),
  Titulos = c(DF_ACUMULADO$uefa_acum, DF_ACUMULADO$conmebol_acum)
)

# Graficamos con líneas punteadas en Y y límite en 15
ggplot(DF_GRAFICO, aes(x = MYEAR, y = Titulos, color = Confederacion)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("UEFA" = "black", "CONMEBOL" = "#228B22")) +
  geom_hline(yintercept = seq(0, 12, 1), color = "gray80", linetype = "dotted") +  # Líneas punteadas
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 15, 1)) +  # Límite hasta 15 y breaks
  labs(title = "Carrera Histórica de Títulos Mundiales por continente",
       x = "Año del Mundial",
       y = "Títulos Acumulados",
       color = "Confederación") +
  theme_minimal()

#---------------------------------------------------------------------
#---** BATALLA POR PAISES **
#---------------------------------------------------------------------

# Preparamos la data de campeones por año y país (único por mundial ganado)
DF_POR_ANIO_PAISES <- DF_CAMPEONES %>%
  select(MYEAR, Seleccion) %>%
  distinct() %>%
  arrange(MYEAR)

# Calculamos el acumulado por país
DF_ACUMULADO_PAISES <- DF_POR_ANIO_PAISES %>%
  group_by(Seleccion) %>%
  arrange(MYEAR) %>%
  mutate(titulos_acumulados = row_number())

# Expandimos la data para graficar el acumulado en el tiempo
DF_GRAFICO_PAISES <- DF_ACUMULADO_PAISES %>%
  ungroup() %>%
  complete(MYEAR = seq(min(MYEAR), max(MYEAR), 4), Seleccion, fill = list(titulos_acumulados = 0)) %>%
  group_by(Seleccion) %>%
  arrange(MYEAR) %>%
  mutate(titulos_acumulados = cummax(titulos_acumulados))

# Graficamos la evolución de títulos por país
ggplot(DF_GRAFICO_PAISES, aes(x = MYEAR, y = titulos_acumulados, color = Seleccion)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = seq(0, 6, 1), color = "gray80", linetype = "dotted") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +
  scale_color_manual(values = c(
    "Brasil" = "#1b9e77",
    "Alemania Federal" = "#d95f02",
    "Italia" = "#7570b3",
    "Argentina" = "#e7298a",
    "Uruguay" = "#66a61e",
    "Francia" = "#e6ab02",
    "Inglaterra" = "#a6761d",
    "España" = "#666666"
  )) +
  labs(title = "Carrera Histórica de Títulos Mundiales por País",
       x = "Año del Mundial",
       y = "Títulos Acumulados",
       color = "País") +
  theme_minimal()
#---------------------------------------------------------------------
#---** GOLES **
#---------------------------------------------------------------------
DF_GOLES_CAMPEONES <- DF_CAMPEONES %>%
  group_by(MYEAR, Seleccion) %>%
  summarise(Goles_totales = sum(`Goles Marcados(mundial)`, na.rm = TRUE)) %>%
  arrange(MYEAR)

print(DF_GOLES_CAMPEONES)

#---------------------------------------------------------------------
#----------[ANALISIS DE LOS SEMIFINALSTAS (GRUPO COMPLETO)]-----------
#---------------------------------------------------------------------
#---** ESTADISTICOS **
DF_SEMIFINALISTAS_stats <- data.frame(
  promedio = mean(DF_SEMIFINALISTAS$edad_Player, na.rm = TRUE),
  mediana = median(DF_SEMIFINALISTAS$edad_Player, na.rm = TRUE),
  moda = moda(DF_SEMIFINALISTAS$edad_Player),
  desviacion_estandar = sd(DF_SEMIFINALISTAS$edad_Player, na.rm = TRUE),
  varianza = var(DF_SEMIFINALISTAS$edad_Player, na.rm = TRUE),
  Q1 = quantile(DF_SEMIFINALISTAS$edad_Player, 0.25, na.rm = TRUE),
  Q2 = quantile(DF_SEMIFINALISTAS$edad_Player, 0.50, na.rm = TRUE),  # Es la mediana
  Q3 = quantile(DF_SEMIFINALISTAS$edad_Player, 0.75, na.rm = TRUE),
  Q4 = quantile(DF_SEMIFINALISTAS$edad_Player, 1, na.rm = TRUE)      # Máximo
)

# Visualiza la tabla
print(DF_SEMIFINALISTAS_stats)


#----------EXPORT TO EXCEL---------------------------------------------
ruta_guardado_selecciones <- "test_analisis.xlsx"
write_xlsx(DF_MUNDIALES, ruta_guardado_selecciones)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado_selecciones, "\n")