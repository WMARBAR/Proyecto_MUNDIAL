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
         player_anotador = if_else(`Goles Marcados(mundial)` > 0, 1, 0),
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
  select(-max_goles)  # Eliminas la columna auxiliar si no la necesitas



#---------------------------------------------------------------------
#---3. [CATEGORIZACIÓN POR EDAD Y EXP]
#---------------------------------------------------------------------
# EDAD
n <- nrow(DF_MUNDIALES)
k <- ceiling(1 + log2(n))  # Número de clases según Sturges

# Calcular valores mínimos y máximos de edad
min_edad <- floor(min(DF_MUNDIALES$edad_Player, na.rm = TRUE))
max_edad <- ceiling(max(DF_MUNDIALES$edad_Player, na.rm = TRUE))

# Generar secuencia de cortes enteros
breaks <- seq(min_edad, max_edad, length.out = k + 1)
breaks <- unique(round(breaks))  # Redondea y elimina duplicados

# Aplicar el corte
DF_MUNDIALES <- DF_MUNDIALES %>%
  mutate(edad_categoria = cut(edad_Player,
                              breaks = breaks,
                              include.lowest = TRUE,
                              right = FALSE,
                              dig.lab = 4))
# EXP por years_expMundial
n_exp <- nrow(DF_MUNDIALES)
k_exp <- ceiling(1 + log2(n_exp))  # Número de clases según Sturges

# Calcular valores mínimos y máximos de years_expMundial
min_exp <- floor(min(DF_MUNDIALES$years_expMundial, na.rm = TRUE))
max_exp <- ceiling(max(DF_MUNDIALES$years_expMundial, na.rm = TRUE))

# Generar secuencia de cortes enteros
breaks_exp <- seq(min_exp, max_exp, length.out = k_exp + 1)
breaks_exp <- unique(round(breaks_exp))  # Redondea y elimina duplicados

# Aplicar el corte y crear la nueva columna exp_categoria
DF_MUNDIALES <- DF_MUNDIALES %>%
  mutate(exp_categoria = cut(years_expMundial,
                             breaks = breaks_exp,
                             include.lowest = TRUE,
                             right = FALSE,
                             dig.lab = 4))

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
# PORCENTAJE JUGADORES SEMIFINALSTAS (CON LOS FINALISTAS): 
#---------------------------------------------------------------------
# Crear la categoría combinada de Semifinalistas y Finalistas
# Calcular posición personalizada para el texto

DF_PORCENTAJE_FINAL <- DF_MUNDIALES %>%
  mutate(categoria = if_else(etapa_general %in% c("campeon", "subcampeon", "semifinalista"),
                             "Semifinalistas y Finalistas", "Perdedores")) %>%
  group_by(categoria) %>%
  summarise(Jugadores = n(), .groups = 'drop') %>%
  mutate(Porcentaje = round((Jugadores / sum(Jugadores)) * 100, 1))


grafico_semiftas <- ggplot(DF_PORCENTAJE_FINAL, aes(x = "", y = Porcentaje, fill = categoria)) +
  geom_col(width = 1, color = "black", size = 1) +  # Contorno negro marcado
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("Semifinalistas y Finalistas" = "#fdfd96",
                               "Perdedores" = "#FEA1B6")) +  # Rojo ladrillo - Derrota
  geom_text(aes(label = paste0(Porcentaje, "%"),
                y = cumsum(Porcentaje) - (Porcentaje / 2)),
            size = 12,
            family = "Consolas",
            color = "black",
            vjust = -0.5) +
  labs(title = "Semifinalistas\nsobre el total de jugadores",
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

grafico_semiftas



# Guardar el gráfico como imagen PNG
ggsave(filename = "GRAPHS/proporcion_semifinalistas.png",
       plot = grafico_semiftas,
       width = 10, height = 8, dpi = 300)

#---------------------------------------------------------------------
# DISTRIBUCIÓN GLOBAL DE LA EDAD: 
#---------------------------------------------------------------------

# Crear la distribución de jugadores por edad
DF_EDAD_DIST <- DF_MUNDIALES %>%
  group_by(edad_Player) %>%
  summarise(jugadores = sum(player_ones, na.rm = TRUE)) %>%
  mutate(densidad = jugadores / sum(jugadores))

# Ver la tabla si quieres
head(DF_EDAD_DIST)

dist_edades <- ggplot(DF_MUNDIALES, aes(x = edad_Player)) +
  geom_density(aes(weight = player_ones),
               fill = "seagreen4",
               alpha = 0.4,
               color = "darkgreen",
               size = 1.2) +
  labs(title = "Edad de los futbolistas",
       x = "Edad",
       y = "Densidad") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 20, hjust = 0.5),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dist_edades

# Guardar el gráfico como imagen PNG
ggsave(filename = "GRAPHS/dist_edadGlobal.png",
       plot = dist_edades,
       width = 10, height = 8, dpi = 300)


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

#---------------------------------------------------------------------
#----------[DE LOS ANOTADORES]--------------------------------
#---------------------------------------------------------------------
# VOLUMETRIA por intervalo de edad ---------------------------------------------------
#---------------------------------------------------------------------

total_anotadores <- sum(DF_ANOTADORES$player_ones, na.rm = TRUE)

hist_edad_categoria <- ggplot(DF_ANOTADORES, aes(x = edad_categoria, weight = player_ones)) +
  geom_bar(fill = "seagreen4", color = "black", size = 1) +
  labs(title = "Anotadores por Edad",
       x = "Edad intervalo",
       y = "Anotadores") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / total_anotadores * 100, name = "Porcentaje (%)")
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 20, hjust = 0.5),
    
    # 🔥 Aumentamos tamaño de números en ambos ejes (+2)
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 20),
    axis.text.y.right = element_text(size = 20),  # También el secundario
    
    axis.title = element_text(size = 20),
    
    # Líneas guía horizontales punteadas y gruesas
    panel.grid.major.y = element_line(color = "gray40", size = 1, linetype = "dotted"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

hist_edad_categoria

ggsave(filename = "GRAPHS/hist_edad_anotadores.png",
       plot = hist_edad_categoria,
       width = 12, height = 8, dpi = 300)


# EXPERIENCIA
total_anotadores <- sum(DF_ANOTADORES$player_ones, na.rm = TRUE)

hist_exp_categoria <- ggplot(DF_ANOTADORES, aes(x = exp_categoria, weight = player_ones)) +
  geom_bar(fill = "#374080", color = "black", size = 1) +
  labs(title = "Anotadores por Experiencia",
       x = "Experiencia en Mundiales (intervalo de años)",
       y = "Anotadores") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / total_anotadores * 100, name = "Porcentaje (%)")
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 20, hjust = 0.5),
    
    # 🔥 Aumentamos tamaño de números en ambos ejes (+2)
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 20),
    axis.text.y.right = element_text(size = 20),  # También el secundario
    
    axis.title = element_text(size = 20),
    
    # Líneas guía horizontales punteadas y gruesas
    panel.grid.major.y = element_line(color = "gray40", size = 1, linetype = "dotted"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

hist_exp_categoria

# Guardar el gráfico
ggsave(filename = "GRAPHS/hist_exp_anotadores.png",
       plot = hist_exp_categoria,
       width = 12, height = 8, dpi = 300)

#---------------------------------------------------------------------
# VOLUMETRIA distribución de la edad ---------------------------------
#---------------------------------------------------------------------

DF_SCORERage_DIST <- ggplot(DF_ANOTADORES, aes(x = edad_Player)) +
  geom_density(aes(weight = player_ones),
               fill = "indianred1",     # Relleno rojo claro
               alpha = 0.4,
               color = "darkred",       # Línea roja oscura
               size = 1.5) +
  labs(title = "Densidad de Anotadores por Edad",
       x = "Edad",
       y = "Densidad Estimada") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 20, hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "top",
    legend.justification = "center"
  )

DF_SCORERage_DIST

library(dplyr)
library(ggplot2)

# Crear columna 'Grupo' en cada dataset
DF_MUNDIALES_plot <- DF_MUNDIALES %>%
  mutate(Grupo = "Todos los Jugadores")

DF_ANOTADORES_plot <- DF_ANOTADORES %>%
  mutate(Grupo = "Anotadores")

# Unir los datasets
DF_DENSIDAD <- bind_rows(DF_MUNDIALES_plot, DF_ANOTADORES_plot)

# Gráfico combinado con leyenda arriba centrada
dist_comparativa <- ggplot(DF_DENSIDAD, aes(x = edad_Player, weight = player_ones, color = Grupo, fill = Grupo)) +
  geom_density(alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c("Todos los Jugadores" = "seagreen4",
                               "Anotadores" = "indianred1")) +
  scale_color_manual(values = c("Todos los Jugadores" = "darkgreen",
                                "Anotadores" = "darkred")) +
  labs(title = "Comparativa de Densidad por Edad\nFutbolistas vs Anotadores",
       x = "Edad",
       y = "Densidad Estimada",
       fill = "Grupo",
       color = "Grupo") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 22, hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.title = element_text(family = "Consolas", size = 16),
    legend.text = element_text(family = "Consolas", size = 14),
    
    # 🔥 Leyenda arriba y centrada
    legend.position = "top",
    legend.justification = "center"
  )

dist_comparativa

ggsave(filename = "GRAPHS/hist_edadVStodo.png",
       plot = dist_comparativa,
       width = 12, height = 8, dpi = 300)

#---------------------------------------------------------------------
#----------[ANALISIS DE LOS CAMPEONES]--------------------------------
#---------------------------------------------------------------------
#---** ESTADISTICOS EDAD**--------------------------------------------
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
#---** EDAD ANOTADORES boxplot**------------------------------------------------
#---------------------------------------------------------------------
DF_PERDEDORES_ANOTADORES <- DF_ELIMINADOS %>%
  filter(player_anotador == 1) %>%
  mutate(grupo = "Perdedores")

DF_CAMPEONES_ANOTADORES <- DF_CAMPEONES %>% 
  filter(player_anotador == 1) %>%
  mutate(grupo = "Campeones")

DF_SUB_SEMIS_ANOTADORES <- DF_SUB_SEMIS %>%
  filter(player_anotador == 1) %>%
  mutate(grupo = "Subcampeones/Semis")

DF_BOXPLOT <- bind_rows(DF_CAMPEONES_ANOTADORES, 
                        DF_PERDEDORES_ANOTADORES, 
                        DF_SUB_SEMIS_ANOTADORES)

boxplot_completo <- ggplot(DF_BOXPLOT, aes(x = grupo, y = edad_Player, fill = grupo)) +
  geom_boxplot(color = "black", size = 1.9) +
  scale_fill_manual(values = c("Campeones" = "goldenrod1",
                               "Subcampeones/Semis" = "lightskyblue",
                               "Perdedores" = "lightcoral")) +
  labs(title = "Edad de anotadores\n Campeones, Perdedores y\n Subcampeones/Semis",
       x = "",
       y = "Edad") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 40, hjust = 0.5),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 30),
    legend.position = "none",
    
    # 🔥 Gridlines horizontales activas y gruesas
    panel.grid.major.y = element_line(color = "gray50", size = 1),
    panel.grid.minor.y = element_blank(),  # Opcional: sin grilla menor
    panel.grid.major.x = element_blank()   # Sin grid vertical
  )

boxplot_completo



ggsave(filename = "GRAPHS/boxplot_comparativo.png",
       plot = boxplot_completo,
       width = 12, height = 8, dpi = 300)

#---------------------------------------------------------------------
#---** ANOTADORES % por POSICIONES**------------------------------------------------
#---------------------------------------------------------------------

# Calcular el total de goles por grupo (se mantiene igual)
total_goles_campeones <- sum(DF_CAMPEONES_ANOTADORES$`Goles Marcados(mundial)`, na.rm = TRUE)
total_goles_perdedores <- sum(DF_PERDEDORES_ANOTADORES$`Goles Marcados(mundial)`, na.rm = TRUE)
total_goles_subsemis <- sum(DF_SUB_SEMIS_ANOTADORES$`Goles Marcados(mundial)`, na.rm = TRUE)

# Agrupar por categ_posicion y calcular porcentaje por grupo
DF_CAMPEONES_BARRA <- DF_CAMPEONES_ANOTADORES %>%
  group_by(categ_posicion) %>%
  summarise(goles = sum(`Goles Marcados(mundial)`, na.rm = TRUE)) %>%
  mutate(grupo = "Campeones",
         porcentaje = (goles / total_goles_campeones) * 100)

DF_PERDEDORES_BARRA <- DF_PERDEDORES_ANOTADORES %>%
  group_by(categ_posicion) %>%
  summarise(goles = sum(`Goles Marcados(mundial)`, na.rm = TRUE)) %>%
  mutate(grupo = "Perdedores",
         porcentaje = (goles / total_goles_perdedores) * 100)

DF_SUBSEMIS_BARRA <- DF_SUB_SEMIS_ANOTADORES %>%
  group_by(categ_posicion) %>%
  summarise(goles = sum(`Goles Marcados(mundial)`, na.rm = TRUE)) %>%
  mutate(grupo = "Subcampeones/Semis",
         porcentaje = (goles / total_goles_subsemis) * 100)

# Unir todo
DF_BARRA_CATEG <- bind_rows(DF_CAMPEONES_BARRA, DF_PERDEDORES_BARRA, DF_SUBSEMIS_BARRA)

# 🔥 GRAFICO DE BARRAS AGRUPADAS por categ_posición 🔥
grafico_barras_categ <- ggplot(DF_BARRA_CATEG, aes(x = categ_posicion, y = porcentaje, fill = grupo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", size = 1) +
  scale_fill_manual(values = c("Campeones" = "goldenrod1",
                               "Perdedores" = "lightcoral",
                               "Subcampeones/Semis" = "lightskyblue")) +
  labs(title = "Porcentaje de Goles por posición ",
       x = "Posición",
       y = "Porcentaje (%)",
       fill = "Grupo") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 28, hjust = 0.5),
    axis.text.x = element_text(size = 25, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.title = element_text(family = "Consolas", size = 20),
    legend.text = element_text(family = "Consolas", size = 20),
    legend.position = "top",
    legend.justification = "center"
  )

grafico_barras_categ

ggsave(filename = "GRAPHS/P_goles_posicion.png",
       plot = grafico_barras_categ,
       width = 12, height = 8, dpi = 300)





#---------------------------------------------------------------------
#---** EDADES DE LOS 3 GRUPOS **------------------------------------------------
#---------------------------------------------------------------------

# Crear el gráfico de densidad
densidad_edades <- ggplot() +
  # Densidad de Campeones
  geom_density(data = DF_CAMPEONES, aes(x = edad_Player, weight = player_ones),
               fill = "goldenrod1", color = "goldenrod3", alpha = 0.4, size = 1.2) +
  
  # Densidad de Perdedores
  geom_density(data = DF_ELIMINADOS, aes(x = edad_Player, weight = player_ones),
               fill = "lightcoral", color = "darkred", alpha = 0.4, size = 1.2) +
  
  # Densidad de Subcampeones / Semifinalistas
  geom_density(data = DF_SUB_SEMIS, aes(x = edad_Player, weight = player_ones),
               fill = "lightskyblue", color = "steelblue", alpha = 0.4, size = 1.2) +
  
  labs(title = "Distribución de Edad por Grupo",
       x = "Edad de los Jugadores",
       y = "Densidad") +
  
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 22, hjust = 0.5),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25)
  )

# Mostrar gráfico
densidad_edades

ggsave(filename = "GRAPHS/dist_edades.png",
       plot = densidad_edades,
       width = 12, height = 8, dpi = 300)


library(e1071)
# Curtosis de los campeones
curtosis_campeones <- kurtosis(DF_CAMPEONES$edad_Player, na.rm = TRUE)

# Curtosis de los perdedores
curtosis_perdedores <- kurtosis(DF_ELIMINADOS$edad_Player, na.rm = TRUE)

# Curtosis de los subcampeones / semifinalistas
curtosis_semis <- kurtosis(DF_SUB_SEMIS$edad_Player, na.rm = TRUE)

# Mostrar resultados
cat("Curtosis Campeones:", curtosis_campeones, "\n")
cat("Curtosis Perdedores:", curtosis_perdedores, "\n")
cat("Curtosis Subcampeones/Semis:", curtosis_semis, "\n")



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
#---** CORRELACION GOLES EDAD**------------------------------------------------
#---------------------------------------------------------------------

# Campeones anotadores
DF_CAMPEONES_GOLES <- DF_CAMPEONES %>%
  filter(player_anotador == 1) %>%
  group_by(edad_Player) %>%
  summarise(goles = sum(`Goles Marcados(mundial)`, na.rm = TRUE)) %>%
  mutate(grupo = "Campeones")
DF_CAMPEONES_GOLES
# Perdedores anotadores
DF_PERDEDORES_GOLES <- DF_ELIMINADOS %>%
  filter(player_anotador == 1) %>%
  group_by(edad_Player) %>%
  summarise(goles = sum(`Goles Marcados(mundial)`, na.rm = TRUE)) %>%
  mutate(grupo = "Perdedores")

# Subcampeones y semifinalistas anotadores
DF_SUBSEMIS_GOLES <- DF_SUB_SEMIS %>%
  filter(player_anotador == 1) %>%
  group_by(edad_Player) %>%
  summarise(goles = sum(`Goles Marcados(mundial)`, na.rm = TRUE)) %>%
  mutate(grupo = "Subcampeones/Semis")

DF_GOLES_EDAD <- bind_rows(DF_CAMPEONES_GOLES, DF_PERDEDORES_GOLES, DF_SUBSEMIS_GOLES)

plot_campeones <- ggplot(DF_CAMPEONES_GOLES, aes(x = edad_Player, y = goles)) +
  geom_point(color = "goldenrod1", size = 3, alpha = 0.7) +
  labs(title = "Campeones", x = "Edad", y = "Goles") +
  theme_minimal(base_family = "serif") +
  theme(plot.title = element_text(family = "Consolas", face = "bold", size = 18, hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
plot_campeones
# Gráfico de SUBCAMPEONES/SEMIS
plot_subsemis <- ggplot(DF_SUBSEMIS_GOLES, aes(x = edad_Player, y = goles)) +
  geom_point(color = "lightskyblue", size = 3, alpha = 0.7) +
  labs(title = "Subcampeones / Semis", x = "Edad", y = "Goles") +
  theme_minimal(base_family = "serif") +
  theme(plot.title = element_text(family = "Consolas", face = "bold", size = 18, hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

plot_subsemis
# Gráfico de PERDEDORES SIN LÍNEA
plot_perdedores <- ggplot(DF_PERDEDORES_GOLES, aes(x = edad_Player, y = goles)) +
  geom_point(color = "lightcoral", size = 3, alpha = 0.7) +
  labs(title = "Perdedores", x = "Edad", y = "Goles") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
# Combinar los datos de campeones y perdedores
# Agregar el grupo a cada dataset
DF_CAMPEONES_GOLES$grupo <- "Campeones"
DF_PERDEDORES_GOLES$grupo <- "Perdedores"
DF_SUBSEMIS_GOLES$grupo <- "Subcampeones/Semis"

# Combinar todos los grupos
DF_COMBINADO <- bind_rows(DF_CAMPEONES_GOLES, DF_PERDEDORES_GOLES, DF_SUBSEMIS_GOLES)

# Graficar
plot_comparativo <- ggplot(DF_COMBINADO, aes(x = edad_Player, y = goles, color = grupo)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Campeones" = "#E3B62C",          # Dorado
                                "Perdedores" = "#8F0E00",         # Rojo oscuro
                                "Subcampeones/Semis" = "#3D87B6"  # Azul
  )) +
  labs(title = "Relación entre Edad y Goles\nCampeones, Subcampeones/Semis y Perdedores",
       x = "Edad",
       y = "Goles",
       color = "Grupo") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 22, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(family = "Consolas", size = 16),
    legend.text = element_text(family = "Consolas", size = 14),
    legend.position = "top",
    legend.justification = "center"
  )

plot_comparativo


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

graf_champs <- ggplot(DF_TITULOS_PAISES, aes(x = reorder(Seleccion, Titulos), y = Titulos)) +
  geom_col(fill = "goldenrod") +
  coord_flip() +
  labs(title = "Selecciones ganadoras del mundial",
       x = NULL,   # 🔥 Eliminamos la etiqueta "Selección"
       y = "Cantidad de Títulos") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", size = 25, face = "bold"),
    axis.title.x = element_blank(),  # 🔥 Por si acaso, aseguramos que no salga
    axis.title.y = element_text(family = "Consolas", size = 25),
    axis.text.x = element_text(family = "Consolas", size = 25),
    axis.text.y = element_text(family = "Consolas", size = 25)
  )

graf_champs

ggsave(filename = "GRAPHS/graf_champs.png",
       plot = graf_champs,
       width = 12, height = 8, dpi = 300)

#---------------------------------------------------------------------
#---** ESTADISTICAS EDAD CAMPEONES **
#---------------------------------------------------------------------

moda <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Calcular las estadísticas descriptivas incluyendo la moda
stats_edad_campeones <- data.frame(
  promedio = mean(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  desviacion_estandar = sd(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  coef_variacion = sd(DF_CAMPEONES$edad_Player, na.rm = TRUE) / mean(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  moda = moda(DF_CAMPEONES$edad_Player),
  Q1 = quantile(DF_CAMPEONES$edad_Player, 0.25, na.rm = TRUE),
  Q2 = quantile(DF_CAMPEONES$edad_Player, 0.50, na.rm = TRUE),  # Mediana
  Q3 = quantile(DF_CAMPEONES$edad_Player, 0.75, na.rm = TRUE),
  Q4 = quantile(DF_CAMPEONES$edad_Player, 1, na.rm = TRUE)      # Máximo
)

print(stats_edad_campeones)


# Calcular las estadísticas descriptivas incluyendo la moda
stats_edad_perdedores <- data.frame(
  promedio = mean(DF_ELIMINADOS$edad_Player, na.rm = TRUE),
  desviacion_estandar = sd(DF_ELIMINADOS$edad_Player, na.rm = TRUE),
  coef_variacion = sd(DF_ELIMINADOS$edad_Player, na.rm = TRUE) / mean(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  moda = moda(DF_ELIMINADOS$edad_Player),
  Q1 = quantile(DF_ELIMINADOS$edad_Player, 0.25, na.rm = TRUE),
  Q2 = quantile(DF_ELIMINADOS$edad_Player, 0.50, na.rm = TRUE),  # Mediana
  Q3 = quantile(DF_ELIMINADOS$edad_Player, 0.75, na.rm = TRUE),
  Q4 = quantile(DF_ELIMINADOS$edad_Player, 1, na.rm = TRUE)      # Máximo
)

print(stats_edad_perdedores)


# Calcular las estadísticas descriptivas incluyendo la moda
stats_edad_semis <- data.frame(
  promedio = mean(DF_SUB_SEMIS$edad_Player, na.rm = TRUE),
  desviacion_estandar = sd(DF_SUB_SEMIS$edad_Player, na.rm = TRUE),
  coef_variacion = sd(DF_SUB_SEMIS$edad_Player, na.rm = TRUE) / mean(DF_CAMPEONES$edad_Player, na.rm = TRUE),
  moda = moda(DF_SUB_SEMIS$edad_Player),
  Q1 = quantile(DF_SUB_SEMIS$edad_Player, 0.25, na.rm = TRUE),
  Q2 = quantile(DF_SUB_SEMIS$edad_Player, 0.50, na.rm = TRUE),  # Mediana
  Q3 = quantile(DF_SUB_SEMIS$edad_Player, 0.75, na.rm = TRUE),
  Q4 = quantile(DF_SUB_SEMIS$edad_Player, 1, na.rm = TRUE)      # Máximo
)

print(stats_edad_semis)


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

#---------------------------------------------------------------------
#----------[ANALISIS EN BASE A LA EXPERIENCIA]-----------
#---------------------------------------------------------------------


#----------EXPORT TO EXCEL---------------------------------------------
ruta_guardado_selecciones <- "test_analisis.xlsx"
write_xlsx(DF_MUNDIALES, ruta_guardado_selecciones)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado_selecciones, "\n")