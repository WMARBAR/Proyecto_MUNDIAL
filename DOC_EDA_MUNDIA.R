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
#******************************************************************
#**********[TRATAMIENTO DE LA BASE]********************************
#******************************************************************
#-----------------------------------------------------------------
#----------[CONTINENTES]------------------------------------------
#-----------------------------------------------------------------
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
#----------[COLUMNA ETAPA GENERAL]------------------------------------
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
#--------[DUMMYS, COLUMNA POSICION]-----------------------------------
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
  select(-max_goles)  
#---------------------------------------------------------------------
#--- [CATEGORIZACIÓN EDAD Y EXP]
#---------------------------------------------------------------------
#>>>>[EDAD]-----------------------------------------------------------
#---------------------------------------------------------------------
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
#---------------------------------------------------------------------
#>>>>[EXP]----------------------------------------------------------
#---------------------------------------------------------------------
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

#******************************************************************
#**********[GRAFICOS E INSIGTHS DE LA POBLACIÓN TOTAL]*************
#******************************************************************

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
#---** EDAD DE LOS JUGADORES por GRUPO (Boxplot)**--------------------
#---------------------------------------------------------------------

# Crear dataframes por grupo usando la edad del jugador
DF_PERDEDORES_JUGADORES <- DF_ELIMINADOS %>%
  mutate(grupo = "Perdedores") %>%
  filter(!is.na(edad_Player))

DF_CAMPEONES_JUGADORES <- DF_CAMPEONES %>% 
  mutate(grupo = "Campeones") %>%
  filter(!is.na(edad_Player))

DF_SUB_SEMIS_JUGADORES <- DF_SUB_SEMIS %>%
  mutate(grupo = "Subcampeones/Semis") %>%
  filter(!is.na(edad_Player))

# Unir todo
DF_BOXPLOT_JUGADORES <- bind_rows(DF_CAMPEONES_JUGADORES, 
                                  DF_PERDEDORES_JUGADORES, 
                                  DF_SUB_SEMIS_JUGADORES)

# Etiqueta para el gráfico
DF_BOXPLOT_JUGADORES <- DF_BOXPLOT_JUGADORES %>%
  mutate(grupo = ifelse(grupo == "Subcampeones/Semis", "Subcampeones\nSemis", grupo))

# Crear el boxplot de EDAD DE JUGADORES
boxplot_jugadores <- ggplot(DF_BOXPLOT_JUGADORES, aes(x = grupo, y = edad_Player, fill = grupo)) +
  geom_boxplot(color = "black", size = 1.9) +
  scale_fill_manual(values = c("Campeones" = "goldenrod1",
                               "Subcampeones\nSemis" = "lightskyblue",
                               "Perdedores" = "lightcoral")) +
  scale_x_discrete(expand = expansion(add = 0.5)) +   
  labs(title = "Edad de los Jugadores",
       x = "",
       y = "Edad del Jugador") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 40, hjust = 0.5, color = "black"),
    axis.text.x = element_text(size = 35, family = "Consolas", color = "black", margin = margin(t = 15)),
    axis.text.y = element_text(size = 35, family = "Consolas", color = "black"),
    axis.title.x = element_text(size = 30, family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 30, family = "Consolas", color = "black"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray50", size = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(boxplot_jugadores)

# Guardar el gráfico
ggsave(filename = "GRAPHS/boxplot_edades_golbal.png",
       plot = boxplot_jugadores,
       width = 12, height = 8, dpi = 300)



#---------------------------------------------------------------------
#---** DENSIDAD de EXPERIENCIA en Mundiales por GRUPO **--------------
#---------------------------------------------------------------------

densidad_experiencia <- ggplot() +
  # Densidad de Campeones
  geom_density(data = DF_CAMPEONES, aes(x = years_expMundial, weight = player_ones, color = "Campeones", fill = "Campeones"),
               alpha = 0.4, size = 1.2) +
  
  # Densidad de Perdedores
  geom_density(data = DF_ELIMINADOS, aes(x = years_expMundial, weight = player_ones, color = "Perdedores", fill = "Perdedores"),
               alpha = 0.4, size = 1.2) +
  
  # Densidad de Subcampeones/Semis
  geom_density(data = DF_SUB_SEMIS, aes(x = years_expMundial, weight = player_ones, color = "Subcampeones/Semis", fill = "Subcampeones/Semis"),
               alpha = 0.4, size = 1.2) +
  
  scale_fill_manual(values = c("Campeones" = "goldenrod1",
                               "Perdedores" = "lightcoral",
                               "Subcampeones/Semis" = "lightskyblue")) +
  scale_color_manual(values = c("Campeones" = "goldenrod3",
                                "Perdedores" = "darkred",
                                "Subcampeones/Semis" = "steelblue")) +
  
  labs(title = "Distribución de Años de Experiencia en Mundiales por Grupo",
       x = "Años de Experiencia",
       y = "Densidad",
       fill = "Grupo",
       color = "Grupo") +
  
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 22, hjust = 0.5),
    axis.text = element_text(size = 25, family = "Consolas"),
    axis.title = element_text(size = 25, family = "Consolas"),
    legend.title = element_text(family = "Consolas", size = 22, face = "bold"),
    legend.text = element_text(family = "Consolas", size = 20),
    legend.position = "top",
    legend.justification = "center"
  )

# Mostrar gráfico
print(densidad_experiencia)

# Guardar el gráfico como imagen PNG
ggsave(filename = "GRAPHS/dist_experiencia.png",
       plot = densidad_experiencia,
       width = 12, height = 8, dpi = 300)
#---------------------------------------------------------------------
#---** CURTOSIS sobre los AÑOS DE EXPERIENCIA por grupo **------------
#---------------------------------------------------------------------

library(e1071)

# Curtosis de los campeones
curtosis_campeones_exp <- kurtosis(DF_CAMPEONES$years_expMundial, na.rm = TRUE)

# Curtosis de los perdedores
curtosis_perdedores_exp <- kurtosis(DF_ELIMINADOS$years_expMundial, na.rm = TRUE)

# Curtosis de subcampeones/semis
curtosis_semis_exp <- kurtosis(DF_SUB_SEMIS$years_expMundial, na.rm = TRUE)

# Mostrar resultados
cat("Curtosis Campeones (Experiencia):", curtosis_campeones_exp, "\n")
cat("Curtosis Perdedores (Experiencia):", curtosis_perdedores_exp, "\n")
cat("Curtosis Subcampeones/Semis (Experiencia):", curtosis_semis_exp, "\n")

#---------------------------------------------------------------------
#---** DENSIDAD de la distribucion ganadores en edad por posicion **--------------
#---------------------------------------------------------------------

# 📌 Filtrar solo los campeones
DF_CAMP_POSICION <- DF_CAMPEONES %>%
  filter(!is.na(categ_posicion))  # Asegura que la posición no esté vacía

# 📌 Crear el gráfico de densidad por posición
densidad_edades_posicion <- ggplot(DF_CAMP_POSICION, aes(x = edad_Player, color = categ_posicion, fill = categ_posicion)) +
  geom_density(alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c("Arquero" = "goldenrod1",
                               "Defensa" = "lightcoral",
                               "Centrocampista" = "lightskyblue",
                               "Delantero" = "seagreen4")) +
  scale_color_manual(values = c("Arquero" = "goldenrod3",
                                "Defensa" = "darkred",
                                "Centrocampista" = "steelblue",
                                "Delantero" = "darkgreen")) +
  labs(title = "Distribución de Edad por Posición\n(Jugadores Campeones)",
       x = "Edad de los Jugadores",
       y = "Densidad",
       fill = "Posición",
       color = "Posición") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 30, hjust = 0.5),
    axis.text = element_text(size = 20, family = "Consolas"),
    axis.title = element_text(size = 22, family = "Consolas"),
    legend.title = element_text(family = "Consolas", size = 22, face = "bold"),
    legend.text = element_text(family = "Consolas", size = 20),
    legend.position = "top",
    legend.justification = "center"
  )

# 📌 Mostrar el gráfico
print(densidad_edades_posicion)

# 📌 Guardar como imagen
ggsave(filename = "GRAPHS/densidad_edad_campeones_por_posicion.png",
       plot = densidad_edades_posicion,
       width = 12, height = 8, dpi = 300)

#---------------------------------------------------------------------
#---** DENSIDAD de la distribucion perdedores en edad por posicion **--------------
#---------------------------------------------------------------------
# 📌 Filtrar solo jugadores perdedores CON posición definida
DF_PERDEDOR_POSICION <- DF_ELIMINADOS %>%
  filter(!is.na(categ_posicion), categ_posicion %in% c("Arquero", "Defensa", "Centrocampista", "Delantero"))

# 📌 Gráfico de densidad SOLO por posiciones de Perdedores
densidad_edades_perdedores <- ggplot(DF_PERDEDOR_POSICION, aes(x = edad_Player, color = categ_posicion, fill = categ_posicion)) +
  geom_density(alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c("Arquero" = "goldenrod1",
                               "Defensa" = "lightcoral",
                               "Centrocampista" = "lightskyblue",
                               "Delantero" = "seagreen4")) +
  scale_color_manual(values = c("Arquero" = "goldenrod3",
                                "Defensa" = "darkred",
                                "Centrocampista" = "steelblue",
                                "Delantero" = "darkgreen")) +
  labs(title = "Distribución de Edad por Posición\n(Jugadores Perdedores)",
       x = "Edad de los Jugadores",
       y = "Densidad",
       fill = "Posición",
       color = "Posición") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 30, hjust = 0.5),
    axis.text = element_text(size = 22, family = "Consolas"),
    axis.title = element_text(size = 24, family = "Consolas"),
    legend.title = element_text(family = "Consolas", size = 22, face = "bold"),
    legend.text = element_text(family = "Consolas", size = 20),
    legend.position = "top",
    legend.justification = "center"
  )

print(densidad_edades_perdedores)

# 📌 Exportar la gráfica limpia y sin grises
ggsave(filename = "GRAPHS/densidad_edad_perdedores_por_posicion.png",
       plot = densidad_edades_perdedores,
       width = 12, height = 8, dpi = 300)
#----------------------------------------------------------------------
# SUBPLOTS
#-----------------------------------------------------------------------
library(ggpubr)

# Reajustamos las leyendas para que estén a la izquierda en ambos gráficos
densidad_edades_posicion <- densidad_edades_posicion +
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(family = "Consolas", size = 20, face = "bold"),
        legend.text = element_text(family = "Consolas", size = 18))

densidad_edades_perdedores <- densidad_edades_perdedores +
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(family = "Consolas", size = 20, face = "bold"),
        legend.text = element_text(family = "Consolas", size = 18))

# 🔥 Combina los gráficos en vertical con leyenda alineada a la izquierda
grafico_combinado <- ggarrange(
  densidad_edades_posicion,
  densidad_edades_perdedores,
  ncol = 1,            # Vertical
  nrow = 2,
  heights = c(1, 1),
  align = "v",
  labels = c("A", "B"),
  font.label = list(family = "Consolas", size = 18, face = "bold"),
  common.legend = TRUE,
  legend = "right"      # La leyenda se pone a la izquierda, en vertical
)

# 📌 Mostrar gráfico
print(grafico_combinado)

# 📌 Guardar como imagen
ggsave("GRAPHS/densidad_edad_pos_vs_perdedores.png",
       plot = grafico_combinado,
       width = 14, height = 12, dpi = 300)


#---------------------------------------------------------------------
# PORCENTAJE JUGADORES GANADORES (toda la base)
#---------------------------------------------------------------------
DF_PORCENTAJE <- DF_MUNDIALES %>%
  mutate(categoria = if_else(dummy_campeon == 1, "Campeones", "No Campeones")) %>%
  group_by(categoria) %>%
  summarise(Jugadores = n()) %>%
  mutate(Porcentaje = round((Jugadores / sum(Jugadores)) * 100, 1))

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



#******************************************************************
#**********[GRAFICOS E INSIGTHS DE LOS ANOTADORES]*****************
#******************************************************************

#---------------------------------------------------------------------
# INTERVALO DE EDAD DE LOS ANOTADORES---------------------------------
#---------------------------------------------------------------------

total_anotadores <- sum(DF_ANOTADORES$player_ones, na.rm = TRUE)

hist_edad_categoria <- ggplot(DF_ANOTADORES, aes(x = edad_categoria, weight = player_ones)) +
  geom_bar(fill = "seagreen4", color = "black", size = 1) +
  labs(title = "Anotadores por Edad",
       x = "Edad (intervalo)",
       y = "Anotadores") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / total_anotadores * 100, name = "Porcentaje (%)")
  ) +
  theme_minimal(base_family = "Consolas") +  # 🔥 Consolas como base
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 23, hjust = 0.5, color = "black"),
    
    # 🔥 Tamaño +3 y todo negro
    axis.text.x = element_text(size = 23, angle = 45, hjust = 1, color = "black", family = "Consolas"),
    axis.text.y = element_text(size = 23, color = "black", family = "Consolas"),
    axis.text.y.right = element_text(size = 23, color = "black", family = "Consolas"),  # Secundario también
    
    # 🔥 Ejes en negrita y negro
    axis.title.x = element_text(size = 23, face = "bold", color = "black", family = "Consolas"),
    axis.title.y = element_text(size = 23, face = "bold", color = "black", family = "Consolas"),
    axis.title.y.right = element_text(size = 23, face = "bold", color = "black", family = "Consolas"),
    
    panel.grid.major.y = element_line(color = "gray40", size = 1, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

hist_edad_categoria


ggsave(filename = "GRAPHS/hist_edad_anotadores.png",
       plot = hist_edad_categoria,
       width = 12, height = 8, dpi = 300)

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

# Modificar la etiqueta solo para la gráfica
DF_BOXPLOT <- DF_BOXPLOT %>%
  mutate(grupo = ifelse(grupo == "Subcampeones/Semis", "Subcampeones\nSemis", grupo))

boxplot_completo <- ggplot(DF_BOXPLOT, aes(x = grupo, y = edad_Player, fill = grupo)) +
  geom_boxplot(color = "black", size = 1.9) +
  scale_fill_manual(values = c("Campeones" = "goldenrod1",
                               "Subcampeones\nSemis" = "lightskyblue",
                               "Perdedores" = "lightcoral")) +
  scale_x_discrete(expand = expansion(add = 0.5)) +   
  labs(title = "Edad de anotadores\nCampeones, Perdedores y\nSubcampeones/Semis",
       x = "",
       y = "Edad") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 40, hjust = 0.5, color = "black"),
    axis.text.x = element_text(size = 35, family = "Consolas", color = "black", margin = margin(t = 15)),
    axis.text.y = element_text(size = 35, family = "Consolas", color = "black"),
    axis.title.x = element_text(size = 30, family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 30, family = "Consolas", color = "black"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray50", size = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(boxplot_completo)


ggsave(filename = "GRAPHS/boxplot_comparativo.png",
       plot = boxplot_completo,
       width = 12, height = 8, dpi = 300)
#---------------------------------------------------------------------
#---** EDAD DE LOS TECNICOS**--------------------------------------
#----------------------------------------------------------------
# Crear dataframes por grupo usando edad del técnico (edad_enMundial)
DF_PERDEDORES_TECNICOS <- DF_ELIMINADOS %>%
  mutate(grupo = "Perdedores") %>%
  filter(!is.na(edad_enMundial))

DF_CAMPEONES_TECNICOS <- DF_CAMPEONES %>% 
  mutate(grupo = "Campeones") %>%
  filter(!is.na(edad_enMundial))

DF_SUB_SEMIS_TECNICOS <- DF_SUB_SEMIS %>%
  mutate(grupo = "Subcampeones/Semis") %>%
  filter(!is.na(edad_enMundial))

# Unir todo
DF_BOXPLOT_TECNICOS <- bind_rows(DF_CAMPEONES_TECNICOS, 
                                 DF_PERDEDORES_TECNICOS, 
                                 DF_SUB_SEMIS_TECNICOS)

# Etiqueta para el gráfico
DF_BOXPLOT_TECNICOS <- DF_BOXPLOT_TECNICOS %>%
  mutate(grupo = ifelse(grupo == "Subcampeones/Semis", "Subcampeones\nSemis", grupo))

# Crear el boxplot
boxplot_tecnicos <- ggplot(DF_BOXPLOT_TECNICOS, aes(x = grupo, y = edad_enMundial, fill = grupo)) +
  geom_boxplot(color = "black", size = 1.9) +
  scale_fill_manual(values = c("Campeones" = "goldenrod1",
                               "Subcampeones\nSemis" = "lightskyblue",
                               "Perdedores" = "lightcoral")) +
  scale_x_discrete(expand = expansion(add = 0.5)) +   
  labs(title = "Edad de los Técnicos",
       x = "",
       y = "Edad del Técnico en el Mundial") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 40, hjust = 0.5, color = "black"),
    axis.text.x = element_text(size = 35, family = "Consolas", color = "black", margin = margin(t = 15)),
    axis.text.y = element_text(size = 35, family = "Consolas", color = "black"),
    axis.title.x = element_text(size = 30, family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 30, family = "Consolas", color = "black"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray50", size = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(boxplot_tecnicos)

# Guardar el gráfico
ggsave(filename = "GRAPHS/boxplot_tecnicos.png",
       plot = boxplot_tecnicos,
       width = 12, height = 8, dpi = 300)


#---------------------------------------------------------------------
#---** exp DE LOS TECNICOS**--------------------------------------
#----------------------------------------------------------------
# Crear dataframes por grupo usando edad del técnico (edad_enMundial)
DF_PERDEDORES_TECNICOS <- DF_ELIMINADOS %>%
  mutate(grupo = "Perdedores") %>%
  filter(!is.na(edad_enMundial))

DF_CAMPEONES_TECNICOS <- DF_CAMPEONES %>% 
  mutate(grupo = "Campeones") %>%
  filter(!is.na(edad_enMundial))

DF_SUB_SEMIS_TECNICOS <- DF_SUB_SEMIS %>%
  mutate(grupo = "Subcampeones/Semis") %>%
  filter(!is.na(edad_enMundial))

# Unir todo
DF_BOXPLOT_TECNICOS <- bind_rows(DF_CAMPEONES_TECNICOS, 
                                 DF_PERDEDORES_TECNICOS, 
                                 DF_SUB_SEMIS_TECNICOS)

# Etiqueta para el gráfico
DF_BOXPLOT_TECNICOS <- DF_BOXPLOT_TECNICOS %>%
  mutate(grupo = ifelse(grupo == "Subcampeones/Semis", "Subcampeones\nSemis", grupo))

# Crear el boxplot
boxplot_tecnicos <- ggplot(DF_BOXPLOT_TECNICOS, aes(x = grupo, y = edad_enMundial, fill = grupo)) +
  geom_boxplot(color = "black", size = 1.9) +
  scale_fill_manual(values = c("Campeones" = "goldenrod1",
                               "Subcampeones\nSemis" = "lightskyblue",
                               "Perdedores" = "lightcoral")) +
  scale_x_discrete(expand = expansion(add = 0.5)) +   
  labs(title = "Edad de los Técnicos",
       x = "",
       y = "Edad del Técnico en el Mundial") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 40, hjust = 0.5, color = "black"),
    axis.text.x = element_text(size = 35, family = "Consolas", color = "black", margin = margin(t = 15)),
    axis.text.y = element_text(size = 35, family = "Consolas", color = "black"),
    axis.title.x = element_text(size = 30, family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 30, family = "Consolas", color = "black"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray50", size = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(boxplot_tecnicos)

# Guardar el gráfico
ggsave(filename = "GRAPHS/boxplot_tecnicos.png",
       plot = boxplot_tecnicos,
       width = 12, height = 8, dpi = 300)

#---------------------------------------------------------------------
#---** EXP ANOTADORES boxplot**------------------------------------------------
#---------------------------------------------------------------------

# Agrupar anotadores por grupo
DF_PERDEDORES_ANOTADORES <- DF_ELIMINADOS %>%
  filter(player_anotador == 1) %>%
  mutate(grupo = "Perdedores")

DF_CAMPEONES_ANOTADORES <- DF_CAMPEONES %>% 
  filter(player_anotador == 1) %>%
  mutate(grupo = "Campeones")

DF_SUB_SEMIS_ANOTADORES <- DF_SUB_SEMIS %>%
  filter(player_anotador == 1) %>%
  mutate(grupo = "Subcampeones/Semis")

# Unir los tres grupos
DF_BOXPLOT_EXP <- bind_rows(DF_CAMPEONES_ANOTADORES, 
                            DF_PERDEDORES_ANOTADORES, 
                            DF_SUB_SEMIS_ANOTADORES)

# Modificar la etiqueta solo para la gráfica
DF_BOXPLOT_EXP <- DF_BOXPLOT_EXP %>%
  mutate(grupo = ifelse(grupo == "Subcampeones/Semis", "Subcampeones\nSemis", grupo))

# Crear el boxplot de experiencia
boxplot_exp <- ggplot(DF_BOXPLOT_EXP, aes(x = grupo, y = years_expMundial, fill = grupo)) +
  geom_boxplot(color = "black", size = 1.9) +
  scale_fill_manual(values = c("Campeones" = "goldenrod1",
                               "Subcampeones\nSemis" = "lightskyblue",
                               "Perdedores" = "lightcoral")) +
  scale_x_discrete(expand = expansion(add = 0.5)) +   
  labs(title = "Experiencia de los Anotadores\nCampeones, Perdedores y\nSubcampeones/Semis",
       x = "",
       y = "Años de Experiencia") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 40, hjust = 0.5, color = "black"),
    axis.text.x = element_text(size = 35, family = "Consolas", color = "black", margin = margin(t = 15)),
    axis.text.y = element_text(size = 35, family = "Consolas", color = "black"),
    axis.title.x = element_text(size = 30, family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 30, family = "Consolas", color = "black"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray50", size = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Mostrar el gráfico
print(boxplot_exp)

# Guardar como imagen
ggsave(filename = "GRAPHS/boxplot_experiencia.png",
       plot = boxplot_exp,
       width = 12, height = 8, dpi = 300)


#---------------------------------------------------------------------
#---** ANOTADORES % por POSICIONES COMPARANDO CAMPEONES---------------
#---------------------------------------------------------------------
DF_BARRA_CATEG <- DF_MUNDIALES %>%
  filter(player_anotador == 1) %>%
  mutate(grupo = case_when(
    dummy_campeon == 1 ~ "Campeones",
    dummy_subcampeon == 1 | dummy_semifinalista == 1 ~ "Subcampeones/Semis",
    TRUE ~ "Perdedores"
  )) %>%
  group_by(grupo, categ_posicion) %>%
  summarise(goles = sum(`Goles Marcados(mundial)`, na.rm = TRUE), .groups = "drop") %>%
  group_by(grupo) %>%
  mutate(porcentaje = round((goles / sum(goles)) * 100, 2)) %>%
  ungroup()



# Reordenar las categorías de mayor a menor según el porcentaje
DF_BARRA_CATEG <- DF_BARRA_CATEG %>%
  group_by(grupo) %>%
  arrange(desc(porcentaje), .by_group = TRUE) %>%
  ungroup()

# Reordenar el eje x según el promedio de porcentaje por posición
DF_BARRA_CATEG <- DF_BARRA_CATEG %>%
  mutate(categ_posicion = reorder(categ_posicion, -porcentaje))

grafico_barras_categ <- ggplot(DF_BARRA_CATEG, aes(x = categ_posicion, y = porcentaje, fill = grupo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), color = "black", size = 0.8) +
  
  # 🔥 Líneas horizontales de referencia (10%, 20%, 30%, 40%)
  geom_hline(yintercept = seq(10, 40, by = 10), 
             color = "black", 
             linetype = "dotted", 
             size = 0.8) +
  
  scale_fill_manual(values = c("Campeones" = "goldenrod3",
                               "Perdedores" = "lightcoral",
                               "Subcampeones/Semis" = "steelblue")) +
  labs(title = "Porcentaje de Goles\n según la Posición en el Campo",
       x = "Posición",
       y = "Porcentaje (%)",
       fill = "Grupo") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 34, hjust = 0.5),
    axis.title.x = element_text(family = "Consolas", size = 28, face = "bold"),
    axis.title.y = element_text(family = "Consolas", size = 28, face = "bold"),
    axis.text.x = element_text(size = 28, angle = 45, hjust = 1, color = "black", family = "Consolas"),
    axis.text.y = element_text(size = 26, color = "black", family = "Consolas"),
    legend.title = element_text(family = "Consolas", size = 26),
    legend.text = element_text(family = "Consolas", size = 24),
    legend.position = "top",
    legend.justification = "center",
    panel.grid.major.y = element_blank(),  # 🔥 Eliminamos la grilla default
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
print(grafico_barras_categ)
# Guardar gráfico con alta resolución
ggsave(filename = "GRAPHS/P_goles_posicion.png",
       plot = grafico_barras_categ,
       width = 14, height = 9, dpi = 400)


#---------------------------------------------------------------------
# INTERVALO AÑOS DE EXPERIENCIA---------------------------------------
#---------------------------------------------------------------------
total_anotadores <- sum(DF_ANOTADORES$player_ones, na.rm = TRUE)

hist_exp_categoria <- ggplot(DF_ANOTADORES, aes(x = exp_categoria, weight = player_ones)) +
  geom_bar(fill = "#374080", color = "black", size = 1) +
  labs(title = "Anotadores por Experiencia",
       x = "Experiencia en Mundiales (intervalo de años)",
       y = "Anotadores") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / total_anotadores * 100, name = "Porcentaje (%)")
  ) +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 23, hjust = 0.5, color = "black"),
    
    axis.text.x = element_text(size = 23, angle = 45, hjust = 1, family = "Consolas", color = "black"),
    axis.text.y = element_text(size = 23, family = "Consolas", color = "black"),
    axis.text.y.right = element_text(size = 23, family = "Consolas", color = "black"),
    
    axis.title.x = element_text(size = 23, face = "bold", family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 23, face = "bold", family = "Consolas", color = "black"),
    axis.title.y.right = element_text(size = 23, face = "bold", family = "Consolas", color = "black"),
    
    # Líneas guía horizontales punteadas y negras
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
# CORRELACION: EDAD Vs Goles---------------------------------------
#---------------------------------------------------------------------
# Gráfico de correlación: Edad vs Goles Marcados (solo puntos púrpura con fuente Consolas)
grafico_correlacion <- ggplot(DF_ANOTADORES, aes(x = edad_Player, y = `Goles Marcados(mundial)`)) +
  geom_point(color = "purple4", size = 3, alpha = 0.6) +  # Puntos púrpura
  labs(title = "Relación entre Edad y Goles Marcados",
       x = "Edad del Jugador",
       y = "Goles Marcados (Mundial)") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 30, hjust = 0.5, color = "black"),
    axis.text.x = element_text(size = 22, family = "Consolas", color = "black"),
    axis.text.y = element_text(size = 22, family = "Consolas", color = "black"),
    axis.title.x = element_text(size = 26, face = "bold", family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 26, face = "bold", family = "Consolas", color = "black"),
    panel.grid.major.y = element_line(color = "gray70", size = 0.8, linetype = "dotted"),
    panel.grid.major.x = element_line(color = "gray70", size = 0.8, linetype = "dotted")
  )

# Mostrar gráfico
print(grafico_correlacion)

cor_test_kendall <- cor.test(DF_ANOTADORES$edad_Player, DF_ANOTADORES$`Goles Marcados(mundial)`, 
                             method = "kendall", use = "complete.obs")

print(cor_test_kendall)

# Guardar gráfico si quieres
ggsave(filename = "GRAPHS/correlacion_edad_goles.png",
       plot = grafico_correlacion,
       width = 12, height = 8, dpi = 300)



#---------------------------------------------------------------------
# CORRELACION: EXP Vs Goles---------------------------------------
#---------------------------------------------------------------------
# Gráfico de correlación: Años de experiencia vs Goles Marcados
grafico_correlacion_exp <- ggplot(DF_ANOTADORES, aes(x = years_expMundial, y = `Goles Marcados(mundial)`)) +
  geom_point(color = "darkred", size = 3, alpha = 0.6) +  # Puntos púrpura
  labs(title = "Relación entre\n Años de Experiencia y Goles Marcados",
       x = "Años de Experiencia en Mundiales",
       y = "Goles Marcados (Mundial)") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 30, hjust = 0.5, color = "black"),
    axis.text.x = element_text(size = 22, family = "Consolas", color = "black"),
    axis.text.y = element_text(size = 22, family = "Consolas", color = "black"),
    axis.title.x = element_text(size = 26, face = "bold", family = "Consolas", color = "black"),
    axis.title.y = element_text(size = 26, face = "bold", family = "Consolas", color = "black"),
    panel.grid.major.y = element_line(color = "gray70", size = 0.8, linetype = "dotted"),
    panel.grid.major.x = element_line(color = "gray70", size = 0.8, linetype = "dotted")
  )

# Mostrar gráfico
print(grafico_correlacion_exp)

cor.test(DF_ANOTADORES$years_expMundial, 
         DF_ANOTADORES$`Goles Marcados(mundial)`, 
         method = "spearman")

# Guardar el gráfico
ggsave(filename = "GRAPHS/correlacion_exp_goles.png",
       plot = grafico_correlacion_exp,
       width = 12, height = 8, dpi = 300)

#---------------------------------------------------------------------
#---** BATALLA POR CONTINENTE (CONSOLAS y tamaño +5) **
#---------------------------------------------------------------------


grafico_batalla_conts <-ggplot(DF_GRAFICO, aes(x = MYEAR, y = Titulos, color = Confederacion)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +  # Puntos más grandes
  scale_color_manual(values = c("UEFA" = "black", "CONMEBOL" = "#228B22")) +
  geom_hline(yintercept = seq(0, 12, 1), color = "gray80", linetype = "dotted") +  # Líneas punteadas horizontales
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 15, 1)) +  # Limita hasta 12
  labs(title = "Carrera Histórica\n de Títulos Mundiales\n por Continente",
       x = "Año del Mundial",
       y = "Títulos Acumulados",
       color = "Confederación") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(family = "Consolas", face = "bold", size = 35, hjust = 0.5, color = "black"),
    axis.title.x = element_text(family = "Consolas", size = 30, face = "bold", color = "black"),
    axis.title.y = element_text(family = "Consolas", size = 30, face = "bold", color = "black"),
    axis.text.x = element_text(size = 30, family = "Consolas", color = "black"),
    axis.text.y = element_text(size = 30, family = "Consolas", color = "black"),
    legend.title = element_text(family = "Consolas", size = 30, face = "bold", color = "black"),
    legend.text = element_text(family = "Consolas", size = 30, color = "black"),
    legend.position = "top",
    legend.justification = "center"
  )


# Guardar el gráfico
ggsave(filename = "GRAPHS/Batalla_continents.png",
       plot = grafico_batalla_conts,
       width = 12, height = 8, dpi = 300)


#---------------------------------------------------------------------
#---** HISTORICO EDAD PROMEDIO CAMPEONES **
#---------------------------------------------------------------------
DF_EDAD_CAMP <- DF_CAMPEONES %>%
  group_by(MYEAR) %>%
  summarise(Edad_Promedio = mean(edad_Player, na.rm = TRUE))

grafico_edad_avg <- ggplot(DF_EDAD_CAMP, aes(x = MYEAR, y = Edad_Promedio)) +
  geom_line(color = "purple4", size = 1.8) +
  geom_point(size = 4, color = "purple4") +
  labs(title = "Edad Promedio \n Jugadores Campeones por Mundial",
       x = "Año del Mundial",
       y = "Edad Promedio") +
  theme_minimal(base_family = "Consolas") +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 26)
  )
grafico_edad_avg
# Guardar el gráfico
ggsave(filename = "GRAPHS/campeones_edad_avg.png",
       plot = grafico_edad_avg ,
       width = 12, height = 8, dpi = 300)

