##EDA DE LA BASE Players_Mundial_RAWdata.xlsx

library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(writexl)
library(ggplot2)

#----------DATA MUNDIAL FINAL---------------------------------------------
MUNDIALES_DF <- "Players_Mundial_RAWdata.xlsx"
DF_MUNDIALES <- read_excel(MUNDIALES_DF)

colnames(DF_MUNDIALES)

# Reemplazar los valores vacíos en edad_Player con el promedio de la columna
DF_MUNDIALES <- DF_MUNDIALES %>%
  mutate(edad_Player = if_else(is.na(edad_Player), 
                               mean(edad_Player, na.rm = TRUE), 
                               edad_Player))

# Calcular los cuartiles
q1 <- quantile(DF_MUNDIALES$edad_Player, 0.25, na.rm = TRUE)  # Cuartil 1 (25%)
mediana_edad <- quantile(DF_MUNDIALES$edad_Player, 0.50, na.rm = TRUE)  # Mediana (50%)
q3 <- quantile(DF_MUNDIALES$edad_Player, 0.75, na.rm = TRUE)  # Cuartil 3 (75%)

# Crear la curva de densidad con cuartiles marcados
ggplot(DF_MUNDIALES, aes(x = edad_Player)) +
  geom_density(fill = "blue", alpha = 0.4, color = "black") +  # Curva de densidad
  geom_vline(xintercept = q1, color = "purple", linetype = "dashed", size = 1) +  # Línea Q1
  geom_vline(xintercept = mediana_edad, color = "red", linetype = "dashed", size = 1) +  # Línea Mediana (Q2)
  geom_vline(xintercept = q3, color = "purple", linetype = "dashed", size = 1) +  # Línea Q3
  theme_minimal() +
  labs(title = "Distribución General de Edad con Cuartiles",
       x = "Edad del Jugador",
       y = "Densidad") +
  annotate("text", x = q1, y = 0.02, label = paste("Q1:", round(q1, 1)), color = "purple", angle = 90, vjust = -0.5, hjust = 1.2, size = 5) +  # Etiqueta Q1
  annotate("text", x = mediana_edad, y = 0.02, label = paste("Q2", round(mediana_edad, 1)), color = "red", angle = 90, vjust = -0.5, hjust = 1.2, size = 5) +  # Etiqueta Mediana (Q2)
  annotate("text", x = q3, y = 0.02, label = paste("Q3:", round(q3, 1)), color = "purple", angle = 90, vjust = -0.5, hjust = 1.2, size = 5) +  # Etiqueta Q3
  theme(plot.title = element_text(hjust = 0.5))  # Centrar el título


# Filtrar los datos de Brasil
DF_Brasil <- DF_MUNDIALES %>%
  filter(Selección == "Argentina")

# Crear la curva de densidad con la comparación
ggplot() +
  # Curva de densidad para la distribución general
  geom_density(data = DF_MUNDIALES, aes(x = edad_Player), fill = "blue", alpha = 0.4, color = "black") +
  # Curva de densidad para Brasil
  geom_density(data = DF_Brasil, aes(x = edad_Player), fill = "green", alpha = 0.4, color = "darkgreen") +
  # Líneas de los cuartiles de la distribución general
  geom_vline(xintercept = q1, color = "purple", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = mediana_edad, color = "red", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = q3, color = "purple", linetype = "dashed", size = 1) +  
  # Etiquetas de los cuartiles
  annotate("text", x = q1, y = 0.02, label = paste("Q1:", round(q1, 1)), color = "purple", angle = 90, vjust = -0.5, hjust = 1.2, size = 5) +  
  annotate("text", x = mediana_edad, y = 0.02, label = paste("Q2", round(mediana_edad, 1)), color = "red", angle = 90, vjust = -0.5, hjust = 1.2, size = 5) +  
  annotate("text", x = q3, y = 0.02, label = paste("Q3:", round(q3, 1)), color = "purple", angle = 90, vjust = -0.5, hjust = 1.2, size = 5) +  
  # Personalización del gráfico
  theme_minimal() +
  labs(title = "Comparación de Distribución de Edad: General vs Brasil",
       x = "Edad del Jugador",
       y = "Densidad") +
  theme(plot.title = element_text(hjust = 0.5))  # Centrar el título



#----------DATA PUESTOS 1---------------------------------------------
# Filtrar los datos para obtener solo los puestos 1
DF_MUNDIALES_champs <- DF_MUNDIALES %>%
  filter(Puesto_obtenido == 1)

# Verificar el resultado
print(DF_MUNDIALES_champs)

colnames(DF_MUNDIALES_champs)

#----------CREAR avg_LocaliPaises (Resumen por Selección)------------------
avg_LocaliPaises <- DF_MUNDIALES_champs %>%
  group_by(Selección) %>%
  summarise(
    promedio_edad = mean(edad_Player, na.rm = TRUE),
    mediana_edad = median(edad_Player, na.rm = TRUE),
    cuartil_25 = quantile(edad_Player, 0.25, na.rm = TRUE),
    cuartil_75 = quantile(edad_Player, 0.75, na.rm = TRUE)
  )

# Verificar el resultado
print(avg_LocaliPaises)


# Crear box plot por Selección
ggplot(DF_MUNDIALES_champs, aes(x = Selección, y = edad_Player)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribución de Edad por Selección",
       x = "Selección",
       y = "Edad del Jugador") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar nombres de selecciones

# Crear gráfico de densidad para cada selección
ggplot(DF_MUNDIALES_champs, aes(x = edad_Player, color = Selección, fill = Selección)) +
  geom_density(alpha = 0.3) +  # Curvas de densidad con transparencia
  theme_minimal() +
  labs(title = "Distribución de Edad por Selección (Curvas de Densidad)",
       x = "Edad del Jugador",
       y = "Densidad",
       color = "Selección",
       fill = "Selección") +
  theme(legend.position = "right")  # Posicionar la leyenda a la derecha


#----------EXPORT TO EXCEL---------------------------------------------
ruta_guardado_selecciones <- "test_analisis.xlsx"
write_xlsx(DF_MUNDIALES, ruta_guardado_selecciones)
# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado_selecciones, "\n")