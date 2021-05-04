
# url  https://kc.kobotoolbox.org/api/v1/
library(tidyverse)
library(koboloadeR)
library(leaflet)

#install_github("mrdwab/koboloadeR")

kobo_datasets("flisol_2021:flisol_2021") # Ver los proyectos

# kobo_data_downloader("codigo", "usuario:contraseña")
kobo_data_downloader("650683", "flisol_2021:flisol_2021") # Importat el proyecto

# Ejecuta una Shiny app para cargar los datos
#kobo_apps("app_name") 


# creamos un data frame con las variables de interes y modificamos algunos nombres

DF <- data_650683 %>% 
  select(ultima_compra, nombre, edad,sexo, gusto_flisol, "_ubicacion_latitude",
         "_ubicacion_longitude") %>% 
  rename(latitud = "_ubicacion_latitude" , longitud = "_ubicacion_longitude")


# Mapa interactivo --------------------------------------------------------------------


leaflet() %>% addTiles() %>% addCircleMarkers(data = DF, lng = DF$longitud, lat = DF$latitud, 
                                        label = DF$nombre,
                                        clusterOptions = markerClusterOptions()) %>% addMiniMap(
                                          tiles = providers$Esri.WorldStreetMap,
                                          toggleDisplay = TRUE)


# Graficos ----------------------------------------------------------------

#Grafico de piramide de poblacion

# Transformamos los datos

hombres <- DF %>% select(edad, sexo) %>%  
  filter(sexo == "Hombre") %>% 
  mutate(Grupos = case_when(edad >= 0 & edad <=10 ~ "0-10",
   edad >= 11 & edad <=20 ~ "11-20",
   edad >= 21 & edad <=40 ~ "21-30",
   edad >= 31 & edad <=40 ~ "31-40",
   edad >= 41 & edad <=50 ~ "41-50",
   edad >= 51 & edad <=60 ~ "51-60",
   edad >= 61 & edad <=70 ~ "61-70",
   edad >= 71 & edad <=80 ~ "71-80",
   edad >= 81 & edad <=90 ~ "81-90",
   edad >= 91 & edad <=100 ~ "91-100")) %>% 
  group_by(Grupos) %>% 
  summarise(numero = n()) %>% 
  mutate(porcentaje = round((numero/sum(numero))*100,2)*-1) %>% 
  mutate(sexo = "Hombres")

mujer <- DF %>% select(edad, sexo) %>% 
  filter(sexo == "Mujer") %>% 
  mutate(Grupos = case_when(edad >= 0 & edad <=10 ~ "0-10",
   edad >= 11 & edad <=20 ~ "11-20",
   edad >= 21 & edad <=40 ~ "21-30",
   edad >= 31 & edad <=40 ~ "31-40",
   edad >= 41 & edad <=50 ~ "41-50",
   edad >= 51 & edad <=60 ~ "51-60",
   edad >= 61 & edad <=70 ~ "61-70",
   edad >= 71 & edad <=80 ~ "71-80",
   edad >= 81 & edad <=90 ~ "81-90",
   edad >= 91 & edad <=100 ~ "91-100")) %>% 
  group_by(Grupos) %>% 
  summarise(numero = n()) %>% 
  mutate(porcentaje = round((numero/sum(numero))*100,2)) %>% 
  mutate(sexo = "Mujeres")

#Los juntamos
comunidad <- bind_rows(hombres, mujer)


# Graficas ----------------------------------------------------------------

ggplot(data = comunidad,
    aes(x = Grupos,
        y = porcentaje, fill = sexo)
  ) +
  geom_col(position = "stack", alpha = 0.6, color = "white") + 
  coord_flip() +
  scale_fill_manual(values = c("midnightblue", "darkred")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)) +
  scale_y_continuous(breaks = seq(-100, 100, by = 10), 
                     labels = c(rev(seq(0, 100, by = 10)), seq(10, 100, by = 10))) +
  labs(
    y = "Porcentaje",
    x = "Grupos de edades",
    title = "Población asistente")


ggplot(DF) + geom_boxplot(aes(gusto_flisol, edad, fill =sexo )) + 
  labs(x="Gusto a Flisol", y = "Edades", title = "Diagrama de caja") + theme_bw() +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))







