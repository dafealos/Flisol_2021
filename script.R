library(tidyverse)
library(koboloadeR)
library(leaflet)
# url  https://kc.kobotoolbox.org/api/v1/

datos <- read.csv("https://kc.kobotoolbox.org/api/v1/data/649050?format=csv")

datos <- curl::curl_download(url = "https://kc.kobotoolbox.org/api/v1/data/649050?format=csv", destfile = "datos.csv")

install_github("mrdwab/koboloadeR")

kobo_datasets("flisol_2021:flisol_2021") # Ver los proyectos

kobo_submission_count("647865") # SAber el numero de observaciones subidos

kobo_data_downloader("649081", "flisol_2021:flisol_2021") # Importat el proyecto
kobo_apps("app_name") # Ejecuta una Shiny app para cargar los datos


# creamos un data frame con las variables de interes y modificamos algunos nombres
DF <- data_649081 %>% 
  select(ultima_compra, nombre, edad,sexo, gusto_flisol, "_ubicacion_latitude", "_ubicacion_longitude") %>% 
  rename(latitud = "_ubicacion_latitude" , longitud = "_ubicacion_longitude")

# Mapa interactivo
leaflet() %>% addTiles() %>% addCircleMarkers(data = DF, lng = DF$longitud, lat = DF$latitud, 
                                        label = DF$nombre,
                                        clusterOptions = markerClusterOptions()) %>% addMiniMap(
                                          tiles = providers$Esri.WorldStreetMap,
                                          toggleDisplay = TRUE)


grafico
class(comunidad$sexo)
comunidad <- DF %>% mutate(Grupos = case_when(edad >= 0 & edad <=10 ~ "0-10",
                                     edad >= 11 & edad <=20 ~ "11-20",
                                     edad >= 21 & edad <=40 ~ "21-30",
                                     edad >= 31 & edad <=40 ~ "31-40",
                                     edad >= 41 & edad <=50 ~ "41-50",
                                     edad >= 51 & edad <=60 ~ "51-60",
                                     edad >= 61 & edad <=70 ~ "61-70",
                                     edad >= 71 & edad <=80 ~ "71-80",
                                     edad >= 81 & edad <=90 ~ "81-90",
                                     edad >= 91 & edad <=100 ~ "91-100"
                                     )) %>% group_by(Grupos, sexo) %>% summarise(n = n()) 


ggplot(comunidad, aes(x = Grupos,
                       y = n,
                       fill = sexo)) +
  # Seccion de HOMBRES
  geom_col(data = subset(comunidad, sexo == "Hombre") %>% 
             # Convertimos los datos de los Hombres en negativos
             mutate(`n` = -`n`),
           width = 0.5, fill = "blue") +
  # Seccion de MUJERES
  geom_col(width = 0.5, fill = "pink") + 
  # Cambio de ejes de coordenadas
  coord_flip() + 
  scale_y_continuous(
    breaks = c(seq(-100, -5, by = 5), 
               seq(0, 100, by = 5)),
    labels = c(seq(-100, -5, by = 5) * -1, 
               seq(0, 100, by = 5)))

ggplot(DF) + geom_boxplot(aes(gusto_flisol, edad, fill = gusto_flisol)) + theme_bw() + 
  scale_fill_brewer(type = "qual", name= "Les gusta flisol")




