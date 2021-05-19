
# url  https://kc.kobotoolbox.org/api/v1/
library(tidyverse)
library(koboloadeR)
library(raster)
library(leaflet)
library(sf)
library(osmdata)
library(rworldxtra)
library(cowplot)
library(ggspatial)
library(tmap)

# instalar desde GitHub
#devtools::install_github("mrdwab/koboloadeR")

kobo_datasets("flisol_2021:flisol_2021") # Ver los proyectos

# kobo_data_downloader("codigo", "usuario:contraseña")
kobo_data_downloader(formid = 650683, "flisol_2021:flisol_2021", check = F) # Importat el proyecto

# Ejecuta una Shiny app para cargar los datos
#kobo_apps("app_name") 


# creamos un data frame con las variables de interes y modificamos algunos nombres

DF <- data_650683 %>% 
  dplyr::select(ultima_compra, nombre, edad,sexo, gusto_flisol, "_ubicacion_latitude",
         "_ubicacion_longitude") %>% 
  rename(latitud = "_ubicacion_latitude" , longitud = "_ubicacion_longitude")


# Mapa interactivo --------------------------------------------------------------------


leaflet() %>% addTiles() %>% addCircleMarkers(data = DF, lng = DF$longitud, lat = DF$latitud, 
                                        label = DF$nombre,
                                        clusterOptions = markerClusterOptions()) %>% addMiniMap(
                                          tiles = providers$Esri.WorldStreetMap,
                                          toggleDisplay = TRUE)
# Obteniendo mapa de colombia
data("countriesHigh")
Mundo <- st_as_sf(countriesHigh)
colombia <- Mundo %>%  dplyr::select(ne_10m_adm) %>% filter(ne_10m_adm=="COL")

#Mapa de costa caribe
departamentos <- getData('GADM', country='COL', level=1)
departamentos <- st_as_sf(departamentos)
caribe <- departamentos %>% dplyr::filter(NAME_1 == "Atlántico" | NAME_1 == "Bolívar" | 
                                            NAME_1 == "Magdalena" | NAME_1 == "Sucre" |
                                            NAME_1 ==  "Córdoba" | NAME_1 == "Cesar" |
                                            NAME_1 == "La Guajira")


puntos <- st_as_sf(DF, coords = c(7,6), crs = "+proj=longlat +datum=WGS84")

box <- st_as_sfc(st_bbox(caribe))

# Macrolocalizacion

mapa_1 <- ggplot() + geom_sf(data = colombia, fill = "white") + 
  geom_sf(data = box, fill = NA, color= "red") + 
  ggtitle("Macrolocalizacion") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     plot.title = element_text(hjust = 0.5, size = 7))

#Mapa region caribe
mapa_2 <- ggplot()  +
  geom_sf(data = caribe) +
  geom_sf(data = puntos, aes(color = sexo)) + 
  annotation_north_arrow(location ="tl",
                         which_north ="true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "br") + 
  scale_color_manual(values = c("blue", "red")) +  theme_bw() + xlab(NULL) + ylab(NULL)

# Juntamos los mapas
mapa_final <- ggdraw() + 
  draw_plot(mapa_2) +
  draw_plot(mapa_1, x=0.5, y= 0.17, width=0.3, height=0.28)


# Graficos ----------------------------------------------------------------

#Grafico de piramide de poblacion

# Transformamos los datos

hombres <- DF %>% dplyr::select(edad, sexo) %>%  
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

mujer <- DF %>% dplyr::select(edad, sexo) %>% 
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


