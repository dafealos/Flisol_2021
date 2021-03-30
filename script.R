library(tidyverse)
library(koboloadeR)

# url  https://kc.kobotoolbox.org/api/v1/


install_github("mrdwab/koboloadeR")

kobo_datasets("flisol_2021:flisol_2021") # Ver los proyectos

kobo_submission_count("#codigo") # SAber el numero de observaciones subidos

DF <- kobo_data_downloader("#codigo", "flisol_2021:flisol_2021") # Importat el proyecto

kobo_apps("app_name") # Ejecuta una Shiny app para cargar los datos

DF <- kobo_data_downloader("", "flisol_2021:flisol_2021")




