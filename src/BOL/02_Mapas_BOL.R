#################################################
#             Proyecto : AWPunfpa2025           #
#   Construcción de mosaico del mapa - BOL      #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(ggplot2)
library(haven)
library(sp)
library(sf)
library(tmap)
library(tidyr)
library(stringr)


################################################################################
###----------------------------- Loading dataset  ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <- file.path(b_path, "src")

Shape_Bolivia <- st_read(file.path(input, "BOL", "BOL.shp"))


################################################################################
###----------------------------- Map constrution  ---------------------------###
################################################################################


#### Indicador 1. Proporción de mujeres de 20 a 24 años que estuvieron casadas o en unión antes de los 15 o 18 años.

#### Indicador 2. Proporción de mujeres de 15 a 49 años que toman sus propias decisiones informadas sobre relaciones sexuales, uso de anticonceptivos y atención en salud reproductiva.

indicador2_area <- readRDS(file.path(output, "BOL", "indicator2_area.rds"))%>%
  mutate(nombre_area = case_when(
    area == 1 ~ "Urbano",
    area == 2 ~ "Rural",
    TRUE ~ "Sin clasificar"
  ),
  dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador2_area <- Shape_Bolivia %>%
  left_join(indicador2_area, by = "dam")

brks <- seq(0, 0.30, by = 0.05)



mapa_indicador2_urb <- tm_shape(mapa_indicador2_area %>% filter(area == 1)) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2 - Urbano",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.7, 1.1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )


mapa_indicador2_rur <- tm_shape(mapa_indicador2_area %>% filter(area == 2)) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2 - Rural",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.7, 1.1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_arrange(mapa_indicador2_urb, mapa_indicador2_rur, ncol = 2)

