#################################################
#             Proyecto : AWPunfpa2025           #
#   Construcción de mosaico del mapa - MEX      #
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

Shape_MEX <- st_read(file.path(input, "MEX", "MEX.shp"))


#### Indicador 1. Proporción de mujeres de 20 a 24 años que estuvieron casadas o en unión antes de los 15 o 18 años.

########################################## Total ###############################
indicador1_total <- readRDS(file.path(output, "MEX", "indicator1_total.rds")) %>%
  mutate(
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

datos_mapa_ind1 <- Shape_MEX %>%
  left_join(indicador1_total, by = "dam")

brks <- seq(0, 0.6, by = 0.1)

mapa_indicador1_total <- tm_shape(datos_mapa_ind1) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador 1 - Total",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

########################################## Desagregación por area ######################
indicador1_area <- readRDS(file.path(output, "MEX", "indicator1_area.rds"))%>%
  mutate(nombre_area = case_when(
    area == 1 ~ "Urbano",
    area == 2 ~ "Rural",
    TRUE ~ "Sin clasificar"
  ),
  dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_area <- Shape_MEX %>%
  left_join(indicador1_area, by = "dam")

mapa_indicador1_urb <- tm_shape(mapa_indicador1_area %>% filter(area == 1)) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador1 - Urbano",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )


mapa_indicador1_rur <- tm_shape(mapa_indicador1_area %>% filter(area == 2)) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador 1 - Rural",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador1_total,mapa_indicador1_urb, mapa_indicador1_rur, ncol = 3),
  filename = file.path(output, "MEX", "img","mosaico_indicador1_area.png"),
  width = 3000, height = 1500, dpi = 300
)



########################################## Desagregación por étnia ######################

indicador1_etnia <- readRDS(file.path(output, "MEX", "indicator1_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Afro",
      etnia == 2 ~ "Indigena",
      etnia == 3 ~ "Otros",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_afro <- Shape_MEX %>%
  left_join(indicador1_etnia %>% filter(etnia == "Afro"), by = "dam")

brks <- seq(0, 1, by = 0.20)

mapa_indicador1_afro <- tm_shape(mapa_indicador1_afro) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador 1 - Afro",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.5, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_nob <- Shape_MEX %>%
  left_join(indicador1_etnia %>% filter(etnia == "Indigena"), by = "dam")

mapa_indicador1_nob <- tm_shape(mapa_indicador1_nob) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador1 - Indigena",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.5, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_nop <- Shape_MEX %>%
  left_join(indicador1_etnia %>% filter(etnia == "Otros"), by = "dam")

mapa_indicador1_nop <- tm_shape(mapa_indicador1_nop) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador1 - Otros",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.5, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador1_afro, mapa_indicador1_nop, mapa_indicador1_nob, ncol = 3),
  filename = file.path(output, "MEX", "img", "mosaico_indicador1_etnia.png"),
  width = 3000, height = 1500, dpi = 300
)


########################################## Desagregación por anoest ######################

indicador1_anoest <- readRDS(file.path(output, "MEX", "indicator1_anoest.rds")) %>%
  mutate(
    anoest = case_when(
      anoest == 1 ~ "Sin educación",
      anoest == 2 ~ "1 - 6 años",
      anoest == 3 ~ "7 - 12 años",
      anoest == 4 ~ "Más de 12 años",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_sine <- Shape_MEX %>%
  left_join(indicador1_anoest %>% filter(anoest == "Sin educación"), by = "dam")

brks <- seq(0, 1, by = 0.20)

mapa_indicador1_sine <- tm_shape(mapa_indicador1_sine) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador 1/Sin educación",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_pre <- Shape_MEX %>%
  left_join(indicador1_anoest %>% filter(anoest == "1 - 6 años"), by = "dam")

mapa_indicador1_pre <- tm_shape(mapa_indicador1_pre) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador 1/1 - 6 años",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_med <- Shape_MEX %>%
  left_join(indicador1_anoest %>% filter(anoest == "7 - 12 años"), by = "dam")


mapa_indicador1_med <- tm_shape(mapa_indicador1_med) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador 1/7 - 12 años",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_sup <- Shape_MEX %>%
  left_join(indicador1_anoest %>% filter(anoest == "Más de 12 años"), by = "dam")

mapa_indicador1_sup <- tm_shape(mapa_indicador1_sup ) +
  tm_polygons(
    col = "prop_antes_18",
    title = " Indicador 1/Más de 12 años",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )


tmap_save(
  tmap_arrange(mapa_indicador1_sine, mapa_indicador1_pre, mapa_indicador1_med, mapa_indicador1_sup ,ncol = 2, nrow = 2),
  filename = file.path(output, "MEX", "img", "mosaico_indicador1_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)
