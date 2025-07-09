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

########################################## Total ###############################
indicador1_total <- readRDS(file.path(output, "BOL", "indicator1_total.rds")) %>%
  mutate(
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

datos_mapa_ind1 <- Shape_Bolivia %>%
  left_join(indicador1_total, by = "dam")

brks <- seq(0, 1, by = 0.20)

mapa_indicador1_total <- tm_shape(datos_mapa_ind1) +
  tm_polygons(
    col = "proporcion",
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
indicador1_area <- readRDS(file.path(output, "BOL", "indicator1_area.rds"))%>%
  mutate(nombre_area = case_when(
    area == 1 ~ "Urbano",
    area == 2 ~ "Rural",
    TRUE ~ "Sin clasificar"
  ),
  dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_area <- Shape_Bolivia %>%
  left_join(indicador1_area, by = "dam")

mapa_indicador1_urb <- tm_shape(mapa_indicador1_area %>% filter(area == 1)) +
  tm_polygons(
    col = "proporcion",
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
    col = "proporcion",
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
  filename = file.path(output, "BOL", "img","mosaico_indicador1_area.png"),
  width = 3000, height = 1500, dpi = 300
)



########################################## Desagregación por étnia ######################

indicador1_etnia <- readRDS(file.path(output, "BOL", "indicator1_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Indígena o Afroboliviana",
      etnia == 2 ~ "No pertenece",
      etnia == 3 ~ "No es boliviana",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_afro <- Shape_Bolivia %>%
  left_join(indicador1_etnia %>% filter(etnia == "Indígena o Afroboliviana"), by = "dam")

brks <- seq(0, 1, by = 0.20)

mapa_indicador1_afro <- tm_shape(mapa_indicador1_afro) +
  tm_polygons(
    col = "proporcion",
    title = " Indicador 1 - Indígena/Afro",
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

mapa_indicador1_nob <- Shape_Bolivia %>%
  left_join(indicador1_etnia %>% filter(etnia == "No es boliviana"), by = "dam")

mapa_indicador1_nob <- tm_shape(mapa_indicador1_nob) +
  tm_polygons(
    col = "proporcion",
    title = " Indicador1 - No es boliviana",
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

mapa_indicador1_nop <- Shape_Bolivia %>%
  left_join(indicador1_etnia %>% filter(etnia == "No pertenece"), by = "dam")

mapa_indicador1_nop <- tm_shape(mapa_indicador1_nop) +
  tm_polygons(
    col = "proporcion",
    title = " Indicador1 - No pertenece",
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
  filename = file.path(output, "BOL", "img", "mosaico_indicador1_etnia.png"),
  width = 3000, height = 1500, dpi = 300
)


########################################## Desagregación por anoest ######################

indicador1_anoest <- readRDS(file.path(output, "BOL", "indicator1_anoest.rds")) %>%
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

mapa_indicador1_sine <- Shape_Bolivia %>%
  left_join(indicador1_anoest %>% filter(anoest == "Sin educación"), by = "dam")

brks <- seq(0, 1, by = 0.20)

mapa_indicador1_sine <- tm_shape(mapa_indicador1_sine) +
  tm_polygons(
    col = "proporcion",
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

mapa_indicador1_pre <- Shape_Bolivia %>%
  left_join(indicador1_anoest %>% filter(anoest == "1 - 6 años"), by = "dam")

mapa_indicador1_pre <- tm_shape(mapa_indicador1_pre) +
  tm_polygons(
    col = "proporcion",
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

mapa_indicador1_med <- Shape_Bolivia %>%
  left_join(indicador1_anoest %>% filter(anoest == "7 - 12 años"), by = "dam")


mapa_indicador1_med <- tm_shape(mapa_indicador1_med) +
  tm_polygons(
    col = "proporcion",
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

mapa_indicador1_sup <- Shape_Bolivia %>%
  left_join(indicador1_anoest %>% filter(anoest == "Más de 12 años"), by = "dam")

mapa_indicador1_sup <- tm_shape(mapa_indicador1_sup ) +
  tm_polygons(
    col = "proporcion",
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
  filename = file.path(output, "BOL", "img", "mosaico_indicador1_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)

#### Indicador 2. Proporción de mujeres de 15 a 49 años que toman sus propias decisiones informadas sobre relaciones sexuales, uso de anticonceptivos y atención en salud reproductiva.

########################################## Total ###############################
indicador2_total <- readRDS(file.path(output, "BOL", "indicator_2total.rds"))%>%
  mutate(
  dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador2_total <- Shape_Bolivia %>%
  left_join(indicador2_total, by = "dam")

brks <- seq(0, 0.30, by = 0.05)

mapa_indicador2_total <- tm_shape(mapa_indicador2_total) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2 - Total",
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
    legend.position = c(0.6, 0.98), 
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
    legend.position = c(0.6, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador2_total,mapa_indicador2_urb, mapa_indicador2_rur, ncol = 3),
  filename = file.path(output, "BOL", "img","mosaico_indicador2_area.png"),
  width = 3000, height = 1500, dpi = 300
)



########################################## Desagregación por étnia ######################
indicador2_etnia <- readRDS(file.path(output, "BOL", "indicator2_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Indígena o Afroboliviana",
      etnia == 2 ~ "No pertenece",
      etnia == 3 ~ "No es boliviana",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador2_etnia <- Shape_Bolivia %>%
  left_join(indicador2_etnia, by = "dam")

brks <- seq(0, 1, by = 0.15)

mapa_indicador2_afro <- tm_shape(mapa_indicador2_etnia %>% filter(etnia == "Indígena o Afroboliviana")) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2 - Indígena/Afro",
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


mapa_indicador2_nob <- tm_shape(mapa_indicador2_etnia %>% filter(etnia == "No es boliviana")) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2 - No es boliviana",
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

mapa_indicador2_nop <- tm_shape(mapa_indicador2_etnia %>% filter(etnia == "No pertenece")) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2 - No pertenece",
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
  tmap_arrange(mapa_indicador2_afro, mapa_indicador2_nop, mapa_indicador2_nob, ncol = 3),
  filename = file.path(output, "BOL", "img", "mosaico_indicador2_etnia.png"),
  width = 3000, height = 1500, dpi = 300
)



########################################## Desagregación por anoest ######################

indicador2_anoest <- readRDS(file.path(output, "BOL", "indicator2_anoest.rds")) %>%
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

mapa_indicador2_anoest <- Shape_Bolivia %>%
  left_join(indicador2_anoest, by = "dam")

brks <- c(0.15, 0.2, 0.25, 0.3, 0.35, 0.4)

mapa_indicador2_sine <- tm_shape(mapa_indicador2_anoest %>% filter(anoest == "Sin educación")) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2/Sin educación",
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


mapa_indicador2_pre <- tm_shape(mapa_indicador2_anoest %>% filter(anoest == "1 - 6 años")) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2/1 - 6 años",
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

mapa_indicador2_med <- tm_shape(mapa_indicador2_anoest %>% filter(anoest == "7 - 12 años")) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2/7 - 12 años",
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

mapa_indicador2_sup <- tm_shape(mapa_indicador2_anoest %>% filter(anoest == "Más de 12 años")) +
  tm_polygons(
    col = "indicator_2",
    title = " Indicador 2/Más de 12 años",
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
  tmap_arrange(mapa_indicador2_sine, mapa_indicador2_pre, mapa_indicador2_med, mapa_indicador2_sup ,ncol = 2, nrow = 2),
  filename = file.path(output, "BOL", "img", "mosaico_indicador2_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)













#### Indicador 3. Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja  actual o anterior en los últimos 12 meses (15-49 años)#

########################################## Total ###############################
indicador3_total <- readRDS(file.path(output, "BOL", "indicator3_total.rds"))%>%
  mutate(
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_total <- Shape_Bolivia %>%
  left_join(indicador3_total, by = "dam")

brks <- seq(0, 0.40, by = 0.05)

mapa_indicador3_total <- tm_shape(mapa_indicador3_total) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3 - Total",
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
indicador3_area <- readRDS(file.path(output, "BOL", "indicator3_area.rds"))%>%
  mutate(nombre_area = case_when(
    area == 1 ~ "Urbano",
    area == 2 ~ "Rural",
    TRUE ~ "Sin clasificar"
  ),
  dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_area <- Shape_Bolivia %>%
  left_join(indicador3_area, by = "dam")


mapa_indicador3_urb <- tm_shape(mapa_indicador3_area %>% filter(area == 1)) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3 - Urbano",
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


mapa_indicador3_rur <- tm_shape(mapa_indicador3_area %>% filter(area == 2)) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3 - Rural",
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
  tmap_arrange(mapa_indicador3_total,mapa_indicador3_urb, mapa_indicador3_rur, ncol = 3),
  filename = file.path(output, "BOL", "img","mosaico_indicador3_area.png"),
  width = 3000, height = 1500, dpi = 300
)



########################################## Desagregación por étnia ######################
indicador3_etnia <- readRDS(file.path(output, "BOL", "indicator3_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Indígena o Afroboliviana",
      etnia == 2 ~ "No pertenece",
      etnia == 3 ~ "No es boliviana",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_afro <- Shape_Bolivia %>%
  left_join(indicador3_etnia %>% filter(etnia == "Indígena o Afroboliviana"), by = "dam")

brks <- seq(0, 1, by = 0.15)

mapa_indicador3_afro <- tm_shape(mapa_indicador3_afro) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3 - Indígena/Afro",
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

mapa_indicador3_nob <- Shape_Bolivia %>%
  left_join(indicador3_etnia %>% filter(etnia == "No es boliviana"), by = "dam")

mapa_indicador3_nob <- tm_shape(mapa_indicador3_nob) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3 - No es boliviana",
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

mapa_indicador3_nop <- Shape_Bolivia %>%
  left_join(indicador3_etnia %>% filter(etnia == "No pertenece"), by = "dam")

mapa_indicador3_nop <- tm_shape(mapa_indicador3_nop) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3 - No pertenece",
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
  tmap_arrange(mapa_indicador3_afro, mapa_indicador3_nop, mapa_indicador3_nob, ncol = 3),
  filename = file.path(output, "BOL", "img", "mosaico_indicador3_etnia.png"),
  width = 3000, height = 1500, dpi = 300
)



########################################## Desagregación por anoest ######################

indicador3_anoest <- readRDS(file.path(output, "BOL", "indicator3_anoest.rds")) %>%
  mutate(
    anoest = case_when(
      anoest == 1 ~ "Sin educacion",
      anoest == 2 ~ "1 - 6 anos",
      anoest == 3 ~ "7 - 12 anos",
      anoest == 4 ~ "Mas de 12 anos",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_sine <- Shape_Bolivia %>%
  left_join(indicador3_anoest %>% filter(anoest == "Sin educacion"), by = "dam")

brks <- c(0.15, 0.2, 0.25, 0.3, 0.35, 0.4)

mapa_indicador3_sine <- tm_shape(mapa_indicador3_sine) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3/Sin educacion",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.7, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador3_pre <- Shape_Bolivia %>%
  left_join(indicador3_anoest %>% filter(anoest == "1 - 6 anos"), by = "dam")

mapa_indicador3_pre <- tm_shape(mapa_indicador3_pre) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3/1 - 6 anos",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.7, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador3_med <- Shape_Bolivia %>%
  left_join(indicador3_anoest %>% filter(anoest == "7 - 12 anos"), by = "dam")


mapa_indicador3_med <- tm_shape(mapa_indicador3_med) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3/7 - 12 anos",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.7, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador3_sup <- Shape_Bolivia %>%
  left_join(indicador3_anoest %>% filter(anoest == "Mas de 12 anos"), by = "dam")

mapa_indicador3_sup <- tm_shape(mapa_indicador3_sup ) +
  tm_polygons(
    col = "indicator_3",
    title = " Indicador 3/Mas 12 anos",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.7, 0.98), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )


tmap_save(
  tmap_arrange(mapa_indicador3_sine, mapa_indicador3_pre, mapa_indicador3_med, mapa_indicador3_sup ,ncol = 2, nrow = 3),
  filename = file.path(output, "BOL", "img", "mosaico_indicador3_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)



