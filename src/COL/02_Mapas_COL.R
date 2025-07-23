#################################################
#             Proyecto : AWPunfpa2025           #
#   Construcción de mosaico del mapa - COL      #
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

Shape_COL <- st_read(file.path(input, "COL", "COL.shp"))



########### Indicador 1. Proporción de mujeres de 20 a 24 años que estuvieron casadas o en unión antes de los 15 o 18 años. ##########################

### Total 
indicador1_total <- readRDS(file.path(output, "COL", "indicator1_total.rds")) %>%
  mutate(
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

datos_mapa_ind1 <- Shape_COL %>%
  left_join(indicador1_total, by = "dam")

brks <- seq(0, 0.7, by = 0.15)

mapa_indicador1_total <- tm_shape(datos_mapa_ind1) +
  tm_polygons(
    col = "union18",
    title = " Indicador 1 - Total",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador1_total),
  filename = file.path(output, "COL", "img", "mosaico_indicador1_total.png"),
  width = 3000, height = 1500, dpi = 300
)

### Area 

indicador1_area <- readRDS(file.path(output, "COL", "indicator1_area.rds")) %>%
  mutate(
    area = case_when(
      area == 1 ~ "Urbana",
      area == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_urb <- Shape_COL %>%
  left_join(indicador1_area %>% filter(area == "Urbana"), by = "dam")

brks <- seq(0, 0.7, by = 0.15)

mapa_indicador1_urb <- tm_shape(mapa_indicador1_urb) +
  tm_polygons(
    col = "union18",
    title = " Indicador 1 - Urbano",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_rur <- Shape_COL %>%
  left_join(indicador1_area %>% filter(area == "Rural"), by = "dam")

mapa_indicador1_rur <- tm_shape(mapa_indicador1_rur) +
  tm_polygons(
    col = "union18",
    title = " Indicador1 - Rural",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador1_urb, mapa_indicador1_rur, ncol = 2),
  filename = file.path(output, "COL", "img", "mosaico_indicador1_area.png"),
  width = 3000, height = 1500, dpi = 300
)

### Etnia

indicador1_etnia <- readRDS(file.path(output, "COL", "indicator1_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Indigena",
      etnia == 2 ~ "Afro",
      etnia == 3 ~ "Otra",
      etnia == 4 ~ "Ninguno"
      ,
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_ind <- Shape_COL %>%
  left_join(indicador1_etnia %>% filter(etnia == "Indigena"), by = "dam")

brks <- seq(0, 1, by = 0.25)

mapa_indicador1_ind <- tm_shape(mapa_indicador1_ind) +
  tm_polygons(
    col = "union18",
    title = " Indicador 1 - Indi",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_afr <- Shape_COL %>%
  left_join(indicador1_etnia %>% filter(etnia == "Afro"), by = "dam")

mapa_indicador1_afr <- tm_shape(mapa_indicador1_afr) +
  tm_polygons(
    col = "union18",
    title = " Indicador 1 - Afro",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_ots <- Shape_COL %>%
  left_join(indicador1_etnia %>% filter(etnia == "Otra"), by = "dam")

mapa_indicador1_ots <- tm_shape(mapa_indicador1_ots) +
  tm_polygons(
    col = "union18",
    title = " Indicador1 - Otra",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador1_nin <- Shape_COL %>%
  left_join(indicador1_etnia %>% filter(etnia == "Ninguno"), by = "dam")

mapa_indicador1_nin <- tm_shape(mapa_indicador1_nin) +
  tm_polygons(
    col = "union18",
    title = " Indicador1 - Ninguna",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador1_ind,mapa_indicador1_afr, mapa_indicador1_ots , mapa_indicador1_nin, ncol = 2, nrow = 2),
  filename = file.path(output, "COL", "img", "mosaico_indicador1_etnia.png"),
  width = 3000, height = 1500, dpi = 300
)

### Anoest

indicador1_anoest <- readRDS(file.path(output, "COL", "indicator1_anoest.rds")) %>%
  mutate(
    anoest = case_when(
      anoest == 0 ~ "Sin educación",
      anoest == 1 ~ "1 - 6 años",
      anoest == 2 ~ "7 - 12 años",
      anoest == 3 ~ "Más de 12 años",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_sine <- Shape_COL %>%
  left_join(indicador1_anoest %>% filter(anoest == "Sin educación"), by = "dam")

brks <- seq(0, 1, by = 0.25)

mapa_indicador1_sine <- tm_shape(mapa_indicador1_sine) +
  tm_polygons(
    col = "union18",
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

mapa_indicador1_pre <- Shape_COL %>%
  left_join(indicador1_anoest %>% filter(anoest == "1 - 6 años"), by = "dam")

mapa_indicador1_pre <- tm_shape(mapa_indicador1_pre) +
  tm_polygons(
    col = "union18",
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

mapa_indicador1_med <- Shape_COL %>%
  left_join(indicador1_anoest %>% filter(anoest == "7 - 12 años"), by = "dam")


mapa_indicador1_med <- tm_shape(mapa_indicador1_med) +
  tm_polygons(
    col = "union18",
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

mapa_indicador1_sup <- Shape_COL %>%
  left_join(indicador1_anoest %>% filter(anoest == "Más de 12 años"), by = "dam")

mapa_indicador1_sup <- tm_shape(mapa_indicador1_sup ) +
  tm_polygons(
    col = "union18",
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
  filename = file.path(output, "COL", "img", "mosaico_indicador1_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)


########### Indicador 3. Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja  actual o anterior en los últimos 12 meses. ##########################

### Total 
indicador3_total <- readRDS(file.path(output, "COL", "indicator3_total.rds")) %>%
  mutate(
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

datos_mapa_ind3 <- Shape_COL %>%
  left_join(indicador3_total, by = "dam")

brks <- seq(0, 0.75, by = 0.15)

mapa_indicador3_total <- tm_shape(datos_mapa_ind3) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3 - Total",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador3_total),
  filename = file.path(output, "COL", "img", "mosaico_indicador3_total.png"),
  width = 3000, height = 1500, dpi = 300
)

### Area 

indicador3_area <- readRDS(file.path(output, "COL", "indicator3_area.rds")) %>%
  mutate(
    area = case_when(
      area == 1 ~ "Urbana",
      area == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_urb <- Shape_COL %>%
  left_join(indicador3_area %>% filter(area == "Urbana"), by = "dam")

brks <- seq(0, 0.75, by = 0.15)

mapa_indicador3_urb <- tm_shape(mapa_indicador3_urb) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3 - Urbano",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador3_rur <- Shape_COL %>%
  left_join(indicador3_area %>% filter(area == "Rural"), by = "dam")

mapa_indicador3_rur <- tm_shape(mapa_indicador3_rur) +
  tm_polygons(
    col = "violencia",
    title = " indicador3 - Rural",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador3_urb, mapa_indicador3_rur, ncol = 2),
  filename = file.path(output, "COL", "img", "mosaico_indicador3_area.png"),
  width = 3000, height = 1500, dpi = 300
)

### Etnia

indicador3_etnia <- readRDS(file.path(output, "COL", "indicator3_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Indigena",
      etnia == 2 ~ "Afro",
      etnia == 3 ~ "Otra",
      etnia == 4 ~ "Ninguno"
      ,
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_ind <- Shape_COL %>%
  left_join(indicador3_etnia %>% filter(etnia == "Indigena"), by = "dam")

brks <- seq(0, 1, by = 0.25)

mapa_indicador3_ind <- tm_shape(mapa_indicador3_ind) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3 - Indi",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador3_afr <- Shape_COL %>%
  left_join(indicador3_etnia %>% filter(etnia == "Afro"), by = "dam")

mapa_indicador3_afr <- tm_shape(mapa_indicador3_afr) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3 - Afro",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador3_ots <- Shape_COL %>%
  left_join(indicador3_etnia %>% filter(etnia == "Otra"), by = "dam")

mapa_indicador3_ots <- tm_shape(mapa_indicador3_ots) +
  tm_polygons(
    col = "violencia",
    title = " indicador3 - Otra",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

mapa_indicador3_nin <- Shape_COL %>%
  left_join(indicador3_etnia %>% filter(etnia == "Ninguno"), by = "dam")

mapa_indicador3_nin <- tm_shape(mapa_indicador3_nin) +
  tm_polygons(
    col = "violencia",
    title = " indicador3 - Ninguna",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 1), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador3_ind,mapa_indicador3_afr, mapa_indicador3_ots , mapa_indicador3_nin, ncol = 2, nrow = 2),
  filename = file.path(output, "COL", "img", "mosaico_indicador3_etnia.png"),
  width = 3000, height = 1500, dpi = 300
)

### Anoest

indicador3_anoest <- readRDS(file.path(output, "COL", "indicator3_anoest.rds")) %>%
  mutate(
    anoest = case_when(
      anoest == 0 ~ "Sin educación",
      anoest == 1 ~ "1 - 6 años",
      anoest == 2 ~ "7 - 12 años",
      anoest == 3 ~ "Más de 12 años",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_sine <- Shape_COL %>%
  left_join(indicador3_anoest %>% filter(anoest == "Sin educación"), by = "dam")

brks <- seq(0, 1, by = 0.25)

mapa_indicador3_sine <- tm_shape(mapa_indicador3_sine) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3/Sin educación",
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

mapa_indicador3_pre <- Shape_COL %>%
  left_join(indicador3_anoest %>% filter(anoest == "1 - 6 años"), by = "dam")

mapa_indicador3_pre <- tm_shape(mapa_indicador3_pre) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3/1 - 6 años",
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

mapa_indicador3_med <- Shape_COL %>%
  left_join(indicador3_anoest %>% filter(anoest == "7 - 12 años"), by = "dam")


mapa_indicador3_med <- tm_shape(mapa_indicador3_med) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3/7 - 12 años",
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

mapa_indicador3_sup <- Shape_COL %>%
  left_join(indicador3_anoest %>% filter(anoest == "Más de 12 años"), by = "dam")

mapa_indicador3_sup <- tm_shape(mapa_indicador3_sup ) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3/Más de 12 años",
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
  tmap_arrange(mapa_indicador3_sine, mapa_indicador3_pre, mapa_indicador3_med, mapa_indicador3_sup ,ncol = 2, nrow = 2),
  filename = file.path(output, "COL", "img", "mosaico_indicador3_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)

