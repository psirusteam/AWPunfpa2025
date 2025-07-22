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

brks <- seq(0, 1, by = 0.25)

mapa_indicador1_total <- tm_shape(datos_mapa_ind1) +
  tm_polygons(
    col = "union18",
    title = " Indicador 1 - Total",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.8), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )
    
tmap_save(
      tmap_arrange(mapa_indicador1_total),
      filename = file.path(output, "MEX", "img", "mosaico_indicador1_total.png"),
      width = 3000, height = 1500, dpi = 300
    )
    
  

########################################## Desagregación por étnia ######################

indicador1_etnia <- readRDS(file.path(output, "MEX", "indicator1_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Indigena",
      etnia == 2 ~ "Ninguna",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador1_ind <- Shape_MEX %>%
  left_join(indicador1_etnia %>% filter(etnia == "Indigena"), by = "dam")

brks <- seq(0, 1, by = 0.25)

mapa_indicador1_indi <- tm_shape(mapa_indicador1_ind) +
  tm_polygons(
    col = "union18",
    title = " Indicador 1 - Indi",
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

mapa_indicador1_ots <- Shape_MEX %>%
  left_join(indicador1_etnia %>% filter(etnia == "Ninguna"), by = "dam")

mapa_indicador1_ots <- tm_shape(mapa_indicador1_ots) +
  tm_polygons(
    col = "union18",
    title = " Indicador1 - Ninguna",
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
  tmap_arrange(mapa_indicador1_indi, mapa_indicador1_ots, ncol = 2),
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

mapa_indicador1_pre <- Shape_MEX %>%
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

mapa_indicador1_med <- Shape_MEX %>%
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

mapa_indicador1_sup <- Shape_MEX %>%
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
  filename = file.path(output, "MEX", "img", "mosaico_indicador1_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)


#### Indicador 2. Proporción de mujeres de 15 a 49 años que toman sus propias decisiones informadas sobre relaciones sexuales, uso de anticonceptivos y atención en salud reproductiva.

########################################## Total ###############################

indicador2_total <- readRDS(file.path(output, "MEX", "indicator2_total.rds")) %>%
  mutate(
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

datos_mapa_ind2 <- Shape_MEX %>%
  left_join(indicador2_total, by = "dam")

brks <- seq(0, 0.1, by = 0.025)

mapa_indicador2_total <- tm_shape(datos_mapa_ind2) +
  tm_polygons(
    col = "dec_autonomia",
    title = " Indicador 2 - Total",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.8), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador2_total),
  filename = file.path(output, "MEX", "img", "mosaico_indicador2total.png"),
  width = 3000, height = 1500, dpi = 300
)

########################################## Desagregación por anoest ######################

indicador2_anoest <- readRDS(file.path(output, "MEX", "indicator2_anoest.rds")) %>%
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

mapa_indicador2_sine <- Shape_MEX %>%
  left_join(indicador2_anoest %>% filter(anoest == "Sin educación"), by = "dam")

brks <- seq(0, 0.1, by = 0.025)

mapa_indicador2_sine <- tm_shape(mapa_indicador2_sine) +
  tm_polygons(
    col = "dec_autonomia",
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

mapa_indicador2_pre <- Shape_MEX %>%
  left_join(indicador2_anoest %>% filter(anoest == "1 - 6 años"), by = "dam")

mapa_indicador2_pre <- tm_shape(mapa_indicador2_pre) +
  tm_polygons(
    col = "dec_autonomia",
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

mapa_indicador2_med <- Shape_MEX %>%
  left_join(indicador2_anoest %>% filter(anoest == "7 - 12 años"), by = "dam")


mapa_indicador2_med <- tm_shape(mapa_indicador2_med) +
  tm_polygons(
    col = "dec_autonomia",
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

mapa_indicador2_sup <- Shape_MEX %>%
  left_join(indicador2_anoest %>% filter(anoest == "Más de 12 años"), by = "dam")

mapa_indicador2_sup <- tm_shape(mapa_indicador2_sup ) +
  tm_polygons(
    col = "dec_autonomia",
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
  filename = file.path(output, "MEX", "img", "mosaico_indicador2_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)

########################################## Desagregación por edad ######################

indicador2_edad <- readRDS(file.path(output, "MEX", "indicator2_edad.rds")) %>%
  mutate(
    edad = case_when(
      edad == 1 ~ "Menores 15 años",
      edad == 2 ~ "15-29 años",
      edad == 3 ~ "30-34 años",
      edad == 4 ~ "45-64 años",
      edad == 5 ~ "Mayor 65 años",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador2_2 <- Shape_MEX %>%
  left_join(indicador2_edad %>% filter(edad == "15-29 años"), by = "dam")

brks <- seq(0, 0.1, by = 0.025)


mapa_indicador2_2 <- tm_shape(mapa_indicador2_2) +
  tm_polygons(
    col = "dec_autonomia",
    title = " Indicador 2/15-29 años",
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

mapa_indicador2_3 <- Shape_MEX %>%
  left_join(indicador2_edad %>% filter(edad == "30-34 años"), by = "dam")

mapa_indicador2_3 <- tm_shape(mapa_indicador2_3) +
  tm_polygons(
    col = "dec_autonomia",
    title = " Indicador 2/30-34 años",
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

mapa_indicador2_4 <- Shape_MEX %>%
  left_join(indicador2_edad %>% filter(edad == "45-64 años"), by = "dam")


mapa_indicador2_4 <- tm_shape(mapa_indicador2_4) +
  tm_polygons(
    col = "dec_autonomia",
    title = " Indicador 2/45-64 años",
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
  tmap_arrange(mapa_indicador2_2, mapa_indicador2_3, mapa_indicador2_4 ,ncol = 3, nrow = 1),
  filename = file.path(output, "MEX", "img", "mosaico_indicador2_edad.png"),
  width = 3000, height = 1500, dpi = 300
)



########################################## Desagregación por étnia ######################

indicador2_etnia <- readRDS(file.path(output, "MEX", "indicator2_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Indigena",
      etnia == 2 ~ "Ninguna",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador2_ind <- Shape_MEX %>%
  left_join(indicador2_etnia %>% filter(etnia == "Indigena"), by = "dam")

brks <- seq(0, 0.1, by = 0.025)

mapa_indicador2_ind <- tm_shape(mapa_indicador2_ind) +
  tm_polygons(
    col = "dec_autonomia",
    title = " Indicador 2 - Indi",
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

mapa_indicador2_ots <- Shape_MEX %>%
  left_join(indicador2_etnia %>% filter(etnia == "Ninguna"), by = "dam")

mapa_indicador2_ots <- tm_shape(mapa_indicador2_ots) +
  tm_polygons(
    col = "dec_autonomia",
    title = " Indicador2 - Ninguna",
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
  tmap_arrange(mapa_indicador2_ind, mapa_indicador2_ots, ncol = 2),
  filename = file.path(output, "MEX", "img", "mosaico_indicador2_etnia.png"),
  width = 3000, height = 1500, dpi = 300
)


#### Indicador 3. Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja  actual o anterior en los últimos 12 meses (15-49 años)#

########################################## Total ###############################

indicador3_total <- readRDS(file.path(output, "MEX", "indicator3_total.rds")) %>%
  mutate(
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

datos_mapa_ind3 <- Shape_MEX %>%
  left_join(indicador3_total, by = "dam")

brks <- seq(0, 0.3, by = 0.05)

mapa_indicador3_total <- tm_shape(datos_mapa_ind3) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3 - Total",
    palette = "YlOrRd",
    breaks = brks,
    bstyle = "kmeans", na.color = "grey", legend.show = TRUE,
  ) + tm_layout(
    legend.position = c(0.6, 0.8), 
    legend.title.size = 0.6,
    legend.text.size = 0.5,
    legend.bg.alpha = 1,                 
    inner.margins = c(0.05, 0.05, 0.05, 0.25)
  )

tmap_save(
  tmap_arrange(mapa_indicador3_total),
  filename = file.path(output, "MEX", "img", "mosaico_indicador3total.png"),
  width = 3000, height = 1500, dpi = 300
)

########################################## Desagregación por anoest ######################

indicador3_anoest <- readRDS(file.path(output, "MEX", "indicator3_anoest.rds")) %>%
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

mapa_indicador3_sine <- Shape_MEX %>%
  left_join(indicador3_anoest %>% filter(anoest == "Sin educación"), by = "dam")

brks <- seq(0, 0.3, by = 0.05)

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

mapa_indicador3_pre <- Shape_MEX %>%
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

mapa_indicador3_med <- Shape_MEX %>%
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

mapa_indicador3_sup <- Shape_MEX %>%
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
  filename = file.path(output, "MEX", "img", "mosaico_indicador3_anoest.png"),
  width = 3000, height = 1500, dpi = 300
)

########################################## Desagregación por edad ######################

indicador3_edad <- readRDS(file.path(output, "MEX", "indicator3_edad.rds")) %>%
  mutate(
    edad = case_when(
      edad == 1 ~ "Menores 15 años",
      edad == 2 ~ "15-29 años",
      edad == 3 ~ "30-34 años",
      edad == 4 ~ "45-64 años",
      edad == 5 ~ "Mayor 65 años",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_2 <- Shape_MEX %>%
  left_join(indicador3_edad %>% filter(edad == "15-29 años"), by = "dam")

brks <- seq(0, 0.3, by = 0.05)


mapa_indicador3_2 <- tm_shape(mapa_indicador3_2) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3/15-29 años",
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

mapa_indicador3_3 <- Shape_MEX %>%
  left_join(indicador3_edad %>% filter(edad == "30-34 años"), by = "dam")

mapa_indicador3_3 <- tm_shape(mapa_indicador3_3) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3/30-34 años",
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

mapa_indicador3_4 <- Shape_MEX %>%
  left_join(indicador3_edad %>% filter(edad == "45-64 años"), by = "dam")


mapa_indicador3_4 <- tm_shape(mapa_indicador3_4) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3/45-64 años",
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
  tmap_arrange(mapa_indicador3_2, mapa_indicador3_3, mapa_indicador3_4 ,ncol = 3, nrow = 1),
  filename = file.path(output, "MEX", "img", "mosaico_indicador3_edad.png"),
  width = 3000, height = 1500, dpi = 300
)



########################################## Desagregación por étnia ######################

indicador3_etnia <- readRDS(file.path(output, "MEX", "indicator3_etnia.rds")) %>%
  mutate(
    etnia = case_when(
      etnia == 1 ~ "Indigena",
      etnia == 2 ~ "Ninguna",
      TRUE ~ NA_character_
    ),
    dam = str_pad(as.character(dam), width = 2, side = "left", pad = "0")
  )

mapa_indicador3_ind <- Shape_MEX %>%
  left_join(indicador3_etnia %>% filter(etnia == "Indigena"), by = "dam")

brks <- seq(0, 0.3, by = 0.05)

mapa_indicador3_ind <- tm_shape(mapa_indicador3_ind) +
  tm_polygons(
    col = "violencia",
    title = " Indicador 3- Indi",
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

mapa_indicador3_ots <- Shape_MEX %>%
  left_join(indicador3_etnia %>% filter(etnia == "Ninguna"), by = "dam")

mapa_indicador3_ots <- tm_shape(mapa_indicador3_ots) +
  tm_polygons(
    col = "violencia",
    title = " Indicador3 - Otros",
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
  tmap_arrange(mapa_indicador3_ind, mapa_indicador3_ots, ncol = 2),
  filename = file.path(output, "MEX", "img", "mosaico_indicador3_etnia.png"),
  width = 3000, height = 1500, dpi = 300
)

