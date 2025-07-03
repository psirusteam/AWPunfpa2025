#################################################
#             Proyecto : AWPunfpa2025           #
#       Procesamiento indicadores - BOL         #
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


################################################################################
###----------------------------- Loading dataset  ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <- file.path(b_path, "src")

load(file.path(output, "BOL/EDSA_modulo_mujeres_2023.RData"))

################################################################################
###---------------------------- Variables standardization--------------------###
################################################################################

base_mujeres <- base_mujeres %>% mutate(
  
  anoest = case_when(
    ms01_0101a < 5 | aestudio == -1   ~ "98"  , #No aplica
    aestudio == 99 | (ms01_0101a >= 5 & is.na(aestudio)) ~ "99", #NS/NR
    aestudio == 0  ~ "1", # Sin educacion
    aestudio %in% c(1:6) ~ "2",       # 1 - 6
    aestudio %in% c(7:12) ~ "3",      # 7 - 12
    aestudio > 12 ~ "4",      # mas de 12
    TRUE ~ "Error"),
  
  edad = case_when(
    ms01_0101a < 15 ~ "1",
    ms01_0101a < 30 ~ "2",
    ms01_0101a < 45 ~ "3",
    ms01_0101a < 65 ~ "4",
    TRUE~"5"),
  
  area = area, 
  
  dam = departamento,
  
  etnia = ms01_0108,
  
  # Decisión de usar método anticonceptivo
  dec_met = case_when(
    ms07_0720 == 1 ~ 1,
    ms07_0720 %in% c(2, 3, 6, 999) ~ 0,
    TRUE ~ NA_real_),
  
  # Consentimiento en la primera relación sexual
  consentim_sex = case_when(
    ms06_0610_b == 1 ~ 1,
    ms06_0610_b %in% c(2,999) ~ 0,
    TRUE ~ NA_real_),
  
  #Decisión atención sanitaria
  dec_atmed = case_when(
    ms08_0825_A == 1 ~ 1,
    ms08_0825_A %in% c(2,3,4,6,999) ~ 0,
    TRUE ~ NA_real_),
  
  # Violencia psicológica o económica (últimos 12 meses)
  insultos = case_when(
    ms11_1105_B %in% c(1, 2, 3) ~ 1,
    ms11_1105_B %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  encierros = case_when(
    ms11_1105_C %in% c(1, 2, 3) ~ 1,
    ms11_1105_C %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  humillaciones = case_when(
    ms11_1105_D %in% c(1, 2, 3) ~ 1,
    ms11_1105_D %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  amenazas_abandono = case_when(
    ms11_1105_E %in% c(1, 2, 3) ~ 1,
    ms11_1105_E %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  amenazas_hijos = case_when(
    ms11_1105_F %in% c(1, 2, 3) ~ 1,
    ms11_1105_F %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  romper_objetos = case_when(
    ms11_1105_G %in% c(1, 2, 3) ~ 1,
    ms11_1105_G %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  amenaza_economica = case_when(
    ms11_1105_H %in% c(1, 2, 3) ~ 1,
    ms11_1105_H %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  control_economico = case_when(
    ms11_1105_I %in% c(1, 2, 3) ~ 1,
    ms11_1105_I %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  amenaza_sex = case_when(
    ms11_1105_J %in% c(1, 2, 3) ~ 1,
    ms11_1105_J %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  amenaza_muerte = case_when(
    ms11_1105_K %in% c(1, 2, 3) ~ 1,
    ms11_1105_K %in% c(4, 999) ~ 0,
    TRUE ~ NA_real_
  ),
  
  #edad de su primera union con su esposo o compañero
  
  edad_union1 = ms06_0607_02_2 - ms01_0101b_3,
  
  #Unión antes de los 15 y 18 años
  union15 = case_when(
    !is.na(edad_union1) & edad_union1 < 15 ~ 1,
    is.na(edad_union1) ~ NA_real_,
    TRUE ~ 0),
  
  union18 = case_when(
    !is.na(edad_union1) & edad_union1 < 18 ~ 1,
    is.na(edad_union1) ~ NA_real_,
    TRUE ~ 0), 
  
  )


################################################################################
###---------------------------- Indicators ----------------------------------###
################################################################################  
options(survey.lonely.psu = "adjust")

diseno_mujeres <- base_mujeres %>%
  as_survey_design(
    ids = upm,
    strata = estrato,
    weights = ponderadorm,
    nest = TRUE
  )

### Proporción de mujeres de 20-24 años que estuvieron casadas o en 
### unión antes de los 15 o 18 años.



