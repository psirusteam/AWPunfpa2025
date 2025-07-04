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
library(haven)


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

base_mujeres$ms07_0720 <- as.numeric((base_mujeres$ms07_0720))

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
  
  #si ha tenido una relacion alguna vez
  rela_algunavez = case_when(
    ms06_0602 %in% c(1, 2) ~ 1,
    ms06_0602 == 3 ~ 0,
    TRUE ~ NA_real_
  ),
  
  esta_unida = case_when(
    econyugd_m %in% c(2, 3) ~ 1,
    econyugd_m %in% c(1, 4, 5, 999) ~ 0,
    TRUE ~ NA_real_
  )
  
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

diseno_mujeres_20_24 <- diseno_mujeres %>%
  filter(ms01_0101a >= 20 & ms01_0101a <= 24)

indicator1_area <- diseno_mujeres_20_24 %>%
  group_by(dam, area) %>%
  summarise(
    proporcion = survey_mean(union18, vartype = "ci", na.rm = TRUE)*100
  )

indicator1_etnia <- diseno_mujeres_20_24 %>%
  group_by(dam, etnia) %>%
  summarise(
    proporcion = survey_mean(union18, vartype = "ci", na.rm = TRUE)*100
  )

indicator1_anoest <- diseno_mujeres_20_24 %>%
  group_by(dam, anoest) %>%
  summarise(
    proporcion = survey_mean(union18, vartype = "ci", na.rm = TRUE)*100
  )

### Proporción de mujeres de 15-49 años que toman sus propias decisiones informadas 
### sobre relaciones sexuales, uso de anticonceptivos y atención en salud reproductiva. 

diseño_indicator2 <- diseno_mujeres %>%
  filter(ms01_0101a >= 15, ms01_0101a <= 49, esta_unida == 1) %>%
  mutate(
    decision_informada = case_when(
      dec_met == 1 & dec_atmed == 1 & consentim_sex == 1 ~ 1,
      is.na(dec_met) | is.na(dec_atmed) | is.na(consentim_sex) ~ NA_real_,
      TRUE ~ 0
    )
  )


indicator2_area <- diseño_indicator2 %>%
  group_by(dam, area) %>%
  summarise(
    indicator_2 = survey_mean(decision_informada, vartype = "cv", na.rm = TRUE)*100
  )

indicator2_etnia <- diseño_indicator2 %>%
  group_by(dam, etnia) %>%
  summarise(
    indicator_2 = survey_mean(decision_informada, vartype = "cv", na.rm = TRUE)*100
  )

indicator2_anoest <- diseño_indicator2 %>%
  group_by(dam, anoest) %>%
  summarise(
    indicator_2 = survey_mean(decision_informada, vartype = "cv", na.rm = TRUE)*100
  )

### Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna 
### vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja 
## actual o anterior en los últimos 12 meses (15-49 años)

diseño_indicator3 <- diseno_mujeres %>%
  filter(ms01_0101a >= 15, ms01_0101a <= 49, rela_algunavez == 1) %>%
  mutate(
    violencia_pareja_ult12m = case_when(
      insultos == 1 |
        encierros == 1 |
        humillaciones == 1 |
        amenazas_abandono == 1 |
        amenazas_hijos == 1 |
        romper_objetos == 1 |
        amenaza_economica == 1 |
        control_economico == 1 |
        amenaza_sex == 1 |
        amenaza_muerte == 1 ~ 1,
      insultos == 0 & encierros == 0 & humillaciones == 0 &
        amenazas_abandono == 0 & amenazas_hijos == 0 &
        romper_objetos == 0 & amenaza_economica == 0 &
        control_economico == 0 & amenaza_sex == 0 &
        amenaza_muerte == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

indicator3_area <- diseño_indicator3 %>%
  group_by(dam, area) %>%
  summarise(
    indicator_3 = survey_mean(violencia_pareja_ult12m, vartype = "cv", na.rm = TRUE)*100
  )

indicator3_etnia <- diseño_indicator3 %>%
  group_by(dam, etnia) %>%
  summarise(
    indicator_3 = survey_mean(violencia_pareja_ult12m, vartype = "cv", na.rm = TRUE)*100
  )

indicator3_anoest <- diseño_indicator3 %>%
  group_by(dam, anoest) %>%
  summarise(
    indicator_3 = survey_mean(violencia_pareja_ult12m, vartype = "cv", na.rm = TRUE)*100
  )
