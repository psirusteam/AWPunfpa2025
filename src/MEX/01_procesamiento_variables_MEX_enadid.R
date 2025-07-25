#################################################
#             Proyecto : AWPunfpa2025           #
#       Provesamiento indicadoeres - MEX        #
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

load(file.path(output, "MEX/ENADID_modulo_mujeres_2023.RData"))

################################################################################
###---------------------------- Variables standardization--------------------###
################################################################################

base_modelomuj <- base_modelomuj %>%
  mutate(
    anoest  = case_when(
      edad_muj < 15 | niv == -1 ~ "98",                                # No aplica
      niv == 99 | (edad_muj >= 15 & is.na(niv)) ~ "99",                # No especificado / NSNR
      niv == 0 ~ "1",                                                  # Ninguno
      niv %in% 1:2 ~ "2",                                              # Preescolar o primaria
      niv %in% 3:7 ~ "3",                                              # Secundaria, técnica con secundaria o media
      niv %in% 8:11 ~ "4",                                             # Educación superior: licenciatura o más
      TRUE ~ "Error"
    ),
    
    edad = case_when(
      edad_muj < 15 ~ "1",
      edad_muj < 30 ~ "2",
      edad_muj < 45 ~ "3",
      edad_muj < 65 ~ "4",
      TRUE~"5"),
    
    dam = ent,
    
    area = t_loc_ur,
    
    etnia = case_when(
      p3_7 == 1 ~ "1",      #Afro
      p3_12 == 1 ~ "2",     #indigena
      TRUE ~ "3"),          #otros
    
    #Unión antes de los 15 y 18 años
    union15 = case_when(
      !is.na(edpruni) & edpruni < 15 ~ 1,
      TRUE ~ 0),
    
    union18 = case_when(
      !is.na(edpruni) & edpruni < 18 ~ 1,
      TRUE ~ 0), 
    
    # Consentimiento en la primera relación sexual
    consentim_sex = case_when(
      p8_40 == 1 ~ 1,
      p8_40 %in% c(2,9) ~ 0,
      TRUE ~ NA_real_),
    
    # Conocimiento de métodos anticonceptivos
    conoc_met = case_when(
      conoce %in% c(1,2,3,4,5) ~ 1,
      conoce == 6 ~ 0,
      TRUE ~ NA_real_),
    
    # Uso actual de método anticonceptivo
    usa_met = case_when(
      p8_10 == 1 ~ 1,
      p8_10 == 2 ~ 0,
      TRUE ~ NA_real_),
    
    # Decisión de usar método anticonceptivo
    dec_met = case_when(
      p8_19 == 3 ~ 1,
      p8_19 %in% c(1, 2, 4, 5,8,9) ~ 0,
      TRUE ~ NA_real_),
    
    
    # Casada o en unión libre actualmente
    esta_unida = case_when(
      p10_1 %in% c(1, 7) ~ 1,  
      p10_1 %in% c(2,3,4,5,6,8) ~ 0,
      TRUE ~ NA_real_),
    
    ## Decisión sobre número de hijos
    dec_hijos = case_when(
      p7_16 == 1 ~ 1,                         # Decidió ella
      p7_16 %in% c(2,3,4,9) ~ 0,              # Decidió otra persona
      TRUE ~ NA_real_
    )
    
  )


################################################################################
###---------------------------- Indicators ----------------------------------###
################################################################################  
options(survey.lonely.psu = "adjust")

diseño_mujeres <- base_modelomuj %>%
  as_survey_design(
    ids = upm_dis,
    strata = est_dis,
    weights = fac_mod,
    nest = TRUE
  )


### Proporción de mujeres de 20-24 años que estuvieron casadas o en 
### unión antes de los 15 o 18 años.

diseño_indicador1 <- diseño_mujeres %>%
  filter(edad_muj >= 20 & edad_muj <= 24) 

indicator1_area <- diseño_indicador1 %>%
  group_by(dam) %>%
  summarise(
    prop_antes_18 = survey_mean(union18, vartype = "se", na.rm = TRUE)*100
  )

indicador1_total <- svyby(~union18, ~dam, diseño_indicador1, svymean, na.rm = TRUE)
