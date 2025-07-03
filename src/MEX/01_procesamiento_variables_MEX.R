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
      edad_muj < 5 | niv == -1 ~ "98",                                # No aplica
      niv == 99 | (edad_muj >= 5 & is.na(niv)) ~ "99",                # No especificado / NSNR
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
    
    edad_union <- pmin(edpruni, edprmat, na.rm = TRUE), 
    
    union15 = case_when(
      !is.na(edad_union) & edad_union < 15 ~ 1,
      is.na(edad_union) ~ NA_real_,
      TRUE ~ 0),
    
    union18 = case_when(
      !is.na(edad_union) & edad_union < 18 ~ 1,
      is.na(edad_union) ~ NA_real_,
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
  group_by(dam, area) %>%
  summarise(
    prop_antes_18 = survey_mean(union18, vartype = "se", na.rm = TRUE)*100
  )

indicator1_etnia <- diseño_indicador1 %>%
  group_by(dam, etnia) %>%
  summarise(
    prop_antes_18 = survey_mean(union18, vartype = "se", na.rm = TRUE)*100
  )

indicator1_anoest <- diseño_indicador1 %>%
  group_by(dam, anoest) %>%
  summarise(
    prop_antes_18 = survey_mean(union18, vartype = "se", na.rm = TRUE)*100
  )


### Proporción de mujeres de 15-49 años que toman sus propias decisiones informadas 
### sobre relaciones sexuales, uso de anticonceptivos y atención en salud reproductiva. 

diseño_indicator2 <- diseño_mujeres %>%
  filter(edad_muj >= 15 & edad_muj <= 49, esta_unida == 1) %>%
  mutate(
    indicator_2 = case_when(
      dec_hijos == 1 & dec_met == 1 & consentim_sex == 1 ~ 1,
      is.na(dec_hijos) | is.na(dec_met) | is.na(consentim_sex) ~ NA_real_,
      TRUE ~ 0
    )
  )

indicator2_area <- diseño_indicator2 %>%
  group_by(dam, area) %>%
  summarise(
    indicator_2 = survey_mean(indicator_2, vartype = "se", na.rm = TRUE)*100
  )

indicator2_etnia <- diseño_indicator2 %>%
  group_by(dam, etnia) %>%
  summarise(
    indicator_2 = survey_mean(indicator_2, vartype = "se", na.rm = TRUE)*100
  )

indicator2_anoest <- diseño_indicator2 %>%
  group_by(dam, anoest) %>%
  summarise(
    indicator_2 = survey_mean(indicator_2, vartype = "se", na.rm = TRUE)*100
  )



### Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna 
### vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja 
## actual o anterior en los últimos 12 meses (15-49 años)




