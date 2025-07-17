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

load(file.path(output, "MEX/ENDIREH_modulo_mujeres_2021.RData"))

################################################################################
###---------------------------- Variables standardization--------------------###
################################################################################

base_modelomuj <- base_unida %>%
  mutate(
    anoest  =  case_when(
      NIV == "00" ~ "1",  # Ninguno
      NIV %in% c("01", "02") ~ "2",  # Preescolar o primaria
      NIV %in% c("03", "04", "05", "06", "07", "08", "09") ~ "3",  # Secundaria, preparatoria, técnicos o normales
      NIV %in% c("10", "11") ~ "4"   # Educación superior (licenciatura o posgrado)
    ),
    
    edad = case_when(
      EDAD < 15 ~ "1",
      EDAD < 30 ~ "2",
      EDAD < 45 ~ "3",
      EDAD < 65 ~ "4",
      TRUE~"5"),
    
    dam = CVE_ENT,
    
    etnia = case_when(
      P2_10 %in% 1:2 ~ "1",      #Indigena
      TRUE ~ "2"),          #otros

    #Unión antes de los 18 años
    
    edad_union = P13_15C,
    
    union18 = case_when(
      !is.na(edad_union) & edad_union < 18 ~ 1,
      is.na(edad_union) ~ NA_real_,
      TRUE ~ 0), 
    
    # Quien decide cuando tener sexo
    dec_sex = case_when(
      P15_1C_12 %in% c("1","7") ~ 1,
      P15_1C_12 %in% c("2","3","4","5","6","8") ~ 0,
      TRUE ~ NA_real_),
    
    # Quien decide de métodos anticonceptivos
    dec_met = case_when(
      P15_1C_13  %in% c("1","7") ~ 1,
      P15_1C_13 %in% c("2","3","4","5","6","7","8") ~ 0,
      TRUE ~ NA_real_),
    
    # Decisión salud reproductiva y sexual
    dec_sal = case_when(
      P15_1C_14 %in% c("1","7") ~ 1,
      P15_1C_14 %in% c("2","3","4","5","6","7","8") ~ 0,
      TRUE ~ NA_real_),
    
    #Algun tipo de violencia
    
    violencia = case_when(
      rowSums(across(
        starts_with("P14_3_"), 
        ~ .x %in% c(1, 2, 3)
      ), na.rm = TRUE) > 0 ~ 1,
      
      rowSums(across(
        starts_with("P14_3_"), 
        ~ .x %in% c(0, 4)
      ), na.rm = TRUE) == length(select(., starts_with("P14_3_"))) ~ 0,
      
      TRUE ~ NA_real_
    )
    
  )
    
    
################################################################################
###---------------------------- Indicators ----------------------------------###
################################################################################  
options(survey.lonely.psu = "adjust")

diseño_mujeres <- base_modelomuj %>%
  as_survey_design(
    ids = UPM_DIS,
    strata = EST_DIS,
    weights = FAC_MUJ,
    nest = TRUE
  )


### Proporción de mujeres de 20-24 años que estuvieron casadas o en 
### unión antes de los 15 o 18 años.

diseño_indicador1 <- diseño_mujeres %>%
  filter(EDAD >= 20 & EDAD <= 24) 

indicator1_total <- diseño_indicador1 %>%
  group_by(dam) %>%
  summarise(
    prop_antes_18 = survey_mean(union18, vartype = "cv", na.rm = TRUE)
  )


saveRDS(indicator1_total, file.path(output, "MEX/indicator1_total.rds"))


indicator1_edad <- diseño_indicador1 %>%
  group_by(dam, edad) %>%
  summarise(
    prop_antes_18 = survey_mean(union18, vartype = "cv", na.rm = TRUE)
  )

saveRDS(indicator1_edad, file.path(output, "MEX/indicator1_edad.rds"))

indicator1_etnia <- diseño_indicador1 %>%
  group_by(dam, etnia) %>%
  summarise(
    prop_antes_18 = survey_mean(union18, vartype = "cv", na.rm = TRUE)
  )

saveRDS(indicator1_etnia, file.path(output, "MEX/indicator1_etnia.rds"))

indicator1_anoest <- diseño_indicador1 %>%
  group_by(dam, anoest) %>%
  summarise(
    prop_antes_18 = survey_mean(union18, vartype = "cv", na.rm = TRUE)
  )

saveRDS(indicator1_anoest, file.path(output, "MEX/indicator1_anoest.rds"))

### Proporción de mujeres de 15-49 años que toman sus propias decisiones informadas 
### sobre relaciones sexuales, uso de anticonceptivos y atención en salud reproductiva. 

diseño_indicator2 <- diseño_mujeres %>%
  filter(EDAD >= 15 & EDAD <= 49) %>%
  mutate(
    indicator_2 = case_when(
      dec_sex == 1 & dec_met == 1 & dec_sal == 1 ~ 1,
      is.na(dec_sex) | is.na(dec_met) | is.na(dec_sal) ~ NA_real_,
      TRUE ~ 0
    )
  )

indicator2_total <- diseño_indicator2 %>%
  group_by(dam) %>%
  summarise(
    indicator_2 = survey_mean(indicator_2, vartype = "ci", na.rm = TRUE)
  )

indicator2_area <- diseño_indicator2 %>%
  group_by(dam, area) %>%
  summarise(
    indicator_2 = survey_mean(indicator_2, vartype = "ci", na.rm = TRUE)*100
  )

indicator2_etnia <- diseño_indicator2 %>%
  group_by(dam, etnia) %>%
  summarise(
    indicator_2 = survey_mean(indicator_2, vartype = "ci", na.rm = TRUE)*100
  )

indicator2_anoest <- diseño_indicator2 %>%
  group_by(dam, anoest) %>%
  summarise(
    indicator_2 = survey_mean(indicator_2, vartype = "ci", na.rm = TRUE)*100
  )



### Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna 
### vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja 
## actual o anterior en los últimos 12 meses (15-49 años)




