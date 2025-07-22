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
    
    dam2 = NOM_ENT,
    
    etnia = case_when(
      P2_10 %in% 1:2 ~ "1",      #Indigena
      TRUE ~ "2"),          #Ninguno

    #Unión antes de los 18 años
    
    edad_union = P13_15C,
    
    union18 = case_when(
      !is.na(edad_union) & edad_union < 18 & EDAD >= 20 & EDAD <= 24 ~ 1,
      is.na(edad_union) | EDAD < 20 | EDAD > 24 ~ NA_real_,
      TRUE ~ 0
    ), 
    
    # Quien decide cuando tener sexo
    dec_sex = case_when(
      P15_1AB_12 %in% c("1","4") ~ 1,
      P15_1AB_12 %in% c("2","3","5","6","7") ~ 0,
      TRUE ~ NA_real_),
    
    # Quien decide de métodos anticonceptivos
    dec_met = case_when(
      P15_1AB_13 %in% c("1","4","5") ~ 1,
      P15_1AB_13 %in% c("2","3","6","7") ~ 0,
      TRUE ~ NA_real_),
    
    # Decisión salud reproductiva y sexual
    dec_sal = case_when(
      P15_1AB_14 %in% c("1","4","5") ~ 1,
      P15_1AB_14 %in% c("2","3","6","7") ~ 0,
      TRUE ~ NA_real_),
    
    # Variable combinada: autonomía en las tres decisiones
    dec_autonomia = case_when(
      dec_sex == 1 & dec_met == 1 & dec_sal == 1 ~ 1,
      !is.na(dec_sex) & !is.na(dec_met) & !is.na(dec_sal) ~ 0,
      TRUE ~ 0
    ),
    
    
    par_12m = case_when(
      T_INSTRUM %in% c("A1", "A2", "B1", "B2") ~ 1,
      P13_C_1 %in% c("1", "2") ~ 1,
      P13_C_1 == "3" ~ 0,
      TRUE ~ NA_real_
    ),
    
    violencia = case_when(
      par_12m == 1 &
      P14_3_1 %in% c("1","2","3") |
        P14_3_2 %in% c("1","2","3") |
        P14_3_3 %in% c("1","2","3") |
        P14_3_4 %in% c("1","2","3") |
        P14_3_5 %in% c("1","2","3") |
        P14_3_6 %in% c("1","2","3") |
        P14_3_7 %in% c("1","2","3") |
        P14_3_8 %in% c("1","2","3") |
        P14_3_9 %in% c("1","2","3") |
        P14_3_10 %in% c("1","2","3") |
        P14_3_11 %in% c("1","2","3") |
        P14_3_12 %in% c("1","2","3") |
        P14_3_13 %in% c("1","2","3") |
        P14_3_14 %in% c("1","2","3") |
        P14_3_15 %in% c("1","2","3") |
        P14_3_16 %in% c("1","2","3") |
        P14_3_17 %in% c("1","2","3") |
        P14_3_18 %in% c("1","2","3") |
        P14_3_19 %in% c("1","2","3") |
        P14_3_20 %in% c("1","2","3") |
        P14_3_21 %in% c("1","2","3") |
        P14_3_22 %in% c("1","2","3") |
        P14_3_23AB %in% c("1","2","3") |
        P14_3_24AB %in% c("1","2","3") |
        P14_3_25 %in% c("1","2","3") |
        P14_3_26 %in% c("1","2","3") |
        P14_3_27 %in% c("1","2","3") |
        P14_3_28 %in% c("1","2","3") |
        P14_3_29 %in% c("1","2","3") |
        P14_3_30 %in% c("1","2","3") |
        P14_3_31 %in% c("1","2","3") |
        P14_3_32 %in% c("1","2","3") |
        P14_3_33 %in% c("1","2","3") |
        P14_3_34 %in% c("1","2","3") |
        P14_3_35AB %in% c("1","2","3") |
        P14_3_36AB %in% c("1","2","3") |
        P14_3_37AB %in% c("1","2","3") |
        P14_3_38AB %in% c("1","2","3") ~ 1,
      TRUE ~ 0
    )
    
  )
    
    
################################################################################
###---------------------------- Indicators ----------------------------------###
################################################################################  
options(survey.lonely.psu="adjust")

### Proporción de mujeres de 20-24 años que estuvieron casadas o en 
### unión antes de los 15 o 18 años.

universo_indicador_1 <- base_modelomuj %>% filter(EDAD >= 20 & EDAD <= 24)

design1 <- svydesign(
  ids = ~UPM_DIS,
  strata = ~EST_DIS,
  weights = ~FAC_MUJ,
  data = universo_indicador_1,
  nest = TRUE
)
# ---- Total ----
indicador1_total <- svyby(~union18, ~dam, design1, svymean, na.rm = TRUE) %>%
  mutate(cv = (se / union18) * 100)
saveRDS(indicador1_total, file.path(output, "MEX/indicator1_total.rds"))

# ---- Etnia ----
indicador1_etnia <- svyby(~union18, ~interaction(dam, etnia), design1, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, etnia)`, into = c("dam", "etnia"), sep = "\\.") %>%
  mutate(cv = (se / union18) * 100)
saveRDS(indicador1_etnia, file.path(output, "MEX/indicator1_etnia.rds"))

# ---- Año de estudio ----
indicador1_anoest <- svyby(~union18, ~interaction(dam, anoest), design1, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, anoest)`, into = c("dam", "anoest"), sep = "\\.") %>%
  mutate(cv = (se / union18) * 100)
saveRDS(indicador1_anoest, file.path(output, "MEX/indicator1_anoest.rds"))


### Proporción de mujeres de 15-49 años que toman sus propias decisiones informadas 
### sobre relaciones sexuales, uso de anticonceptivos y atención en salud reproductiva. 

universo_indicador_2 <- base_modelomuj %>% filter(EDAD >= 15 & EDAD <= 49 & par_12m == 1)

design2 <- svydesign(
  ids = ~UPM_DIS,
  strata = ~EST_DIS,
  weights = ~FAC_MUJ,
  data = universo_indicador_2,
  nest = TRUE
)

# ---- Total ----
indicador2_total <- svyby(~dec_autonomia, ~dam, design2, svymean, na.rm = TRUE) %>%
  mutate(cv = (se / dec_autonomia) * 100)
saveRDS(indicador2_total, file.path(output, "MEX/indicator2_total.rds"))

# ---- Etnia ----
indicador2_etnia <- svyby(~dec_autonomia, ~interaction(dam, etnia), design2, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, etnia)`, into = c("dam", "etnia"), sep = "\\.") %>%
  mutate(cv = (se / dec_autonomia) * 100)
saveRDS(indicador2_etnia, file.path(output, "MEX/indicator2_etnia.rds"))

# ---- Año de estudio ----
indicador2_anoest <- svyby(~dec_autonomia, ~interaction(dam, anoest), design2, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, anoest)`, into = c("dam", "anoest"), sep = "\\.") %>%
  mutate(cv = (se / dec_autonomia) * 100)
saveRDS(indicador2_anoest, file.path(output, "MEX/indicator2_anoest.rds"))

# ---- Edad ----
indicador2_edad <- svyby(~dec_autonomia, ~interaction(dam, edad), design2, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, edad)`, into = c("dam", "edad"), sep = "\\.") %>%
  mutate(cv = (se / dec_autonomia) * 100)
saveRDS(indicador2_edad, file.path(output, "MEX/indicator2_edad.rds"))
### Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna 
### vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja 
## actual o anterior en los últimos 12 meses (15-49 años)

universo_indicador_3 <- base_modelomuj %>% filter(par_12m == 1)

# Crear diseño muestral
design3 <- svydesign(
  ids = ~UPM_DIS,
  strata = ~EST_DIS,
  weights = ~FAC_MUJ,
  data = universo_indicador_3,
  nest = TRUE
)


# ---- Total ----
indicador3_total <- svyby(~violencia, ~dam, design3, svymean, na.rm = TRUE) %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_total, file.path(output, "MEX/indicator3_total.rds"))

# ---- Etnia ----
indicador3_etnia <- svyby(~violencia, ~interaction(dam, etnia), design3, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, etnia)`, into = c("dam", "etnia"), sep = "\\.") %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_etnia, file.path(output, "MEX/indicator3_etnia.rds"))

# ---- Año de estudio ----
indicador3_anoest <- svyby(~violencia, ~interaction(dam, anoest), design3, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, anoest)`, into = c("dam", "anoest"), sep = "\\.") %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_anoest, file.path(output, "MEX/indicator3_anoest.rds"))

# ---- Edad ----
indicador3_edad <- svyby(~violencia, ~interaction(dam, edad), design3, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, edad)`, into = c("dam", "edad"), sep = "\\.") %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_edad, file.path(output, "MEX/indicator3_edad.rds"))