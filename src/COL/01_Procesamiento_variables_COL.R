#################################################
#             Proyecto : AWPunfpa2025           #
#       Procesamiento indicadores - COL         #
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

load(file.path(output, "COL/ENDS_modulo_mujeres_2025.RData"))


################################################################################
###---------------------------- Variables standardization--------------------###
################################################################################

base_muj <- base_muj %>%
  mutate(
    anoest  =  v106,
    
    edad = case_when(
      v012 < 15 ~ "1",
      v012 < 30 ~ "2",
      v012 < 45 ~ "3",
      v012 < 65 ~ "4",
      TRUE~"5"),
    
    dam = sdepto,
    
    area = v102,
    
    etnia = case_when(
      v131 == 1 ~ "1",         #Indigena
      v131 == 5 ~ "2",         #Afro
      v131 %in% c("2","3","4")  ~ "3", #Otros
      TRUE ~ "4"),             #Ninguno
    
    #Unión antes de los 18 años
    
    edad_union = v511,
    
    union18 = case_when(
      !is.na(edad_union) & edad_union < 18 ~ 1,
      TRUE ~ 0), 
    
    # Alguna vez casada o unida 
    
    par_12m = case_when(
      v502 %in% c(1,2) | v501 %in% c(1,2,3,4,5) ~ 1,
      TRUE ~ 0),
    
    
    violencia = case_when(
      par_12m == 1 &
        (
          d105a == 1 |d105b == 1 |d105c == 1 |d105d == 1 |d105e == 1 |
          d105g == 1 |d105h == 1 |d101a == 1 |d101b == 1 |d101c == 1 |
          d101d == 1 |d101e == 1 |d101g == 1 |d101i == 1 |d103a == 1 |
          d103b == 1 |d103c == 1 |d103d == 1 |d103e == 1
        ) ~ 1,
      TRUE ~ 0
    )
    
  )


################################################################################
###---------------------------- Indicators ----------------------------------###
################################################################################  
options(survey.lonely.psu="adjust")

### Proporción de mujeres de 20-24 años que estuvieron casadas o en 
### unión antes de los 15 o 18 años.

universo_indicador_1 <- base_muj %>% filter(v012 >= 20 & v012 <= 24)

design1 <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~v005,
  data = universo_indicador_1,
  nest = TRUE
)

# ---- Total ----
indicador1_total <- svyby(~union18, ~dam, design1, svymean, na.rm = TRUE) %>%
  mutate(cv = (se / union18) * 100)
saveRDS(indicador1_total, file.path(output, "COL/indicator1_total.rds"))

# ---- Area ----
indicador1_area <- svyby(~union18, ~interaction(dam, area), design1, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, area)`, into = c("dam", "area"), sep = "\\.") %>%
  mutate(cv = (se / union18) * 100)
saveRDS(indicador1_area, file.path(output, "COL/indicator1_area.rds"))

# ---- Etnia ----
indicador1_etnia <- svyby(~union18, ~interaction(dam, etnia), design1, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, etnia)`, into = c("dam", "etnia"), sep = "\\.") %>%
  mutate(cv = (se / union18) * 100)
saveRDS(indicador1_etnia, file.path(output, "COL/indicator1_etnia.rds"))

# ---- Año de estudio ----
indicador1_anoest <- svyby(~union18, ~interaction(dam, anoest), design1, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, anoest)`, into = c("dam", "anoest"), sep = "\\.") %>%
  mutate(cv = (se / union18) * 100)
saveRDS(indicador1_anoest, file.path(output, "COL/indicator1_anoest.rds"))



### Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna 
### vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja 
## actual o anterior en los últimos 12 meses (15-49 años)

universo_indicador_3 <- base_muj %>% filter(par_12m == 1 & v012 >= 15 )

# Crear diseño muestral
design3 <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~v005,
  data = universo_indicador_3,
  nest = TRUE
)


# ---- Total ----
indicador3_total <- svyby(~violencia, ~dam, design3, svymean, na.rm = TRUE) %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_total, file.path(output, "COL/indicator3_total.rds"))

# ---- Area ----
indicador3_area <- svyby(~violencia, ~interaction(dam, area), design3, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, area)`, into = c("dam", "area"), sep = "\\.") %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_area, file.path(output, "COL/indicator3_area.rds"))

# ---- Etnia ----
indicador3_etnia <- svyby(~violencia, ~interaction(dam, etnia), design3, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, etnia)`, into = c("dam", "etnia"), sep = "\\.") %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_etnia, file.path(output, "COL/indicator3_etnia.rds"))

# ---- Año de estudio ----
indicador3_anoest <- svyby(~violencia, ~interaction(dam, anoest), design3, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, anoest)`, into = c("dam", "anoest"), sep = "\\.") %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_anoest, file.path(output, "COL/indicator3_anoest.rds"))

# ---- Edad ----
indicador3_edad <- svyby(~violencia, ~interaction(dam, edad), design3, svymean, na.rm = TRUE) %>%
  separate(`interaction(dam, edad)`, into = c("dam", "edad"), sep = "\\.") %>%
  mutate(cv = (se / violencia) * 100)
saveRDS(indicador3_edad, file.path(output, "COL/indicator3_edad.rds"))
