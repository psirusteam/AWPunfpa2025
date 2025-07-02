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
unique(base_mujeres2$niv)


base_mujeres2 <- base_mujeres2 %>%
  mutate(
    anoest  = case_when(
      edad_muj < 15 | niv == -1 ~ "98",                                # No aplica
      niv == 99 | (edad_muj >= 15 & is.na(niv)) ~ "99",                # No especificado / NSNR
      niv == 0 ~ "1",                                              # Ninguno
      niv %in% 1:2 ~ "2",                                          # Preescolar o primaria
      niv %in% 3:7 ~ "3",                                          # Secundaria, técnica con secundaria o media
      niv %in% 8:11 ~ "4",                                         # Educación superior: licenciatura o más
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
      TRUE ~ "3"            #Otros
    )
  )
    
    
################################################################################
###---------------------------- Indicators ----------------------------------###
################################################################################  

### Proporción de mujeres y niñas de 15 años o más que hayan tenido pareja alguna 
### vez y hayan sufrido violencia física, sexual o psicológica por parte de una pareja 
## actual o anterior en los últimos 12 meses (15-49 años)

