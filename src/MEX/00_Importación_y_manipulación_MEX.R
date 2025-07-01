#########################################################
# Lectura y preparación de las bases de datos - MEX     #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <- file.path(b_path, "src")
# --------------------------------------#
#            Módulo mujeres             #
# --------------------------------------#

base_mujeres<- read.csv(file.path(input, "MEX/TMUJER2.csv"), encoding = "UTF-8") #Módulo de mujeres
base_mujeres2 <- base_mujeres %>% select(

  # Identificación de la persona y del hogar
 
  upm,        # Unidad Primaria de Muestreo
  viv_sel,    # Vivienda seleccionada
  hogar,      # Número de hogar en la vivienda
  n_ren,      # Número de renglón (persona en el hogar)
  llave_hog,  # Identificador único del hogar
  llave_viv,  # Identificador único de la vivienda
  llave_muj,  #Identificador mujer (15 a 54 años)
  ent,        #Entidad federativa
  tam_loc,    #Tamaño de localidad

  # Diseño muestral y expansión

  fac_mod,    # Factor de expansión por persona(mujer)
  t_loc_ur,   # Localidad ubrano - rural
  estrato,    # Estrato sociodemográfico
  est_dis,    # Estrato de diseño muestral
  upm_dis,    # UPM de diseño muestral

  # Características

  edad_muj,       # Edad de la mujer
  cond_act,   # Condición actividad
  niv,        #nivel de escolaridad
  edprmat,    #edad primer matrimonio
  edpruni,    #edad primera unión
  p10_1,      #situacion conyugal actual
  p10_8,      #uniones anteriores

  # Salud reproductiva

  p9_3,       # Recibió atención preconcepcional
  p9_5_1,     # Tiempo de inicio de revisión prenatal
  repretrim,  # Tuvo revisión prenatal en el primer trimestre
  trevpren,   # Número total de revisiones prenatales
  conrevpre,  # Condición de revisión prenatal
  conrevpos,  # Condición de revisión posparto
  conrevpri,  # Revisión en el primer año de vida del infante
  p9_26,      # Está lactando actualmente
  p9_34,      # Si deseaba el embarazo (último hijo nacido vivo)
  
)

sum(base_mujeres2$fac_mod)#37.964.542 mujeres de 15 a 54 años de edad

diseno_mujeres <- base_mujeres2 %>%
  as_survey_design(
    ids = upm_dis,         # UPM de diseño muestral
    strata = est_dis,      # Estrato de diseño
    weights = fac_mod,     # Factor de expansión del módulo de mujer
    nest = TRUE
  )















TSDEM      <- read.csv(file.path(input, "MEX/TSDEM.csv"), encoding = "UTF-8")#Características sociodemográficas


sum(ENADID_MEX$fac_mod, na.rm = TRUE)



colnames(TMUJER2)
