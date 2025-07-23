#################################################
#             Proyecto : AWPunfpa2025           #
#       Lectura y manipulación  bases - COL     #
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
library(labelled)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <- file.path(b_path, "src")



# --------------------------------------#
#            Woman module               #
# --------------------------------------#

base_mujeres <- read_dta(file.path(input, "COL","COIR72FL.DTA"))
dictionary <- labelled::generate_dictionary(base_mujeres)

base_muj <- base_mujeres %>% select(
  
  v102,         # area: urbana/rural
  v012,         # Edad de la mujer
  v106,         # Años de educación
  v131,         # Grupo étnico
  v501,         # Estado civil
  v502,         # Estado civil pasado y actual
  v022,         # Estrato muestral
  v021,         # UPM
  v005,         # Ponderador de mujer
  v012,         # Edad de la mujer actual
  sdepto,       # Departamento
  
  # =============================
  # Violencia de pareja últimos 12 meses
  # =============================
  
  d105a,  # Empujada o zarandeada
  d105b,  # Golpeada con la mano
  d105c,  # Golpeada con un objeto
  d105d,  # Pateada o arrastrada
  d105g,  # Atacada con arma
  d105e,  # Estrangulada o quemada
  
  d105h,  # Forzada a tener relaciones sexuales no deseadas
  
  d101a,  # Celos si habla con otros hombres
  d101b,  # Acusación de infidelidad
  d101c,  # No le permite ver amigas
  d101d,  # Limita contacto con familia
  d101e,  # Controla dónde está
  d101g,  # La ignora
  d101i,  # No le consulta decisiones importantes
  d103a,  # Humillación
  d103b,  # Amenaza con daño físico
  d103c,  # Insultos o hacerla sentir mal
  d103d,  # Amenaza con dejarla
  d103e,  # Amenaza con quitarle los hijos
  
  # =============================
  # Variables de autonomía y decisiones
  # =============================
  v511,               # Edad a la primera unión o convivencia
  v632,               # ¿Quién tomó la decisión de usar anticonceptivos?
  s1243,              # Fue forzada por su pareja a tener relaciones sexuales
  v743a,              # Quién decide sobre el cuidado de su salud
  v326                #Fuente más reciente del método anticonceptivo utilizado por mujeres usuarias actuales.
)

save(base_muj, file = file.path(output, "COL", "ENDS_modulo_mujeres_2025.RData"))

# --------------------------------------#
#           Verification                #
# --------------------------------------#

base_muj_ve <- base_muj %>%
  mutate(
    dec_salud = case_when(
      v743a %in% 1:3 ~ 1,  # Mujer decide sola o con alguien más
      v743a %in% c(0,4,5,6) ~ 0,  # No decide o decide otra persona
      TRUE ~ NA_real_     # NA para los que no respondieron
    )
  )

base_muj_ve$v326_lbl <- to_factor(base_muj_ve$v326)

design <- svydesign(ids = ~v021,strata = ~v022,weights = ~v005,data = base_muj_ve,nest = TRUE
)

svymean(~dec_salud, design, na.rm = TRUE)

svymean(~v326_lbl, design, na.rm = TRUE)
