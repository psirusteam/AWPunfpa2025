#################################################
#             Proyecto : AWPunfpa2025           #
#       Lectura y manipulación  bases - BOL     #
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

base_mujeres <- read_sav(file.path(input, "BOL/EDSA2023_Mujer.sav")) %>%
  select(
    # Identificación y muestreo
    folio,           # Folio del hogar
    nro,             # Número de orden de la mujer
    upm,             # Unidad primaria de muestreo
    estrato,         # Estrato
    ponderadorm,     # ponderador
    departamento,    # departamento
    region,          #region
    area,            #area
    
    # Características sociodemográficas
    ms01_0101a,         # Edad (años cumplidos)
    ms01_0108,          # Pertenece a nación o pueblo indígena/campesino/afro
    ms01_0108_npiocs,   # Nombre del pueblo o nación
    aestudio,           # Años de estudio
    econyugd_m,         # Estado conyugal
    ms01_0101b_3,       # año de nacimiento
    
    # Autonomía en decisiones
    ms07_0720,          # Decisión sobre anticoncepción
    ms08_0825_A,        # Decisión sobre cuidado de salud
    
    # Salud sexual y reproductiva
    ms06_0610,          # Edad primera relación sexual
    ms06_0610_b,        # Si tuvo consentimiento
    ms06_0608,          # Edad al irse a vivir con su esposo o compañero
    ms06_0607_02_2,     #Año en que se fue a vivir con su primer esposo o compañero
    ms06_0607_01_2,     #Año en que se fue a vivir con su esposo o compañero
    ms06_0602,          #Alguna vez ha tenido vpareja
    
    # Violencia psicológica y económica (últimos 12 meses)
    ms11_1105_B,        # Insultos
    ms11_1105_C,        # Encierros
    ms11_1105_D,        # Humillaciones
    ms11_1105_E,        # Amenazas de abandono
    ms11_1105_F,        # Amenazas con quitarle hijos
    ms11_1105_G,        # Romper objetos como amenaza
    ms11_1105_H,        # Amenaza de no cumplir con responsabilidad económica
    ms11_1105_I,        # Control económico / restricción de libertad
    ms11_1105_J,        # Amenaza con publicar contenido sexual
    ms11_1105_K         # Amenaza con matarla
  )

sum(base_mujeres$ponderadorm)


### Contrucción y comparación indicadores ya existentes en el anexo técnico ###

# -----------------------------------------------------------------#
#            Mujeres de 14 a 49 años , según situación conyugal    #
# -----------------------------------------------------------------#


base_mujeres <- base_mujeres %>%
  mutate(econyugd_factor = as_factor(econyugd_m))

diseno_mujeres <- base_mujeres %>%
  as_survey_design(
    ids = upm,
    strata = estrato,
    weights = ponderadorm,
    nest = TRUE
  )

diseno_m12_49 <- diseno_mujeres %>%
  filter(ms01_0101a >= 15, ms01_0101a <= 49)


resultado_conyugal <- diseno_m12_49 %>%
  group_by(econyugd_factor) %>%
  summarize(porcentaje = survey_mean(proportion = TRUE, vartype = NULL) * 100) %>%
  ggplot(aes(x = reorder(econyugd_factor, porcentaje), y = porcentaje)) +
  geom_col(fill = "#6baed6") +
  geom_text(aes(label = round(porcentaje, 1)), hjust = -0.1) +
  coord_flip() +
  labs(x = NULL, y = "Porcentaje (%)") +
  theme_minimal()

ggsave(filename = file.path(output, "BOL/img/situacion_conyugal_mujeres_15_49.png"),
       width = 7, height = 5, dpi = 300)

save(base_mujeres, file = file.path(output, "BOL/EDSA_modulo_mujeres_2023.RData"))


base_mujeres %>% distinct(ms06_0607_02_2, ms01_0101b_3, edad_union1) %>%  View() 
