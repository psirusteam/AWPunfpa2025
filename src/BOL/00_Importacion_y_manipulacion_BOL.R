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
    
    # Características sociodemográficas
    ms01_0101a,         # Edad (años cumplidos)
    ms01_0108,          # Pertenece a nación o pueblo indígena/campesino/afro
    ms01_0108_npiocs,   # Nombre del pueblo o nación
    aestudio,           # Años de estudio
    econyugd_m,         # Estado conyugal
    
    # Autonomía en decisiones
    ms07_0720,          # Decisión sobre anticoncepción
    ms08_0825_A,        # Decisión sobre cuidado de salud
    
    # Salud sexual y reproductiva
    ms06_0610,          # Edad primera relación sexual
    ms06_0610_b,        # Si tuvo consentimiento
    ms06_0608,          # Edad al irse a vivir con su esposo o compañero
    
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

diseno_mujeres <- svydesign(
  id = ~upm,
  strata = ~estrato,
  weights = ~ponderadorm,
  data = base_mujeres,
  nest = TRUE
)
