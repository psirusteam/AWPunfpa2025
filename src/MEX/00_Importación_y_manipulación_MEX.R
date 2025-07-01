#################################################
#             Proyecto : AWPunfpa2025           #
#       Lectura y manipulación  bases - MEX     #
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
  gra,        #grado aprobado
  p10_1,      #situacion conyugal actual
  p10_8,      #uniones anteriores
  p3_19,      #Condición de habla indigena

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

base_pers <- read.csv(file.path(input, "MEX/TSDEM.csv"), encoding = "UTF-8") %>%
  select(
    upm,           # Unidad primaria de muestreo
    viv_sel,       # Vivienda seleccionada
    hogar,         # Hogar
    n_ren,         # Número de renglón (identificador de persona)
    p3_7,          # Se reconoce como afrodescendiente
    p3_12          # Habla lengua indígena
  )

base_mujeres2 <- base_mujeres2 %>%
  left_join(base_pers, by = c("upm", "viv_sel", "hogar", "n_ren"))

sum(base_mujeres2$fac_mod)#37.964.542 mujeres de 15 a 54 años de edad

diseño_mujeres <- base_mujeres2 %>%
  as_survey_design(
    ids = upm_dis,         # UPM de diseño muestral
    strata = est_dis,      # Estrato de diseño
    weights = fac_mod,     # Factor de expansión del módulo de mujer
    nest = TRUE
  )

summary(diseño_mujeres)

diseño_mujeres_15_49 <- diseño_mujeres %>%
  filter(edad_muj >= 15, edad_muj <= 49)



### Contrucción y comparación indicadores ya existentes en el anexo técnico ###

options(survey.lonely.psu = "adjust") #hay un estrato con una solo UPM, se usa esto para seguir con el analisis sin eliminar datos 

# -----------------------------------------------------------------#
#            Mujeres de 14 a 49 años , según situación conyugal    #
# -----------------------------------------------------------------#

resultado_conyugal <- diseño_mujeres_15_49 %>%
  group_by(p10_1) %>% 
  summarize(
    porcentaje = survey_mean(vartype = NULL, proportion = TRUE) * 100
  ) %>%
  mutate(etiqueta = recode(as.character(p10_1),
                           "1" = "Unión libre",
                           "2" = "Separada unión libre",
                           "3" = "Separada matrimonio",
                           "4" = "Divorciada",
                           "5" = "Viuda unión libre",
                           "6" = "Viuda matrimonio",
                           "7" = "Casada",
                           "8" = "Soltera"
  )) %>%
  ggplot(aes(x = reorder(etiqueta, porcentaje), y = porcentaje)) +
  geom_col(fill = "#6baed6") +
  geom_text(aes(label = round(porcentaje, 1)), hjust = -0.1) +
  coord_flip() +
  labs(x = NULL, y = "Porcentaje (%)") +
  theme_minimal()

ggsave(filename = file.path(output, "MEX/img/situacion_conyugal_mujeres_15_49.png"),
       width = 7, height = 5, dpi = 300)


save(base_mujeres2, file = file.path(output, "MEX/ENADID_modulo_mujeres_2023.RData"))


