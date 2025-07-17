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

input <- file.path(b_path, "input" ,"MEX")
output <- file.path(b_path,"output", "MEX")
src <- file.path(b_path, "src", "MEX")

load(file.path(input, "bd_endireh_2021.RData"))


# --------------------------------------#
#            Módulo demograficos        #
# --------------------------------------#

base_demo <- TSDem %>% select(
  ID_VIV,   # Identificador único de la vivienda (equivale a folioviv)
  ID_PER,   # Identificador de la persona en la vivienda (equivale a num_ren)
  SEXO,     # Sexo de la persona (1 = Hombre, 2 = Mujer)
  EDAD,     # Edad en años cumplidos
  NIV,       # Nivel de escolaridad alcanzado (codificado)
  P2_10      # Se considera indigena
)

# --------------------------------------#
#            Módulo mujeres             #
# --------------------------------------#

base_muj <- TB_SEC_IVaVD %>% select(
  ID_VIV,        # Identificador de vivienda
  ID_PER,        # Identificador de persona
  UPM,           # Unidad primaria de muestreo
  VIV_SEL,       # Vivienda seleccionada
  FAC_MUJ,       # Factor de expansión
  EST_DIS,       # Estrato
  UPM_DIS,       # Disposición UPM
  T_INSTRUM,     # Estado conyugal
  P13_15C,       # Edad cuando se caso o se unio por primera vez
  CVE_ENT,       # Entidad federal
  
  # =============================
  # Sección 14.3 - Violencia de pareja últimos 12 meses
  # =============================
  P14_3_1,  # La ha empujado o jalado el cabello
  P14_3_2,  # La ha abofeteado o cacheteado
  P14_3_3,  # La ha amarrado
  P14_3_4,  # La ha pateado
  P14_3_5,  # Le ha aventado algún objeto
  P14_3_6,  # La ha golpeado con el puño o con algún objeto
  P14_3_7,  # La ha tratado de ahorcar o asfixiar
  P14_3_8,  # La ha agredido con cuchillo o navaja
  P14_3_9,  # Le ha disparado con arma de fuego
  P14_3_10, # La ha humillado, insultado o comparado con otras mujeres
  P14_3_11, # La ha ignorado o no le brinda afecto
  P14_3_12, # Le ha dicho que lo engaña
  P14_3_13, # Le ha hecho sentir miedo
  P14_3_14, # La ha amenazado con abandonarla o dañarla
  P14_3_15, # Le ha prohibido salir o recibir visitas
  P14_3_16, # La ha espiado o seguido
  P14_3_17, # La llama o le escribe constantemente para saber dónde está
  P14_3_18, # La ha amenazado con algún arma
  P14_3_19, # La ha amenazado con matarla o matarse
  P14_3_20, # Le ha destruido o escondido sus pertenencias
  P14_3_21, # Le ha dejado de hablar
  P14_3_22, # Le revisa el celular o correo, o exige contraseñas
  P14_3_23AB, # Le ha exigido con amenazas tener relaciones sexuales
  P14_3_24AB, # La ha obligado a hacer cosas sexuales no deseadas
  P14_3_25,   # Ha usado la fuerza para tener relaciones sexuales
  P14_3_26,   # La ha obligado a ver pornografía
  P14_3_27,   # La ha forzado a tener relaciones sin protección
  P14_3_28,   # Le ha enviado mensajes con contenido sexual ofensivo
  P14_3_29,   # Ha difundido información íntima o imágenes sin consentimiento
  P14_3_30,   # Le ha prohibido estudiar o trabajar
  P14_3_31,   # Le ha quitado el dinero o lo ha usado sin permiso
  P14_3_32,   # Se ha adueñado de sus bienes
  P14_3_33,   # Le ha robado directamente
  P14_3_34,   # Le ha quitado bienes personales
  P14_3_35AB, # Se ha gastado el dinero de la casa sin consultarla
  P14_3_36AB, # La ha amenazado con no darle dinero para el hogar
  P14_3_37AB, # Ha sido tacaño o la ha castigado económicamente
  P14_3_38AB, # Ha controlado de forma abusiva el uso del dinero en el hogar
  
  # =============================
  # Sección 15.1 Toma de decisiones sobre salud sexual y reproductiva
  # =============================
  
  P15_1C_12,  # Quién decide cuándo tener relaciones sexuales
  P15_1C_13,  # Quién decide si se usan métodos anticonceptivos
  P15_1C_14   # Quién decide sobre el cuidado de la salud sexual y reproductiva
  
  
  
)

base_unida <- base_muj %>%
  left_join(base_demo, by = c("ID_VIV", "ID_PER"))

# Verificamos la unión
glimpse(base_unida)

save(base_unida, file = file.path(output, "ENDIREH_modulo_mujeres_2021.RData"))
