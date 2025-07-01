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
