#########################################################
# Lectura y preparación de las bases de datos - MEX     #
#########################################################

### Cleaning R environment ###

rm(list = ls())
gc()

#################
### Libraries ###
#################

library(dplyr)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <<- file.path(b_path, "src")

TMUJER2    <- read.csv(file.path(input, "MEX/TMUJER2.csv"), encoding = "UTF-8")
TSDEM      <- read.csv(file.path(input, "MEX/TSDEM.csv"), encoding = "UTF-8")
TVIVIENDA  <- read.csv(file.path(input, "MEX/TVIVIENDA.csv"), encoding = "UTF-8")
TFECHISEMB <- read.csv(file.path(input, "MEX/TFECHISEMB.csv"), encoding = "UTF-8")
THOGAR     <- read.csv(file.path(input, "MEX/THOGAR.csv"), encoding = "UTF-8")
TMIGRANTE  <- read.csv(file.path(input, "MEX/TMIGRANTE.csv"), encoding = "UTF-8")


