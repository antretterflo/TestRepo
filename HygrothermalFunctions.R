
# Hygrothermal Functions --------------------------------------------------

###
# Functions to compute hygrothermal values
###
# Florian Antretter
# Date: 2018-04-12
# Version: 0.1



# Load required packages --------------------------------------------------

library(tidyverse)



# Functions ---------------------------------------------------------------

# f.FtoC : Compute temperature in Celsius from Fahrenheit
f.FtoC <- function(Temperature_F) {
  (Temperature_F - 32) * (5/9)
}

# f.CtoF : Compute temperature in Fahrenheit from Celsius
f.CtoF <- function(Temperature_C) {
  Temperature_C * (9/5) + 32
}

# f.VP : Calculate saturation vapor pressure from temperature in Celsius
f.VP <- function(Temperature_C) { # Arden Buck equation
  con1 <- ifelse(Temperature_C < 0, 611.15, 611.21)
  con2 <- ifelse(Temperature_C < 0, 23.036, 18.678)
  con3 <- ifelse(Temperature_C < 0, 333.7, 234.5)
  con4 <- ifelse(Temperature_C < 0, 279.82, 257.14)
  
  con1 * exp((con2 - (Temperature_C / con3)) * (Temperature_C / (con4 + Temperature_C)))
}
