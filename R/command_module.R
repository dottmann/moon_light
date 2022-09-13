
# Script name: Command module
#
#
# Authors: Daniel Ottmann
# Email: daniel.ottmann.riera@gmail.com
#
# Date created: September 2021
# Last update:  July 2022
#
# ---------------------------
#
# Readme:
#
# This script runs all modules of the model
# It enables to modify the model to run different tests (see Test runs below)
#
# ---------------------------


#####################################################################
# Clear environment:
rm(list = ls())

# Load packages:
require(tidyverse)
require(patchwork)
require(ggthemes)
require(scales)
library(viridis)
library(mgcv)
library(suncalc)
library(gggibbous)
require(here)
require(lubridate)

# Suppress tidiverse summarise() messages:
options(dplyr.summarise.inform = FALSE)


######################################################################
# Test runs:
#---------------------------------------------------------------------
# 0     - Basic run
# 2021  - Basic run in 2021
# 1     - Exclude mesopelagic fish
# 2     - Nauplii = 500
# 3     - Nauplii = 500 & exclude mesopelagics
# 4     - Nauplii = 300
# 5     - Nauplii = 300 & exclude mesopelagics
# 6     - Background mortality scale = 1 & mesopelagic fish present
# 7     - Background mortality scale = 1 & mesopelagic fish absent
# 8     - Background mortality scale = .5 & mesopelagic fish present
# 9     - Background mortality scale = .5 & mesopelagic fish absent
# 10    - Mesopelagic fish scale = 0.1
# 11    - Mesopelagic fish scale = 10
# 12    - Constant environment except for moon
# 13    - Sensitivity to invertebrate padding
######################################################################


#--------------------------------------------------------------------
# Run lightfield module:
# Settings:
moon_effect = T                        # Include/exclude moonlight
year_moon = 2022                       # Lunar cycles belonging to 2022 vs 2021
source("R/lightfield_module.R")        # Estimated time: 25s

#--------------------------------------------------------------------
# Run environmental module:
# Settings:
constant_temperature = F               # Constant/observed
constant_food = F                      # Constant/observed
constant_light_hours = F               # Constant/observed
constant_invertebrates = F             # Constant/observed
invertebrate_scalar <- 1               # Scale invertebrate density
sensitivity_padding_invertebrates <- F #Sensitivity to invertebrate padding
nauplia_dens <- 400                    # Nauplii m^-3 - smaller larvae only eat nauplii. Density is from field surveys
source("R/environmental_module.R")     # Estimated time: 15s

#--------------------------------------------------------------------
# Run mesopelagic fish module:
# Settings:
myctophid_scalar <- 1                  # Scale mesopelagics density
source("R/myctophid_module.R")         # Estimated time: 2s

#--------------------------------------------------------------------
# Run fitness module:
# Settings:
test_run <- 0                          # Replace with the test run code below
background_mortality_scale <- .85      # Scale background mortality down to "discount" the mortality by mesopelagics and Pelagia
source("R/fitness_module.R")           # Estimated time: 1-4 minutes

#--------------------------------------------------------------------
# Run model plots:
# Settings:
plot_lightfield = T
plot_environment = T
plot_mictophids = T
plot_fitness = T
source("R/plotting_module.R")           # Estimated time: 

#                                          END OF SCRIPT
#############################################################################################################