
# Script name: data preparation
#
#
# Author: Daniel Ottmann
#
# Email: daniel.ottmann.riera@gmail.com
#
# Date created: July 2022
# Last update:  July 2023
#
# ---------------------------
#
# Readme:
#
# This scripts calculates the mean ages post-spawning and includes the lunar conditions at the time of each sampling
#
########################################################################################################################################


#############################
# Load packages:
library(tidyverse)
library(mgcv)
library(suncalc)
library(lubridate)


##################################################################
# LOAD  DATA

# Clean environment:
rm(list = ls())

# Load data:
load("../data/length_data.RData")
load("../data/field_data.RData")


################################
# Create a function to calculate the yolkl-sack duration:

f_yolk_duration <- function(temperature){
  
  # Return the number of days it takes to develop through the yolk sac stage assuming Q10 and 2.5 days at 25 degrees
  
  q10 <- 2                                                      # If Q10 is higher then the gut-based model will start to deviate from the temperature dependency in oberved potential growth rate (Reglero & al. 2018)
  T_measured <- 25                                              # The temperature where development time has been measured to 2.5 days
  yolk_stage_days <- 2.5 * q10^((T_measured - temperature)/10)  # d
  yolk_stage_hours <- yolk_stage_days                           # d -> h
  return(yolk_stage_days)  
}

# Edit length data to estimate time post spawning:
length_data <- length_data %>%
  mutate(n_larvae = round(n_larvae, 0),
         sgr = 0.0418 * (t_mld) - 0.8355,
         egg_hatching_time = 8787.5 * (t_mld^(-1.701)) / 24,
         ysl_duration = f_yolk_duration(t_mld),
         length_fresh = 1.09 * length_formaline + 1.06,   
         dry_weight = 0.0008 * exp(length_fresh *  0.9052),
         age = log(dry_weight/0.018) / log(1 + sgr),
         age = case_when(age < 0 ~ 0,
                         T ~ age),
         time_post_spawning = round(age + ysl_duration + egg_hatching_time)) %>%
  uncount(n_larvae) %>%
  filter(!is.na(id_netcolector), !is.na(time_post_spawning)) %>%
  group_by(id_netcolector) %>%
  summarise(mean_time_post_spawning = mean(time_post_spawning))

# Get a rough average for time post spawning:
mean_time_post_spawning <- ceiling(mean(length_data$mean_time_post_spawning, na.rm = T))


# Make the data frame that will be used for the statistical analysis:
data <- field_data %>%
  mutate(bft_ab_gs = bft_ab_gs * 10,
         year = as.factor(year),
         date = as.Date(jd - 1 - mean_time_post_spawning, origin = paste0(year, "-01-01")),
         mo = month(date, label = T, abbr = T),
         day = day(date),
         moon_brightness = getMoonIllumination(date = date,
                                               keep = "fraction")$fraction,
         moon_phase = getMoonIllumination(date = date,
                                          keep = "phase")$phase)


# Outfile data:
save(data, file = "data/data.RData")

# Make raw data figure:

# First, remove the back calculation fo spawning time in the data frame:
data <- field_data %>%
  mutate(bft_ab_gs = bft_ab_gs * 10,
         year = as.factor(year),
         date = as.Date(jd - 1 - 0, origin = paste0(year, "-01-01")),
         mo = month(date, label = T, abbr = T),
         day = day(date),
         moon_brightness = getMoonIllumination(date = date,
                                               keep = "fraction")$fraction,
         moon_phase = getMoonIllumination(date = date,
                                          keep = "phase")$phase)

# Make plot:
p <- ggplot(data = data) + 
  geom_point(aes(x = moon_phase, y = log10((bft_ab_gs) + 1))) + 
  theme_bw() + 
  ylab(expression(paste("log(standardized larvae ", m^-2," + 1)"))) +
  xlab("Moon phase")

# Save figure:
ggsave("plots/standardized_larval_abundances_sampling.png", p, height = 35 , width = 35, units = "mm", scale = 3)



#                                          END OF SCRIPT
#############################################################################################################