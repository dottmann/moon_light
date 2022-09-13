
## Script name: Myctophid module
##
##
## Author: Daniel Ottmann & Tom Langbehn
## Email:  tom.langbehn@uib.no
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: August 2021
## Last update:  March 2022
##
## ---------------------------
##
## Readme:
##
## This script models the position and density of myctophids as in relation to light intensity in the water column in an annual cycle 
##
########################################################################################################################################


##################################################################
# Set time:
time0 <- Sys.time()

# Set english language:
Sys.setlocale("LC_TIME", "English")

# Suppress tidiverse summarise() messages:
options(dplyr.summarise.inform = FALSE)

# Create an "those that are not within" function:
"%ni%" <- Negate("%in%")


#############
# Load data:
load("data/light_field.RData")


#############################################################################################################

#############################################################################################################
# Find isolume depth of the upper and lower bound of the myctophid Light Comfort Zone and overlap with tuna:

# Set constants:

# Isolume thresholds:
isolume_upr = 1.6E-5
isolume_lwr = 2.5E-7

# Tuna depth limit: 
prey_depth_limit <- 25

# Myctophid abundance (fish m^-3):
max_myctophid_dens <- 0.0061 * myctophid_scalar # m^-3  #  Olivar et al 2012 Deep Sea Res I

max_myctophid_dens <- max_myctophid_dens * 10 #  Kaartvedet 2012 MEPS


# Get a data frame for light confrot zone and overlapping portion of the myctophid population:
myctophid_df <- light_field_df %>% 
  group_by(datetime, hour, yday) %>%
  
  summarise(lcz_upr = which.min(abs(Ez-isolume_upr)),
            lcz_lwr = which.min(abs(Ez-isolume_lwr))) %>%
  
  mutate(lcz_upr = lcz_upr - 1,
         lcz_lwr = lcz_lwr - 1,
         lcz_range = lcz_lwr - lcz_upr,
         lcz_mean = mean(c(lcz_lwr, lcz_upr)),
         lcz_sd = lcz_range/4,                                   # We assume the uper and lower limits are 2 standard deviations from the mean
         overlapping_proportion = pnorm(prey_depth_limit, mean = lcz_mean, sd = lcz_sd, lower.tail = T),
         # myctophid_overlaping_density = max_myctophid_dens * overlapping_proportion / prey_depth_limit,
         myctophid_overlaping_density = max_myctophid_dens * overlapping_proportion)


# Get highest mictophid densities of each day:
max_myctophid_df <- myctophid_df %>%
  group_by(yday) %>%
  summarise(max_myctophid_overlaping_density = max(myctophid_overlaping_density))


# Oufile data:
save(myctophid_df, file = "data/myctophid_df.RData")
save(max_myctophid_df, file = "data/max_myctophid_df.RData")

# Time out:
print(Sys.time() - time0) 

# Clean environment:
rm(list = ls())

#                                          END OF SCRIPT
#############################################################################################################