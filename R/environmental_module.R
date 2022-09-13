
## Script name: Food & temperature module
##
##
## Author: Daniel Ottmann & Oyvind Fiksen
## Email:  oyvind.fiksen@uib.no
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: September 2021
## Last update:  July 2022
##
## ---------------------------
##
## Readme:
##
## This script models the cladocera abundance and temperature of the mixed layer depth on an annual cycle: 
##
########################################################################################################################################


##################################################################
# Set time:
time0 <- Sys.time()

# Set english language:
Sys.setlocale("LC_TIME", "English")

# Load data
load("data/light_field.RData") 
load("data/invertebrate_data.RData") 


###############################################################################

###############################################################################
# Get daily values of temperature and cladocera.
# We use observed mean values of temperature and cladocera:

# Temperature:
# From NOAA CoastWatch Program and NASA's Goddard Space Flight Center (https://coastwatch.pfeg.noaa.gov/erddap/index.html)
temperature_mid_month <- c(14.8049, 14.1126, 14.1915, 15.4093, 17.9667, 21.6733, 24.958,
                26.6576, 25.8626, 23.2371, 20.0049, 16.7536)

# Cladocera density: (de Puelles & al 2007):
clado_mid_month <- c(8, 16, 32, 77, 115, 162, 215, 140, 81, 57, 13, 12)


# Interpolate to get continuous values from day 1 to day 365:
# Days in each month:
month_days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 

# Create empty vectors for daily temperature and cladocera density:
temperature <- c() 
clado_dens <- c()

# Create a for loop to interpolate the mean values of temperature and cladocera:
for (i in 1:12){ 
        if (i == 1){
            for (j_day in 1:15){   
                jan1_temperature <- mean(c(temperature_mid_month[12], temperature_mid_month[1])) 
                Jan1_clado <- mean(c(clado_mid_month[12], clado_mid_month[1]))
                day_incr_temperature <- (temperature_mid_month[1] - jan1_temperature)/15
                day_incr_clado <- (clado_mid_month[1] - Jan1_clado)/15
                temperature[j_day] <- jan1_temperature + (j_day ) * day_incr_temperature
                clado_dens[j_day] <- Jan1_clado + (j_day ) * day_incr_clado
            }
        } else {
            day_incr_temperature <- (temperature_mid_month[i] - temperature_mid_month[i - 1])/month_days[i]               # Can be replaced by mean()
            day_incr_clado <- (clado_mid_month[i] - clado_mid_month[i - 1])/month_days[i]       # Can bre replaced by mean()
            for (j in 1:month_days[i]){
                temperature[j_day] <- temperature_mid_month[i - 1] + (j - 1) * day_incr_temperature
                clado_dens[j_day] <- clado_mid_month[i - 1] + (j - 1) * day_incr_clado
                j_day <- j_day + 1
          }
          if (i==12){
            day_incr_temperature <- (jan1_temperature - temperature_mid_month[i])/15
            day_incr_clado <- (Jan1_clado - clado_mid_month[i])/15
            for (j in 1:16){
                temperature[j_day] <- temperature_mid_month[i] + (j - 1) * day_incr_temperature
                clado_dens[j_day] <- clado_mid_month[i] + (j - 1) * day_incr_clado
                j_day <- j_day + 1                
            }
          }    
        }                
}


# Cladocera density by de Puelles et al (2007) are coastal values. Offshore values are ~1/10 of coastal values (Atineza et al 2016):
clado_dens <- clado_dens * 0.1

# Put the variables in a data frame:
environmental_df <- data.frame(yday = 1:365, temperature, clado_dens)


# Fit the data in a GAM to smoohtern the values:
m1 <- gam(temperature ~ s(yday, bs = "cc"), data = environmental_df)
m2 <- gam(clado_dens ~ s(yday, bs = "cc"), data = environmental_df)


# Update the environmental data frame with the new values:

# Temperature:
if(constant_temperature == T){
  environmental_df <- environmental_df %>%
    mutate(temperature = 24)
} else {
  environmental_df <- environmental_df %>%
    mutate(temperature = predict(m1))
}

# Cladocera:
if(constant_food == T){
  environmental_df <- environmental_df %>%
    mutate(clado_dens = 215 * .1)
} else {
  environmental_df <- environmental_df %>%
    mutate(clado_dens = predict(m2))
}


# # Alternatively, fitt the cladocera in a Gaussian curve:
# environmental_df <- environmental_df %>%
# mutate(clado_dens = 220.74 * exp(-0.5 * abs((yday-190.81)/41.31)^1.2717))


# Add invertebrate predators:
if(constant_invertebrates == T){
  environmental_df$invertebrate_dens <- 1.1 * invertebrate_scalar                                   # Ottmann et al 2021 L&O
} else {
  if(sensitivity_padding_invertebrates == T) {
    invertebrate_data <- invertebrate_data %>%
      group_by(yday) %>%
      summarise(pnoctiluca_dens = mean(pnoctiluca_dens)) %>%
      ungroup()
    
    # Padd with 0 values values:
    padding <- data.frame(yday = c(1:80, 230:365), pnoctiluca_dens = c(rep(1.1, 80), rep(1.1, 136)))
    invertebrate_data <- rbind(invertebrate_data, padding)
    
    # Model the density of pnoctiluca:
    m1 <- gam(pnoctiluca_dens ~ s(yday), data = invertebrate_data)
    
    environmental_df <- environmental_df %>%
      mutate(invertebrate_dens = predict(m1, newdata = environmental_df),
             invertebrate_dens = case_when(invertebrate_dens < 0 ~ 0,
                                           yday < 50 | yday > 225 ~ 0,
                                           T ~ invertebrate_dens * invertebrate_scalar))
  } else {
    invertebrate_data <- invertebrate_data %>%
      group_by(yday) %>%
      summarise(pnoctiluca_dens = mean(pnoctiluca_dens)) %>%
      ungroup()
    
    # Padd with 0 values values:
    padding <- data.frame(yday = c(1:80, 230:365), pnoctiluca_dens = c(rep(0, 80), rep(0, 136)))
    invertebrate_data <- rbind(invertebrate_data, padding)
    
    # Model the density of pnoctiluca:
    m1 <- gam(pnoctiluca_dens ~ s(yday), data = invertebrate_data)
    
    environmental_df <- environmental_df %>%
      mutate(invertebrate_dens = predict(m1, newdata = environmental_df),
             invertebrate_dens = case_when(invertebrate_dens < 0 ~ 0,
                                           yday < 50 | yday > 225 ~ 0,
                                           T ~ invertebrate_dens * invertebrate_scalar))
  }
}

# Add light data:
irradiance_df <- light_field_df %>%
  filter(depth_m == 0) %>%
  dplyr::select(yday, hour, datetime, irradiance, solalt, light_lim)

environmental_df <- irradiance_df %>%
  left_join(environmental_df, by = "yday")

if(constant_light_hours == T){
  environmental_df <- environmental_df %>%
    mutate(light_lim = case_when(hour %in% 8:21 ~ 1,
                                 T ~ 0))
}

# Add moon phase and nauplia dens:
environmental_df <- environmental_df %>%
  mutate(moon_phase = getMoonIllumination(date = datetime,
                                          keep = "phase")$phase,
         moon_fraction = getMoonIllumination(date = datetime,
                                          keep = "fraction")$fraction,
         nauplia_dens = nauplia_dens)

# Outfile data:
save(environmental_df, file = "data/environmental_df.RData")


# Time out:
print(Sys.time() - time0)

# Clean environment:
rm(list = ls())

#                                          END OF SCRIPT
#############################################################################################################