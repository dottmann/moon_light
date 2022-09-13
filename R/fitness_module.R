
# Script name: fitness module
#
#
# Author: Daniel Ottmann, Oyvind Fiksen & Tom Langbehn
#
# Email: daniel.ottmann.riera@gmail.com
# Email: oyvind.fiksen@uib.no
# Email: tom.langbehn@uib.no
#
# Date created: September 2021
# Last update:  March 2022
#
# ---------------------------
#
# Readme:
#
# Modified from Fiksen & Reglero 2021
# Find fitness (= survival from egg to flexion stage) of all spawning times of the year
# We release one tuna egg each hour of the year and follow it forward in time
# until it reach the flexion stage. The egg has to hatch successfully - which require
# temperatures above 18 degrees. Then the yolk sac period, and the first feeding,
# from 0.018 mgDW to 0.77 mgDW. We also include a size-dependent mortality,
# which gives a penalty for longer stage duration. Feeding larvae include gut dynamics.
#
# Modified from Ljungstr?m et al 2021 Nat Clim Change
# Estimate predation of myctphids on ABFT as a function of myctophid density and prey size
# Myctophid are visual predators that migrate in the water column upon light intensity
#
# Myctophid density and habitat conditions are imported from the environmental module
#
# We assume only eggs and pre-flexion larvae are vulnerable to predation of invertebrate predators
# (ephyrae of P. noctilucaelagia)
#
########################################################################################################################################


##################################################################
# Set time:
time0 <- Sys.time()

# Set english language:
Sys.setlocale("LC_TIME", "English")

# Load data:
load("data/myctophid_df.RData")
load("data/environmental_df.RData")


##################################################################

##################################################################
# Create model functions:

#-----------------------------------------------------------------
f_egg_surv_myctophids <- function(predator_density){
  
  # Returns the probability that an egg of ABFT will survive 
  # the predation by myctophids at a given time density
  
  fixed_prey_area <- (fixed_prey_length_m * prey_length_to_width_ratio) * fixed_prey_length_m * .75    # m^2
  prey_area <- pi*(egg_diameter/2)^2                                                                   # m^2
  predator_visual_capacity <- predator_maximum_visual_range / (prey_contrast * fixed_prey_area)        # m
  predator_visual_range <- sqrt(egg_contrast * prey_area * predator_visual_capacity *                  # m
                                  (ambient_light / (egg_composite_saturation_parameter + ambient_light)))  
  # predator_visual_range <- max(c(predator_non_visual_detection_distance, predator_visual_range))     # Non visual detection when prey is close enough that the predator will surelly detect it, regradless of ligth
  predator_clearance_rate <- pi * predator_visual_range^2 * predator_swimming_velocity                 # m^3/h ; We assume that predators have no handling time nor satiation. They would migrate down if they were satiated
  encounter_rate <- predator_clearance_rate * predator_density                                         # encounters/h
  predator_mortality_rate <- encounter_rate                                                            # killings/h
  prey_probability_of_surviving_predators <- exp(-predator_mortality_rate)                             # Probability of surviving 1h, unitless
  return(prey_probability_of_surviving_predators)
}

#-----------------------------------------------------------------
f_larva_surv_myctophids <- function(prey_length_m, predator_density){
  
  # Returns the probability that a size-specific larva of ABFT will survive 
  # to predation by myctophids at a given density
  
  fixed_prey_area <- (fixed_prey_length_m * prey_length_to_width_ratio) * fixed_prey_length_m * .75    # m^2
  prey_area <- (prey_length_m * prey_length_to_width_ratio) * prey_length_m * .75                      # m^2
  predator_visual_capacity <- predator_maximum_visual_range^2 / (prey_contrast * fixed_prey_area)      # dimensionless
  predator_visual_range <- sqrt(prey_contrast * prey_area * predator_visual_capacity *                 # m
                                  (ambient_light / (larvae_composite_saturation_parameter + ambient_light)))  # m
  # predator_visual_range <- max(c(predator_non_visual_detection_distance, predator_visual_range))     # Non visual detection when prey is close enough that the predator will surelly detect it, regradless of ligth
  predator_clearance_rate <- pi * predator_visual_range^2 * predator_swimming_velocity                 # m^3/h ; We assume that predators have no handling time nor satiation. They would migrate down if they were satiated
  encounter_rate <- predator_clearance_rate * predator_density                                         # encounters/h
  predator_mortality_rate <- encounter_rate * predator_probability_capture_success                     # killings/h
  prey_probability_of_surviving_predators <- exp(-predator_mortality_rate)                             # Probability of surviving 1h, unitless
  return(prey_probability_of_surviving_predators)
}

#-----------------------------------------------------------------
f_sgr_temp <- function(temperature){
  
  # Returns the specific per hour growth rate of ABFT larvae as a function of 
  # temperature. From Reglero & al 2018, their fig 1. 
  # NB - in the lab exp they only feed during 15 hours of light, and eat nothing during night.
  # In the lab, their guts run with empty within a couple of hours after darkness (Reglero, pers com), 
  # and growth in mass must then logically cease. This means we have to distrbute 
  # the average daily growth potential over ca 15 hours, when they have food in their gut, and not 24.
  # If the larvae have to long days in the lab, they do not thrive (Reglero, pers com)
  
  return(max(0, 0.0418 * temperature - 0.8355) / 15.)
}

#-----------------------------------------------------------------
f_wgt_to_sl <- function(wgt){
  
  # Returns the standard length in m as a funciton of body mass in mg DW
  # Reglero et al 2018
  
  return((log(wgt_mgDW / 0.0008) / 0.9052) / 1000)
}

#-----------------------------------------------------------------
f_larva_surv <- function(wgt, hours){
  
  # Returns larval chance of surviving n hours as a function of 
  # Body mass in mg DW (McGurk 1986). Converts to gram! Note error in Reglero et al 2018, Fig. 1d legend text
  
  m <- dh * 0.5 * 0.00022 * (0.001 * wgt)^(-0.85)  # Note we multiply by dh to convert daily to hourly mortality
  m <- m * background_mortality_scale              # Scale mortality to account for the other sources of natural mortality (mictophids + invertsebrates)
  s <- exp(-m * hours)                             # s = probability of surving 1h
  return(s)
}

#-----------------------------------------------------------------
f_larva_feeding <- function(time_id, wgt){
  
  # The feeding process of larva - clearance from visual acuity. Return mass of prey encontered in mg dw per hour.
  # Include light, prey density, larval size. Handling time can limit feeding on nauplii, but is not a 
  # factor for the larger cladocera (rare encounters, large prey)
  # hour = 12;     day = 180;     wgt = 0.2
  
  length_m <- log(wgt/0.0008)/905.2                                       # Larvae length in m
  handling <- 2/3600                                                      # s -> h handling time, hours per prey
  swim_vel <- 3 * length_m * 3600                                         # BL/s -> m/h swimming velocity (m/s) scale with BL
  msa <- 4.699 * ((length_m * 1000)^(-1.129))                             # From Hilder et al (2019)
  behav_anatomical_ratio <- 0.5                                           # The ratio between anatomical and behavioural measures of visual detection
  light <- light_lim[time_id]                                             # Proportion of 1 h with enough light to see (Hilder et al, 2017)
  
  clado_dens <- clado_dens[time_id]
  clad_length <- 0.8 * 1E-3                                               # Cladoceran length in m
  dw_clad <- 3.15 * 1E-03                                                 # mgDW DW_prey_micg -> mg  (ref?)
  vis_r_clad <- 0.5 * clad_length/tan(msa * 0.5 * pi/180)                 # See eg Job & Bellwood 1996 JFB 48:952-963. vis_r in m
  vis_r_clad <- vis_r_clad * behav_anatomical_ratio                       # How far away the larva detect prey in m
  pr_capt_clad <- min(1, max(0, (wgt - 0.018)/(0.77 - 0.018)))            # Capture probability of cladocerans
  clearance_vol_clad <- 0.5 * pi * (vis_r_clad^2) * swim_vel * light      # m3/h, daylight is on or off
  clad_enc <- pr_capt_clad * dw_clad * clearance_vol_clad * clado_dens / 
    (1. + handling * clearance_vol_clad * clado_dens)                     # Holling disk

  naup_dens <- environmental_df$nauplia_dens[1]                           # nauplii m^-3
  naup_length <- 0.3 * 1E-3                                               # typical size of nauplii, Oithona, in m
  dw_naup <- 0.5 * 1E-03                                                  # Dw in mg from Hay &al 1988, 0.3 mm long nauplia
  vis_r_naup <- 0.5 * naup_length/tan(msa * 0.5 * pi/180)
  vis_r_naup <-  vis_r_naup * behav_anatomical_ratio                      # How far away the larva detect prey in m
  clearance_vol_naup <- 0.5 * pi * (vis_r_naup^2) * swim_vel * light      # 0.5 - larva only look up
  naup_enc <- dw_naup * clearance_vol_naup * naup_dens / 
    (1 + handling * clearance_vol_naup * naup_dens)                       # Holling disk
  
  food_enc <- clad_enc + naup_enc                                         # Larvae eat both cladcerans and nauplii, but capture of Clado is size-dependent
  return(food_enc)
}

#-----------------------------------------------------------------
f_metab_rate <- function(wgt, temperature){
  
  # Find metabolic rate in mgDW per hour from body mass. 
  # Weight in is in mg DW
  
  q10 <- 2                                                  # If Q10 is higher then the gut-based model will start to deviate from the temperature dependency in oberved potential growth rate (Reglero & al. 2018)
  metabol_rate <- 0.404 * (wgt^0.994)                       # In umol O2/h at 26 degr (Reglero & al in revision)
  metabol_rate <- metabol_rate * 32 * 0.88                  # ug O2 * RQ protein = 0.88  
  metabol_rate <- metabol_rate * 12/34                      # ug C
  metabol_rate <- metabol_rate * 100/(1000 * 45)            # ug C -> mg DW (mgC = 45% of DW)
  metabol_rate <- metabol_rate * q10^((temperature-26)/10)  # Measured at 26 degrees only. Q10 fitted from growth 
  return(metabol_rate)
}

#-----------------------------------------------------------------
f_yolk_duration <- function(temperature){
  
  # Return the number of days it takes to develop through the yolk sac stage assuming Q10 and 2.5 days at 25 degrees
  
  q10 <- 2                                                      # If Q10 is higher then the gut-based model will start to deviate from the temperature dependency in oberved potential growth rate (Reglero & al. 2018)
  T_measured <- 25                                              # The temperature where development time has been measured to 2.5 days
  yolk_stage_days <- 2.5 * q10^((T_measured - temperature)/10)  # d
  yolk_stage_hours <- yolk_stage_days / dh                      # d -> h
  return(yolk_stage_hours)  
}


###########################################################################

###########################################################################
# Define parameters of the myctophid model:

dh <- 1/24                                                    # Diel resolution = 1 hour
predator_length_m <- 60e-3                                    # m
predator_swimming_velocity <- 1 * predator_length_m * 3600    # m h^-1
egg_diameter <- 3e-3                                          # m
ysl_length <- 3.4e-3                                          # m
prey_length_to_width_ratio <- 0.5                             # unitless
predator_probability_capture_success <- 0.7                   # unitless
predator_maximum_visual_range <- 60e-3                        # m
predator_non_visual_detection_distance <- 3e-3                # m
prey_contrast <- .3                                           # unitless 
egg_contrast <- .2                                            # unitless Measured in imageJ as % black/white contrast
egg_composite_saturation_parameter <- 5e-5                    # unitless
larvae_composite_saturation_parameter <- 1e-8                 # unitless
ambient_light <- 1.9e-6                                       # W m^-2 Langbhen et al 2019 at 486 nm ; We asume it is the same for the top 25 m
fixed_prey_length_m <- .0041
preflexion_threshold <- .0045

# Defie parameters from the invertebrates model:
invertebrate_cr_23 <- .00414                                  # m^3 ind^-1 h^-1 at 23?C
invertebrate_q10 <- 2.8                                       # Hansen et al 1997

# Define parameters of the tuna biological model:
ysl_wgt_mgDW <- 0.018                                         # Larvae hatch with body mass 0.018 mg DW
assimilation_eff <- 0.7                                       # Assimilation efficiency (ref?)
end_size <- 0.77                                              # ug DW   # end of the flexion
max_gut <- 0.12                                               # Gut size, in fractions of body mass  # Gut content constraint to a % of body mass - 12% in BFT


# Create vectors where we will store the probability of surviving the egg + ysl periods of eggs released at every hour of the year:
v_egg_surv <- c()   # Probability, unitless
v_ysl_surv <- c()   # Probability, unitless


# Create large matrices to track the condition of each individual throughout every hour of the year:
m_growth_mgDW <- matrix(NA, nrow = nrow(environmental_df), ncol = nrow(environmental_df))   # Growth mg DW
m_wgt_mgDW <- matrix(NA, nrow = nrow(environmental_df), ncol = nrow(environmental_df))      # Boyd mass in mg DW
m_gut_limit <- matrix(NA, nrow = nrow(environmental_df), ncol = nrow(environmental_df))     # Limit of the gut in mg DW (larger fish have larger guts)
m_gut <- matrix(NA, nrow = nrow(environmental_df), ncol = nrow(environmental_df))           # The content of the gut in mg DW
m_fish_age <- matrix(NA, nrow = nrow(environmental_df), ncol = nrow(environmental_df))      # Fish age in h
m_fish_sl <- matrix(NA, nrow = nrow(environmental_df), ncol = nrow(environmental_df))       # Fish standard length in m
m_surv <-  matrix(NA, nrow = nrow(environmental_df), ncol = nrow(environmental_df))         # Probability of survival is unitless


##################################################################
# Create master data frame:
master_df <- environmental_df %>%
  
  # Assumming that neither eggs nor YSL grow nor change their predator escapre rate, we can calculate 
  # state parameters of individuals released at every hour as a function of the environment.
  # We don't need a for-loop for this, but we will need it later on for some cumulative parameter values.
  
  ungroup() %>%
  mutate(time_id = 1:n(),
         myctophid_dens = myctophid_df$myctophid_overlaping_density,                                          # myctophids/m^3 
         
         egg_dev_time =  8787.5 * (temperature^(-1.701)),                                                     # h ; From Reglero & al 2018. Minimum viable temperature is 19 degrees C
         time_hatching = pmin(c(time_id + egg_dev_time), max(time_id)),                                       # Hour of year when egg hatch
         egg_hatching_probability = pmax(((-1.27 * temperature^2. + 63.78 * temperature -727.98) * 0.01), 0), # Unitless
         temperature_hatching = temperature[time_hatching],                                                   # Temperature when egg hatches
         ysl_dev_time = f_yolk_duration(temperature_hatching),                                                # h ; YSL development time
         
         invertebrate_cr = invertebrate_cr_23 * invertebrate_q10^((temperature - 23)/10),                     # Calculate clearance rate m^3 h^-1
         invertebrate_er = invertebrate_dens * invertebrate_cr,                                               # Calculate encounter rate encounters h^-1
         survival_to_invertebrate = exp(-invertebrate_er),                                                    # Probabiity of survival h^-1
         
         egg_background_mortality_h =  dh * 0.5 * 0.00022 * (0.001 * ysl_wgt_mgDW)^(-0.85),                   # Background mortality (McGurk 1986)
         egg_background_mortality_h = egg_background_mortality_h * background_mortality_scale,                # Background mortality scaled down to account for mortality casued by mictophid and pelagia h^-1
         egg_background_survival = exp(-egg_background_mortality_h * egg_dev_time),                           # Probability to survie after background mortality
         
         ysl_background_surv_h = f_larva_surv(ysl_wgt_mgDW, 1),                                               # Probability to survie after background mortality h^-1 (scaled in function)
         ysl_background_survival = f_larva_surv(ysl_wgt_mgDW, ysl_dev_time),                                  # Probability to survie after background mortality (scaled in function)
         
         egg_invertebrate_survival_h = exp(-invertebrate_er),                                                 # Probability to survive to invertebrates h^-1
         ysl_invertebrate_survival_h = exp(-invertebrate_er),                                                 # Probability to survive to invertebrates h^-1
         egg_myctophid_survival_h = f_egg_surv_myctophids(myctophid_dens),
         ysl_myctophid_survival_h = f_larva_surv_myctophids(ysl_length, myctophid_dens),                      # Probability to survive to myctophids
         
         
         time_first_feeding = pmin(c(time_hatching + ysl_dev_time), max(time_id)),                            # Hour of year when larva starts feeding
         temperature_first_feeding = temperature[time_first_feeding],                                         # Temperature larva starts feeding
         fish_age_h = egg_dev_time + ysl_dev_time) %>%                                                        # Age at first feeding in h
  
  relocate(time_id) 


# Create vectors to store cumulative survival of egg and myctophids:
egg_invertebrate_survival <- NULL
ysl_invertebrate_survival <- NULL
egg_myctophid_survival <- NULL
ysl_myctophid_survival <- NULL

# Run a for_loop to fill the vectors.
# The time of YSL starts after the egg development time:
for(i in 1:nrow(master_df)) {
  
  egg_invertebrate_survival <- rbind(egg_invertebrate_survival, prod(master_df[(i:(i + round(as.numeric(master_df[i, "egg_dev_time"])))), "egg_invertebrate_survival_h"]))
  ysl_invertebrate_survival <- rbind(ysl_invertebrate_survival, 
                                     prod(master_df[((i + round(as.numeric(master_df[i, "egg_dev_time"]))):
                                                       (i + round(as.numeric(master_df[i, "egg_dev_time"])) + round(as.numeric(master_df[i, "ysl_dev_time"])))), "ysl_invertebrate_survival_h"]))

    egg_myctophid_survival <- rbind(egg_myctophid_survival, prod(master_df[(i:(i + round(as.numeric(master_df[i, "egg_dev_time"])))), "egg_myctophid_survival_h"]))
  ysl_myctophid_survival <- rbind(ysl_myctophid_survival, 
                                  prod(master_df[((i + round(as.numeric(master_df[i, "egg_dev_time"]))):
                                                     (i + round(as.numeric(master_df[i, "egg_dev_time"])) + round(as.numeric(master_df[i, "ysl_dev_time"])))), "ysl_myctophid_survival_h"]))
  
}

# Update master_df:
master_df <- master_df %>%
  mutate(egg_invertebrate_survival = as.vector(egg_invertebrate_survival),
         ysl_invertebrate_survival = as.vector(ysl_invertebrate_survival),
         egg_myctophid_survival = as.vector(egg_myctophid_survival),
         ysl_myctophid_survival = as.vector(ysl_myctophid_survival),
         
         egg_total_survival = egg_hatching_probability * egg_invertebrate_survival  * 
           egg_myctophid_survival * egg_background_survival,                                        # Combine effect of background mortality & invertebrate & myctophids
         ysl_total_survival = ysl_invertebrate_survival * ysl_myctophid_survival *
           ysl_background_survival,                                                                 # Combine effect of background mortality & myctophids
         first_feeding_survival = egg_total_survival * ysl_total_survival                           # Combine survival of the entire egg + ysl period
         ) %>%
  replace(is.na(.), 0)

##################################################################
# Convert some columns of master_df into vectors to simplify the code in the loops:
time_id <- as.numeric(master_df$time_id)
fish_age_h <- as.numeric(master_df$fish_age_h)
time_first_feeding <- as.numeric(master_df$time_first_feeding)
temperature <- as.numeric(master_df$temperature)
light_lim <- as.numeric(master_df$light_lim)
clado_dens <- as.numeric(master_df$clado_dens)
myctophid_dens <- as.numeric(master_df$myctophid_dens)
first_feeding_survival <- as.numeric(master_df$first_feeding_survival)
survival_to_invertebrate <- as.numeric(master_df$survival_to_invertebrate)


##################################################################
# Run the fitness model:

# -- First loop: select each fish, reset its parameters:
for (fish_id in time_id){                                                                                               # fish_id = fish released every hour
  
  gut = 0                                                                                                               # Starts with empty gut
  wgt_mgDW <- ysl_wgt_mgDW                                                                                              # Initial weight = YSL
  fish_age <- fish_age_h[fish_id]                                                                                       # -1 is because we ad +1 in the next loop
  
  ysl_period <- as.numeric(master_df[fish_id, "time_hatching"]):as.numeric(master_df[fish_id, "time_first_feeding"])    # Vector of the year hours when larva is YSL
  m_fish_sl[fish_id, ysl_period] <- ysl_length                                                                          # Standard length during the year hours when is YSL
  
  surv <- first_feeding_survival[fish_id]                                                                               # Comined survival of egg + ysl periods
  if (first_feeding_survival[fish_id] == 0) next                                                                                                   # If the egg doesn't hatch, move to the next egg
  
  # -- Second loop: For each fish, track parameters that change hour after hour and update it in the matrices:
  for (j in c(min((round(time_first_feeding[fish_id])), (nrow(master_df))):(nrow(master_df)))){                         # j = each hour after fish becomes first feeding
    
    if (wgt_mgDW >= end_size) break                                                                                     # If flexion, go to next fish
    if (surv < 1e-10) break                                                                                             # If survival is neglicent, go to next fish
    
    fish_age <- fish_age + 1                                                                                            # Add 1h
    growthT <- wgt_mgDW * f_sgr_temp(temperature[min(j, nrow(master_df))])                                              # How much does the fish grow in mgDW
    gut_lim <- max_gut * wgt_mgDW                                                                                       # Size limit of the gut mgDW
    food_enc <- f_larva_feeding(j, wgt_mgDW)                                                                            # Food encountered in mgDW 
    if (growthT == 0) {food_enc <- 0}                                                                                   # No feeding if to cold to grow
    resp <- f_metab_rate(wgt_mgDW, temperature[min(j, nrow(master_df))])                                                # Respiration in mgDW
    digestion <- (resp + growthT)/assimilation_eff                                                                      # Digestion is driven by growth, metabolism and assimilation efficiency 
    
    # The gut is a container which determines if the larva can grow at max rate, limited by temperature or food        
    # if the growth is temperature limited; enough food in the gut:
    if (gut >= digestion){                                                                                              # If energy intaque is enough to sustain metabolic demands and growth
      wgt_mgDW <- wgt_mgDW + growthT                                                                                    # The energy is enough to cover respiration and growth 
      gut  <- gut + (food_enc - digestion)                                                                              # Add prey remove digestion
      gut  <- min(gut, gut_lim)                                                                                         # Constrain gutcontent
    } else {
      wgt_mgDW <- wgt_mgDW + (gut * assimilation_eff - resp)                                                            # What is left in gut is used to grow or cover respiration 
      gut  <- food_enc                                                                                                  # Empty gut, add feeding
      gut  <- min(max(0, gut), gut_lim)                                                                                 # And it may fill up the gut by feeding
    }  
    
    wgt_mgDW <- max(wgt_mgDW, ysl_wgt_mgDW)                                                                             # Do not shrink below hatch size
    larval_sl <- f_wgt_to_sl(wgt_mgDW)                                                                                  # mg_DW -> m
    
    surv <- surv * f_larva_surv(wgt_mgDW, 1)                                                                            # 1h background survival chance
    if (larval_sl < preflexion_threshold){
      surv <- surv * survival_to_invertebrate[j]                                                                        # Survival to invertebrates if sl < 0.0045 m
      surv <- surv *  f_larva_surv_myctophids(larval_sl, myctophid_dens[j])}                                            # Add 1h survival to myctophids to survival if sl < 0.0045 m
    
    if (time_first_feeding[j] > nrow(environmental_df)) {surv <- 0}                                                     # If the larva do not reach flexion by the end (avoid terminal effects)
    
    # Update variable matrices:
    m_gut_limit[fish_id, j] <- gut_lim          
    m_wgt_mgDW[fish_id, j] <- wgt_mgDW
    m_growth_mgDW[fish_id, j] <- growthT
    m_gut[fish_id, j] <- gut  
    m_fish_age[fish_id, j] <- fish_age -1
    m_surv[fish_id, j] <- surv
    m_fish_sl[fish_id, j] <- larval_sl
    
  } # -- Close hour loop
} # -- Close individual fish loop


##############################################################################################################

###################
# Edit results:
fitness <- apply(m_surv, 1, min, na.rm = TRUE)                  # Selecting fitness this way will generate
fitness[fitness[] == Inf] <- 0

body_mass <- apply(m_wgt_mgDW, 1, max, na.rm = TRUE)       
body_mass[body_mass[] == -Inf] <- 0


# Edit master_df:
master_df <- master_df %>%
  mutate(body_mass = body_mass,
         fitness = case_when(time_id > 8000 ~ 0,                # Avoid terminal effects after hour of year = 8000
         T ~ fitness),
         test_run = test_run)


# Outfile master_df and matrices:
save(master_df, file = paste0("out/master_df_", test_run,".RData"))

output_matrices <- list(m_gut_limit = m_gut_limit, m_wgt_mgDW = m_wgt_mgDW, 
                        m_growth_mgDW = m_growth_mgDW, m_gut = m_gut, m_fish_age = m_fish_age, 
                        m_surv = m_surv, m_fish_sl = m_fish_sl)
save(output_matrices, file = paste0("out/output_matrices_", test_run,".RData"))

# Time out:
print(Sys.time() - time0)

# Clean environment:
# rm(list = ls())

#                                          END OF SCRIPT
#############################################################################################################