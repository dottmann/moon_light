
## Script name: moon phase
##
##
## Author: Daniel Ottmann
##
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: October 2021
## Last update:  July 2023
##
## ---------------------------
##
## Readme:
##
## This script runs the statistical analysis to identify if moon phase has an effect on the larval index (spawning intensity)
##
########################################################################################################################################


##################################################################
# Clear environment:
rm(list = ls())
time0 <- Sys.time()

# Set english language:
Sys.setlocale("LC_TIME", "English")


# Load packages:
library(tidyverse)
library(suncalc)
library(ggthemes)
library(brms)
library(rstanarm)
library(tidybayes)
library(lubridate)
library(patchwork)

# Load data:
load("data/data.RData")


# Make color pallet & specify size:
col1 = "blue"
col2 = "darkorange"
col3 = "bisque3"
col4 = "red"
col5 = "darkgoldenrod1"
col6 = "darkblue"
col7 = "darkturquoise"
col8 = "darkslategray1"
col9 = "deeppink"
col10 = "burlywood"


##################################################################

##################################################################

backgorund_moon <- expand.grid(year = unique(data$year), jd = (min(data$jd)-1):(max(data$jd)+1))
backgorund_moon <- backgorund_moon %>%
  mutate(year = as.factor(year),
         date = as.Date(jd - 1, origin = paste0(year, "-01-01")),
         moon_brightness = getMoonIllumination(date = date,
                                               keep = "fraction")$fraction,
         moon_phase = getMoonIllumination(date = date,
                                          keep = "phase")$phase)

p <- ggplot() + 
  geom_raster(data = backgorund_moon, aes(x = jd, y = fct_rev(year), fill = moon_brightness), alpha = .7) +
  scale_x_continuous(breaks = c(152, 182, 213), labels = c("Jun", "Jul", "Aug")) +
  #  geom_text(aes(label = paste0(day, "\n", mo)), lineheight = 0.8, size = 4) + 
  labs(x = "", y = "") +
  scale_color_viridis_c(option = "cividis", name = "Moon brightness  ") +
  scale_fill_viridis_c(option = "cividis", name = "Moon brightness  ", breaks = c(.0001, .999),
                       labels = c(0, 1)) +
  theme_base() +
  geom_point(data = data, aes(x = jd, y = fct_rev(year)), alpha = 0.7) +
  theme(plot.background = element_blank(),
        legend.position = "bottom") 

p

# ggsave("plots/moon_phases.png", p, height = 50 , width = 70, units = "mm", scale = 3)


#####################################################
# START BAYESIAN ANALYSIS


# Set model parameters:
chains <- 4
cores <- 4
iter <- 3500 #3500
warmup <- 1000 # 1000
thin <- 10



#################
# Fit the models:

# Model 0: Full model residual of temperature
time0 <- Sys.time()
m0 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year +   
                 s(hournorm, bs = "cc", k = 3) +
                 s(moon_phase, bs = "cc", k = 3) +
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year +   
                 s(hournorm, bs = "cc", k = 3) +
                 s(moon_phase, bs = "cc", k = 3) +
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0")
Sys.time() -time0


# Model 0_1: mu: -moon_phase ; hu:
time0 <- Sys.time()
m0_1 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year +   
                 s(hournorm, bs = "cc", k = 3)+
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year +   
                 s(hournorm, bs = "cc", k = 3) +
                 s(moon_phase, bs = "cc", k = 3) +
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0_1")
Sys.time() -time0

# Model 0_2: mu:  ; hu: -moon_phase
time0 <- Sys.time()
m0_2 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3) +   
                 s(hournorm, bs = "cc", k = 3)+
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year +   
                 s(hournorm, bs = "cc", k = 3)+
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0_2")
Sys.time() -time0


# Model 0_3: mu: -moon_phase ; hu: -moon_phase
time0 <- Sys.time()
m0_3 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(hournorm, bs = "cc", k = 3)+
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year +   
                 s(hournorm, bs = "cc", k = 3)+
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0_3")
Sys.time() -time0


# Model 0_4: mu: -hournorm  ; hu: 
time0 <- Sys.time()
m0_4 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3)+   
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3)+   
                 s(hournorm, bs = "cc", k = 3)+
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0_4")
Sys.time() -time0


# Model 0_5: mu:  ; hu: -hournorm
time0 <- Sys.time()
m0_5 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year +
                 s(moon_phase, bs = "cc", k = 3) +   
                 s(hournorm, bs = "cc", k = 3)+
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3)+   
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0_5")
Sys.time() -time0


# Model 0_6: mu: -hournorm ; hu: -hournorm
time0 <- Sys.time()
m0_6 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year +
                 s(moon_phase, bs = "cc", k = 3) +   
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3)+   
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0_6")
Sys.time() -time0


# Model 0_7: mu: -jd  ; hu: 
time0 <- Sys.time()
m0_7 <- brm(bf(bft_ab_gs ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3) +   
               s(hournorm, bs = "cc", k = 3)+
               s(tempres2, k = 3) +
               s(s_anom, k = 3),
             hu ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3)+   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(tempres2, k = 3) +
               s(s_anom, k = 3)),
          data = data,
          family = hurdle_gamma(),
          chains = chains,
          cores = cores,
          iter = iter,
          warmup = warmup,
          thin = thin,
          control = list(adapt_delta = 0.99),
          file = "out/hurd_gamma_m0_7")
Sys.time() -time0


#  Model 0_8: mu:  ; -jd hu: 
time0 <- Sys.time()
m0_8 <- brm(bf(bft_ab_gs ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3) +   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(tempres2, k = 3) +
               s(s_anom, k = 3),
             hu ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3)+   
               s(hournorm, bs = "cc", k = 3)+
               s(tempres2, k = 3) +
               s(s_anom, k = 3)),
          data = data,
          family = hurdle_gamma(),
          chains = chains,
          cores = cores,
          iter = iter,
          warmup = warmup,
          thin = thin,
          control = list(adapt_delta = 0.99),
          file = "out/hurd_gamma_m0_8")
Sys.time() -time0


#  Model 0_9: mu: -jd  ; -jd hu: 
time0 <- Sys.time()
m0_9 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3) +   
                 s(hournorm, bs = "cc", k = 3)+
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3)+   
                 s(hournorm, bs = "cc", k = 3)+
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0_9")
Sys.time() -time0


# Model 0_10: mu: -tempres2  ; hu: 
time0 <- Sys.time()
m0_10 <- brm(bf(bft_ab_gs ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3) +   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(s_anom, k = 3),
             hu ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3)+   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(tempres2, k = 3) +
               s(s_anom, k = 3)),
          data = data,
          family = hurdle_gamma(),
          chains = chains,
          cores = cores,
          iter = iter,
          warmup = warmup,
          thin = thin,
          control = list(adapt_delta = 0.99),
          file = "out/hurd_gamma_m0_10")
Sys.time() -time0


# Model 0_11: mu:  ; hu: -tempres2
time0 <- Sys.time()
m0_11 <- brm(bf(bft_ab_gs ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3) +   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(tempres2, k = 3) +
               s(s_anom, k = 3),
             hu ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3)+   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(s_anom, k = 3)),
          data = data,
          family = hurdle_gamma(),
          chains = chains,
          cores = cores,
          iter = iter,
          warmup = warmup,
          thin = thin,
          control = list(adapt_delta = 0.99),
          file = "out/hurd_gamma_m0_11")
Sys.time() -time0


# Model 0_12: mu: -tempres2 ; hu: -tempres2
time0 <- Sys.time()
m0_12 <- brm(bf(bft_ab_gs ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3) +   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(s_anom, k = 3),
             hu ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3)+   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(s_anom, k = 3)),
          data = data,
          family = hurdle_gamma(),
          chains = chains,
          cores = cores,
          iter = iter,
          warmup = warmup,
          thin = thin,
          control = list(adapt_delta = 0.99),
          file = "out/hurd_gamma_m0_12")
Sys.time() -time0


# Model 0_13: mu: -s_anom  ; hu: 
time0 <- Sys.time()
m0_13 <- brm(bf(bft_ab_gs ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3) +   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(tempres2, k = 3),
             hu ~
               s(lon, lat, k = 9) +   # Spatial effect
               year  +
               s(moon_phase, bs = "cc", k = 3)+   
               s(hournorm, bs = "cc", k = 3)+
               s(jd, k = 3) +
               s(tempres2, k = 3) +
               s(s_anom, k = 3)),
          data = data,
          family = hurdle_gamma(),
          chains = chains,
          cores = cores,
          iter = iter,
          warmup = warmup,
          thin = thin,
          control = list(adapt_delta = 0.99),
          file = "out/hurd_gamma_m0_13")
Sys.time() -time0


# Model 0_14: mu:  ; hu: -s_anom
time0 <- Sys.time()
m0_14 <- brm(bf(bft_ab_gs ~
                  s(lon, lat, k = 9) +   # Spatial effect
                  year  +
                  s(moon_phase, bs = "cc", k = 3) +   
                  s(hournorm, bs = "cc", k = 3)+
                  s(jd, k = 3) +
                  s(tempres2, k = 3) +
                  s(s_anom, k = 3),
                hu ~
                  s(lon, lat, k = 9) +   # Spatial effect
                  year  +
                  s(moon_phase, bs = "cc", k = 3)+   
                  s(hournorm, bs = "cc", k = 3)+
                  s(jd, k = 3) +
                  s(tempres2, k = 3)),
             data = data,
             family = hurdle_gamma(),
             chains = chains,
             cores = cores,
             iter = iter,
             warmup = warmup,
             thin = thin,
             control = list(adapt_delta = 0.99),
             file = "out/hurd_gamma_m0_14")
Sys.time() -time0


# Model 0_15: mu: -s_anom ; hu: -s_anom
time0 <- Sys.time()
m0_15 <- brm(bf(bft_ab_gs ~
                  s(lon, lat, k = 9) +   # Spatial effect
                  year  +
                  s(moon_phase, bs = "cc", k = 3) +   
                  s(hournorm, bs = "cc", k = 3)+
                  s(jd, k = 3) +
                  s(tempres2, k = 3),
                hu ~
                  s(lon, lat, k = 9) +   # Spatial effect
                  year  +
                  s(moon_phase, bs = "cc", k = 3)+   
                  s(hournorm, bs = "cc", k = 3)+
                  s(jd, k = 3) +
                  s(tempres2, k = 3)),
             data = data,
             family = hurdle_gamma(),
             chains = chains,
             cores = cores,
             iter = iter,
             warmup = warmup,
             thin = thin,
             control = list(adapt_delta = 0.99),
             file = "out/hurd_gamma_m0_15")
Sys.time() -time0


# Compare ICs of each model:

# Make a vector with our models:
model <- data.frame(model = c("m0", "m0_1", "m0_2", "m0_3", "m0_4", "m0_5", "m0_6", "m0_7", "m0_8", 
                              "m0_9", "m0_10", "m0_11", "m0_12", "m0_13", "m0_14", "m0_15"))

waics <- c(waic(m0)$waic, waic(m0_1)$waic, waic(m0_2)$waic, waic(m0_3)$waic, waic(m0_4)$waic, waic(m0_5)$waic, waic(m0_6)$waic, waic(m0_7)$waic, 
           waic(m0_8)$waic, waic(m0_9)$waic, waic(m0_10)$waic, waic(m0_11)$waic, waic(m0_12)$waic, waic(m0_13)$waic, waic(m0_14)$waic, waic(m0_15)$waic)

loos <- c(loo(m0)$looic, loo(m0_1)$looic, loo(m0_2)$looic, loo(m0_3)$looic, loo(m0_4)$looic, loo(m0_5)$looic, loo(m0_6)$looic, loo(m0_7)$looic, 
          loo(m0_8)$looic, loo(m0_9)$looic, loo(m0_10)$looic, loo(m0_11)$looic, loo(m0_12)$looic, loo(m0_13)$looic, loo(m0_14)$looi, loo(m0_15)$looic)

R2s <- c(bayes_R2(m0)[1], bayes_R2(m0_1)[1], bayes_R2(m0_2)[1], bayes_R2(m0_3)[1], bayes_R2(m0_4)[1], bayes_R2(m0_5)[1], bayes_R2(m0_6)[1], bayes_R2(m0_7)[1], 
         bayes_R2(m0_8)[1], bayes_R2(m0_9)[1], bayes_R2(m0_10)[1], bayes_R2(m0_11)[1], bayes_R2(m0_12)[1], bayes_R2(m0_13)[1], bayes_R2(m0_14)[1], 
         bayes_R2(m0_15)[1])

model_ics <- data.frame(model = model,
                        waic = waics,
                        loo = loos,
                        R2 = round(R2s, 3))[order(loos),]

model_ics



#####################################################


# Model 0_7_1: mu: -jd -s_anom ; hu: -moon_phase
time0 <- Sys.time()
m0_7_1 <- brm(bf(bft_ab_gs ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3) +   
                 s(hournorm, bs = "cc", k = 3)+
                 s(tempres2, k = 3),
               hu ~
                 s(lon, lat, k = 9) +   # Spatial effect
                 year  +
                 s(moon_phase, bs = "cc", k = 3)+   
                 s(hournorm, bs = "cc", k = 3)+
                 s(jd, k = 3) +
                 s(tempres2, k = 3) +
                 s(s_anom, k = 3)),
            data = data,
            family = hurdle_gamma(),
            chains = chains,
            cores = cores,
            iter = iter,
            warmup = warmup,
            thin = thin,
            control = list(adapt_delta = 0.99),
            file = "out/hurd_gamma_m0_7_1")
Sys.time() -time0


# Model 0_7_2: mu: -jd -s_anom -moon_phase; hu: -moon_phase
time0 <- Sys.time()
m0_7_2 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(hournorm, bs = "cc", k = 3)+
                   s(tempres2, k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3)+   
                   s(hournorm, bs = "cc", k = 3)+
                   s(jd, k = 3) +
                   s(tempres2, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_2")
Sys.time() -time0


# Model 0_7_3: mu: -jd -s_anom -hournorm; hu: -moon_phase
time0 <- Sys.time()
m0_7_3 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(tempres2, k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3)+   
                   s(hournorm, bs = "cc", k = 3)+
                   s(jd, k = 3) +
                   s(tempres2, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_3")
Sys.time() -time0


# Model 0_7_4: mu: -jd -s_anom ; hu: -moon_phase -hournorm
time0 <- Sys.time()
m0_7_4 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(hournorm, bs = "cc", k = 3)+
                   s(tempres2, k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3)+   
                   s(jd, k = 3) +
                   s(tempres2, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_4")
Sys.time() -time0


# Model 0_7_5: mu: -jd -s_anom -tempres2; hu: -moon_phase
time0 <- Sys.time()
m0_7_5 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(hournorm, bs = "cc", k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3)+   
                   s(hournorm, bs = "cc", k = 3)+
                   s(jd, k = 3) +
                   s(tempres2, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_5")
Sys.time() -time0


# Model 0_7_6: mu: -jd -s_anom ; hu: -moon_phase -tempres2
time0 <- Sys.time()
m0_7_6 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(hournorm, bs = "cc", k = 3)+
                   s(tempres2, k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3)+   
                   s(hournorm, bs = "cc", k = 3)+
                   s(jd, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_6")
Sys.time() -time0


# Model 0_7_7: mu: -jd -s_anom -tempres2; hu: -moon_phase -tempres2
time0 <- Sys.time()
m0_7_7 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(hournorm, bs = "cc", k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3)+   
                   s(hournorm, bs = "cc", k = 3)+
                   s(jd, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_7")
Sys.time() -time0


# Model 0_7_8: mu: -jd -s_anom -tempres2; hu: -moon_phase -tempres2
time0 <- Sys.time()
m0_7_8 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(hournorm, bs = "cc", k = 3)+
                   s(tempres2, k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3)+   
                   s(hournorm, bs = "cc", k = 3)+
                   s(jd, k = 3) +
                   s(tempres2, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_8")
Sys.time() -time0


# Compare ICs of each model:

# Make a vector with our models:
model <- data.frame(model = c("m0_7", "m0_7_1", "m0_7_2", "m0_7_3", "m0_7_4", "m0_7_5", "m0_7_6", "m0_7_7", "m0_7_8"))

waics <- c(waic(m0_7)$waic, waic(m0_7_1)$waic, waic(m0_7_2)$waic, waic(m0_7_3)$waic, waic(m0_7_4)$waic, waic(m0_7_5)$waic, waic(m0_7_6)$waic, 
           waic(m0_7_7)$waic, waic(m0_7_8)$waic)

loos <- c(loo(m0_7)$looic, loo(m0_7_1)$looic, loo(m0_7_2)$looic, loo(m0_7_3)$looic, loo(m0_7_4)$looic, loo(m0_7_5)$looic, loo(m0_7_6)$looic, loo(m0_7_7)$looic, 
          loo(m0_7_8)$looic)

R2s <- c(bayes_R2(m0_7)[1], bayes_R2(m0_7_1)[1], bayes_R2(m0_7_2)[1], bayes_R2(m0_7_3)[1], bayes_R2(m0_7_4)[1], bayes_R2(m0_7_5)[1], bayes_R2(m0_7_6)[1], bayes_R2(m0_7_7)[1], 
         bayes_R2(m0_7_8)[1])

model_ics <- data.frame(model = model,
                        waic = waics,
                        loo = loos,
                        R2 = round(R2s, 3))[order(loos),]

model_ics

# Continue selecting:

# Model 0_7_3_1: mu: -jd -s_anom -hournorm; hu: -moon_phase
time0 <- Sys.time()
m0_7_3_1 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(tempres2, k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(hournorm, bs = "cc", k = 3)+
                   s(jd, k = 3) +
                   s(tempres2, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_3_1")
Sys.time() -time0


# Model 0_7_3_2: mu: -jd -s_anom -hournorm; hu: -hournorm
time0 <- Sys.time()
m0_7_3_2 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(tempres2, k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3)+   
                   s(jd, k = 3) +
                   s(tempres2, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_3_2")
Sys.time() -time0

# Model 0_7_3_3: mu: -jd -s_anom -hournorm; hu: -hournorm -moonphase
time0 <- Sys.time()
m0_7_3_3 <- brm(bf(bft_ab_gs ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year  +
                   s(moon_phase, bs = "cc", k = 3) +   
                   s(tempres2, k = 3),
                 hu ~
                   s(lon, lat, k = 9) +   # Spatial effect
                   year +
                   s(jd, k = 3) +
                   s(tempres2, k = 3) +
                   s(s_anom, k = 3)),
              data = data,
              family = hurdle_gamma(),
              chains = chains,
              cores = cores,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = 0.99),
              file = "out/hurd_gamma_m0_7_3_3")
Sys.time() -time0


# Compare ICs of each model:

# Make a vector with our models:
model <- data.frame(model = c("m0_7_3", "m0_7_3_1", "m0_7_3_2", "m0_7_3_3"))

waics <- c(waic(m0_7_3)$waic, waic(m0_7_3_1)$waic, waic(m0_7_3_2)$waic, waic(m0_7_3_3)$waic)

loos <- c(loo(m0_7_3)$looic, loo(m0_7_3_1)$looic, loo(m0_7_3_2)$looic, loo(m0_7_3_3)$looic)

R2s <- c(bayes_R2(m0_7_3)[1], bayes_R2(m0_7_3_1)[1], bayes_R2(m0_7_3_2)[1], bayes_R2(m0_7_3_3)[1])

model_ics <- data.frame(model = model,
                        waic = waics,
                        loo = loos,
                        R2 = round(R2s, 3))[order(loos),]

model_ics


# Further selection:
# Model 0_7_3_1_1: mu: -jd -s_anom -hournorm; hu: -moon_phase -hournorm
time0 <- Sys.time()
m0_7_3_1_1 <- brm(bf(bft_ab_gs ~
                     s(lon, lat, k = 9) +   # Spatial effect
                     year  +
                     s(moon_phase, bs = "cc", k = 3) +   
                     s(tempres2, k = 3),
                   hu ~
                     s(lon, lat, k = 9) +   # Spatial effect
                     year  +
                     s(jd, k = 3) +
                     s(tempres2, k = 3) +
                     s(s_anom, k = 3)),
                data = data,
                family = hurdle_gamma(),
                chains = chains,
                cores = cores,
                iter = iter,
                warmup = warmup,
                thin = thin,
                control = list(adapt_delta = 0.99),
                file = "out/hurd_gamma_m0_7_3_1_1")
Sys.time() -time0


# Make a vector with our models:
model <- data.frame(model = c("m0_7_3_1", "m0_7_3_1_1"))

waics <- c(waic(m0_7_3_1)$waic, waic(m0_7_3_1_1)$waic)

loos <- c(loo(m0_7_3_1)$looic, loo(m0_7_3_1_1)$looic)

R2s <- c(bayes_R2(m0_7_3_1)[1], bayes_R2(m0_7_3_1_1)[1])

model_ics <- data.frame(model = model,
                        waic = waics,
                        loo = loos,
                        R2 = round(R2s, 3))[order(loos),]


# End of model selection
###################################################################


# Maringal smooths and effects:
m0_7_3_1ms <- marginal_smooths(m0_7_3_1)
m0_7_3_1me <- marginal_effects(m0_7_3_1)

# Plot the marginal smooths of brightness:C

# Abundance
png(filename = "plots/hurd_gamma_model/m0_7_3_1ms_moon_phase_mu.png", width = 12, height = 12, units = "cm", res = 400)
plot(m0_7_3_1ms, line_args = list(color = col2, fill = col5, alpha = .1), theme = theme_base(),  ask = F)[[2]] + 
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0.5, linetype = 2, color = col2) +
  theme(plot.background = element_blank()) +
  labs(x = "Lunar cycle", y = "Smoother effect on spawning magnitude") 
dev.off()

# png(filename = "plots/hurd_gamma_model/m0_7_3_1ms_temp_mu.png", width = 12, height = 12, units = "cm", res = 400)
p2 <- plot(m0_7_3_1ms, line_args = list(color = col2, fill = col5, alpha = .1), theme = theme_base(),  ask = F)[[3]] +
  theme(plot.background = element_blank()) +
  labs(x = "Temperature residuals", y = "Smoother effect on spawning magnitude") 


# Presence:
p3 <- plot(m0_7_3_1ms, line_args = list(color = col2, fill = col5, alpha = .1), theme = theme_base(), ask = F)[[5]] +
  theme(plot.background = element_blank()) +
  labs(x = "Normalized time of the day", y = "Smoother effect on presence of spawning") 

p4 <- plot(m0_7_3_1ms, line_args = list(color = col2, fill = col5, alpha = .1), theme = theme_base(), ask = F)[[6]] +
  theme(plot.background = element_blank()) +
  labs(x = "Day-of-year", y = "Smoother effect on presence of spawning") 

p5 <- plot(m0_7_3_1ms, line_args = list(color = col2, fill = col5, alpha = .1), theme = theme_base(), ask = F)[[7]] +
  theme(plot.background = element_blank()) +
  labs(x = "Temperature residuals", y = "Smoother effect on presence of spawning") 

p6 <- plot(m0_7_3_1ms, line_args = list(color = col2, fill = col5, alpha = .1), theme = theme_base(), ask = F)[[8]] +
  theme(plot.background = element_blank()) +
  labs(x = "Normalized salinity", y = "Smoother effect on presence of spawning") 

# Combine plots:
S4 <- p3 + p4 + p5 + p6 + p2 + plot_layout(ncol = 2) & 
  theme(plot.background = element_blank()) &
  plot_annotation(tag_levels = "a")

ggsave("plots/hurd_gamma_model/S4.png", S4, height = 120 , width = 80, units = "mm", scale = 3)


my_new_data <- data %>%
  mutate(year = NA,
         lat = mean(lat),
         lon = mean(lon),
         hournorm = mean(hournorm),
         jd = mean(jd),
         tempres2 = mean(tempres2),
         t_mld = mean(t_mld),
         s_anom = mean(s_anom))

my_fits <- fitted(m0_7_3_1, newdata = my_new_data)

my_new_data$predictions <- my_fits[, 1]
my_new_data$se <- my_fits[, 2]

p <- ggplot() +
  geom_line(data = my_new_data, aes(x = moon_phase, y = predictions)) +
  geom_ribbon(data = my_new_data, aes(y = predictions, x = moon_phase, ymin = predictions - se, ymax = predictions + se),
              fill = "orange", alpha = .1) +
  theme_base()

p <- plot(m0_7_3_1me, line_args = list(color = col2, fill = col5, alpha = .1), theme = theme_base(),  ask = F)[[5]] + 
  geom_vline(xintercept = 0.5, linetype = 2, color = col2) +
  theme(plot.background = element_blank()) +
  labs(x = "Lunar cycle", y = "Partial effect on spawning magnitude (CPUE)") 

ggsave("plots/hurd_gamma_model/lunar_marginal_effect.png", p, height = 45 , width = 45, units = "mm", scale = 3)


png(filename = "plots/hurd_gamma_model/PSIS.png", width = 90, height = 100, units = "mm", res = 400)
plot(loo(m0_7_3_1))
dev.off()

png(filename = "plots/hurd_gamma_model/loo_pit.png", width = 90, height = 100, units = "mm", res = 400)
pp_check(m0_7_3_1, type = "loo_pit") + 
  theme_base() +
  theme(plot.background = element_blank())
dev.off()

png(filename = "plots/hurd_gamma_model/posterior.png", width = 90, height = 100, units = "mm", res = 400)
pp_check(m0_7_3_1, nsamples = 1000) + 
  xlim(0, 25) +
  xlab("Number of P. noctiluca / tow") +
  ylab("Frequency") +
  theme_base() +
  theme(plot.background = element_blank())
dev.off()

