## Script name: Sensitivity command module
##
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: March 2022
## Last update:  July 2022
##
## ---------------------------
##
## Readme:
##
## This script runs a sensitivity analysis of the model for the coefficient of background mortality, the abundance of mesopelagic fish, and the food abundance
##
## ---------------------------


#####################################################################
# Clear environment:
rm(list = ls())

#####################
# Set time:
time0 <- Sys.time()

# Set english language:
Sys.setlocale("LC_TIME", "English")

# Load packages:
require(tidyverse)
require(patchwork)
require(ggthemes)
require(scales)
library(viridis)
library(mgcv)
library(suncalc)
require(here)
require(lubridate)


# Suppress tidiverse summarise() messages:
options(dplyr.summarise.inform = FALSE)


# Load data:
load("out/master_df_0.RData")
master_df <- as.data.frame(master_df)
master_df_0 <- master_df

load("out/master_df_1.RData")
master_df <- as.data.frame(master_df)
master_df_1 <- master_df

load("out/master_df_2.RData")
master_df <- as.data.frame(master_df)
master_df_2 <- master_df

load("out/master_df_3.RData")
master_df <- as.data.frame(master_df)
master_df_3 <- master_df

load("out/master_df_4.RData")
master_df <- as.data.frame(master_df)
master_df_4 <- master_df

load("out/master_df_5.RData")
master_df <- as.data.frame(master_df)
master_df_5 <- master_df

load("out/master_df_6.RData")
master_df <- as.data.frame(master_df)
master_df_6 <- master_df

load("out/master_df_7.RData")
master_df <- as.data.frame(master_df)
master_df_7 <- master_df

load("out/master_df_8.RData")
master_df <- as.data.frame(master_df)
master_df_8 <- master_df

load("out/master_df_9.RData")
master_df <- as.data.frame(master_df)
master_df_9 <- master_df

load("out/master_df_10.RData")
master_df <- as.data.frame(master_df)
master_df_10 <- master_df

load("out/master_df_11.RData")
master_df <- as.data.frame(master_df)
master_df_11 <- master_df

load("out/master_df_12.RData")
master_df <- as.data.frame(master_df)
master_df_12 <- master_df

load("out/master_df_13.RData")
master_df <- as.data.frame(master_df)
master_df_13 <- master_df

############################################################################
# Make color palette & specify size:
col0 <- "black"
col1 <- "blue"
col2 <- "darkorange"
col3 <- "bisque3"
col4 <- "red"
col5 <- "darkgoldenrod1"
col6 <- "darkblue"
col7 <- "darkturquoise"
col8 <- "darkslategray1"
col9 <- "deeppink"
col10 <- "burlywood"
col11 <- "chartreuse4"

# Set some global parameters for plotting:
size1 = 0.8

#############################################################################

#############################################################################
# Test sensitivity to nauplii abundance:
master_df <- rbind(master_df_0, master_df_1, master_df_2, master_df_3, master_df_4, master_df_5)


# Calculate  daily values
master_df <- master_df %>%
  mutate(date = as.Date(datetime),
         month = month(date)) %>%
  
  group_by(month, test_run) %>%
  summarise(datetime = datetime,
            time_id = time_id,
            yday = yday,
            date = date,
            moon_fraction = moon_fraction,
            temperature = temperature,
            egg_total_survival = egg_total_survival,
            ysl_total_survival = ysl_total_survival,
            fitness = fitness,
            test_run = as.factor(test_run),
            full_moon = case_when(moon_fraction == max(moon_fraction) ~ T,
                                  T ~ F),
            new_moon = case_when(moon_fraction == min(moon_fraction) ~ T,
                                 T ~ F)) %>%
  ungroup() 

master_df_day <- master_df %>%
  group_by(yday, test_run) %>%
  
  summarize(date = date,
            moon_fraction = moon_fraction,
            temperature = temperature,
            test_run,
            
            mean_egg_total_survival = mean(egg_total_survival),
            min_egg_total_survival = min(egg_total_survival),
            max_egg_total_survival = max(egg_total_survival),
            
            mean_ysl_total_survival = mean(ysl_total_survival),
            min_ysl_total_survival = min(ysl_total_survival),
            max_ysl_total_survival = max(ysl_total_survival),
            
            mean_fitness = mean(fitness),
            min_fitness = min(fitness),
            max_fitness = max(fitness),
            
            mean_survival_feeding_larva = mean_fitness / (mean_egg_total_survival * mean_ysl_total_survival),
            
            full_moon = full_moon,
            new_moon = case_when(yday == 61 ~ T,           # Need to add this one manually because March  has 2 new moons
                                 T ~ new_moon)) 


# Plot fitness over a year:
lims <- as.Date(c(120, 330), origin = "2021-12-31")

s1_0 <- ggplot(data = subset(master_df_day, yday %in% 120:330)) +
  geom_point(data = master_df_day %>% filter(full_moon == T, yday %in% 120:330), aes(x = date, y = full_moon), shape = 21, size = 5) +
  geom_point(data = master_df_day %>% filter(new_moon == T, yday %in% 120:330), aes(x = date, y = new_moon), size = 5) +
  labs(x = "", y = "Moon
2022") +
  scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0)), limits = lims) +
  theme_base() +
  theme(plot.background = element_blank(),
        axis.text.y.left = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank())
# 
# s1a <- ggplot(data = subset(master_df_day, yday %in% 120:330 & test_run %in% c(0, 1))) +
#   geom_ribbon(aes(x = date, ymin = min_egg_total_survival, ymax = max_egg_total_survival, fill = test_run), alpha = .3) +
#   geom_path(aes(date, mean_egg_total_survival, colour = test_run), linetype = 1,  size = size1) +
#   scale_color_manual(labels = c("Present", "Absent"), values = c("col2", "col0")) +
#   scale_fill_manual(labels = c("Present", "Absent"), values = c("col2", "col0")) +
#   labs(x = "Spawning time (year h)", y = "Egg survival", color = "Mesopelagic fish", fill = "Mesopelagic fish") +
#   scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
#   theme_base() +
#   theme(plot.background = element_blank(),
#         axis.title.x.bottom = element_blank(),
#         axis.text.x.bottom = element_blank(),
#         legend.position = "none")
# 
# s1b <- ggplot(data = subset(master_df_day, yday %in% 120:330 & test_run %in% c(0, 1))) +
#   geom_ribbon(aes(x = date, ymin = min_ysl_total_survival, ymax = max_ysl_total_survival, fill = test_run), alpha = .3) +
#   geom_path(aes(date, mean_ysl_total_survival, colour = test_run), linetype = 1, size = size1) +
#   scale_color_manual(labels = c("Present", "Absent"), values = c("col2", "col0")) +
#   scale_fill_manual(labels = c("Present", "Absent"), values = c("col2", "col0")) +
#   labs(x = "Spawning time (year h)", y = "YSL survival", color = "Mesopelagic fish", fill = "Mesopelagic fish") +
#   scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
#   theme_base() +
#   theme(plot.background = element_blank(),
#         axis.title.x.bottom = element_blank(),
#         axis.text.x.bottom = element_blank(),
#         legend.position = "none")

# For plotting purposes, set values of fitness < 10^10 = 0
master_df_day_temp <- master_df_day %>% mutate(mean_fitness = case_when(mean_fitness > 10^-10 ~ mean_fitness,
                                                                        T ~ 0),
                                               min_fitness = case_when(mean_fitness > 10^-10 ~ min_fitness,
                                                                       T ~ 0),
                                               max_fitness = case_when(mean_fitness > 10^-10 ~ max_fitness,
                                                                       T ~ 0))

s1b <- ggplot(data = subset(master_df_day_temp, yday %in% 120:330)) + # , yday %in% 150:190
  geom_ribbon(aes(x = date, ymin = min_fitness, ymax = max_fitness, fill = test_run), alpha = .2) +
  geom_path(aes(date, mean_fitness, colour = test_run, linetype = test_run), size = size1) +
  labs(x = "Spawning time (year h)", y = "Larval fitness", color = expression("Mesopelagic fish & nauplii m" ^-3*""), 
       fill = expression("Mesopelagic fish & nauplii m" ^-3*""), linetype = expression("Mesopelagic fish & nauplii m" ^-3*"")) +
  scale_color_manual(labels = c("Present & 400", "Absent & 400", "Present & 500", "Absent & 500", "Present & 300", "Absent & 300"), 
                     values = c(col2, col2, col0,  col0, col1, col1)) +
  scale_fill_manual(labels = c("Present & 400", "Absent & 400", "Present & 500", "Absent & 500", "Present & 300",  "Absent & 300"), 
                    values = c(col2, col2, col0,  col0,col1, col1)) +
  scale_linetype_manual(labels = c("Present & 400", "Absent & 400", "Present & 500",  "Absent & 500",  "Present & 300", "Absent & 300"),
                        values = c(1, 3, 1, 3, 1, 3)) +
  scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_base() +
  theme(plot.background = element_blank(),
        axis.title.x.bottom = element_blank(),
        legend.position = "none",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))


# Zoom in:
lims <- unique(master_df %>% filter(time_id %in% c(3455, 4495)) %>% select(datetime))
lims <- lims$datetime

s1c <- ggplot(data = subset(master_df, time_id %in% 3460:5200 )) +
  geom_vline(xintercept = as.POSIXct(c("2022-06-14 00:00:00", "2022-07-13 00:00:00")), color = col2, alpha = .5) +
  geom_path(aes(datetime, fitness, colour = test_run, alpha = test_run), linetype = 1, size = size1) +
  scale_color_manual(labels = c("Present & 400", "Absent & 400", "Present & 500", "Absent & 500", "Present & 300", "Absent & 300"), 
                     values = c(col2, col2, col0,  col0, col1, col1)) +
  scale_alpha_manual(labels = c("Present & 400", "Absent & 400", "Present & 500",  "Absent & 500",  "Present & 300", "Absent & 300"),
                        values = c(1, .3, 1, .3, 1, .3)) +
  labs(x = "Time of spawning", y = "Larval fitness", color = expression("Mesopelagic fish & nauplii m" ^-3*""), alpha = expression("Mesopelagic fish & nauplii m" ^-3*"")) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_base() +
  theme(plot.background = element_blank(),
        # axis.text.y.left = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom")  +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

# 
# S1 <- s1_0 / s1a / s1b / s1c + plot_layout(ncol = 1,  heights = unit(c(10, 20, 35, 25), c('mm', 'null'))) & 
#   theme(plot.background = element_blank()) &
#   plot_annotation(tag_levels = "a")

S1 <- s1_0 / s1b / s1c + plot_layout(ncol = 1,  heights = unit(c(10, 10, 70), c('mm', 'null'))) & 
  theme(plot.background = element_blank()) &
  plot_annotation(tag_levels = "a")

ggsave("plots/sensitivity/fitness_plot_11_sens_nauplii.png", S1, height = 80 , width = 80, units = "mm", scale = 3)


###########################################################################

###########################################################################

# Test sensitivity to mesopelagic fish:
master_df <- rbind(master_df_0, master_df_1, master_df_10, master_df_11)

# Calculate  daily values
master_df <- master_df %>%
  mutate(date = as.Date(datetime),
         month = month(date)) %>%
  
  group_by(month, test_run) %>%
  summarise(datetime = datetime,
            time_id = time_id,
            yday = yday,
            date = date,
            moon_fraction = moon_fraction,
            temperature = temperature,
            egg_total_survival = egg_total_survival,
            ysl_total_survival = ysl_total_survival,
            fitness = fitness,
            test_run = as.factor(test_run),
            full_moon = case_when(moon_fraction == max(moon_fraction) ~ T,
                                  T ~ F),
            new_moon = case_when(moon_fraction == min(moon_fraction) ~ T,
                                 T ~ F)) %>%
  ungroup() 

master_df_day <- master_df %>%
  group_by(yday, test_run) %>%
  
  summarize(date = date,
            moon_fraction = moon_fraction,
            temperature = temperature,
            test_run,
            
            mean_egg_total_survival = mean(egg_total_survival),
            min_egg_total_survival = min(egg_total_survival),
            max_egg_total_survival = max(egg_total_survival),
            
            mean_ysl_total_survival = mean(ysl_total_survival),
            min_ysl_total_survival = min(ysl_total_survival),
            max_ysl_total_survival = max(ysl_total_survival),
            
            mean_fitness = mean(fitness),
            min_fitness = min(fitness),
            max_fitness = max(fitness),
            full_moon = full_moon,
            new_moon = case_when(yday == 61 ~ T,           # Need to add this one manually because March  has 2 new moons
                                 T ~ new_moon)) 


# Plot fitness over a year:
lims <- as.Date(c(120, 330), origin = "2021-12-31")


# For plotting purposes, set values of fitness < 10^10 = 0
master_df_day_temp <- master_df_day %>% mutate(mean_fitness = case_when(mean_fitness > 10^-10 ~ mean_fitness,
                                                                        T ~ 0),
                                               min_fitness = case_when(mean_fitness > 10^-10 ~ min_fitness,
                                                                       T ~ 0),
                                               max_fitness = case_when(mean_fitness > 10^-10 ~ max_fitness,
                                                                       T ~ 0))

S2 <- ggplot(data = subset(master_df_day_temp, yday %in% 120:210)) +
  geom_ribbon(aes(x = date, ymin = min_fitness, ymax = max_fitness, fill = test_run), alpha = .25) +
  geom_path(aes(date, mean_fitness, colour = test_run, linetype = test_run), size = size1) +
  labs(x = "Spawning time (year h)", y = "Larval fitness", color = ("Density of mesopelagic fish"), 
       fill = ("Density of mesopelagic fish"), linetype = ("Density of mesopelagic fish")) +
  scale_color_manual(labels = c("x 1", "x 0", "x 0.1", "x 10"), 
                     values = c(col2, col0, col1, col11)) +
  scale_fill_manual(labels = c("x 1", "x 0", "x 0.1", "x 10"), 
                    values = c(col2, col0, col1, col11)) +
  scale_linetype_manual(labels = c("x 1", "x 0", "x 0.1", "x 10"),
                        values = c(1, 3, 1, 1)) +
  scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_base() +
  theme(plot.background = element_blank(),
        axis.title.x.bottom = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

ggsave("plots/sensitivity/fitness_plot_11_sens_meso_fish.png", S2, height = 60 , width = 80, units = "mm", scale = 2)


###########################################################################

###########################################################################
# Sensitivity to background mortality:

# Edit table:
master_df <- rbind(master_df_0, master_df_1, master_df_6, master_df_7, master_df_8, master_df_9)

# Calculate  daily values
master_df <- master_df %>%
  mutate(date = as.Date(datetime),
         month = month(date)) %>%
  
  group_by(month, test_run) %>%
  summarise(datetime = datetime,
            time_id = time_id,
            yday = yday,
            date = date,
            moon_fraction = moon_fraction,
            temperature = temperature,
            egg_total_survival = egg_total_survival,
            ysl_total_survival = ysl_total_survival,
            fitness = fitness,
            test_run = as.factor(test_run),
            full_moon = case_when(moon_fraction == max(moon_fraction) ~ T,
                                  T ~ F),
            new_moon = case_when(moon_fraction == min(moon_fraction) ~ T,
                                 T ~ F)) %>%
  ungroup() 

master_df_day <- master_df %>%
  group_by(yday, test_run) %>%
  
  summarize(date = date,
            moon_fraction = moon_fraction,
            temperature = temperature,
            test_run,
            
            mean_egg_total_survival = mean(egg_total_survival),
            min_egg_total_survival = min(egg_total_survival),
            max_egg_total_survival = max(egg_total_survival),
            
            mean_ysl_total_survival = mean(ysl_total_survival),
            min_ysl_total_survival = min(ysl_total_survival),
            max_ysl_total_survival = max(ysl_total_survival),
            
            mean_fitness = mean(fitness),
            min_fitness = min(fitness),
            max_fitness = max(fitness),
            full_moon = full_moon,
            new_moon = case_when(yday == 61 ~ T,           # Need to add this one manually because March  has 2 new moons
                                 T ~ new_moon)) 



# Plot fitness over a year:
lims <- as.Date(c(120, 330), origin = "2021-12-31")


# For plotting purposes, set values of fitness < 10^10 = 0
master_df_day_temp <- master_df_day %>% mutate(mean_fitness = case_when(mean_fitness > 10^-10 ~ mean_fitness,
                                                                        T ~ 0),
                                               min_fitness = case_when(mean_fitness > 10^-10 ~ min_fitness,
                                                                       T ~ 0),
                                               max_fitness = case_when(mean_fitness > 10^-10 ~ max_fitness,
                                                                       T ~ 0))

S3 <- ggplot(data = subset(master_df_day_temp, yday %in% 120:210)) +
  geom_ribbon(aes(x = date, ymin = min_fitness, ymax = max_fitness, fill = test_run), alpha = .25) +
  geom_path(aes(date, mean_fitness, colour = test_run, linetype = test_run), size = size1) +
  labs(x = "Spawning time (year h)", y = "Larval fitness", color = ("Mesopelagic fish & scaling 
coefficient for background mortality"), 
       fill = ("Mesopelagic fish & scaling 
coefficient for background mortality"), linetype = ("Mesopelagic fish & scaling 
coefficient for background mortality")) +
  scale_color_manual(labels = c("Present & 0.85", "Absent & 0.85", "Present & 1", "Absent & 1", "Present & 0.5", "Absent & 0.5"), 
                     values = c(col2, col2, col0, col0, col1, col1)) +
  scale_fill_manual(labels = c("Present & 0.85", "Absent & 0.85", "Present & 1", "Absent & 1", "Present & 0.5", "Absent & 0.5"), 
                    values = c(col2, col2, col0, col0, col1, col1)) +
  scale_linetype_manual(labels = c("Present & 0.85", "Absent & 0.85", "Present & 1", "Absent & 1", "Present & 0.5", "Absent & 0.5"),
                        values = c(1, 3, 1, 3, 1, 3)) +
  scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_base() +
  theme(plot.background = element_blank(),
        axis.title.x.bottom = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

ggsave("plots/sensitivity/fitness_plot_11_sens_bm.png", S3, height = 60 , width = 80, units = "mm", scale = 2)



###########################################################################

###########################################################################
# Sensitivity to invertebrate 0-padding:

# Edit table:
master_df <- rbind(master_df_0, master_df_13)

# Calculate  daily values
master_df <- master_df %>%
  mutate(date = as.Date(datetime),
         month = month(date)) %>%
  
  group_by(month, test_run) %>%
  summarise(datetime = datetime,
            time_id = time_id,
            yday = yday,
            date = date,
            moon_fraction = moon_fraction,
            temperature = temperature,
            egg_total_survival = egg_total_survival,
            ysl_total_survival = ysl_total_survival,
            fitness = fitness,
            test_run = as.factor(test_run),
            full_moon = case_when(moon_fraction == max(moon_fraction) ~ T,
                                  T ~ F),
            new_moon = case_when(moon_fraction == min(moon_fraction) ~ T,
                                 T ~ F)) %>%
  ungroup() 

master_df_day <- master_df %>%
  group_by(yday, test_run) %>%
  
  summarize(date = date,
            moon_fraction = moon_fraction,
            temperature = temperature,
            test_run,
            
            mean_egg_total_survival = mean(egg_total_survival),
            min_egg_total_survival = min(egg_total_survival),
            max_egg_total_survival = max(egg_total_survival),
            
            mean_ysl_total_survival = mean(ysl_total_survival),
            min_ysl_total_survival = min(ysl_total_survival),
            max_ysl_total_survival = max(ysl_total_survival),
            
            mean_fitness = mean(fitness),
            min_fitness = min(fitness),
            max_fitness = max(fitness),
            full_moon = full_moon,
            new_moon = case_when(yday == 61 ~ T,           # Need to add this one manually because March  has 2 new moons
                                 T ~ new_moon)) 



# Plot fitness over a year:
lims <- as.Date(c(120, 330), origin = "2021-12-31")


# For plotting purposes, set values of fitness < 10^10 = 0
master_df_day_temp <- master_df_day %>% mutate(mean_fitness = case_when(mean_fitness > 10^-10 ~ mean_fitness,
                                                                        T ~ 0),
                                               min_fitness = case_when(mean_fitness > 10^-10 ~ min_fitness,
                                                                       T ~ 0),
                                               max_fitness = case_when(mean_fitness > 10^-10 ~ max_fitness,
                                                                       T ~ 0))

S4 <- ggplot(data = subset(master_df_day_temp, yday %in% 120:210)) +
  geom_ribbon(aes(x = date, ymin = min_fitness, ymax = max_fitness, fill = test_run), alpha = .25) +
  geom_path(aes(date, mean_fitness, colour = test_run, linetype = test_run), size = size1) +
  labs(x = "Spawning time (year h)", y = "Larval fitness", color = ("Invertebrate predators 
before May and after July"), 
       fill = ("Invertebrate predators 
before May and after July"), linetype = ("Invertebrate predators 
before May and after July")) +
  scale_color_manual(labels = c(expression("1.1 invertebrates m" ^-3*""), expression("0 invertebrates m" ^-3*"")), 
                     values = c(col2, col0)) +
  scale_fill_manual(labels = c(expression("1.1 invertebrates m" ^-3*""), expression("0 invertebrates m" ^-3*"")), 
                    values = c(col2, col0)) +
  scale_linetype_manual(labels = c(expression("1.1 invertebrates m" ^-3*""), expression("0 invertebrates m" ^-3*"")),
                        values = c(1, 3)) +
  scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_base() +
  theme(plot.background = element_blank(),
        axis.title.x.bottom = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

ggsave("plots/sensitivity/S4.png", S4, height = 30 , width = 40, units = "mm", scale = 3)



# Time out:
print(Sys.time() - time0)

#                                          END OF SCRIPT
#############################################################################################################