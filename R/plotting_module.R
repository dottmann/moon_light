
## Script name: Model plotting module
##
##
## Author: Daniel Ottmann
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: November 2021
## Last update:  July 2022
##
## ---------------------------
##
## Readme:
##
## This script plots the light field, the environmental variables, the myctophid distribution and the larval fitness
## It also plots many state variables involved in the fitness model
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
col9 <- "purple"
col10 <- "burlywood"

size1 = 1.2


##############
# Lightfield:

if(plot_lightfield == T){
  
  # Load data: 
  load("data/light_field.RData")
  load("data/irradiance_df.RData")
  attenuation_df <- read.delim("data/modeled_coeff_light_extinction_tuna.txt") %>%
    rename(K = modeled_coefficient_extinction)
  
  
  
  # Plot 1: surface irrdiance on log and linear scale
  
  #  Get the surface light environment, and make a quick test plot
  p1a <- irradiance_df %>%
    
    filter(between(yday, 170, 174)) %>%              # Activate to narrow down the days plotted, "yday" the days of the year from 1- 365
    # filter(hour %in% c(12)) %>%                    # Activate if you only want to see e.g. midday values at 12 o'clock
    ggplot(., aes(x = datetime, y = irradiance)) +
    geom_line(col = col1, size = size1) +
    geom_vline(xintercept = pull(filter(irradiance_df, hour == 12), datetime), linetype = "dashed") +   # Activate to get vertical lines at midday
    # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +   # Activate to plot on log axis
    labs(x = "", y = "Irradiance") +
    scale_x_datetime(date_labels = "%d %h") +
    theme_base()
  
  p1b <- irradiance_df %>%
    filter(between(yday, 170, 174)) %>%               # Activate to narrow down the days plotted, "yday" the days of the year from 1- 365
    # filter(hour %in% c(12)) %>%                     #  activate if you only want to see e.g. midday values at 12 o'clock
    ggplot(., aes(x = datetime, y = irradiance)) +
    geom_line(col = col1, size = size1) +
    geom_vline(xintercept = pull(filter(irradiance_df, hour == 12), datetime), linetype = "dashed") +   # Activate to get vertical lines at midday
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +   # Activate to plot on log axis
    labs(x = "", y = "") +
    scale_x_datetime(date_labels = "%d %h") +
    theme_base()
  
  p1 <- p1a + p1b & theme(plot.background = element_blank())
  
  ggsave("plots/light_plot_1.png", p1, height = 40 , width = 160, units = "mm", scale = 3)
  
  # Plot 2a: K (diffuse attenuation coefficient) over depth
  p2a <- attenuation_df %>%
    ggplot(., aes(x = K, y = depth_m)) +
    geom_path(col = col1, size = size1) +
    scale_y_reverse() +
    theme_base() +
    labs(y = "Depth [m]", x = expression(italic(K)))
  
  
  # Plot 2b: Ambient light over depth
  p2b <- light_field_df %>%
    filter(yday == 172, hour %in% c(0, 12)) %>%
    ggplot() +
    geom_path(aes(x = Ez, y = depth_m, group = hour, linetype = as.factor(hour)), col = col1, size = size1) +
    scale_linetype_manual(values = c(1, 2), labels = c("Midnight", "Midday"), name="") +
    guides(linetype = guide_legend(label.position = "left", label.hjust = 1)) +
    scale_y_reverse() +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    labs(y = "Depth [m]", x = expression(italic("E")["z"]), caption = "Light over depth during midsummer at midday and midnight") +
    theme_base() +
    theme(plot.title = element_text(hjust = 0, size = 10),
          legend.position = c(0.8, 0.2),
          legend.direction = "vertical",
          axis.text.y.left = element_blank(),
          axis.title.y.left = element_blank(),
          legend.key.width = unit(15,"mm"))
  
  p2 <- p2a + p2b + plot_layout( width = c(2,4)) & theme(plot.background = element_blank())
  
  ggsave("plots/light_plot_2.png", p2, height = 80 , width = 100, units = "mm", scale = 3)
  
  # Plot 3: 
  p3a <- irradiance_df %>%
    filter(hour %in% c(0, 6, 12)) %>%
    ggplot() +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_line(aes(x = datetime, y = solalt, group = hour, linetype = as.factor(hour)), col = col1, size = size1) +
    scale_linetype_manual(values = c(1, 2, 3), labels = c("Midnight", "6am/pm", "Midday"), name = "") +
    guides(linetype = guide_legend(label.position = "left", label.hjust = 1)) +
    scale_x_datetime(date_labels = "%b", date_breaks = "months") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank()) +
    labs(y = "Solar elevation [?]")+
    guides(linetype = "none")
  
  p3b <- irradiance_df %>%
    filter(hour %in% c(0, 6, 12)) %>%
    ggplot() +
    geom_line(aes(x = datetime, y = irradiance, group = hour, linetype = as.factor(hour)), col = col1, size = size1) +
    scale_linetype_manual(values = c(1, 2, 3), labels = c("Midnight", "6am/pm", "Midday"), name = "") +
    guides(linetype = guide_legend(label.position = "left", label.hjust = 1)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    scale_x_datetime(date_labels = "%b", date_breaks = "months") +
    theme_base() +
    theme(plot.background = element_blank(),
          legend.position = c(0.85, 0.5),
          legend.direction = "vertical",
          legend.key.width = unit(15,"mm")) +
    labs(x = "", y = "Irradiance")
  
  
  p3 <- p3a / p3b + plot_layout(heights = c(2, 4)) & theme(plot.background = element_blank())
  
  ggsave("plots/light_plot_3.png", p3, height = 80 , width = 80, units = "mm", scale = 3)
  
  # Plot 4: 
  p4 <- irradiance_df %>% 
    ggplot(aes(x = solalt, y = irradiance)) +
    geom_point(shape = 1, col = col1) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    theme_base() +
    theme(plot.background = element_blank()) +
    geom_vline(xintercept = 0, linetype = "longdash") +
    labs(x = "Solar elevation [?]", y = "Irradiance") +
    theme(plot.background = element_blank())
  
  ggsave("plots/light_plot_4.png", p4, height = 60 , width = 60, units = "mm", scale = 2)
  
  
  # Plot 5: 
  p5 <- light_field_df %>%
    filter(hour %in% c(0, 6, 12)) %>%
    mutate(hour_lab = as.factor(case_when(hour == 12 ~ "Midday",
                                          hour %in% c(6, 18) ~ "6am/pm",
                                          hour == 0 ~ "Midnight"))) %>% 
    ggplot() +
    geom_raster(aes(x = datetime, y = depth_m, fill = log10(Ez))) +
    geom_contour(aes(x = datetime, y = depth_m, z = Ez), breaks = 10^seq(-20, 4, 1), col = "white") +
    metR::geom_text_contour(aes(x = datetime, y = depth_m, z = Ez), breaks = 10^seq(-20, 3), stroke = 0.2) +
    scale_fill_viridis(option = "cividis") +
    scale_x_datetime(expand = expansion(mult = c(0, 0))) +
    scale_y_reverse(expand = expansion(mult = c(0, 0))) +
    facet_wrap(vars(hour_lab)) +
    labs(x ="", y = "Depth [m]") +
    theme_base() +
    theme(plot.background = element_blank())
  
  ggsave("plots/light_plot_5.png", p5, height = 80 , width = 160, units = "mm", scale = 3)
  
  # Plot 6: 
  p6 <- light_field_df %>%
    filter(yday %in% c(79, 172, 266, 365)) %>%
    mutate(yday_lab = as.factor(case_when(yday == 79 ~ "Spring equinox",
                                          yday == 172 ~"Summer solstice",
                                          yday == 266 ~ "Fall equinox",
                                          yday == 365 ~ "Winter solstice"))) %>% 
    ggplot(aes(x = hour, y = depth_m, z = Ez, fill = log10(Ez))) +
    geom_raster() +
    geom_contour(breaks = 10^seq(-20, 4, 2), col = "white") +
    metR::geom_text_contour(breaks = 10^seq(-20, 4, 2), stroke = 0.2) +
    scale_fill_viridis(option = "cividis") +
    scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                       breaks = c(0, 6, 12, 18), labels = c("Midnight", "6am", "Midday", "6pm")) +
    scale_y_reverse(expand = expansion(mult = c(0, 0))) +
    facet_wrap(vars(yday_lab), nrow = 1) +
    labs(x = "", y = "Depth [m]") +
    theme_base() +
    theme(plot.background = element_blank())
  
  ggsave("plots/light_plot_6.png", p6, height = 80 , width = 160, units = "mm", scale = 3)
  
} 

##########################
# Seasonal environment:
# (including mesopelagic fish)

if(plot_environment == T){
  
  # Load data: 
  load("data/environmental_df.RData")
  load("data/max_myctophid_df.RData")
  load("data/irradiance_df.RData")
  
  p7a <- environmental_df %>%
    mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise(date = unique(date),
              temperature = unique(temperature),
              clado_dens = unique(clado_dens),
              light_duration = sum(light_lim)) %>%
    ggplot() +
    geom_path(aes(x = date, y = temperature), linetype = 1, colour = col1, size = size1) +
    scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    ylab("T (?C)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  
  p7b <- environmental_df %>%
    mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise(date = unique(date),
              temperature = unique(temperature),
              clado_dens = unique(clado_dens),
              light_duration = sum(light_lim)) %>%
    ggplot() +
    geom_path(aes(x = date, y = clado_dens), linetype = 1, colour = col1, size = size1) +
    scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    ylab(expression("Cladocera m" ^-3*"")) +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  
  p7c <- environmental_df %>%
    mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise(date = unique(date),
              temperature = unique(temperature),
              clado_dens = unique(clado_dens),
              light_duration = sum(light_lim),
              invertebrate_dens = unique(invertebrate_dens)) %>%
    ggplot() +
    geom_path(aes(x = date, y = invertebrate_dens), linetype = 1, colour = col1, size = size1) +
    scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    ylab(expression("Metaephyrae m" ^-3*"")) +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank())
  
  
  p7d <- environmental_df %>%
    mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise(date = unique(date),
              temperature = unique(temperature),
              clado_dens = unique(clado_dens),
              light_duration = sum(light_lim)) %>%
    ggplot() +
    geom_path(aes(x = date, y = light_duration), linetype = 1, colour = col1, size = size1) +
    scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    ylab("Hours of light") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank())
  
  
  p7e <- environmental_df %>%
    mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise(date = unique(date),
              temperature = unique(temperature),
              clado_dens = unique(clado_dens),
              light_duration = sum(light_lim),
              moon_fraction = unique(moon_fraction)) %>%
    ggplot() +
    geom_tile(aes(x = date, y = 0.5, fill = moon_fraction), alpha = .03) +
    geom_path(aes(x = date, y = moon_fraction), linetype = 1, colour = col1, size = size1) +
    scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    scale_color_viridis_c(option = "cividis") +
    scale_fill_viridis_c(option = "cividis", name = "Moon brightness") +
    ylab("Moon fraction") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          legend.position = "none")
  
  
  p7 <- p7a / p7b / p7c / p7d / p7e & theme(plot.background = element_blank())
  
  ggsave("plots/environment_plot_7.png", p7, height = 120 , width = 80, units = "mm", scale = 2)
  
  # Make dual axis plots:
  p7a <- environmental_df %>%
    mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise(date = unique(date),
              temperature = unique(temperature),
              clado_dens = unique(clado_dens),
              nauplia_dens = nauplia_dens,
              light_duration = sum(light_lim)) %>%
    ggplot() +
    geom_path(aes(x = date, y = temperature), linetype = 1, colour = col4, size = size1) +
    geom_path(aes(x = date, y = light_duration * 1), linetype = 1, colour = col5, size = size1) +
    scale_x_date(date_labels = "%b", breaks = "months") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~.* 1/1, name = expression("Hours of light"))) +
    ylab("T (?C)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          # axis.line.y.left = element_line(color = col4), 
          axis.ticks.y.left = element_line(color = col4),
          axis.text.y.left = element_text(color = col4),
          axis.title.y.left = element_text(color = col4),
          # axis.line.y.right = element_line(color = col5), 
          axis.ticks.y.right = element_line(color = col5),
          axis.text.y.right = element_text(color = col5),
          axis.title.y.right = element_text(color = col5))
  
  p7b <- environmental_df %>%
    mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise(date = unique(date),
              temperature = unique(temperature),
              clado_dens = unique(clado_dens),
              nauplia_dens = nauplia_dens,
              light_duration = sum(light_lim)) %>%
    ggplot() +
    geom_path(aes(x = date, y = clado_dens), linetype = 1, colour = col2, size = size1) +
    geom_path(aes(x = date, y = nauplia_dens * 1/40), linetype = 1, colour = col3, size = size1) +
    scale_x_date(date_labels = "%b", breaks = "months") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~.* 40, name = expression("Nauplii m" ^-3*""))) +
    ylab(expression("Cladocera m" ^-3*"")) +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          # axis.line.y.left = element_line(color = col2), 
          axis.ticks.y.left = element_line(color = col2),
          axis.text.y.left = element_text(color = col2),
          axis.title.y.left = element_text(color = col2),
          # axis.line.y.right = element_line(color = col3), 
          axis.ticks.y.right = element_line(color = col3),
          axis.text.y.right = element_text(color = col3),
          axis.title.y.right = element_text(color = col3))
  
  p7c <- irradiance_df %>%
    filter(hour %in% c(0, 6, 12)) %>%
    ggplot() +
    geom_path(aes(x = datetime, y = irradiance, group = hour,  colour = as.factor(hour)), size = size1) +
    scale_color_manual(values = c(col6, col7, col8), labels = c("Midnight", "6am/pm", "Midday")) +
    #  scale_linetype_manual(values = c(3, 2, 1), labels = c("Midnight", "6am/pm", "Midday"), name = "") +
    guides(linetype = guide_legend(label.position = "left", label.hjust = 1)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    scale_x_datetime(date_labels = "%b", date_breaks = "months") +
    ylab(expression("Irradiance (mW m" ^-2*" nm" ^-1*")")) +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          # axis.text.x.bottom = element_blank(),
          legend.position = "top", #c(0.45, 0.6),
 #         legend.direction = "vertical",
          legend.title = element_blank(),
          # axis.line.y = element_line(color = col6), 
          axis.ticks.y = element_line(color = col6),
          axis.text.y = element_text(color = col6),
          axis.title.y = element_text(color = col6))
    
  
  
  p7d <- environmental_df %>%
    mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise(yday = yday,
              date = unique(date),
              temperature = unique(temperature),
              clado_dens = unique(clado_dens),
              light_duration = sum(light_lim),
              invertebrate_dens = unique(invertebrate_dens)) %>%
    left_join(max_myctophid_df, by = "yday") %>%
    ggplot() +
    geom_path(aes(x = date, y = invertebrate_dens), linetype = 1, colour = col9, size = size1) +
    geom_path(aes(x = date, y = max_myctophid_overlaping_density * 45), linetype = 1, colour = col0, alpha = .8, size = size1) +
    scale_x_date(date_labels = "%b", breaks = "months") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~.* 1/45, name = expression("Mesopelagic fish m" ^-3*""))) +
    ylab(expression("Inv. predators m" ^-3*"")) +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x = (element_text(size = 11)),
          # axis.line.y.left = element_line(color = col9), 
          axis.ticks.y.left = element_line(color = col9),
          axis.text.y.left = element_text(color = col9),
          axis.title.y.left = element_text(color = col9),
          # axis.line.y.right = element_line(color = col10), 
          axis.ticks.y.right = element_line(color = col0),
          axis.text.y.right = element_text(color = col0),
          axis.title.y.right = element_text(color = col0))
  
  
  p7 <- p7a + p7b + p7c + p7d & theme(plot.background = element_blank()) & plot_annotation(tag_levels = "a")
  
  ggsave("plots/environment_plot_7_dual.png", p7, height = 60 , width = 110, units = "mm", scale = 3)
  
}


###################
# Mesopelagic fish:

if(plot_mictophids == T){
  
  # Load data: 
  load("data/myctophid_df.RData")
  
  prey_depth_limit <- 25
  
  # Plot 8:
  quarter_moons <- c(172, 180, 188, 195)
  lims <- as.POSIXct(as.Date(c(171, 199), origin = "2021-12-31"))
  
  p8_0 <- myctophid_df %>%
    filter(yday %in% quarter_moons, hour == 0)  %>%
    mutate(date = as.Date(yday, origin = "2021-12-31"),
           moon_brightness = round(getMoonIllumination(date = date,
                                                 keep = "fraction")$fraction, 1),
           moon_phase = getMoonIllumination(date = date,
                                            keep = "phase")$phase,
           right = case_when(moon_phase < .5 ~ T,
                             T ~ F)) %>%
    ggplot() +
    geom_moon(aes(x = datetime, y = 1, ratio = 1), fill = col0, size = 20) +
    geom_moon(aes(x = (datetime), y = 1, ratio = moon_brightness, right = right), size = 19, fill = "white") +  # , ratio = ratio, right = right, fill = right
    scale_x_datetime(date_labels = "%d %h", breaks = "weeks", expand = expansion(mult = c(0, 0)), limits = lims) +  
    labs(x = "", y = "Moon") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  

  p8a <- light_field_df %>%
    filter(yday %in% c(171:199)) %>%
    ggplot(aes(x = datetime, y = depth_m, z = Ez, fill = log10(Ez))) +
    geom_raster() +
    scale_fill_viridis(option = "cividis") +
    geom_contour(breaks = 10^seq(-20, 4, 1), col= "white", alpha = 0.4) +
    scale_x_datetime(date_labels = "%d %h", breaks = "weeks", expand = expansion(mult = c(0, 0))) +
    scale_y_reverse(expand = expansion(mult = c(0, 0)), limits = c(600, 0)) +
    labs(x = "", y = "Depth [m]") + #, subtitle = "Ambient light") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p8b <- myctophid_df %>%
    filter(yday %in% c(171:199)) %>%
    ggplot() +
    geom_ribbon(aes(x = datetime, ymin = lcz_upr, ymax = lcz_lwr), fill = "grey", alpha = 0.5, inherit.aes = F) +
    geom_path(aes(x = datetime, y = lcz_upr),  inherit.aes = F, linetype = "dashed") +
    geom_path(aes(x = datetime, y = lcz_lwr),  inherit.aes = F, linetype = "dashed") +
    geom_hline(yintercept = prey_depth_limit) +
    scale_x_datetime(date_labels = "%d %h", breaks = "weeks", expand = expansion(mult = c(0, 0))) +
    scale_y_reverse(expand = expansion(mult = c(0, 0)), limits = c(600, 0)) +
    labs(x = "", y = "Depth [m]") + #, subtitle = "Myctophid light comfort zone (Norheim et al. 2016)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  
  p8 <- p8a / p8b & theme(plot.subtitle = element_text(hjust = 0, size = 10))
  
  ggsave("plots/myctophid_plot_8.png", p8, height = 80 , width = 160, units = "mm", scale = 3)
  
  
  # Plot 9:
  p9a <- myctophid_df %>%
    filter(yday %in% c(79, 172, 365)) %>%
    mutate(yday = as.factor(case_when(yday == 79 ~ "Spring/fall equinox",
                                      yday == 172 ~"Summer solstice",
                                      yday == 365 ~ "Winter solstice"))) %>% 
    ggplot() +
    geom_path(aes(x = hour, y = overlapping_proportion * 100, linetype = as.factor(yday)), col = col1, size = size1) +
    scale_linetype_manual(values = c(1, 2, 3)) +
    theme_base() +
    labs(x = "", y = "% population") + #, subtitle = "Poportion of myctophid population in the top 25m") +
    scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                       breaks = c(0, 6, 12, 18), labels = c("Midnight", "6am", "Midday", "6pm")) +
    scale_y_continuous(limits = c(0, 100)) +
    theme(legend.title = element_blank())
  
  
  p9b <- myctophid_df %>%
    filter(yday %in% c(79, 172, 365)) %>%
    mutate(yday = as.factor(case_when(yday == 79 ~ "Spring/fall equinox",
                                      yday == 172 ~"Summer solstice",
                                      yday == 365 ~ "Winter solstice"))) %>% 
    ggplot() +
    geom_path(aes(x = hour, y = myctophid_overlaping_density, linetype = as.factor(yday)), col = col1, size = size1) +
    scale_linetype_manual(values = c(1, 2, 3)) +
    theme_base() +
    labs(x = "", y = expression("Myctophids m" ^-3*"")) + #, subtitle = "Density of myctophid  in the top 25m") +
    scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                       breaks = c(0, 6, 12, 18), labels = c("Midnight", "6am", "Midday", "6pm")) +
    theme(legend.title = element_blank(),
          legend.position = "none")
  
  p9 <- p9a / p9b & theme(plot.background = element_blank())
  
  
  ggsave("plots/myctophid_plot_9.png", p9, height = 80 , width = 100, units = "mm", scale = 2)
  
  
  # Plot 10:
  p10a <- myctophid_df %>%
    filter(yday %in% c(171:199)) %>%
    ggplot() +
    geom_path(aes(x = datetime, y = overlapping_proportion * 100), col = col10, size = size1) +
    theme_base() +
    labs(x = "", y = "% mesopelagic fish population") + #, subtitle = "Poportion of myctophid population in the top 25m") +
    scale_x_datetime(date_labels = "%d %h", breaks = "weeks", expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(limits = c(0, 100)) +
    theme(legend.title = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  
  p10b <- myctophid_df %>%
    filter(yday %in% c(171:199)) %>%
    ggplot() +
    geom_path(aes(x = datetime, y = myctophid_overlaping_density), col = col10, size = size1) +
    theme_base() +
    labs(x = "", y = expression("Mesopelagic fish m" ^-3*"")) + #, subtitle = "Density of myctophid in the top 25m") +
    scale_x_datetime(date_labels = "%d %h", breaks = "weeks", expand = expansion(mult = c(0, 0))) +
    theme(legend.title = element_blank(),
          legend.position = "none")
  
  p10 <- p8_0 / p8a / p8b / p10b + plot_layout(ncol = 1, heights = unit(c(2, 1, 6, 1), c('cm', 'null'))) &
    plot_annotation(tag_levels = "a") & theme(plot.background = element_blank()) 
  
  ggsave("plots/myctophid_plot_10.png", p10, height = 110 , width = 100, units = "mm", scale = 2)
  
}


##############
# Fitness:

if(plot_fitness == T){
  
  # Load data:
  load("out/master_df_0.RData")
  master_df <- as.data.frame(master_df)
  master_df_0 <- master_df
  
  # Load data:
  load("out/master_df_2021.RData")
  master_df <- as.data.frame(master_df)
  master_df_0_2021 <- master_df
  
  load("out/master_df_1.RData")
  master_df <- as.data.frame(master_df)
  master_df_1 <- master_df
  
  load("out/master_df_2.RData")
  master_df <- as.data.frame(master_df)
  master_df_2 <- master_df
  
  load("out/master_df_3.RData")
  master_df <- as.data.frame(master_df)
  master_df_3 <- master_df
  
  
  load("out/output_matrices_0.RData")
  
  master_df <- rbind(master_df_0, master_df_1, master_df_0_2021)
  list2env(output_matrices, globalenv())
  

  ##################################################################
  
  # # Plot all columns of master_df:
  # for (i in 1:length(master_df_0)) {
  #   png(paste0("plots/master_df/", names(master_df[i]), ".png"), width = 20, height = 15, units = "cm", res = 100)
  #   plot(master_df[, i], main = names(master_df[i]))
  #   dev.off()
  # }
  
  # Change name of test_run to get them in the right order:
  master_df <- master_df %>% 
    mutate(test_run = case_when(test_run == 1 ~ 1,
                                test_run == 2021 ~ 2,
                                T ~ 3))

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

  p11_0 <- ggplot(data = subset(master_df_day, yday %in% 120:330)) +
    geom_point(data = master_df_day %>% filter(full_moon == T, yday %in% 120:330), aes(x = date, y = full_moon), shape = 21, size = 5) +
    # geom_point(data = master_df_day %>% filter(new_moon == T, yday %in% 120:330), aes(x = date, y = new_moon), size = 5, color = col4) +
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

  p11a <- ggplot(data = subset(master_df_day, yday %in% 120:330 & test_run %in% c(1, 2, 3))) +
    # geom_vline(xintercept = as.Date(c("2022-02-16", "2022-03-18", "2022-04-16", "2022-05-16", 
    #                                   "2022-06-14", "2022-07-13", "2022-08-12", "2022-09-10", 
    #                                   "2022-10-09", "2022-11-08")), color = col4, alpha = .3) +
    geom_ribbon(aes(x = date, ymin = min_egg_total_survival, ymax = max_egg_total_survival, fill = test_run), alpha = .3) +
    geom_path(aes(date, mean_egg_total_survival, colour = test_run), linetype = 1,  size = size1) +
    geom_path(aes(date, (temperature - 13) * 1/100), linetype = 2,  size = size1, color = col4) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~.* 100/1 + 13, name = expression("T (?C)"))) +
    scale_color_manual(labels = c("Present", "Absent"), values = c(col0, col1, col2)) +
    scale_fill_manual(labels = c("Present", "Absent"), values = c(col0, col1, col2)) +
    labs(x = "Spawning time (year h)", y = "Egg survival", color = "Mesopelagic fish", fill = "Mesopelagic fish") +
    scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          axis.title.y.right = element_text(color = col4),
          axis.text.y.right = element_text(color = col4),
          legend.position = "none")
  
  p11b <- ggplot(data = subset(master_df_day, yday %in% 120:330 & test_run %in% c(1, 3))) +
    geom_ribbon(aes(x = date, ymin = min_ysl_total_survival, ymax = max_ysl_total_survival, fill = test_run), alpha = .3) +
    geom_path(aes(date, mean_ysl_total_survival, colour = test_run), linetype = 1, size = size1) +
    scale_color_manual(labels = c("Present", "Absent"), values = c(col0, col2)) +
    scale_fill_manual(labels = c("Present", "Absent"), values = c(col0, col2)) +
    labs(x = "Spawning time (year h)", y = "YSL survival", color = "Mesopelagic fish", fill = "Mesopelagic fish") +
    scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          legend.position = "none")
  
  # For plotting purposes, set values of fitness < 10^10 = 0
  master_df_day_temp <- master_df_day %>% mutate(mean_fitness = case_when(mean_fitness > 10^-10 ~ mean_fitness,
                                                                          T ~ 0),
                                                 min_fitness = case_when(mean_fitness > 10^-10 ~ min_fitness,
                                                                         T ~ 0),
                                                 max_fitness = case_when(mean_fitness > 10^-10 ~ max_fitness,
                                                                         T ~ 0))
  
  p11c <- ggplot(data = subset(master_df_day_temp, yday %in% 120:330)) + # , yday %in% 150:190
    # geom_vline(xintercept = as.Date(c("2022-02-16", "2022-03-18", "2022-04-16", "2022-05-16", "2022-06-14", 
    #                                   "2022-07-13", "2022-08-12", "2022-09-10", "2022-10-09", "2022-11-08")), 
    #            color = col4, alpha = .3) +
    # geom_vline(xintercept = as.Date(c("2022-02-27", "2022-03-28", "2022-04-27", "2022-05-24", "2022-06-24", 
    #                                   "2022-07-24", "2022-08-22", "2022-09-21", "2022-10-20", "2022-11-2019")),
    #            color = col2, alpha = .3) +
    geom_ribbon(aes(x = date, ymin = min_fitness, ymax = max_fitness, fill = test_run), alpha = .5) +
    geom_path(aes(date, mean_fitness, colour = test_run), linetype = 1, size = size1) +
    labs(x = "Spawning time (year h)", y = "Larval fitness", color = "Year", fill = "Year") +
    scale_color_manual(labels = c("2022", "No mesopelagic fish", "2021"), values = c(col0, col1, col2)) +
    scale_fill_manual(labels = c("2022", "No mesopelagic fish", "2021"), values = c(col0, col1, col2)) +
    scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides = "l") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          legend.position = "none", #"bottom"
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10)) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE))
  
  
  p11 <- p11_0 / p11a / p11b / p11c + plot_layout(ncol = 1,  heights = unit(c(10, 20, 35, 25), c('mm', 'null'))) & 
    theme(plot.background = element_blank()) &
    plot_annotation(tag_levels = "a")
  
  ggsave("plots/fitness_plot_11.png", p11, height = 100 , width = 80, units = "mm", scale = 2)
  
  
  
  # Zoom in:
  lims <- unique(master_df %>% filter(time_id %in% c(3455, 4495)) %>% select(datetime))
  lims <- lims$datetime
  
    p12_0 <- ggplot(data = subset(master_df, time_id %in% 3455:4495)) +
    geom_point(data = master_df %>% filter(full_moon == T, time_id %in% 3455:4495), aes(x = datetime, y = full_moon), shape = 21, size = 10) +
    geom_point(data = master_df %>% filter(new_moon == T, time_id %in% 3455:4495), aes(x = datetime, y = new_moon), size = 10) +
    labs(x = "", y = "Moon") +
    xlim(lims) +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.text.y.left = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          legend.position = "none",
          axis.ticks.y = element_blank())
  
  p12a <- ggplot(data = subset(master_df, time_id %in% 3455:4495 & test_run %in% c(1, 3))) +
    geom_path(aes(datetime, egg_total_survival, colour = test_run), linetype = 1, size = size1) +
    scale_color_manual(labels = c("Present & 400", "Absent & 400"), values = c(col0, col2)) +
    labs(x = "Time of spawning", y = "Egg survival", color = expression("Mesopelagic fish & nauplii m" ^-3*""), fill = expression("Mesopelagic fish & nauplii m" ^-3*"")) +

    theme_base() +
    theme(plot.background = element_blank(),
          # axis.text.y.left = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          legend.position = "none")
  
  p12b <- ggplot(data = subset(master_df, time_id %in% 3455:4495 & test_run %in% c(1, 3))) +
    geom_path(aes(datetime, ysl_total_survival, colour = test_run), linetype = 1, size = size1) +
    scale_color_manual(labels = c("Present & 400", "Absent & 400"), values = c(col0, col2)) +
    labs(x = "Time of spawning", y = "YSL survival", color = expression("Mesopelagic fish & nauplii m" ^-3*""), fill = expression("Mesopelagic fish & nauplii m" ^-3*"")) +
    theme_base() +
    theme(plot.background = element_blank(),
          # axis.text.y.left = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          legend.position = "none")
  
  p12c <- ggplot(data = subset(master_df, time_id %in% 3460:4600 & test_run %in% c(1, 2, 3))) +
    geom_vline(xintercept = as.POSIXct(c("2022-06-14 00:00:00", "2022-07-13 00:00:00")), color = col2, alpha = .3) +
    geom_vline(xintercept = as.POSIXct(c("2022-06-24 00:00:00")), color = col2, alpha = .3) +
    geom_path(aes(datetime, fitness, colour = test_run), linetype = 1, size = size1) +
    scale_color_manual(labels = c("No mesopelagic fish", "2021", "2022"), values = c(col0, col1, col2)) +
    labs(x = "Time of spawning", y = "Larval fitness", color = "Year", fill = "Year") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides = "l") +
    theme_base() +
    theme(plot.background = element_blank(),
          # axis.text.y.left = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE))
  
  p12 <- p12_0 / p12a / p12b / p12c + plot_layout(ncol = 1,  heights = unit(c(10, 20, 30, 20), c('mm', 'null'))) & 
    theme(plot.background = element_blank()) &
    plot_annotation(tag_levels = "a")
  
  ggsave("plots/fitness_plot_12.png", p12, height = 90 , width = 80, units = "mm", scale = 2)
  

  # Combined plot:
  p13 <- p11_0 / p11a / p11c/ p12c + plot_layout(ncol = 1,  heights = unit(c(10, 20, 30, 20), c('mm', 'null'))) & 
    theme(plot.background = element_blank()) &
    plot_annotation(tag_levels = "a")

  ggsave("plots/fitness_plot_13.png", p13, height = 90 , width = 80, units = "mm", scale = 2)
  

  
  # July 1st at 03:00h corresponds to time_id = 4348, but let's take the optimal time = 4363
  # Lets plot values for this fish
  
  # Make data frame:
  parameters_july1 <- data.frame(fish_age = round(m_fish_age[4338, ]),
                                 wgt_mgDW = m_wgt_mgDW[4338, ],
                                 growth_mgDW = m_growth_mgDW[4338 , ],
                                 gut_limit = m_gut_limit[4338 , ],
                                 gut = m_gut[4338 , ],
                                 fish_sl = m_fish_sl[4338 , ],
                                 surv = m_surv[4338 , ])
  
  
  p2a <- ggplot(data = na.omit(parameters_july1)) +
    geom_path(aes(x = fish_age, y = wgt_mgDW), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Dry weight (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p2b <- ggplot(data = na.omit(parameters_july1)) +
    geom_path(aes(x = fish_age, y = growth_mgDW), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Growth (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p2c <- ggplot(data = na.omit(parameters_july1)) +
    geom_path(aes(x = fish_age, y = gut_limit), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Gut limit (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p2d <- ggplot(data = na.omit(parameters_july1)) +
    geom_path(aes(x = fish_age, y = gut), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Gut content (mg)") +
    theme_base() +
    theme(plot.background = element_blank())
  
  p2e <- ggplot(data = na.omit(parameters_july1)) +
    geom_path(aes(x = fish_age, y = fish_sl), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Standard length (mm)") +
    theme_base() +
    theme(plot.background = element_blank())
  
  p2f <- ggplot(data = na.omit(parameters_july1)) +
    geom_path(aes(x = fish_age, y = surv), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Survival") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    theme_base() +
    theme(plot.background = element_blank())
  
  p2 <- p2a + p2b + p2c +p2d + p2e + p2f & theme(plot.background = element_blank())
  
  ggsave("plots/fitness_plot_2.png", p2, height = 80 , width = 160, units = "mm", scale = 2)
  
  
  # Let's compare with June 1 and August 1 at 3am:
  parameters_august1 <- data.frame(fish_age = round(m_fish_age[5092, ]),
                                   wgt_mgDW = m_wgt_mgDW[5092, ],
                                   growth_mgDW = m_growth_mgDW[5092 , ],
                                   gut_limit = m_gut_limit[5092 , ],
                                   gut = m_gut[5092 , ],
                                   fish_sl = m_fish_sl[5092 , ],
                                   surv = m_surv[5092 , ])
  
  p3a <- ggplot(data = na.omit(parameters_august1)) +
    geom_path(aes(x = fish_age, y = wgt_mgDW), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Dry weight (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p3b <- ggplot(data = na.omit(parameters_august1)) +
    geom_path(aes(x = fish_age, y = growth_mgDW), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Growth (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p3c <- ggplot(data = na.omit(parameters_august1)) +
    geom_path(aes(x = fish_age, y = gut_limit), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Gut limit (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p3d <- ggplot(data = na.omit(parameters_august1)) +
    geom_path(aes(x = fish_age, y = gut), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Gut content (mg)") +
    theme_base() +
    theme(plot.background = element_blank())
  
  p3e <- ggplot(data = na.omit(parameters_august1)) +
    geom_path(aes(x = fish_age, y = fish_sl), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Standard length (mm)") +
    theme_base() +
    theme(plot.background = element_blank())
  
  p3f <- ggplot(data = na.omit(parameters_august1)) +
    geom_path(aes(x = fish_age, y = surv), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Survival") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    theme_base() +
    theme(plot.background = element_blank())
  
  p3 <- p3a + p3b + p3c +p3d + p3e + p3f & theme(plot.background = element_blank())
  
  ggsave("plots/fitness_plot_3.png", p3, height = 80 , width = 160, units = "mm", scale = 2)
  
  
  parameters_june1 <- data.frame(fish_age = round(m_fish_age[3628, ]),
                                 wgt_mgDW = m_wgt_mgDW[3628, ],
                                 growth_mgDW = m_growth_mgDW[3628 , ],
                                 gut_limit = m_gut_limit[3628 , ],
                                 gut = m_gut[3628 , ],
                                 fish_sl = m_fish_sl[3628 , ],
                                 surv = m_surv[3628 , ])
  
  p4a <- ggplot(data = na.omit(parameters_june1)) +
    geom_path(aes(x = fish_age, y = wgt_mgDW), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Dry weight (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p4b <- ggplot(data = na.omit(parameters_june1)) +
    geom_path(aes(x = fish_age, y = growth_mgDW), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Growth (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p4c <- ggplot(data = na.omit(parameters_june1)) +
    geom_path(aes(x = fish_age, y = gut_limit), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Gut limit (mg)") +
    theme_base() +
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank())
  
  p4d <- ggplot(data = na.omit(parameters_june1)) +
    geom_path(aes(x = fish_age, y = gut), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Gut content (mg)") +
    theme_base() +
    theme(plot.background = element_blank())
  
  p4e <- ggplot(data = na.omit(parameters_june1)) +
    geom_path(aes(x = fish_age, y = fish_sl), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Standard length (mm)") +
    theme_base() +
    theme(plot.background = element_blank())
  
  p4f <- ggplot(data = na.omit(parameters_june1)) +
    geom_path(aes(x = fish_age, y = surv), linetype = 1, color = col1, size = size1) +
    labs(x = "Fish age (h)", y = "Survival") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    theme_base() +
    theme(plot.background = element_blank())
  
  p4 <- p4a + p4b + p4c +p4d + p4e + p4f & theme(plot.background = element_blank())
  
  ggsave("plots/fitness_plot_4.png", p4, height = 80 , width = 160, units = "mm", scale = 2)
  
}



#******************************************************************************
# Optimal spawning:
# What is the  best spawning time of the year?
optimal_spawn <- master_df[which.max(master_df$fitness), ]
optimal_spawn <- optimal_spawn[which.max(optimal_spawn$fitness), ]


optimal_spawn_day <- master_df %>%
  group_by(yday) %>%
  summarise(fitness = mean(fitness)) %>%
  ungroup() 

optimal_spawn_day <- optimal_spawn_day[which.max(optimal_spawn_day$fitness), ]

print(paste('best day and time: ', optimal_spawn$datetime))
print(paste('survival: ',optimal_spawn$fitness))

print(paste('best hour: ', which.max(parameters_july1$surv) - 1))
print(paste('survival: ', max(parameters_july1$surv, na.rm = T)))
#******************************************************************************

# Time out:
print(Sys.time() - time0)

# Clean environment:
rm(list = ls())

#                                          END OF SCRIPT
#############################################################################################################