
## Script name: Functions
##
##
## Authors: Tom Langbehn 
## Email:  tom.langbehn@uib.no
##
## Date created: September 2021
## Last update:  March 2022
##
## ---------------------------
##
## Readme:
##
## This script creates functions used in the lightfield_module
##
## ---------------------------


####################
# Define functions:

#-----------------------------------------------------------------
# Function to convert hourly timesteps within a year (i.e. timestep 1-8760) to hours, days or month
ts_to_X <- function(ts, ts_dur, format) {
  library(lubridate)
  t <- as.POSIXct(60 * 60 * (ts * ts_dur), origin = "2021-01-01", tz = "UTC")
  
  if (format %in% c("hour", "Hour", "h", "hr", "hh", "HH", "H")) {
    hour(t)
  } else if (format %in% c("hh.h")){
    sapply(
      strsplit(str_sub(t, 12, 16), ":"),
      function(x) {
        x <- as.numeric(x)
        x[1] + x[2] / 60
      }
    )
  } else if (format %in% c("day", "Day", "d", "dd", "DD", "D")) {
    yday(t)
  } else if (format %in% c("month", "Month", "m", "mm", "MM", "M")) {
    month(t)
  } else if (format %in% c("POSIXct")) {
    t
  } else {
    warning('format needs to be either "day" or "hour" or "hh.h" or "month" or "POSIXct')
  }
}

#-----------------------------------------------------------------
# Function to read .bin output files from fortran and convert them to long format
bin2long <- function(filename) {
  
  # !> activate for debugging
  # filename <- here(folder, "sun_height_2d.bin")
  
  # open connection
  zz <- file(filename, "rb")
  ## first, read rank (number of dimensions i.e. 2d, 3d etc.)
  rank <- readBin(zz, integer(), n = 1)
  ## second, write dimensions (e.g. 20 x 365 x 1000)
  dims <- readBin(zz, integer(), n = rank)
  ## third, read data
  data <- readBin(zz, numeric(), n = prod(dims))
  # close connection
  close(zz)
  ## fill data into array using the dimensions founbd in step 2
  marray <- array(data, dims)
  ## !> number of time steps
  no_ts <- dims[2]
  ## find timestep duration
  ts_dur <- 8760 / no_ts
  
  if (rank == 3) {
    dimnames(marray) <- list(ilatlon = 1:dims[1], ts = 1:dims[2], idepth = 1:dims[3])
  } else if (rank == 2) {
    dimnames(marray) <- list(ilatlon = 1:dims[1], ts = 1:dims[2])
  }
  
  
  data_long <- marray %>%
    cubelyr::as.tbl_cube(met_name = "data") %>%
    as_tibble() %>%
    mutate(ts = ts - 1) %>%
    mutate(
      yday = ts_to_X(ts, ts_dur, "day"),
      hour = ts_to_X(ts, ts_dur, "hh.h"),
      datetime = ts_to_X(ts, ts_dur, "POSIXct")
    )
  return(data_long)
}

#                                          END OF SCRIPT
#############################################################################################################