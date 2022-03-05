#######
# TidyTuesday 2022 W8: Alternative Fuel Stations
# Jennifer Truong
# March 5th, 2022
#######

# Load packages
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(ggmap)
library(sf)
library(usmap)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2022-03-01')
stations <- tuesdata$stations

# Tidy data
stations_tidy <- stations %>%
  clean_names() %>%
  rename("long" = "x",
         "lat" = "y") %>%
  # Filter to just electric stations and remove any data that's not in the 48 continuous states, AK, or HI
  filter(fuel_type_code == "ELEC",
         !state %in% c("PR", "ON")) 

# Load country shapefile
state_map <- plot_usmap(regions = "states")

# Create map of electric stations
elec_map <- ggplot(stations_tidy, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) +
  coord_equal()

elec_map
