#####
# TidyTuesday 2023 W28 - 2023-07-14
# Global Surface Temperatures
# Jennifer Truong
#####

# Inspiration for this plot: USGS Vizlab
# https://waterdata.usgs.gov/blog/ggplot-jazz/

# Load the usual packages ----
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(here)
library(showtext)

# Additional packages to load:
library(lubridate)
library(cowplot)
library(grid)
library(remotes)
library(waffle)

# Install waffle using `remotes`
# remotes::install_github("hrbrmstr/waffle")

# Load dataset ----
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)

global_temps <- tuesdata$global_temps
nh_temps <- tuesdata$nh_temps
sh_temps <- tuesdata$sh_temps
zonann_temps <- tuesdata$zonann_temps

# Data wrangling ----

# We'll be looking at seasonal surface temperatures for Northern Hemisphere
# Dec-Jan-Feb (DJF) = N.H. meteorological winter
# Mar-Apr-May (MAM) = N.H. meteorological spring
# Jun-Jul-Aug (JJA) = N.H. meteorological summer
# Sep-Oct-Nov (SON) = N.H. meteorological autumn

nh_season_temp <- nh_temps %>% 
  clean_names() %>% # Change column names to lower case
  select(year, djf, mam, jja, son) # Only get year and seasonal columns only

# Add fonts ----
font_add_google("Mulish", "mulish")
showtext_auto()
