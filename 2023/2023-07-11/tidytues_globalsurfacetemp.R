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