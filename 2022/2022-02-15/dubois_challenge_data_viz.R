#######
# TidyTuesday 2022 W6: DuBois Challenge
# Jennifer Truong
# February 15th, 2022
#######

# Load packages
library(tidyverse)
library(janitor)
library(here)
library(sf)
library(showtext)

# Read in data for challenge 3 - forked from Anthony Starks' repo
population <- read_csv(here("2022", "2022-02-15","dubois-challenge", "data.csv")) %>%
  clean_names()

# Read in USA Boundary shapefile
# TIGER/Line Shapefile for US boundaries downloaded from 
# https://catalog.data.gov/dataset/tiger-line-shapefile-2017-nation-u-s-current-state-and-equivalent-national
us_boundary <- read_sf(here("2022", "2022-02-15","tl_2017_us_state"),
                       layer = "tl_2017_us_state") %>%
  rename("state"= "STUSPS")

# Join population table to US shapefile
us_black_pop <- left_join(us_boundary, population, by = "state") %>%
  drop_na(population)

# Factor population groups
pop_levels = c("750,000 AND OVER", "600,000 - 750,000", 
               "500,000 - 600,000", "300,000 - 500,000",
               "200,000 - 300,000", #"100,000 - 200,000",
               "50,000 - 100,000", "25,000 - 50,000", 
               "10,000 - 25,000", "UNDER - 10,000")

us_black_pop$population <- factor(us_black_pop$population,
                                  levels = pop_levels)

# Create vector for color scheme
pop_colors <- c("#fffff") 

# Draw map
ggplot() +
  geom_sf(data = us_black_pop,
          aes(fill = population,
              geometry = geometry)) +
  theme(
    legend.position = "bottom"
  )
  
