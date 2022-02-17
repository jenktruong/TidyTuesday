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

# Load fonts 
font_add_google("Chakra Petch", "chakra")
font_add_google("Public Sans", "public")
# Automatically turn on {showtext}
showtext_auto()

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
pop_colors <- c("#000000", "#808080",
                "#a9a9a9", "#654321",
                "navy", #no color for 100,000 - 200,000 since level was missing
                "#dc143c", "#ffc0cb",
                "#ffd700", "#d3d3d3")  

# Draw map
ggplot() +
  geom_sf(data = us_black_pop,
          aes(fill = population,
              geometry = geometry)) +
  scale_fill_manual(values = pop_colors) +
  labs(title = "RELATIVE BLACK POPULATION OF THE STATES OF THE\nUNITED STATES",
       caption = "Adapted by @jenjentro | Data: Anthony Starks | #DuBoisChallenge2022") + 
  theme(
    plot.background = element_rect(fill = "#d2b48c"),
    plot.title = element_text(family = "chakra", 
                              face = "bold", 
                              size = 12,
                              hjust = .5),
    plot.caption = element_text(family = "public",
                                size = 8),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(family = "chakra"),
    legend.background = element_rect(fill = "#d2b48c"),
    legend.key = element_rect(fill = "#d2b48c")
  ) +
  guides(fill = guide_legend(nrow = 5, ncol = 2))

# Save plot
ggsave(here("2022", "2022-02-15", "dubois-map.png"), 
            plot = last_plot(), 
            width = 5.5, 
            height = 4.5, 
            dpi = 100)

