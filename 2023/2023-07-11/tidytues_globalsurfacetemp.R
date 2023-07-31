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
  clean_names() %>% 
  select(year, djf, mam, jja, son) %>% 
  # pivot three-month column names to one row
  pivot_longer(
    cols = djf:son,
    names_to = "season",
    values_to = "temp"
  ) %>% 
  # Create new columns with season name
  mutate(
    season_name = case_when(
      season == "djf" ~ "Winter",
      season == "mam" ~ "Spring",
      season == "jja" ~ "Summer",
      season == "son" ~ "Fall"
    )
  )

# Add fonts ----
font_add_google("Mulish", "mulish")
showtext_auto()

# Create plot ----

# Color scheme
background_color = '#000000'
font_color = "#FFFFFF"

# Background
canvas <- grid::rectGrob(
  x = 0, y = 0,
  width = 16, height = 9,
  gp = grid::gpar(fill = background_color, alpha = 1, col = background_color)
)

# Margins
plot_margin <- 0.05

# Main plot
plot_nh_temp <- ggplot(nh_season_temp,
                       aes(values = year, fill = temp)) +
  geom_waffle(color = "white", size = 1.125, n_rows = 10, 
              make_proportional = TRUE,
              stat = "identity", na.rm = TRUE) +
  facet_wrap(~factor(season_name, 
                     levels = c("Winter", "Spring", "Summer", "Fall"))) +
  coord_equal(clip = "off") + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient2(
                       na.value = "grey50",
                       guide = "colourbar") +
  labs(title = "Northern Hemisphere Surface Temperatures",
       subtitle = "Looking at global surface temperatures in the Northern Hemishere over time by year",
       x = "Year",
       y = "Deviations from 1951-1980 means") #+
  # theme() +
  # guides(fill = guide_legend(title.position = "top"))

# Combine all plot elements and add annotation 
#ggdraw(ylim = c(0,1), # 0-1 bounds make it easy to place viz items on canvas
#       xlim = c(0,1)) +
  # a background
#  draw_grob(canvas,
#            x = 0, y = 1,
#            height = 12, width = 12,
#            hjust = 0, vjust = 1) +
  # the main plot
#  draw_plot(plot_fe,
#            x = plot_margin - 0.1,
#            y = plot_margin,
#            height = 0.94,
#            width = 0.94) 

# Save figure ----
# ggsave("geomcurve.png", width = 12, height = 12, dpi = 300, bg = "white")
