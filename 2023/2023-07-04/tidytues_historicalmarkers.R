#####
# TidyTuesday 2023 W27 - 2023-07-06
# Historical Markers
# Jennifer Truong
#####

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(here)
library(showtext)

# Load dataset ----
tuesdata <- tidytuesdayR::tt_load(2023, week = 27)

historical_markers <- tuesdata$`historical_markers`
no_markers <- tuesdata$`no_markers`

# Data wrangling ----
 
hist_markers_df <- historical_markers %>% 
  select(marker_id, year_erected, city_or_town, state_or_prov) %>% 
  group_by(state_or_prov, year_erected) %>% 
  count()

# Load fonts ----
# font_add_google("actualfont", "nickname")
# showtext_auto()

# Show plot ----
ggplot(hist_markers_df,
       aes(x = year_erected, y = n)) +
  geom_line(aes(color = state_or_prov))

# Save plot ----