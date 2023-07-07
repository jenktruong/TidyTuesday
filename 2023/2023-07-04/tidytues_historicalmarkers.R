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
  #remove_missing() %>% # remove NAs
  group_by(year_erected) %>% 
  count()

# 1936 had the most markers erected (887), followed by 2003 (785), 
# 2008 (698), and 1890 (686)


# Load fonts ----
font_add_google("Libre Baskerville", "libre")
showtext_auto()

# Show plot ----
ggplot(hist_markers_df,
       aes(x = year_erected, y = n)) +
  geom_line() + 
  labs(title = 'Historical Markers in the US', 
       subtitle = 'How many historical markers have been added throughout the United States?',
       caption = 'Data: Historical Marker Database | Viz: @jenktruong | #TidyTuesday 2023 W27',
       x = 'Year', 
       y = 'No. of Historical Markers') +
  theme_classic() +
  theme(
    text = element_text(family = "libre",
                        color = "#d99927"),
    plot.background = element_rect(fill = "#422d07"),
    plot.title = element_text(face = "bold",
                              size = 18,
                              hjust = .5),
    plot.subtitle = element_text(face = "italic",
                                 size = 14,
                                 hjust = .5),
    plot.caption = element_text(size = 10,
                                hjust = 1),
    panel.background = element_rect(fill = "#d99927"),
    axis.title = element_text(size = 12,
                              color = "#d99927"),
    axis.text = element_text(color = "#d99927")
  )

# Save plot ----