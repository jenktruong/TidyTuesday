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
  remove_missing() %>% # remove NAs
  group_by(year_erected) %>% 
  count()

# 1936 had the most markers erected (887), followed by 2003 (785), 
# 2008 (698), and 1890 (686)

# Get the top 4 years for most number of markers
hist_markers_top4 <- hist_markers_df %>% 
  arrange(desc(n)) %>% 
  head(4)

# Load fonts ----
font_add_google("Libre Baskerville", "libre")
showtext_auto()

# Show plot ----
ggplot(hist_markers_df,
       aes(x = year_erected, y = n),
       color = "#422d07") +
  geom_line() + 
  labs(title = 'Historical Markers in the US', 
       subtitle = 'How many historical markers have been added throughout the United States?\n(Any years with a significantly high number of markers have been noted.)',
       caption = 'Data: Historical Marker Database | Viz: @jenktruong | #TidyTuesday 2023 W27',
       x = 'Year', 
       y = 'No. of Historical Markers') +
  # Add points for significant peaks
  geom_point(data = hist_markers_top4,
             mapping = aes(x = year_erected, y = n),
             size = 2,
             color = "#422d07") +
  # Add annotations for peaks
  annotate(geom = "text", x = 1871, y = 686, 
           label = "686 markers (1890)", 
           size = 4,
           color = "#422d07") +
  annotate(geom = "text", x = 1917, y = 887, 
           label = "887 markers (1936)", 
           size = 4,
           color = "#422d07") +
  annotate(geom = "text", x = 2002, y = 830, 
           label = "785 (2003) and 698 (2008) markers", 
           size = 4,
           color = "#422d07") +
  # Adjust theme
  theme_classic() +
  theme(
    text = element_text(family = "libre",
                        color = "#d99927"),
    plot.background = element_rect(fill = "#422d07"),
    plot.title = element_text(face = "bold",
                              size = 20,
                              hjust = .5),
    plot.subtitle = element_text(face = "italic",
                                 size = 13,
                                 hjust = .5),
    plot.caption = element_text(size = 9.5,
                                hjust = 1),
    panel.background = element_rect(fill = "#d99927"),
    axis.title = element_text(size = 12,
                              color = "#d99927"),
    axis.text = element_text(color = "#d99927")
  )

# Save plot ----
ggsave(here("2023", "2023-07-06", "tidytues_historicalmarkers.png"), plot = last_plot())
