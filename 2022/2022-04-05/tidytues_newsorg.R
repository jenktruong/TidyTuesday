#####
# TidyTuesday 2022 W14 - 2022-04-05
# Digital Publications
# Jennifer Truong
#####

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(here)
library(showtext)
 
# Load data ----
tuesdata <- tidytuesdayR::tt_load('2022-04-05')
news_orgs <- tuesdata$news_orgs

# Data wrangling ----

# First wrangling for types of underrepresented communities
data <- news_orgs %>%
  drop_na(underrepresented_communities) %>%
  mutate(
    comm_type = str_split(underrepresented_communities, pattern = ",")
  ) %>%
  unnest(comm_type) %>%
  mutate(across(where(is.character), str_trim))

# Underrepresented communities: 
# [1] "Communities with English as a second language"
# [2] "Ethnic communities"                           
# [3] "People of color"                              
# [4] "Immigrant communities"
# [5] "LGBTQI communities"                           
# [6] "Low-income communities"                       
# [7] "Indigenous Populations" 

# Wrangling by coverage topics for underrepresented communities
data_topics <- data %>%
  mutate(
    topic = str_split(coverage_topics, pattern = ",")
  ) %>%
  unnest(topic) %>%
  mutate(across(where(is.character), str_trim))

# Summarize count of topics per community type
data_topics_count <- data_topics %>%
  group_by(comm_type, topic) %>%
  count() %>%
  ungroup() %>%
  drop_na(topic) %>%
  filter(n >= 10) # Filter to just topics that have at least 10 publications

# Load fonts ----
#font_add_google("actualfont", "nickname")
#showtext_auto()

# Plot ----
# Create circular barplot
ggplot(data_topics_count, 
       aes(x=topic, y=n)) + 
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  ylim(-20,60) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")   
  ) +
  coord_polar(start = 0)

# Save plot ----
ggsave(here("2022", "2022-04-05", "underrep_topics_plot.png"),
       plot = last_plot(),
       dpi = 300)
