#####
# TidyTuesday 2022 W16 - 2022-04-19
# Crosswords
# Jennifer Truong
#####

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(here)
library(lubridate)
library(tidytext)
library(showtext)

# Load dataset ----
times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

# Data wrangling ----
# Remove stop words
times_clean <- times %>%
  select(puzzle_date, answer) %>%
  unnest_tokens(word, answer) %>%
  anti_join(get_stopwords(), by = "word")

# Using bing sentiment lexicon 
bing_df <- get_sentiments("bing")

# Join data frames
times_join <- times_clean %>%
  inner_join(bing_df, by = "word") %>%
  filter(puzzle_date > dmy("01012014")) %>%
  mutate(
    year = year(puzzle_date)
  ) %>%
  drop_na() %>%
  group_by(sentiment, year) %>%
  count()

# Load fonts ----
font_add_google("Domine", "domine")
showtext_auto()

# Plot ----
ggplot(times_join,
       aes(x = year, y = n)) +
  geom_col(aes(fill = sentiment),
           position = "dodge") +
  labs(title = 'New York Times Crossword Sentiment Analysis', 
       subtitle = 'Negative sentiment seems to dominate the Times crossword puzzle answers between 2014-2021.',
       caption = "Data: Cryptic Crossword Clues | Viz: Jennifer Truong | #TidyTuesday 2022 W16",
       x = 'Year', 
       y = 'No. of Answers') +
  scale_fill_manual(values = c("#CD5C5C", "#3498db")) +
  theme_bw() +
  theme(
    text = element_text(family = "domine"),
    plot.title = element_text(face = "bold",
                              size = 18,
                              hjust = .5),
    plot.subtitle = element_text(size = 14,
                                 hjust = .5),
    plot.caption = element_text(size = 12,
                                hjust = 1),
    panel.grid = element_blank(),
    legend.key.size = unit(0.3, 'cm'),
    legend.position = c(.1,.75)
  ) 

# Save plot ----
ggsave(here("2022", "2022-04-19", "times_crossword_sentiment.png"), 
       plot = last_plot())
