#####
# TidyTuesday 2022 W17 - 2022-04-26
# Kaggle Hidden Gems
# Jennifer Truong
#####

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(here)
library(ggwordcloud)
library(tidytext)
library(showtext)

# Load dataset ----
tuesdata <- tidytuesdayR::tt_load('2022-04-26')
hidden_gems <- tuesdata$hidden_gems

# Data wrangling ----
df <- hidden_gems %>%
  mutate(
    word = str_split(title, pattern = " ") # Split so that each word is in a row
  ) %>%
  select(date, word) %>%
  unnest(word) %>% # Make each word its own row
  mutate(across(where(is.character), str_trim)) # Trim white space

# Remove stopwords
df_clean <- df %>%
  unnest_tokens(word, word) %>%
  anti_join(get_stopwords(), by = "word") 

# Get count of words
df_count <- df_clean %>%
  group_by(word) %>%
  count() %>%
  arrange(-n) %>% # Arrange in descending order
  head(50) # Get top 50

# Load fonts ----
#font_add_google("actualfont", "nickname")
#showtext_auto()

# Create wordcloud ----
ggplot(data = df_count,
       aes(label = word)) +
  geom_text_wordcloud_area(aes(color = n, size = n),
                           shape = "diamond") +
  scale_size_area(max_size = 15) +
  scale_color_gradient(low = "#e1f3f8", high = "#00a6d2") +
  labs(title = "What's In a Kaggle Hidden Gem?",
       subtitle = "A word cloud of 50 top words that appeared in titles of Kaggle notebooks that are considered to be 'hidden gems.'",
       caption = "Data: Martin Henze (Kaggle) | Viz: Jen Truong | #TidyTuesday 2022 W17") +
  theme(
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "#112e51"),
    panel.background = element_rect(fill = "#112e51")
  )
