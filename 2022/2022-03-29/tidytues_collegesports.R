######
# Tidy Tuesday 2022 W13 - US Collegiate Sports
# Jennifer Truong
# 2022-03-29
#####

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(here)
library(showtext)


# Load data ----
tuesdata <- tidytuesdayR::tt_load('2022-03-29')
sports <- tuesdata$sports


# Clean and wrangle data ----
sports_track_2019 <- sports %>% 
  filter(year == 2019, 
         sports %in% c("Track and Field, Outdoor", "Track and Field, Indoor", "Track and Field, X-Country")) %>% 
  # Remove 'Track and Field,' part in "sports" values
  mutate(
    sports = str_remove(sports, "Track and Field, ")
  ) %>%
  # Ignore NA values
  drop_na(rev_men, rev_women, exp_men, exp_women) %>%
  # Pivot to long  to keep expenditures and revenue amounts to one column
  pivot_longer(cols = c(rev_men, rev_women, exp_men, exp_women),
               names_to = c("category", "gender"),
               # Define name pattern for splitting columns
               names_pattern = "(.*)_(.*)", 
               values_to = "dollars")


#  Find average revenue ----
track_rev <- sports_track_2019 %>%
  group_by(sports, category, gender) %>%
  summarize(
    average_dollars = mean(dollars)
  ) %>%
  filter(category == "rev")


# Add fonts ----
font_add_google("Roboto Slab", "roboto")
showtext_auto()


# Plot ----
ggplot(track_rev,
       aes(x = sports, y = average_dollars)) +
  geom_col(aes(fill = gender),
           position = "dodge") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 80000)) +
  scale_fill_manual(values = c("#8BB8E8", "#FFC72C"))	+
  labs(title = "US Collegiate Track and Field Revenue in 2019",
       subtitle = "How much did different track and field events throughout US colleges make on average in 2019? \nCross-country (X-country) made the least revenue for both men and women.",
       caption = "Data: Equity in Athletics | Viz: @jenjentro | #TidyTuesday 2022 W13",
       x = "Track and Field Event Type",
       y = "Average Revenue ($)") +
  theme_classic() +
  theme(
    text = element_text(family = "roboto", 
                        color = "white"),
    plot.background = element_rect(fill = "#005587"),
    panel.background = element_rect(fill = "#005587"),
    plot.title = element_text(face = "bold",
                              size = 20,
                              hjust = .5),
    plot.subtitle = element_text(face = "italic",
                                 size = 12,
                                 hjust = .5),
    plot.caption = element_text(size = 12),
    axis.title = element_text(face = "bold",
                              size = 14),
    axis.text = element_text(color = "white", 
                             size = 12),
    legend.background = element_rect(fill = "#003B5C"),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.85),
    legend.key.size = unit(0.5, "cm")
  )


ggsave(here("2022", "2022-03-29", "tidytues_collegiate_sports.png"), plot = last_plot(), dpi = 200)
