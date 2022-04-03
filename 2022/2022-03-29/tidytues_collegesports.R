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
  # Pivot to long  to keep expense and revenue amounts to one column
  pivot_longer(cols = c(rev_men, rev_women, exp_men, exp_women),
               names_to = c("category", "gender"),
               # Define name pattern for splitting columns
               names_pattern = "(.*)_(.*)", 
               values_to = "dollars")


#  Find average revenue and expenses, then find net income ----
sports_net_inc <- sports_track_2019 %>%
  group_by(sports, category, gender) %>%
  summarize(
    average_dollars = mean(dollars)
  ) %>%
  # Pivot back to wide 
  pivot_wider(names_from = category,
              values_from = average_dollars) %>%
  # Add new column to calculate net income
  mutate(
    net_income = rev - exp
  )


# Add fonts ----
font_add_google("Roboto Slab", "roboto")
showtext_auto()


# Create captions ----
indoor_text = data.frame(
  x = 1.75,
  y = -1500,
  label = "Both men's and women's \nindoor events faced net \nlosses of at least $2000."
)

# Find difference between men's and women's outdoor event net income
outdoor_diff = sports_net_inc$net_income[3] - sports_net_inc$net_income[4]
outdoor_pct_diff = (outdoor_diff/sports_net_inc$net_income[3])*100 # 71.796565...

outdoor_text = data.frame(
  x = 1.25,
  y = 2250,
  label = "The net income for \nmen's outdoor events \nwas 71.8% higher than \nwomen's outdoor events."
)


# Plot ----
ggplot(sports_net_inc,
       aes(x = sports, y = net_income)) +
  geom_col(aes(fill = gender),
           position = "dodge") +
  geom_hline(yintercept = 0, size = 1) +
  # Caption for indoor track
  geom_text(data = indoor_text, 
            aes(x = x, y = y, label = label),
            color = "white",
            fontface = "bold",
            size = 3.5) +
  # Caption for outdoor track
  geom_text(data = outdoor_text, 
            aes(x = x, y = y, label = label),
            color = "white",
            fontface = "bold",
            size = 3.5) +
  scale_fill_manual(values = c("#8BB8E8", "#FFC72C"))	+
  labs(title = "How Profitable Was US Collegiate Track and Field in 2019?",
       subtitle = "Here we examine the net income (revenue minus expenses) for each track and field event type for men and women throughout the country in 2019.",
       caption = "Data: Equity in Athletics | Viz: @jenjentro | #TidyTuesday 2022 W13",
       x = "Track and Field Event Type",
       y = "Net Income ($)") +
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
