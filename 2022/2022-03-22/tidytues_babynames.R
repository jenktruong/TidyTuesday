# TidyTuesday - 2022-03-22
# Baby Names
# Jennifer Truong

# load packages
library(tidyverse)
library(tidytuesdayR)
library(here)
library(sysfonts)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')

babynames <- tuesdata$babynames

# Data wrangling 
data <- babynames %>%
  group_by(year) %>%
  #focusing on Jennifer for now for simplicity
  summarize(count=sum(n[name=='Jennifer']),
            total=sum(n)) %>%
  mutate(pct_total = count/total) %>%
  filter(between(year, 1910, 2017))

# Find year with max percent 
jen_max <- data %>%
  slice_max(pct_total) %>%
  slice(1)
# Seems like it's 1974!

# load fonts
font_add_google("Ubuntu", "ubuntu")
showtext_auto()

# Plot time
ggplot(data,
       aes(x = year, y = pct_total)) +
  geom_line(color = "steelblue2") +
  # Draw point for peak popularity
  geom_point(data = jen_max, 
             mapping = aes(x = year,y = pct_total), 
             color = "navyblue",
             size = 1) +
  # Draw line for peak popularity
  geom_vline(xintercept = jen_max$year, linetype = "dashed", color = "navyblue") +
  annotate(geom = "text", x = 1984, y = 0.0205, 
           label = "Peak year: 1974", 
           size = 4,
           color = "gray27") +
  labs(title = "How many Jennifers are there?",
       subtitle = "Popularity of the name 'Jennifer' from the 20th century to the present.",
       caption = "Data: babynames R package | Viz: @jenjentro | #TidyTuesday 2022 W12",
       x = "Year",
       y = "% of Total Names") +
  theme_minimal() +
  theme(
    text = element_text(family = "ubuntu"),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold",
                              size = 18,
                              hjust = .5),
    plot.subtitle = element_text(size = 14,
                                 hjust = .5),
    plot.caption = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )

# Save plot
ggsave(here("2022", "2022-03-22", "tidytues_babynames.png"), plot = last_plot())
