# TidyTuesday - 2022-03-22
# Baby Names
# Jennifer Truong

# load packages
library(tidyverse)
library(tidytuesdayR)
library(ggridges)

# Load data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')

babynames <- tuesdata$babynames

# Data wrangling - Maybe let's look at the name Jennifer and other names I've been mistaken for
jennifer <- babynames %>%
  filter(name %in% c("Jennifer", "Jessica"))

# Plot time
ggplot(jennifer,
       aes(x = year, y = prop)) +
  geom_line(aes(color = name)) 
