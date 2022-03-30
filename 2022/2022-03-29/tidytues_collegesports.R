######
# Tidy Tuesday 2022 W13 - US Collegiate Sports
# Jennifer Truong
# 2022-03-29
#####

# Load packages
library(tidyverse)
library(tidytuesdayR)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2022-03-29')
sports <- tuesdata$sports

# Clean and wrangle data
sports_NCAA_track <- sports %>% 
  filter(classification_name %in% c("NCAA Division I-FCS", "NCAA Division I-FBS"), 
         sports %in% c("Track and Field, Outdoor", "Track and Field, Indoor", "Track and Field, X-Country")) #%>% 
  #drop_na(rev_men, rev_women, exp_men, exp_women)
