#####
# TidyTuesday 2022 W15 - 2022-04-12
# Indoor Air Pollution
# Jennifer Truong
#####

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(gganimate)
library(here)
library(showtext)

# Load dataset ----
tuesdata <- tidytuesdayR::tt_load('2022-04-12')

indoor_pollution <- tuesdata$indoor_pollution
fuel_gdp <- tuesdata$fuel_gdp
fuel_access <- tuesdata$fuel_access
death_fuel <- tuesdata$death_fuel
death_source <- tuesdata$death_source
death_timeseries <- tuesdata$death_timeseries

# I'm mostly interested in access to clean fuels vs deaths

# Didn't know this was a thing to add continents
# mutate(continent = countrycode::countrycode(Code, "iso3c", "continent"))

# Data wrangling ----

# Filter indoor_pollution to years 2000 and after
indoor_pollution_tidy <- indoor_pollution %>%
  filter(Year >= 2000)

# Joining fuel_access with tidy indoor_pollution df
joined_data <- full_join(fuel_access, indoor_pollution_tidy, 
                         by = c("Entity", "Code", "Year")) %>%
  clean_names() %>%
  rename("clean_fuel_pct" = "access_to_clean_fuels_and_technologies_for_cooking_percent_of_population",
         "death_pct" = "deaths_cause_all_causes_risk_household_air_pollution_from_solid_fuels_sex_both_age_age_standardized_percent") %>%
  mutate(continent = countrycode(code, "iso3c", "continent"),
         year = as.integer(year)) %>%
  drop_na(continent, clean_fuel_pct)

# Load fonts ----
font_add_google("Mukta", "mukta") # For title
showtext_auto()

# Set colors for continents
continent_color = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Plot ----
ggplot(joined_data,
       aes(x = clean_fuel_pct, y = death_pct)) +
  geom_point(aes(fill = continent, line = continent),
             size = 4,
             alpha = 0.5,
             show.legend = FALSE) +
  scale_fill_manual(values = continent_color,
                    aesthetics = c("color", "fill")) +
  facet_wrap(~continent) +
  theme_classic() +
  theme(
    text = element_text(family = "mukta"),
    plot.title = element_text(face = "bold",
                              size = 14,
                              hjust = .5),
    plot.subtitle = element_text(size = 12,
                                 hjust = .5),
    plot.caption = element_text(size = 10)
  ) +
  # Adding in gganimate
  labs(title = 'Clean Fuel Access vs Global Indoor \nAir Pollution Deaths', 
       subtitle = 'Year: {frame_time}',
       caption = "Data: OurWorldinData.org | Viz: @jenjentro | #TidyTuesday 2022 W15",
       x = 'Access to clean fuels for cooking (%)', 
       y = 'Deaths from Indoor Air Pollution (%)') +
  transition_time(year) +
  ease_aes('linear')

# Save animated plot as GIF ----
anim_save(here("2022", "2022-04-12", "indoor_air_pollution.gif"), 
          plot = last_plot())
