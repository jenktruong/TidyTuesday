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
  # Filter to just topics that have at least 10 publications
  filter(n >= 10) %>%
  mutate(
    comm_type = as_factor(comm_type)
  ) %>%
  arrange(comm_type, n) 

# Create data frame for labels ----
label_data <- data_topics_count %>%
  mutate(
    index = seq(1, 82),
    n_text = as.character(n)
  ) %>%
  # Concatenate topic and count for label 
  unite(label, c("topic", "n_text"),
        sep = " ") 

# Copied from R Graph Gallery - https://r-graph-gallery.com/297-circular-barplot-with-groups.html
# calculate the angle of the labels
number_of_bar <- nrow(label_data)
# Subtract 0.5 to center label on bars
angle <-  90 - 360 * (label_data$index-0.5)/number_of_bar 

# calculate the alignment of labels to make them readable
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Plot Prep ----
# Set a number of 'empty bars' 
empty_bar <- 4

# !!! This code wouldn't add values for some reason
to_add <- data.frame(matrix(NA, 
                            empty_bar*nlevels(data_topics_count$comm_type), 
                             ncol(data_topics_count)) )
# But the lines below work after
colnames(to_add) <- colnames(data_topics_count)
to_add$comm_type <- rep(levels(data_topics_count$comm_type), each=empty_bar)
data <- rbind(data_topics_count, to_add)
data <- data_topics_count %>% arrange(comm_type)
data$id <- seq(1, nrow(data))

# Load fonts ----
font_add_google("News Cycle", "news")
showtext_auto()

# Create circular barplot ----
ggplot(data, 
       aes(x=as.factor(id), y=n, fill = comm_type)) + 
  geom_bar(stat="identity") +
  # Manage size of bars
  ylim(-30,55) +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  labs(title = "Coverage Topics for Underrepresented Communities",
       subtitle = "Topics covered by at least 10 local news organizations serving underrepresented communties (shown in different colors).",
       caption = "Data: Project Oasis | Viz: @jenjentro | #TidyTuesday 2022 W14") +
  theme_minimal() +
  coord_polar(start = 0) +
  geom_text(data=label_data, 
            aes(x = index, y = n+5, label=label, hjust=hjust), 
            color="white",
            fontface = "bold",
            alpha=0.6, size=4, 
            angle= label_data$angle, 
            inherit.aes = FALSE) +
  theme(
    text = element_text(family = "news",
                        color = "white"),
    plot.background = element_rect(fill = "black"),
    #plot.margin = unit(rep(-1,4), "cm"),
    plot.title = element_text(face = "bold",
                              size = 50,
                              hjust = .5),
    plot.subtitle = element_text(face = "italic",
                                 size = 36,
                                 hjust = .5),
    plot.caption = element_text(size = 28,
                                hjust = .5),
    legend.title = element_blank(),
    legend.text = element_text(size = 28),
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
  ) 

# Save plot ----
ggsave(here("2022", "2022-04-05", "underrep_topics_plot.png"),
       plot = last_plot(),
       dpi = 300,
       width = 8,
       height = 5.5)
