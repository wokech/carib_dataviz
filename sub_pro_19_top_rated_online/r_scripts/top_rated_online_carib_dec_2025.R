#Most Reviewed Places in every Caribbean country (December 2025)
# Highest number of reviews on Google Maps
# Source: top-rated.online

# Load required libraries
library(tidyverse)
library(ggtext)
library(showtext)  # For better font handling
library(readxl)
library(janitor)

install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))
library(ggflags)

# Add Google fonts
font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()

 # Import data
carib_top_rated_dec_2025 <- read_excel("sub_pro_19_top_rated_online/datasets/top_rated_online_carib_dec_2025.xlsx", sheet = 1)

carib_top_rated_dec_2025_clean <- carib_top_rated_dec_2025 %>%
  clean_names()

str(carib_top_rated_dec_2025_clean)

carib_top_rated_dec_2025_clean <- carib_top_rated_dec_2025_clean %>%
  mutate(
    number_of_reviews = as.numeric(number_of_reviews),
    average_rating = as.numeric(average_rating),
    country_population = as.numeric(country_population),
    internet_penetration = as.numeric(internet_penetration)
  ) %>%
  filter(!is.na(number_of_reviews)) %>%
  mutate(label = paste0(most_reviewed, " [", average_rating, "]", " - ", number_of_reviews, " reviews")) 

# Create the main plot
p <- ggplot(carib_top_rated_dec_2025_clean, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 15
  ) +
  # Place names with custom formatting using ggtext
  geom_text(
    aes(x = 0,
        y = country,
        label = label),
    size = 17.5,
    hjust = 0, nudge_x = 500, 
    family = "roboto"
  ) +
  # Country flags
  geom_flag(aes(x = -5000, country = tolower(iso2)), size = 15) +
  #X-axis formatting
  scale_x_continuous(
    name = NULL,
    limits = c(-10000, 60000),
    breaks = c(20000, 40000, 60000),
    labels = c("20K", "40K", "60K"),
    expand = expansion(mult = c(0, 0.75)),
    position = "bottom"
  ) +
  # Title and caption
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  # Theme customization
  theme_classic() +
  theme(
    # Text elements
    text = element_text(family = "roboto"),
    plot.title = element_text(
      family = "roboto_slab", face = "bold", 
      size = 18, color = "#2B4570", hjust = 0, margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      family = "roboto", color = "#637381", 
      size = 12, hjust = 0, margin = margin(b = 20)
    ),
    plot.caption = element_text(
      family = "roboto", color = "#637381", 
      hjust = 0, size = 8, margin = margin(t = 15)
    ),
    # Grid elements
    panel.grid = element_blank(),
    # Axis elements
    axis.text.y = element_text(family = "roboto", size = 60),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 60),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2")
  )

p

ggsave("sub_pro_19_top_rated_online/images/top_rated_online_carib_dec_2025.png", width = 12, height = 12, dpi = 300)
