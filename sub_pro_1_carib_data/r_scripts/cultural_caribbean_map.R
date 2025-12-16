# Map of the Cultural Caribbean

# Load the required libraries and packages

library(tidyverse)
library(janitor)
library(ggrepel)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(patchwork)
library(ggrepel)
library(scales)
library(ggsci)

# Map of the Cultural Caribbean

# Get the data

caribbean_cultural <- ne_countries(scale = "medium", returnclass = "sf") |>
  subset(admin %in% c(
    "Antigua and Barbuda",
    "The Bahamas",
    "Barbados",
    "Belize",
    "Cuba",
    "Dominica",
    "Dominican Republic",
    "Grenada",
    "Guyana",
    "Haiti",
    "Jamaica",
    "Saint Kitts and Nevis",
    "Saint Lucia",
    "Saint Vincent and the Grenadines",
    "Suriname",
    "Trinidad and Tobago"
  ))

# Plot the map

# Labeled

p1 <- ggplot(data = caribbean_cultural) +
  geom_sf(aes(fill = admin), linewidth = 0.5) +
  scale_fill_d3(palette = 'category20') +
  geom_text_repel(
    data = caribbean_cultural,
    aes(label = admin, geometry = geometry),
    stat = "sf_coordinates",
    size = 6,
    min.segment.length = 0
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    legend.position = "none")

p1

ggsave("sub_pro_1_carib_data/images/total_carib/carib_cultural_map_labeled.png", width = 12, height = 12, dpi = 300)


# Unlabeled

p2 <- ggplot(data = caribbean_cultural) +
  geom_sf(aes(fill = admin), linewidth = 0.5) +
  scale_fill_d3(palette = 'category20') +
  theme_void() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = "none")

p2

ggsave("sub_pro_1_carib_data/images/total_carib/carib_cultural_map_unlabeled.png", width = 12, height = 12, dpi = 300)
