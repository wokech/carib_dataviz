# Caribbean Avocado Production Time Series

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

# 1) Load the required datasets and data cleaning

sugar_cane_prod <- read.csv("sub_pro_4_agriculture_owid/datasets/sugar-cane-production-tonnes.csv")

# Clean the datasets

sugar_cane_prod_clean <- sugar_cane_prod %>%
  clean_names() 

# Only include Caribbean Countries

caribbean_countries <- c(
  "Antigua and Barbuda",
  "Bahamas",
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
)

# Global avocado Production in carib

sugar_cane_prod_clean_carib <- sugar_cane_prod_clean |>
  rename("country" = "entity") |>
  filter(country %in% caribbean_countries) |>
  rename(sugar_cane_production = "sugar_cane_00000156_production_005510_tonnes")

################################################################################
# QC to check for missing countries!
################################################################################

# Countries that have data
unique(sugar_cane_prod_clean_carib$country)

# Countries that don't have data
setdiff(caribbean_countries, unique(sugar_cane_prod_clean_carib$country))

# Check whether any countries in the dataset are not in the list of Caribbean countries
setdiff(unique(sugar_cane_prod_clean_carib$country), caribbean_countries)

## Then check the original dataset manually to see if countries are actually missing 

################################################################################

################################################################################
# Highest production in 2020

sugar_cane_prod_clean_carib |>
  arrange(desc(sugar_cane_production)) |>
  filter(year == 2020) |>
  top_n(1)

# Top 3 

top_3 <- sugar_cane_prod_clean_carib |>
  arrange(desc(sugar_cane_production)) |>
  filter(year == 2020) |>
  top_n(3)

# Bottom 3

bottom_3 <- sugar_cane_prod_clean_carib |>
  arrange(desc(sugar_cane_production)) |>
  filter(year == 2020) |>
  top_n(-3)

################################################################################


# 2) Map of countries showing global avocado production between 1965 and 2020

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter Caribbean countries, including Seychelles and Mauritius
carib <- world %>%
  filter(admin %in% caribbean_countries)

# Get 1965 data

sugar_cane_prod_clean_carib_1965 <- sugar_cane_prod_clean_carib |> 
  filter(year == 1965) |>
  arrange(desc(sugar_cane_production))

# Now we have the 1965 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_1965_full_join <- full_join(carib, 
                                                      sugar_cane_prod_clean_carib_1965,
                                                      by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_1965_anti_join_1 <- anti_join(carib, 
                                                        sugar_cane_prod_clean_carib_1965, 
                                                        by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_1965

sugar_cane_prod_clean_carib_1965_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_1965, 
                                                        carib, 
                                                        by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_1965_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1965",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_1965.png", width = 9, height = 16, dpi = 300)



# Get 1970 data

sugar_cane_prod_clean_carib_1970 <- sugar_cane_prod_clean_carib |> 
  filter(year == 1970) |>
  arrange(desc(sugar_cane_production))

# Now we have the 1970 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_1970_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_1970,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_1970_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_1970, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_1970

sugar_cane_prod_clean_carib_1970_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_1970, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_1970_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1970",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_1970.png", width = 9, height = 16, dpi = 300)



# Get 1975 data

sugar_cane_prod_clean_carib_1975 <- sugar_cane_prod_clean_carib |> 
  filter(year == 1975) |>
  arrange(desc(sugar_cane_production))

# Now we have the 1975 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_1975_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_1975,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_1975_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_1975, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_1975

sugar_cane_prod_clean_carib_1975_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_1975, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_1975_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1975",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_1975.png", width = 9, height = 16, dpi = 300)



# Get 1980 data

sugar_cane_prod_clean_carib_1980 <- sugar_cane_prod_clean_carib |> 
  filter(year == 1980) |>
  arrange(desc(sugar_cane_production))

# Now we have the 1980 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_1980_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_1980,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_1980_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_1980, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_1980

sugar_cane_prod_clean_carib_1980_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_1980, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_1980_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1980",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_1980.png", width = 9, height = 16, dpi = 300)



# Get 1985 data

sugar_cane_prod_clean_carib_1985 <- sugar_cane_prod_clean_carib |> 
  filter(year == 1985) |>
  arrange(desc(sugar_cane_production))

# Now we have the 1985 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_1985_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_1985,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_1985_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_1985, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_1985

sugar_cane_prod_clean_carib_1985_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_1985, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_1985_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1985",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_1985.png", width = 9, height = 16, dpi = 300)



# Get 1990 data

sugar_cane_prod_clean_carib_1990 <- sugar_cane_prod_clean_carib |> 
  filter(year == 1990) |>
  arrange(desc(sugar_cane_production))

# Now we have the 1990 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_1990_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_1990,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_1990_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_1990, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_1990

sugar_cane_prod_clean_carib_1990_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_1990, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_1990_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1990",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_1990.png", width = 9, height = 16, dpi = 300)



# Get 1995 data

sugar_cane_prod_clean_carib_1995 <- sugar_cane_prod_clean_carib |> 
  filter(year == 1995) |>
  arrange(desc(sugar_cane_production))

# Now we have the 1995 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_1995_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_1995,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_1995_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_1995, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_1995

sugar_cane_prod_clean_carib_1995_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_1995, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_1995_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1995",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_1995.png", width = 9, height = 16, dpi = 300)



# Get 2000 data

sugar_cane_prod_clean_carib_2000 <- sugar_cane_prod_clean_carib |> 
  filter(year == 2000) |>
  arrange(desc(sugar_cane_production))

# Now we have the 2000 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_2000_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_2000,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_2000_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_2000, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_2000

sugar_cane_prod_clean_carib_2000_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_2000, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_2000_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2000",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_2000.png", width = 9, height = 16, dpi = 300)



# Get 2005 data

sugar_cane_prod_clean_carib_2005 <- sugar_cane_prod_clean_carib |> 
  filter(year == 2005) |>
  arrange(desc(sugar_cane_production))

# Now we have the 2005 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_2005_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_2005,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_2005_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_2005, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_2005

sugar_cane_prod_clean_carib_2005_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_2005, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_2005_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2005",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_2005.png", width = 9, height = 16, dpi = 300)


# Get 2010 data

sugar_cane_prod_clean_carib_2010 <- sugar_cane_prod_clean_carib |> 
  filter(year == 2010) |>
  arrange(desc(sugar_cane_production))

# Now we have the 2010 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_2010_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_2010,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_2010_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_2010, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_2010

sugar_cane_prod_clean_carib_2010_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_2010, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_2010_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2010",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_2010.png", width = 9, height = 16, dpi = 300)



# Get 2015 data

sugar_cane_prod_clean_carib_2015 <- sugar_cane_prod_clean_carib |> 
  filter(year == 2015) |>
  arrange(desc(sugar_cane_production))

# Now we have the 2015 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_2015_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_2015,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_2015_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_2015, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_2015

sugar_cane_prod_clean_carib_2015_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_2015, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_2015_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2015",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_2015.png", width = 9, height = 16, dpi = 300)



# Get 2020 data

sugar_cane_prod_clean_carib_2020 <- sugar_cane_prod_clean_carib |> 
  filter(year == 2020) |>
  arrange(desc(sugar_cane_production))

# Now we have the 2020 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

sugar_cane_prod_clean_carib_2020_full_join <- full_join(carib, 
                                                     sugar_cane_prod_clean_carib_2020,
                                                     by = c("admin" = "country"))

# Find rows only in caribbean

sugar_cane_prod_clean_carib_2020_anti_join_1 <- anti_join(carib, 
                                                       sugar_cane_prod_clean_carib_2020, 
                                                       by = c("admin" = "country"))

# Find rows only in sugar_cane_prod_clean_carib_2020

sugar_cane_prod_clean_carib_2020_anti_join_2 <- anti_join(sugar_cane_prod_clean_carib_2020, 
                                                       carib, 
                                                       by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = sugar_cane_prod_clean_carib_2020_full_join, aes(fill = sugar_cane_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 85000000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2020",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_4_agriculture_owid/images/sugar_cane_time_series/sugar_cane_clean_carib_map_2020.png", width = 9, height = 16, dpi = 300)


