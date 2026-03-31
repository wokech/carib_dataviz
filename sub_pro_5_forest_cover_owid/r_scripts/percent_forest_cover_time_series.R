# Forests (OWID) - Time Series

# Forest cover as a share of total land area within the country or region.

# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
#library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(ggrepel)
library(scales)
library(gghighlight)

# Load data

forest_share_land_area <- read_csv("https://ourworldindata.org/grapher/forest-area-as-share-of-land-area.csv?v=1&csvType=full&useColumnShortNames=true")

# Clean the data

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

forest_share_land_area_select <- forest_share_land_area %>%
  clean_names() %>%
  filter(entity %in% select_countries) %>%
  mutate(entity = ifelse(entity == "Bahamas", "The Bahamas", entity))

# 2) Map of countries showing percentage forest cover (%)

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter Caribbean countries
carib <- world %>%
  filter(admin %in% c(select_countries, "The Bahamas"))

# Get 1990 data

forest_share_land_area_select_carib_1990 <- forest_share_land_area_select |> 
  filter(year == 1990) |>
  arrange(desc(forest_share))

# Now we have the 1990 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

forest_share_land_area_select_carib_1990_full_join <- full_join(carib, 
                                                                forest_share_land_area_select_carib_1990,
                                                                by = c("admin" = "entity"))

# Find missing

forest_share_land_area_select_carib_1990_anti_join <- anti_join(carib, 
                                                                forest_share_land_area_select_carib_1990,
                                                                by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

forest_share_land_area_select_carib_1990_anti_join_2 <- anti_join(forest_share_land_area_select_carib_1990,
                                                                carib,
                                                                by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_select_carib_1990_full_join, aes(fill = forest_share), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
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

ggsave("sub_pro_5_forest_cover_owid/images/forest_share_time_series/forest_share_land_area_select_carib_1990.png", width = 9, height = 16, dpi = 300)



# Get 1995 data

forest_share_land_area_select_carib_1995 <- forest_share_land_area_select |> 
  filter(year == 1995) |>
  arrange(desc(forest_share))

# Now we have the 1995 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

forest_share_land_area_select_carib_1995_full_join <- full_join(carib, 
                                                                forest_share_land_area_select_carib_1995,
                                                                by = c("admin" = "entity"))

# Find missing

forest_share_land_area_select_carib_1995_anti_join <- anti_join(carib, 
                                                                forest_share_land_area_select_carib_1995,
                                                                by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

forest_share_land_area_select_carib_1995_anti_join_2 <- anti_join(forest_share_land_area_select_carib_1995,
                                                                  carib,
                                                                  by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_select_carib_1995_full_join, aes(fill = forest_share), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
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

ggsave("sub_pro_5_forest_cover_owid/images/forest_share_time_series/forest_share_land_area_select_carib_1995.png", width = 9, height = 16, dpi = 300)



# Get 2000 data

forest_share_land_area_select_carib_2000 <- forest_share_land_area_select |> 
  filter(year == 2000) |>
  arrange(desc(forest_share))

# Now we have the 2000 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

forest_share_land_area_select_carib_2000_full_join <- full_join(carib, 
                                                                forest_share_land_area_select_carib_2000,
                                                                by = c("admin" = "entity"))

# Find missing

forest_share_land_area_select_carib_2000_anti_join <- anti_join(carib, 
                                                                forest_share_land_area_select_carib_2000,
                                                                by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

forest_share_land_area_select_carib_2000_anti_join_2 <- anti_join(forest_share_land_area_select_carib_2000,
                                                                  carib,
                                                                  by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_select_carib_2000_full_join, aes(fill = forest_share), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
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

ggsave("sub_pro_5_forest_cover_owid/images/forest_share_time_series/forest_share_land_area_select_carib_2000.png", width = 9, height = 16, dpi = 300)


# Get 2005 data

forest_share_land_area_select_carib_2005 <- forest_share_land_area_select |> 
  filter(year == 2005) |>
  arrange(desc(forest_share))

# Now we have the 2005 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

forest_share_land_area_select_carib_2005_full_join <- full_join(carib, 
                                                                forest_share_land_area_select_carib_2005,
                                                                by = c("admin" = "entity"))

# Find missing

forest_share_land_area_select_carib_2005_anti_join <- anti_join(carib, 
                                                                forest_share_land_area_select_carib_2005,
                                                                by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

forest_share_land_area_select_carib_2005_anti_join_2 <- anti_join(forest_share_land_area_select_carib_2005,
                                                                  carib,
                                                                  by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_select_carib_2005_full_join, aes(fill = forest_share), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
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

ggsave("sub_pro_5_forest_cover_owid/images/forest_share_time_series/forest_share_land_area_select_carib_2005.png", width = 9, height = 16, dpi = 300)



# Get 2010 data

forest_share_land_area_select_carib_2010 <- forest_share_land_area_select |> 
  filter(year == 2010) |>
  arrange(desc(forest_share))

# Now we have the 2010 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

forest_share_land_area_select_carib_2010_full_join <- full_join(carib, 
                                                                forest_share_land_area_select_carib_2010,
                                                                by = c("admin" = "entity"))

# Find missing

forest_share_land_area_select_carib_2010_anti_join <- anti_join(carib, 
                                                                forest_share_land_area_select_carib_2010,
                                                                by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

forest_share_land_area_select_carib_2010_anti_join_2 <- anti_join(forest_share_land_area_select_carib_2010,
                                                                  carib,
                                                                  by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_select_carib_2010_full_join, aes(fill = forest_share), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
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

ggsave("sub_pro_5_forest_cover_owid/images/forest_share_time_series/forest_share_land_area_select_carib_2010.png", width = 9, height = 16, dpi = 300)


# Get 2015 data

forest_share_land_area_select_carib_2015 <- forest_share_land_area_select |> 
  filter(year == 2015) |>
  arrange(desc(forest_share))

# Now we have the 2015 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

forest_share_land_area_select_carib_2015_full_join <- full_join(carib, 
                                                                forest_share_land_area_select_carib_2015,
                                                                by = c("admin" = "entity"))

# Find missing

forest_share_land_area_select_carib_2015_anti_join <- anti_join(carib, 
                                                                forest_share_land_area_select_carib_2015,
                                                                by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

forest_share_land_area_select_carib_2015_anti_join_2 <- anti_join(forest_share_land_area_select_carib_2015,
                                                                  carib,
                                                                  by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_select_carib_2015_full_join, aes(fill = forest_share), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
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

ggsave("sub_pro_5_forest_cover_owid/images/forest_share_time_series/forest_share_land_area_select_carib_2015.png", width = 9, height = 16, dpi = 300)



# Get 2020 data

forest_share_land_area_select_carib_2020 <- forest_share_land_area_select |> 
  filter(year == 2020) |>
  arrange(desc(forest_share))

# Now we have the 2020 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

forest_share_land_area_select_carib_2020_full_join <- full_join(carib, 
                                                                forest_share_land_area_select_carib_2020,
                                                                by = c("admin" = "entity"))

# Find missing

forest_share_land_area_select_carib_2020_anti_join <- anti_join(carib, 
                                                                forest_share_land_area_select_carib_2020,
                                                                by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

forest_share_land_area_select_carib_2020_anti_join_2 <- anti_join(forest_share_land_area_select_carib_2020,
                                                                  carib,
                                                                  by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_select_carib_2020_full_join, aes(fill = forest_share), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
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

ggsave("sub_pro_5_forest_cover_owid/images/forest_share_time_series/forest_share_land_area_select_carib_2020.png", width = 9, height = 16, dpi = 300)
