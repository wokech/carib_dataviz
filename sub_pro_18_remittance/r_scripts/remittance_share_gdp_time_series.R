# # Remittance as a share of GDP (%) Time Series

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
# remittance_share_gdp <- read.csv("https://ourworldindata.org/grapher/money-sent-or-brought-back-by-migrants-as-a-share-of-gdp.csv?v=1&csvType=full&useColumnShortNames=false")

# Save data
# write_csv(remittance_share_gdp, "sub_pro_18_remittance/datasets/remittance_share_gdp.csv")

# Load data again
remittance_share_gdp <- read_csv("sub_pro_18_remittance/datasets/remittance_share_gdp.csv")

# Clean the data

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

remittance_share_gdp_select <- remittance_share_gdp %>%
  clean_names() %>%
  filter(entity %in% select_countries) %>%
  mutate(entity = ifelse(entity == "Bahamas", "The Bahamas", entity))

# Check for number of countries selected
unique(remittance_share_gdp_select$entity)

#############
# Check if the values in the african_countries dataset are present in new dataframes

select_countries[!(select_countries %in% unique(remittance_share_gdp_select$entity))]

#############

# 2) Map of countries showing percentage remittance (%)

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter Caribbean countries
carib <- world %>%
  filter(admin %in% c(select_countries, "The Bahamas"))


# Get 1990 data

remittance_share_gdp_select_carib_1990 <- remittance_share_gdp_select |> 
  filter(year == 1990) |>
  arrange(desc(personal_remittances_received_of_gdp))

# Now we have the 1990 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

remittance_share_gdp_select_carib_1990_full_join <- full_join(carib, 
                                                              remittance_share_gdp_select_carib_1990,
                                                              by = c("admin" = "entity"))

# Find missing

remittance_share_gdp_select_carib_1990_anti_join <- anti_join(carib, 
                                                              remittance_share_gdp_select_carib_1990,
                                                              by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

remittance_share_gdp_select_carib_1990_anti_join_2 <- anti_join(remittance_share_gdp_select_carib_1990,
                                                                carib,
                                                                by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = remittance_share_gdp_select_carib_1990_full_join, aes(fill = personal_remittances_received_of_gdp), linewidth = 1) +
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

ggsave("sub_pro_18_remittance/images/remittance_time_series/remittance_share_gdp_select_carib_1990.png", width = 9, height = 16, dpi = 300)



# Get 1995 data

remittance_share_gdp_select_carib_1995 <- remittance_share_gdp_select |> 
  filter(year == 1995) |>
  arrange(desc(personal_remittances_received_of_gdp))

# Now we have the 1995 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

remittance_share_gdp_select_carib_1995_full_join <- full_join(carib, 
                                                              remittance_share_gdp_select_carib_1995,
                                                              by = c("admin" = "entity"))

# Find missing

remittance_share_gdp_select_carib_1995_anti_join <- anti_join(carib, 
                                                              remittance_share_gdp_select_carib_1995,
                                                              by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

remittance_share_gdp_select_carib_1995_anti_join_2 <- anti_join(remittance_share_gdp_select_carib_1995,
                                                                carib,
                                                                by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = remittance_share_gdp_select_carib_1995_full_join, aes(fill = personal_remittances_received_of_gdp), linewidth = 1) +
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

ggsave("sub_pro_18_remittance/images/remittance_time_series/remittance_share_gdp_select_carib_1995.png", width = 9, height = 16, dpi = 300)



# Get 2000 data

remittance_share_gdp_select_carib_2000 <- remittance_share_gdp_select |> 
  filter(year == 2000) |>
  arrange(desc(personal_remittances_received_of_gdp))

# Now we have the 2000 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

remittance_share_gdp_select_carib_2000_full_join <- full_join(carib, 
                                                              remittance_share_gdp_select_carib_2000,
                                                              by = c("admin" = "entity"))

# Find missing

remittance_share_gdp_select_carib_2000_anti_join <- anti_join(carib, 
                                                              remittance_share_gdp_select_carib_2000,
                                                              by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

remittance_share_gdp_select_carib_2000_anti_join_2 <- anti_join(remittance_share_gdp_select_carib_2000,
                                                                carib,
                                                                by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = remittance_share_gdp_select_carib_2000_full_join, aes(fill = personal_remittances_received_of_gdp), linewidth = 1) +
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

ggsave("sub_pro_18_remittance/images/remittance_time_series/remittance_share_gdp_select_carib_2000.png", width = 9, height = 16, dpi = 300)



# Get 2005 data

remittance_share_gdp_select_carib_2005 <- remittance_share_gdp_select |> 
  filter(year == 2005) |>
  arrange(desc(personal_remittances_received_of_gdp))

# Now we have the 2005 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

remittance_share_gdp_select_carib_2005_full_join <- full_join(carib, 
                                                              remittance_share_gdp_select_carib_2005,
                                                              by = c("admin" = "entity"))

# Find missing

remittance_share_gdp_select_carib_2005_anti_join <- anti_join(carib, 
                                                              remittance_share_gdp_select_carib_2005,
                                                              by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

remittance_share_gdp_select_carib_2005_anti_join_2 <- anti_join(remittance_share_gdp_select_carib_2005,
                                                                carib,
                                                                by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = remittance_share_gdp_select_carib_2005_full_join, aes(fill = personal_remittances_received_of_gdp), linewidth = 1) +
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

ggsave("sub_pro_18_remittance/images/remittance_time_series/remittance_share_gdp_select_carib_2005.png", width = 9, height = 16, dpi = 300)



# Get 2010 data

remittance_share_gdp_select_carib_2010 <- remittance_share_gdp_select |> 
  filter(year == 2010) |>
  arrange(desc(personal_remittances_received_of_gdp))

# Now we have the 2010 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

remittance_share_gdp_select_carib_2010_full_join <- full_join(carib, 
                                                              remittance_share_gdp_select_carib_2010,
                                                              by = c("admin" = "entity"))

# Find missing

remittance_share_gdp_select_carib_2010_anti_join <- anti_join(carib, 
                                                              remittance_share_gdp_select_carib_2010,
                                                              by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

remittance_share_gdp_select_carib_2010_anti_join_2 <- anti_join(remittance_share_gdp_select_carib_2010,
                                                                carib,
                                                                by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = remittance_share_gdp_select_carib_2010_full_join, aes(fill = personal_remittances_received_of_gdp), linewidth = 1) +
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

ggsave("sub_pro_18_remittance/images/remittance_time_series/remittance_share_gdp_select_carib_2010.png", width = 9, height = 16, dpi = 300)



# Get 2015 data

remittance_share_gdp_select_carib_2015 <- remittance_share_gdp_select |> 
  filter(year == 2015) |>
  arrange(desc(personal_remittances_received_of_gdp))

# Now we have the 2015 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

remittance_share_gdp_select_carib_2015_full_join <- full_join(carib, 
                                                              remittance_share_gdp_select_carib_2015,
                                                              by = c("admin" = "entity"))

# Find missing

remittance_share_gdp_select_carib_2015_anti_join <- anti_join(carib, 
                                                              remittance_share_gdp_select_carib_2015,
                                                              by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

remittance_share_gdp_select_carib_2015_anti_join_2 <- anti_join(remittance_share_gdp_select_carib_2015,
                                                                carib,
                                                                by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = remittance_share_gdp_select_carib_2015_full_join, aes(fill = personal_remittances_received_of_gdp), linewidth = 1) +
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

ggsave("sub_pro_18_remittance/images/remittance_time_series/remittance_share_gdp_select_carib_2015.png", width = 9, height = 16, dpi = 300)



# Get 2020 data

remittance_share_gdp_select_carib_2020 <- remittance_share_gdp_select |> 
  filter(year == 2020) |>
  arrange(desc(personal_remittances_received_of_gdp))

# Now we have the 2020 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

remittance_share_gdp_select_carib_2020_full_join <- full_join(carib, 
                                                              remittance_share_gdp_select_carib_2020,
                                                              by = c("admin" = "entity"))

# Find missing

remittance_share_gdp_select_carib_2020_anti_join <- anti_join(carib, 
                                                              remittance_share_gdp_select_carib_2020,
                                                              by = c("admin" = "entity"))

# Find rows only in aqua_farm_fish_prod_clean_carib_1965

remittance_share_gdp_select_carib_2020_anti_join_2 <- anti_join(remittance_share_gdp_select_carib_2020,
                                                                carib,
                                                                by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = remittance_share_gdp_select_carib_2020_full_join, aes(fill = personal_remittances_received_of_gdp), linewidth = 1) +
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

ggsave("sub_pro_18_remittance/images/remittance_time_series/remittance_share_gdp_select_carib_2020.png", width = 9, height = 16, dpi = 300)
