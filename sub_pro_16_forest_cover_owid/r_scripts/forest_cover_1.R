# Forests (OWID)

# Share of global forest area

# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(patchwork)
library(ggrepel)

# Load data

# Share of global forests worldwide
share_global_forest <- read_csv("sub_pro_3_forest_cover_owid/datasets/share-global-forest.csv")

# Clean the data

share_global_forest <- share_global_forest %>%
  clean_names()

# Set up datasets for EDA

############## Share of global forests worldwide

# Share of global forest (By Continent) 
share_global_forest_continent <- share_global_forest %>%
  filter(entity %in% c("Africa", "Asia", "Europe", "Northern America", 
                       "Oceania", "South America", "Central America"))

# Share of global forest (By Country - Worldwide)
share_global_forest_countries <- share_global_forest %>%
  filter(!is.na(code)) %>%
  filter(entity != "World")

# Share of global forest (By African Region)
share_global_forest_africa_regions <- share_global_forest %>%
  filter(entity %in% c("Eastern Africa", "Northern Africa", "Middle Africa",
                       "Southern Africa", "Western Africa")) %>%
  mutate(entity = ifelse(entity == 'Middle Africa', 'Central Africa', entity))

# Share of global forest (By African Countries)

# List the African countries first

african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
              "Burundi", "Cape Verde", "Cameroon", "Central African Republic", 
              "Chad", "Comoros", "Congo", "Democratic Republic of Congo", 
              "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
              "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
              "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", 
              "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
              "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
              "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
              "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
              "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", 
              "Uganda", "Zambia", "Zimbabwe")

# Filter the dataset to include only African countries

share_global_forest_africa_countries <- share_global_forest %>%
  filter(entity %in% african_countries)

# To combine the datasets with mapping dataset - change some of the country names to match

# Change to standard names used in rnaturalearth for maps

share_global_forest_africa_countries_rnaturalearth <- share_global_forest_africa_countries %>%
  mutate(entity = case_when(
    entity == "Cape Verde"  ~ "Cabo Verde",
    entity == "Sao Tome and Principe"  ~ "São Tomé and Principe",
    entity == "Eswatini"  ~ "eSwatini",
    entity == "Democratic Republic of Congo"  ~ "Democratic Republic of the Congo",
    entity == "Tanzania"  ~ "United Republic of Tanzania",
    entity == "Congo"  ~ "Republic of the Congo",
    TRUE ~ entity  # Retain original name if none of the conditions are met
  ))

# B) EDA and Basic Plots

# Share of global forests worldwide

# Stacked area chart for share of global forest (By Continent)

ggplot(share_global_forest_continent, aes(x=year, y=share_of_global_forest_area, fill=entity)) + 
  geom_area(alpha=1 , size=0.25, colour="black") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  geom_text(aes(x = 1994, y = 90, label = "Africa"), 
            inherit.aes = FALSE,
            color = "white",
            family = "sans",
            size = 15) +
  theme_minimal() + 
  labs(x = "Year",
       y = "Share of Global Forests (%)",
       title = "Africa’s share of global forests is decreasing",
       subtitle = "Share of global forest area (1990-2020) has decreased by approximately 2%",
       caption = "Data Source: Our World in Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.x = element_line(linewidth = 1),
        axis.ticks.x = element_line(linewidth = 1),
        axis.line.y = element_line(linewidth = 1),
        axis.ticks.y = element_line(linewidth = 1),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25, vjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.background = element_rect(fill = "bisque1", color = "black"))

ggsave("sub_pro_3_forest_cover_owid/images/share_global_forest/share_global_forest_continent.png", width = 12, height = 12, dpi = 300)

# Line plot for share of global forest in Africa (By Region)

last_points_share_1 <- share_global_forest_africa_regions %>%
  group_by(entity) %>%
  slice_tail(n = 1) %>%
  ungroup()

ggplot(share_global_forest_africa_regions, aes(x=year, y=share_of_global_forest_area, color=entity)) + 
  geom_line(size = 2, alpha = 0.8) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  scale_y_continuous(breaks = seq(0, 9, by = 1), 
                     expand = expansion(mult = c(0, 0.1))) +
  theme_classic() + 
  geom_text(data = last_points_share_1,
            aes(label = entity),
            hjust = 0,
            vjust = 0,
            size = 8,
            color = "black",
            show.legend = FALSE) +  
  labs(x = "Year",
       y = "Share of Global Forests (%)",
       title = "Central Africa has had the largest decrease (0.8%)\nin global share of forest area",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_forest_cover_owid/images/share_global_forest/share_global_forest_region.png", width = 12, height = 12, dpi = 300)

# Countries with the highest and lowest share of global forest (2020)

# Highest

share_global_forest_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(share_of_global_forest_area)) |>
  top_n(5) |>
  ggplot(aes(x=reorder(entity, share_of_global_forest_area), y = share_of_global_forest_area, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  labs(x = "Country",
       y = "Share of Global Forests (%)",
       title = "African countries with the highest share\nof global forest area (2020)",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_forest_cover_owid/images/share_global_forest/share_global_forest_africa_top_5.png", width = 12, height = 12, dpi = 300)


# Lowest

share_global_forest_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(share_of_global_forest_area)) |>
  top_n(-5) |>
  ggplot(aes(x=reorder(entity, share_of_global_forest_area), y = share_of_global_forest_area, fill = entity)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() + 
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.02))) +
  geom_text(aes(y = 0.00001, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  labs(x = "Country",
       y = "Share of Global Forests (%)",
       title = "African countries with the lowest share\nof global forest area (2020)",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_forest_cover_owid/images/share_global_forest/share_global_forest_africa_bottom_5.png", width = 12, height = 12, dpi = 300)


# Map of countries showing share of global forest area between 1990 and 2020

#########.............

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
africa <- world %>%
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))

# Get 1990 data
share_global_forest_africa_countries_1990 <- share_global_forest_africa_countries_rnaturalearth |> 
  filter(year == 1990) |>
  arrange(desc(share_of_global_forest_area))

# Now we have the 1990 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_global_forest_africa_countries_1990_full_join <- full_join(africa, 
                                                          share_global_forest_africa_countries_1990,
                                                          by = c("admin" = "entity"))

# Find rows only in africa

share_global_forest_africa_countries_1990_anti_join_1 <- anti_join(africa, 
                                                                   share_global_forest_africa_countries_1990, 
                                                                   by = c("admin" = "entity"))

# Find rows only in share_global_forest_africa_countries_1990

share_global_forest_africa_countries_1990_anti_join_2 <- anti_join(share_global_forest_africa_countries_1990, 
                                                                   africa, 
                                                                   by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_global_forest_africa_countries_1990_full_join, aes(fill = share_of_global_forest_area), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 4),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "1990",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_3_forest_cover_owid/images/share_global_forest/share_global_forest_map_1990.png", width = 9, height = 16, dpi = 300)

# Get 2000 data
share_global_forest_africa_countries_2000 <- share_global_forest_africa_countries_rnaturalearth |> 
  filter(year == 2000) |>
  arrange(desc(share_of_global_forest_area))

# Now we have the 2000 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_global_forest_africa_countries_2000_full_join <- full_join(africa, 
                                                                 share_global_forest_africa_countries_2000,
                                                                 by = c("admin" = "entity"))

# Find rows only in africa

share_global_forest_africa_countries_2000_anti_join_1 <- anti_join(africa, 
                                                                   share_global_forest_africa_countries_2000, 
                                                                   by = c("admin" = "entity"))

# Find rows only in share_global_forest_africa_countries_2000

share_global_forest_africa_countries_2000_anti_join_2 <- anti_join(share_global_forest_africa_countries_2000, 
                                                                   africa, 
                                                                   by = c("entity" = "admin"))


p2 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_global_forest_africa_countries_2000_full_join, aes(fill = share_of_global_forest_area), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 4),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2000",
       subtitle = "",
       caption = "")

ggsave("sub_pro_3_forest_cover_owid/images/share_global_forest/share_global_forest_map_2000.png", width = 9, height = 16, dpi = 300)

# Get 2010 data
share_global_forest_africa_countries_2010 <- share_global_forest_africa_countries_rnaturalearth |> 
  filter(year == 2010) |>
  arrange(desc(share_of_global_forest_area))

# Now we have the 2010 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_global_forest_africa_countries_2010_full_join <- full_join(africa, 
                                                                 share_global_forest_africa_countries_2010,
                                                                 by = c("admin" = "entity"))

# Find rows only in africa

share_global_forest_africa_countries_2010_anti_join_1 <- anti_join(africa, 
                                                                   share_global_forest_africa_countries_2010, 
                                                                   by = c("admin" = "entity"))

# Find rows only in share_global_forest_africa_countries_2010

share_global_forest_africa_countries_2010_anti_join_2 <- anti_join(share_global_forest_africa_countries_2010, 
                                                                   africa, 
                                                                   by = c("entity" = "admin"))


p3 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_global_forest_africa_countries_2010_full_join, aes(fill = share_of_global_forest_area), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 4),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       ))+
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2010",
       subtitle = "",
       caption = "")

ggsave("sub_pro_3_forest_cover_owid/images/share_global_forest/share_global_forest_map_2010.png", width = 9, height = 16, dpi = 300)

# Get 2020 data
share_global_forest_africa_countries_2020 <- share_global_forest_africa_countries_rnaturalearth |> 
  filter(year == 2020) |>
  arrange(desc(share_of_global_forest_area))

# Now we have the 2020 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_global_forest_africa_countries_2020_full_join <- full_join(africa, 
                                                                 share_global_forest_africa_countries_2020,
                                                                 by = c("admin" = "entity"))

# Find rows only in africa

share_global_forest_africa_countries_2020_anti_join_1 <- anti_join(africa, 
                                                                   share_global_forest_africa_countries_2020, 
                                                                   by = c("admin" = "entity"))

# Find rows only in share_global_forest_africa_countries_2020

share_global_forest_africa_countries_2020_anti_join_2 <- anti_join(share_global_forest_africa_countries_2020, 
                                                                   africa, 
                                                                   by = c("entity" = "admin"))


p4 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_global_forest_africa_countries_2020_full_join, aes(fill = share_of_global_forest_area), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 4),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       ))+
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2020",
       subtitle = "",
       caption = "")

ggsave("sub_pro_3_forest_cover_owid/images/share_global_forest/share_global_forest_map_2020.png", width = 9, height = 16, dpi = 300)

#######################

# Calculate countries with the biggest changes (increase or decrease)

share_global_forest_africa_countries_join_1990_2020 <- share_global_forest_africa_countries_1990 %>% 
  full_join(share_global_forest_africa_countries_2020, by = "entity")

share_global_forest_africa_countries_join_1990_2020 <- share_global_forest_africa_countries_join_1990_2020 |> 
  mutate(changes = share_of_global_forest_area.y - share_of_global_forest_area.x) |>
  mutate(abs_changes = abs(changes)) |>
  arrange(desc(changes))

