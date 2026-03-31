# ICT in the Caribbean (Part 2)

# Share of the population who used the Internet in the last three months
# (International Telecommunication Union).

# Our World In Data and World Bank

# Load the required libraries and packages

# install.packages()
# library()

library(tidyverse)
library(janitor)
library(ggrepel)
library(ggthemes)
library(viridis)
#library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(patchwork)
library(ggrepel)

# 1) Load the required datasets

# a) Share of individuals using the internet

share_net <- read_csv("https://ourworldindata.org/grapher/share-of-individuals-using-the-internet.csv?v=1&csvType=full&useColumnShortNames=true")

# Clean the datasets

# Only include Carib Countries

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

share_net_select_carib <- share_net %>%
  clean_names() %>%
  filter(entity %in% select_countries) %>%
  rename(share_using_net = "it_net_user_zs") %>%
  mutate(entity = ifelse(entity == "Bahamas", "The Bahamas", entity))


# Should be 16 countries
unique(share_net_select_carib$entity)

# 2) Map of countries showing share using internet between 1990 and 2020

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter Caribbean countries
carib <- world %>%
  filter(admin %in% c(select_countries, "The Bahamas"))

# Get 1990 data
share_net_select_carib_1990 <- share_net_select_carib |> 
  filter(year == 1990) |>
  arrange(desc(share_using_net))

# Now we have the 1990 dataset and the carib dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from Carib

share_net_select_carib_1990_full_join <- full_join(carib, 
                                                   share_net_select_carib_1990,
                                                   by = c("admin" = "entity"))

# Find rows only in Carib

share_net_select_carib_1990_anti_join_1 <- anti_join(carib, 
                                                     share_net_select_carib_1990, 
                                                     by = c("admin" = "entity"))

# Find rows only in share_net_clean_carib_1990

share_net_select_carib_1990_anti_join_2 <- anti_join(share_net_select_carib_1990, 
                                                     carib, 
                                                     by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = share_net_select_carib_1990_full_join, aes(fill = share_using_net), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "1990",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/carib_ict_time_series/share_net_clean_africa_map_1990.png", width = 9, height = 16, dpi = 300)



# Get 1995 data
share_net_select_carib_1995 <- share_net_select_carib |> 
  filter(year == 1995) |>
  arrange(desc(share_using_net))

# Now we have the 1995 dataset and the carib dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from Carib

share_net_select_carib_1995_full_join <- full_join(carib, 
                                                   share_net_select_carib_1995,
                                                   by = c("admin" = "entity"))

# Find rows only in Carib

share_net_select_carib_1995_anti_join_1 <- anti_join(carib, 
                                                     share_net_select_carib_1995, 
                                                     by = c("admin" = "entity"))

# Find rows only in share_net_clean_carib_1995

share_net_select_carib_1995_anti_join_2 <- anti_join(share_net_select_carib_1995, 
                                                     carib, 
                                                     by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = share_net_select_carib_1995_full_join, aes(fill = share_using_net), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "1995",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/carib_ict_time_series/share_net_clean_africa_map_1995.png", width = 9, height = 16, dpi = 300)


# Get 2000 data
share_net_select_carib_2000 <- share_net_select_carib |> 
  filter(year == 2000) |>
  arrange(desc(share_using_net))

# Now we have the 2000 dataset and the carib dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from Carib

share_net_select_carib_2000_full_join <- full_join(carib, 
                                                   share_net_select_carib_2000,
                                                   by = c("admin" = "entity"))

# Find rows only in Carib

share_net_select_carib_2000_anti_join_1 <- anti_join(carib, 
                                                     share_net_select_carib_2000, 
                                                     by = c("admin" = "entity"))

# Find rows only in share_net_clean_carib_2000

share_net_select_carib_2000_anti_join_2 <- anti_join(share_net_select_carib_2000, 
                                                     carib, 
                                                     by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = share_net_select_carib_2000_full_join, aes(fill = share_using_net), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2000",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/carib_ict_time_series/share_net_clean_africa_map_2000.png", width = 9, height = 16, dpi = 300)


# Get 2005 data
share_net_select_carib_2005 <- share_net_select_carib |> 
  filter(year == 2005) |>
  arrange(desc(share_using_net))

# Now we have the 2005 dataset and the carib dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from Carib

share_net_select_carib_2005_full_join <- full_join(carib, 
                                                   share_net_select_carib_2005,
                                                   by = c("admin" = "entity"))

# Find rows only in Carib

share_net_select_carib_2005_anti_join_1 <- anti_join(carib, 
                                                     share_net_select_carib_2005, 
                                                     by = c("admin" = "entity"))

# Find rows only in share_net_clean_carib_2005

share_net_select_carib_2005_anti_join_2 <- anti_join(share_net_select_carib_2005, 
                                                     carib, 
                                                     by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = share_net_select_carib_2005_full_join, aes(fill = share_using_net), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2005",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/carib_ict_time_series/share_net_clean_africa_map_2005.png", width = 9, height = 16, dpi = 300)


# Get 2010 data
share_net_select_carib_2010 <- share_net_select_carib |> 
  filter(year == 2010) |>
  arrange(desc(share_using_net))

# Now we have the 2010 dataset and the carib dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from Carib

share_net_select_carib_2010_full_join <- full_join(carib, 
                                                   share_net_select_carib_2010,
                                                   by = c("admin" = "entity"))

# Find rows only in Carib

share_net_select_carib_2010_anti_join_1 <- anti_join(carib, 
                                                     share_net_select_carib_2010, 
                                                     by = c("admin" = "entity"))

# Find rows only in share_net_clean_carib_2010

share_net_select_carib_2010_anti_join_2 <- anti_join(share_net_select_carib_2010, 
                                                     carib, 
                                                     by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = share_net_select_carib_2010_full_join, aes(fill = share_using_net), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2010",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/carib_ict_time_series/share_net_clean_africa_map_2010.png", width = 9, height = 16, dpi = 300)


# Get 2015 data
share_net_select_carib_2015 <- share_net_select_carib |> 
  filter(year == 2015) |>
  arrange(desc(share_using_net))

# Now we have the 2015 dataset and the carib dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from Carib

share_net_select_carib_2015_full_join <- full_join(carib, 
                                                   share_net_select_carib_2015,
                                                   by = c("admin" = "entity"))

# Find rows only in Carib

share_net_select_carib_2015_anti_join_1 <- anti_join(carib, 
                                                     share_net_select_carib_2015, 
                                                     by = c("admin" = "entity"))

# Find rows only in share_net_clean_carib_2015

share_net_select_carib_2015_anti_join_2 <- anti_join(share_net_select_carib_2015, 
                                                     carib, 
                                                     by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = share_net_select_carib_2015_full_join, aes(fill = share_using_net), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2015",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/carib_ict_time_series/share_net_clean_africa_map_2015.png", width = 9, height = 16, dpi = 300)


# Get 2020 data
share_net_select_carib_2020 <- share_net_select_carib |> 
  filter(year == 2020) |>
  arrange(desc(share_using_net))

# Now we have the 2020 dataset and the carib dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from Carib

share_net_select_carib_2020_full_join <- full_join(carib, 
                                                   share_net_select_carib_2020,
                                                   by = c("admin" = "entity"))

# Find rows only in Carib

share_net_select_carib_2020_anti_join_1 <- anti_join(carib, 
                                                     share_net_select_carib_2020, 
                                                     by = c("admin" = "entity"))

# Find rows only in share_net_clean_carib_2020

share_net_select_carib_2020_anti_join_2 <- anti_join(share_net_select_carib_2020, 
                                                     carib, 
                                                     by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = carib) +
  geom_sf() + 
  geom_sf(data = share_net_select_carib_2020_full_join, aes(fill = share_using_net), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2020",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/carib_ict_time_series/share_net_clean_africa_map_2020.png", width = 9, height = 16, dpi = 300)
