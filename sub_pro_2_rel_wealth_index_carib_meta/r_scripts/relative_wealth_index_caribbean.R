# Relative Wealth Index - Caribbean
# using the Meta DataforGood dataset

# Install necessary packages
# install.packages(c("ggplot2", "sf", "viridis", "rnaturalearth", "rnaturalearthdata"))

# Load the libraries
library(ggplot2)
library(sf)
library(ggrepel)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# Belize

# Read the CSV file
blz_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/blz_relative_wealth_index.csv")

# Convert the data to an sf object
blz_data_sf <- st_as_sf(blz_data, coords = c("longitude", "latitude"), crs = 4326)

# Belize country borders
belize <- ne_countries(scale = "medium", country = "Belize", returnclass = "sf")

# Create a data frame with major Belizian towns
major_towns_belize <- data.frame(
  name = c("Belize City", "San Pedro Town", "Orange Walk", "Belmopan", "San Ignacio"),
  longitude = c(-88.198, -87.966, -88.563, -88.764, -89.07),
  latitude = c(17.5, 17.916, 18.081, 17.254, 17.159)
)

# Map of Belize

ggplot() +
  geom_sf(data = belize, fill = NA, color = "black", linewidth = 1) + # Add Belize borders
  geom_point(data = blz_data, aes(x = longitude, y = latitude, color = rwi), size = 0.75, alpha = 0.8) +
  geom_text_repel(data = major_towns_belize, aes(x = longitude, y = latitude, label = name), 
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_belize, aes(x = longitude, y = latitude), 
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom") + 
  guides(color = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/belize.png", width = 12, height = 12, dpi = 300)



# Dominica

# Read the CSV file
dma_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/dma_relative_wealth_index.csv")

# Convert the data to an sf object
dma_data_sf <- st_as_sf(dma_data, coords = c("longitude", "latitude"), crs = 4326)

# Dominica country borders
dominica <- ne_countries(scale = "medium", country = "Dominica", returnclass = "sf")

# Create a data frame with major Dominican towns
major_towns_dominica <- data.frame(
  name = c("Roseau", "Portsmouth", "Marigot", "Berekua", "Mahaut"),
  longitude = c(-61.388, -61.456, -61.282, -61.319, -61.397),
  latitude = c(15.302, 15.583, 15.537, 15.245, 15.364)
)

# Map of Dominica

ggplot() +
  geom_sf(data = dominica, fill = NA, color = "black", linewidth = 1) + # Add Dominica borders
  geom_point(data = dma_data, aes(x = longitude, y = latitude, color = rwi), size = 0.75, alpha = 0.8) +
  geom_text_repel(data = major_towns_dominica, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_dominica, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24.5),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/dominica.png", width = 12, height = 12, dpi = 300)



# Dominican Republic

# Read the CSV file
dom_rep_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/dom_rep_relative_wealth_index.csv")

# Convert the data to an sf object
dom_rep_data_sf <- st_as_sf(dom_rep_data, coords = c("longitude", "latitude"), crs = 4326)

# Dominican Republic country borders
dominican_republic <- ne_countries(scale = "medium", country = "Dominican Republic", returnclass = "sf")

# Create a data frame with major Dominican Republic towns
major_towns_dominican_republic <- data.frame(
  name = c("Santo Domingo", "Santiago", "Santo Domingo Oeste", "Santo Domingo Este", "San Pedro de Macoris"),
  longitude = c(-69.892, -70.691, -70, -69.848, -69.309),
  latitude = c(18.472, 19.45, 18.5, 18.485, 18.454)
)

# Map of Dominican Republic

ggplot() +
  geom_sf(data = dominican_republic, fill = NA, color = "black", linewidth = 1) + # Add Dominican Republic borders
  geom_point(data = dom_rep_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_dominican_republic, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_dominican_republic, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/dominican_republic.png", width = 12, height = 12, dpi = 300)



# Grenada

# Read the CSV file
grd_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/grd_relative_wealth_index.csv")

# Convert the data to an sf object
grd_data_sf <- st_as_sf(grd_data, coords = c("longitude", "latitude"), crs = 4326)

# Grenada country borders
grenada <- ne_countries(scale = "medium", country = "Grenada", returnclass = "sf")

# Create a data frame with major Grenadian towns
major_towns_grenada <- data.frame(
  name = c("Saint George's", "Gouyave", "Grenville", "Grand Roy", "Victoria"),
  longitude = c(-61.752, -61.73, -61.625, -61.745, -61.707),
  latitude = c(12.053, 12.165, 12.123, 12.132, 12.19)
)

# Map of Grenada

ggplot() +
  geom_sf(data = grenada, fill = NA, color = "black", linewidth = 1) + # Add Grenada borders
  geom_point(data = grd_data, aes(x = longitude, y = latitude, color = rwi), size = 0.75, alpha = 0.8) +
  geom_text_repel(data = major_towns_grenada, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_grenada, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        #plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/grenada.png", width = 12, height = 12, dpi = 300)



# Guyana

# Read the CSV file
guy_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/guy_relative_wealth_index.csv")

# Convert the data to an sf object
guy_data_sf <- st_as_sf(guy_data, coords = c("longitude", "latitude"), crs = 4326)

# Guyana country borders
guyana <- ne_countries(scale = "medium", country = "Guyana", returnclass = "sf")

# Create a data frame with major Guyana towns
major_towns_guyana <- data.frame(
  name = c("Georgetown", "Linden", "New Amsterdam", "Anna Regina", "Bartica"),
  longitude = c(-58.155, -58.307, -57.516, -58.483, -58.623),
  latitude = c(6.804, 6.008, 6.249, 7.266, 6.405)
)

# Map of Guyana

ggplot() +
  geom_sf(data = guyana, fill = NA, color = "black", linewidth = 1) + # Add Guyana borders
  geom_point(data = guy_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_guyana, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_guyana, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        #plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/guyana.png", width = 12, height = 12, dpi = 300)



# Haiti

# Read the CSV file
hti_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/hti_relative_wealth_index.csv")

# Convert the data to an sf object
hti_data_sf <- st_as_sf(hti_data, coords = c("longitude", "latitude"), crs = 4326)

# Haiti country borders
haiti <- ne_countries(scale = "medium", country = "Haiti", returnclass = "sf")

# Create a data frame with major Haitian towns
major_towns_haiti <- data.frame(
  name = c("Port-au-Prince", "Delmas", "Port-de-Paix", "Pétionville", "Croix-des-Bouquets"),
  longitude = c(-72.339, -72.3, -72.83, -72.285, -72.226),
  latitude = c(18.543, 18.545, 19.94, 18.512, 18.577)
)

# Map of Haiti

ggplot() +
  geom_sf(data = haiti, fill = NA, color = "black", linewidth = 1) + # Add Haiti borders
  geom_point(data = hti_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_haiti, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_haiti, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        #plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/haiti.png", width = 12, height = 12, dpi = 300)



# Jamaica

# Read the CSV file
jam_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/jam_relative_wealth_index.csv")

# Convert the data to an sf object
jam_data_sf <- st_as_sf(jam_data, coords = c("longitude", "latitude"), crs = 4326)

# Jamaica country borders
jamaica <- ne_countries(scale = "medium", country = "Jamaica", returnclass = "sf")

# Create a data frame with major Jamaican towns
major_towns_jamaica <- data.frame(
  name = c("Kingston", "Spanish Town", "Portmore", "Montego Bay", "Mandeville"),
  longitude = c(-76.794, -76.957, -76.887, -77.919, -77.507),
  latitude = c(17.997, 17.991, 17.971, 18.471, 18.042)
)

# Map of Jamaica

ggplot() +
  geom_sf(data = jamaica, fill = NA, color = "black", linewidth = 1) + # Add Jamaica borders
  geom_point(data = jam_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_jamaica, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_jamaica, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        #plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/jamaica.png", width = 12, height = 12, dpi = 300)



# Saint Lucia

# Read the CSV file
st_lca_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/st_lca_relative_wealth_index.csv")

# Convert the data to an sf object
st_lca_data_sf <- st_as_sf(st_lca_data, coords = c("longitude", "latitude"), crs = 4326)

# Saint Lucia country borders
saint_lucia <- ne_countries(scale = "medium", country = "Saint Lucia", returnclass = "sf")

# Create a data frame with major Saint Lucian towns
major_towns_saint_lucia <- data.frame(
  name = c("Gros Islet", "Castries", "Bisee", "Dennery", "Laborie"),
  longitude = c(-60.95, -61.006, -60.974, -60.891, -60.983),
  latitude = c(14.067, 13.996, 14.024, 13.914, 13.75)
)

# Map of Saint Lucia

ggplot() +
  geom_sf(data = saint_lucia, fill = NA, color = "black", linewidth = 1) + # Add Saint Lucia borders
  geom_point(data = st_lca_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_saint_lucia, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_saint_lucia, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        #plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/saint_lucia.png", width = 12, height = 12, dpi = 300)



# St. Vincent and the Grenadines

# Read the CSV file
st_vct_gre_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/st_vct_gre_relative_wealth_index.csv")

# Convert the data to an sf object
st_vct_gre_data_sf <- st_as_sf(st_vct_gre_data, coords = c("longitude", "latitude"), crs = 4326)

# St. Vincent and the Grenadines country borders
saint_vincent_grenadines <- ne_countries(scale = "medium", country = "Saint Vincent and the Grenadines", returnclass = "sf")

# Create a data frame with major St. Vincent and the Grenadines towns
major_towns_saint_vincent_grenadines <- data.frame(
  name = c("Kingstown", "Calliaqua", "Diamond", "Redemption", "Georgetown"),
  longitude = c(-61.227, -61.192, -61.168, -61.229, -61.118),
  latitude = c(13.155, 13.129, 13.141, 13.164, 13.281)
)

# Map of St. Vincent and the Grenadines

ggplot() +
  geom_sf(data = saint_vincent_grenadines, fill = NA, color = "black", linewidth = 1) + # Add St. Vincent and the Grenadines borders
  geom_point(data = st_vct_gre_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_saint_vincent_grenadines, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_saint_vincent_grenadines, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        #plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/saint_vincent_grenadines.png", width = 12, height = 12, dpi = 300)



# Suriname

# Read the CSV file
sur_data <- read.csv("sub_pro_2_rel_wealth_index_carib_meta/datasets/sur_relative_wealth_index.csv")

# Convert the data to an sf object
sur_data_sf <- st_as_sf(sur_data, coords = c("longitude", "latitude"), crs = 4326)

# Suriname country borders
suriname <- ne_countries(scale = "medium", country = "Suriname", returnclass = "sf")

# Create a data frame with major Suriname towns
major_towns_suriname <- data.frame(
  name = c("Paramaribo", "Lelydorp", "Brokopondo", "Nieuw Nickerie", "Meerzorg"),
  longitude = c(-55.167, -55.233, -54.98, -56.973, -55.141),
  latitude = c(5.866, 5.7, 5.056, 5.926, 5.808)
)

# Map of Suriname

ggplot() +
  geom_sf(data = suriname, fill = NA, color = "black", linewidth = 1) + # Add Suriname borders
  geom_point(data = sur_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_suriname, aes(x = longitude, y = latitude, label = name),
                  color = "black", check_overlap = TRUE, size = 6, vjust = 1.5) +
  geom_point(data = major_towns_suriname, aes(x = longitude, y = latitude),
             color = "black", size = 5, shape = 16, alpha = 0.5) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44),
        #plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = unit(1.5, "cm"), 
                                barwidth = unit(15, "cm")))

ggsave("sub_pro_2_rel_wealth_index_carib_meta/images/suriname.png", width = 12, height = 12, dpi = 300)



