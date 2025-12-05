# Caribbean Single Countries with Maps

# Load the required libraries and packages

library(tidyverse)
library(janitor)
library(scales) # control axis/scale format
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot) # plotting theme
library(gghighlight) # highlight specific data
#install.packages("sf")
library(sf) # simple features
#install.packages("tmap") #Thematic maps 
library(tmap)
#install.packages("leaflet") # Used for creating interactive maps
library(leaflet)
#install.packages("ggbreak")
library(ggbreak)
library(patchwork)
library(ggrepel)
library(ggsflabel)
library(ggsci)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)


# Only include caribbean Countries (both Official and Cultural)

caribbean_countries <- c(
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
)

### HOW MANY COUNTRIES ARE THERE IN THE CARIBBEAN ###

# 2) Get country data for mapping

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
caribbean <- world %>%
  filter(admin %in% caribbean_countries)

# Map of the whole of the Caribbean

ggplot(data = caribbean) +
  geom_sf(fill = "goldenrod2", linewidth = 0.5) +
  theme_void()

# # Capital Cities
# 
# # Read in the data
# capital_cities <- read_excel("sub_pro_0_tutorials_tools/single_countries/datasets/africa_countries_capitals.xlsx")
# # trim whitespaces and clean the names
# capital_cities_split <- capital_cities |>
#   mutate(across(everything(), ~trimws(.))) |>
#   clean_names()
# # transform to sf data
# capital_cities_sf <- st_as_sf(capital_cities_split, coords = c("longitude", "latitude"), crs = 4326)
# # transform to match the shapefile CRS
# capital_cities_sf <- st_transform(capital_cities_sf, st_crs(africa))


# 1) Antigua and Barbuda

antigua_barbuda <- c("Antigua and Barbuda")
antigua_barbuda_df <- caribbean |> filter(admin == "Antigua and Barbuda")
#capital_cities_antigua_barbuda_sf <- capital_cities_sf |> filter(country %in% antigua_barbuda)

map_antigua_barbuda <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% antigua_barbuda_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_antigua_barbuda

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/antigua_barbuda_map.png", width = 12, height = 12, dpi = 300)

map_antigua_barbuda_zoom <- ggplot(data = antigua_barbuda_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  #geom_sf(data = capital_cities_antigua_barbuda_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_antigua_barbuda_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/antigua_barbuda_map_zoom.png", width = 12, height = 12, dpi = 300)


# 2) The Bahamas

bahamas <- c("The Bahamas")
bahamas_df <- caribbean |> filter(admin == "The Bahamas")
#capital_cities_bahamas_sf <- capital_cities_sf |> filter(country %in% bahamas)

map_bahamas <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% bahamas_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_bahamas

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/bahamas_map.png", width = 12, height = 12, dpi = 300)

map_bahamas_zoom <- ggplot(data = bahamas_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_bahamas_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_bahamas_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/bahamas_map_zoom.png", width = 12, height = 12, dpi = 300)


# 3) Barbados

barbados <- c("Barbados")
barbados_df <- caribbean |> filter(admin == "Barbados")
#capital_cities_barbados_sf <- capital_cities_sf |> filter(country %in% barbados)

map_barbados <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% barbados_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_barbados

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/barbados_map.png", width = 12, height = 12, dpi = 300)

map_barbados_zoom <- ggplot(data = barbados_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_barbados_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_barbados_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/barbados_map_zoom.png", width = 12, height = 12, dpi = 300)



# 4) Belize

belize <- c("Belize")
belize_df <- caribbean |> filter(admin == "Belize")
#capital_cities_belize_sf <- capital_cities_sf |> filter(country %in% belize)

map_belize <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% belize_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_belize

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/belize_map.png", width = 12, height = 12, dpi = 300)

map_belize_zoom <- ggplot(data = belize_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_belize_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_belize_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/belize_map_zoom.png", width = 12, height = 12, dpi = 300)



# 5) Cuba

cuba <- c("Cuba")
cuba_df <- caribbean |> filter(admin == "Cuba")
#capital_cities_cuba_sf <- capital_cities_sf |> filter(country %in% cuba)

map_cuba <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% cuba_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_cuba

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/cuba_map.png", width = 12, height = 12, dpi = 300)

map_cuba_zoom <- ggplot(data = cuba_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_cuba_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_cuba_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/cuba_map_zoom.png", width = 12, height = 12, dpi = 300)



# 6) Dominica

dominica <- c("Dominica")
dominica_df <- caribbean |> filter(admin == "Dominica")
#capital_cities_dominica_sf <- capital_cities_sf |> filter(country %in% dominica)

map_dominica <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% dominica_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_dominica

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/dominica_map.png", width = 12, height = 12, dpi = 300)

map_dominica_zoom <- ggplot(data = dominica_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_dominica_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_dominica_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/dominica_map_zoom.png", width = 12, height = 12, dpi = 300)



# 7) Dominican Republic

dominican_republic <- c("Dominican Republic")
dominican_republic_df <- caribbean |> filter(admin == "Dominican Republic")
#capital_cities_dominican_republic_sf <- capital_cities_sf |> filter(country %in% dominican_republic)

map_dominican_republic <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% dominican_republic_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_dominican_republic

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/dominican_republic_map.png", width = 12, height = 12, dpi = 300)

map_dominican_republic_zoom <- ggplot(data = dominican_republic_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_dominican_republic_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_dominican_republic_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/dominican_republic_map_zoom.png", width = 12, height = 12, dpi = 300)



# 8) Grenada

grenada <- c("Grenada")
grenada_df <- caribbean |> filter(admin == "Grenada")
#capital_cities_grenada_sf <- capital_cities_sf |> filter(country %in% grenada)

map_grenada <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% grenada_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_grenada

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/grenada_map.png", width = 12, height = 12, dpi = 300)

map_grenada_zoom <- ggplot(data = grenada_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_grenada_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_grenada_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/grenada_map_zoom.png", width = 12, height = 12, dpi = 300)



# 9) Guyana

guyana <- c("Guyana")
guyana_df <- caribbean |> filter(admin == "Guyana")
#capital_cities_guyana_sf <- capital_cities_sf |> filter(country %in% guyana)

map_guyana <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% guyana_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_guyana

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/guyana_map.png", width = 12, height = 12, dpi = 300)

map_guyana_zoom <- ggplot(data = guyana_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_guyana_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_guyana_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/guyana_map_zoom.png", width = 12, height = 12, dpi = 300)



# 10) Haiti

haiti <- c("Haiti")
haiti_df <- caribbean |> filter(admin == "Haiti")
#capital_cities_haiti_sf <- capital_cities_sf |> filter(country %in% haiti)

map_haiti <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% haiti_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_haiti

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/haiti_map.png", width = 12, height = 12, dpi = 300)

map_haiti_zoom <- ggplot(data = haiti_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_haiti_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_haiti_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/haiti_map_zoom.png", width = 12, height = 12, dpi = 300)



# 11) Jamaica

jamaica <- c("Jamaica")
jamaica_df <- caribbean |> filter(admin == "Jamaica")
#capital_cities_jamaica_sf <- capital_cities_sf |> filter(country %in% jamaica)

map_jamaica <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% jamaica_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_jamaica

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/jamaica_map.png", width = 12, height = 12, dpi = 300)

map_jamaica_zoom <- ggplot(data = jamaica_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_jamaica_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_jamaica_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/jamaica_map_zoom.png", width = 12, height = 12, dpi = 300)



# 12) Saint Lucia

saint_lucia <- c("Saint Lucia")
saint_lucia_df <- caribbean |> filter(admin == "Saint Lucia")
#capital_cities_saint_lucia_sf <- capital_cities_sf |> filter(country %in% saint_lucia)

map_saint_lucia <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% saint_lucia_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_saint_lucia

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/saint_lucia_map.png", width = 12, height = 12, dpi = 300)

map_saint_lucia_zoom <- ggplot(data = saint_lucia_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_saint_lucia_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_saint_lucia_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/saint_lucia_map_zoom.png", width = 12, height = 12, dpi = 300)



# 13) Saint Kitts and Nevis

saint_kitts_and_nevis <- c("Saint Kitts and Nevis")
saint_kitts_and_nevis_df <- caribbean |> filter(admin == "Saint Kitts and Nevis")
#capital_cities_saint_kitts_and_nevis_sf <- capital_cities_sf |> filter(country %in% saint_kitts_and_nevis)

map_saint_kitts_and_nevis <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% saint_kitts_and_nevis_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_saint_kitts_and_nevis

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/saint_kitts_and_nevis_map.png", width = 12, height = 12, dpi = 300)

map_saint_kitts_and_nevis_zoom <- ggplot(data = saint_kitts_and_nevis_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_saint_kitts_and_nevis_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_saint_kitts_and_nevis_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/saint_kitts_and_nevis_map_zoom.png", width = 12, height = 12, dpi = 300)



# 14) Saint Vincent and the Grenadines

saint_vincent_and_the_grenadines <- c("Saint Vincent and the Grenadines")
saint_vincent_and_the_grenadines_df <- caribbean |> filter(admin == "Saint Vincent and the Grenadines")
#capital_cities_saint_vincent_and_the_grenadines_sf <- capital_cities_sf |> filter(country %in% saint_vincent_and_the_grenadines)

map_saint_vincent_and_the_grenadines <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% saint_vincent_and_the_grenadines_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_saint_vincent_and_the_grenadines

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/saint_vincent_and_the_grenadines_map.png", width = 12, height = 12, dpi = 300)

map_saint_vincent_and_the_grenadines_zoom <- ggplot(data = saint_vincent_and_the_grenadines_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_saint_vincent_and_the_grenadines_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_saint_vincent_and_the_grenadines_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/saint_vincent_and_the_grenadines_map_zoom.png", width = 12, height = 12, dpi = 300)



# 15) Suriname

suriname <- c("Suriname")
suriname_df <- caribbean |> filter(admin == "Suriname")
#capital_cities_suriname_sf <- capital_cities_sf |> filter(country %in% suriname)

map_suriname <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% suriname_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_suriname

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/suriname_map.png", width = 12, height = 12, dpi = 300)

map_suriname_zoom <- ggplot(data = suriname_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_suriname_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_suriname_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/suriname_map_zoom.png", width = 12, height = 12, dpi = 300)




# 16) Trinidad and Tobago

trinidad_and_tobago <- c("Trinidad and Tobago")
trinidad_and_tobago_df <- caribbean |> filter(admin == "Trinidad and Tobago")
#capital_cities_trinidad_and_tobago_sf <- capital_cities_sf |> filter(country %in% trinidad_and_tobago)

map_trinidad_and_tobago <- ggplot(data = caribbean)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "black")+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% trinidad_and_tobago_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_trinidad_and_tobago

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/trinidad_and_tobago_map.png", width = 12, height = 12, dpi = 300)

map_trinidad_and_tobago_zoom <- ggplot(data = trinidad_and_tobago_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, fill = "salmon1", color = "black")+
  #geom_sf(data = capital_cities_trinidad_and_tobago_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_trinidad_and_tobago_zoom

# Save the plot
#ggsave("sub_pro_0_tutorials_tools/single_countries/images/trinidad_and_tobago_map_zoom.png", width = 12, height = 12, dpi = 300)



























































































































