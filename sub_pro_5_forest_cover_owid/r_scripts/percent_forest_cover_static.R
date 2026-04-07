# Forests (OWID) - Static

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

# Save data
# write_csv(forest_share_land_area, "sub_pro_5_forest_cover_owid/datasets/forest_share_land_area.csv")

# Load data again
# forest_share_land_area <- read_csv("sub_pro_5_forest_cover_owid/datasets/forest_share_land_area.csv")


# Clean the data

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

forest_share_land_area_select <- forest_share_land_area %>%
  clean_names() %>%
  filter(entity %in% select_countries) 

# B) EDA and Basic Plot

forest_share_land_area_select |> 
  filter(year == 2025) |>
  arrange(desc(forest_share)) |>
  ggplot(aes(x=reorder(entity, forest_share), y = forest_share, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_color_brewer(palette = "Set3") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 7.5,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 90, linetype = "dashed") +
  annotate("text", x=7.5, y=89, label="Forest cover  = 90%", size = 7.5, angle=90) +
  labs(x = "Country",
       y = "Share of land covered by forest (%, 2025)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_5_forest_cover_owid/images/forest_share_africa_top_5.png", width = 12, height = 12, dpi = 300)
