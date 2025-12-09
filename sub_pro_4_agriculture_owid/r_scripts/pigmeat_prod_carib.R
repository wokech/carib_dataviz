# Caribbean Avocado Production

# 1) Load the Required Libraries

# Solve package loading issues with options(timeout = 600) 
# increase download length time

library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)
library(ggplot2)
library(janitor)
library(scales)
library(devtools)
library(treemapify)
library(ggrepel)
library(patchwork)
library(stringr)
library(magick)
library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)
library(jsonlite)

# 2) Data Cleaning and Organization

# Fetch the data

# pigmeat_prod <- read.csv("https://ourworldindata.org/grapher/avocado-production.csv?v=1&csvType=full&useColumnShortNames=true",
#                       na.strings = "")
# 
# # Save the data
# write.csv(pigmeat_prod, "sub_pro_4_agriculture_owid/datasets/avocado-production-tonnes.csv",
#           row.names = FALSE)

# Read in the data
pigmeat_prod <- read.csv("sub_pro_4_agriculture_owid/datasets/pigmeat-production-tonnes.csv")

# Clean the column headings

pigmeat_prod_clean <- pigmeat_prod |>
  clean_names() 

# Change the column title names

pigmeat_prod_clean <- pigmeat_prod_clean |>
  rename("country" = "entity",
         "pigmeat_production_tonnes" = "meat_pig_00001035_production_005510_tonnes") 

# Filter by region

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

pigmeat_prod_clean_carib <- pigmeat_prod_clean |>
  select(c(1,3,4)) |>
  filter(country %in% caribbean_countries)

################################################################################
# QC to check for missing countries!
################################################################################

# Countries that have data
unique(pigmeat_prod_clean_carib$country)

# Countries that don't have data
setdiff(caribbean_countries, unique(pigmeat_prod_clean_carib$country))

# Check whether any countries in the dataset are not in the list of Caribbean countries
setdiff(unique(pigmeat_prod_clean_carib$country), caribbean_countries)

## Then check the original dataset manually to see if countries are actually missing 
######################################################################################

# Plot the stack area chart

carib_pigmeat_2023_plot <- pigmeat_prod_clean_carib |>
  filter(year == "2023") |>
  ggplot(aes(x = reorder(country, pigmeat_production_tonnes), y = pigmeat_production_tonnes)) +
  geom_col(width = 0.95,
           fill = "turquoise")+ 
  coord_flip() + 
  geom_text(aes(x = country, y = pigmeat_production_tonnes+50000, label = comma(pigmeat_production_tonnes)),
            color = "black",
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  theme_classic() +
  labs(x = "", 
       y = "Pig Meat Production\n(tonnes)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28, margin = margin(t = 20)),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        legend.position = "")

carib_pigmeat_2023_plot

ggsave("sub_pro_4_agriculture_owid/images/regional_food_barplots/carib_pigmeat_2023_plot.png",
       width = 12, height = 12, dpi = 300)

