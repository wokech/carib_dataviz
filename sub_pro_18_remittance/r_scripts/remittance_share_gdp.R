# Remittance as a share of GDP (%)

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

# List the African countries first

# Clean the data

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

remittance_share_gdp_select <- remittance_share_gdp %>%
  clean_names() %>%
  filter(entity %in% select_countries) 

# Check for number of countries selected
unique(remittance_share_gdp_select$entity)

# B) EDA and Basic Plot

remittance_share_gdp_select |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=5, y=9.5, label="One-Tenth Share", size = 7.5, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
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

ggsave("sub_pro_18_remittance/images/remittance_share_gdp_2024.png", width = 12, height = 12, dpi = 300)


# B) One-Tenth Plot

remittance_share_gdp_select |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  gghighlight(personal_remittances_received_of_gdp > 10) +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=5, y=9.5, label="One-Tenth Share", size = 7.5, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
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

ggsave("sub_pro_18_remittance/images/remittance_share_gdp_2024_one_tenth.png", width = 12, height = 12, dpi = 300)

