# Olympics and Caribbean Countries

# https://worldpopulationreview.com/country-rankings/olympic-medals-by-country

# (A) Load the required libraries

library(readxl)
library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# (B) Load the required data and include the required sheet

olympic_data <- read_excel("sub_pro_9_olympics_wpr_ioc/datasets/olympic_medal_country.xlsx", sheet = "All countries")

# (C) Clean the data, fix columns and county labels

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

olympic_data_select_carib <- olympic_data |>
  clean_names() %>%
  filter(country %in% select_countries)

str(olympic_data_select_carib)

# Plot 1 - Total medals earned

ggplot(olympic_data_select_carib, aes(reorder(country, +total_medals), total_medals, fill = country)) +
  geom_bar(stat = "identity") + coord_flip(ylim = c(0, 250)) +
  geom_text(aes(label = paste(total_medals), hjust = -0.1), size = 8) +
  gghighlight(max(total_medals) > 10) + 
  scale_fill_brewer(palette="Set2") +
  labs(x = "Country",
       y = "Total number of medals",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 250, 50)) +
  theme(axis.title.x = element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y = element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", color = "black"),
        axis.text.y = element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 20, hjust = 0, vjust = 1),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_9_olympics_wpr_ioc/images/carib_olympic_medals_square.png", width = 12, height = 12, dpi = 72)

# Plot 2 - Total medals earned per 100k people

ggplot(olympic_data_select_carib, aes(reorder(country, +medals_per_100k), medals_per_100k, fill = country)) +
  geom_bar(stat = "identity") + coord_flip(ylim = c(0, 4)) +
  geom_text(aes(label = paste(round(medals_per_100k, 2)), hjust = -0.1), size = 8) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  gghighlight(max(medals_per_100k) > 1) + 
  scale_fill_brewer(palette="Set2") +
  labs(x = "Country",
       y = "Medals per 100K people (2023)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 4, 1)) +
  theme(axis.title.x = element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y = element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", color = "black"),
        axis.text.y = element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 20, hjust = 0, vjust = 1),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_9_olympics_wpr_ioc/images/carib_olympic_medals_per_person_square.png", width = 12, height = 12, dpi = 72)
