# An analysis of Caribbean countries by GDP (2025 Estimates)

# Data Source: Wikipedia

# 1. Introduction
## GDP is defined as the "total monetary or market value of all the finished 
## goods and services produced within a country’s borders in a specific time period"

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)
library(treemapify)
library(scales)
library(ggrepel)

# (C) Get the data from Wikipedia

# link <- "https://en.wikipedia.org/wiki/List_of_Latin_American_and_Caribbean_countries_by_GDP_(nominal)"
# carib_country <- link %>%
#   read_html("[class='wikitable sortable']") %>%
#   html_table(fill = TRUE)
# 
# carib_country_GDP <- carib_country[[1]]
# 
# # Filter out the relevant countries
# 
# select_countries <- c("Antigua and Barbuda", "The Bahamas", "Barbados", "Belize", 
#                       "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
#                       "Haiti", "Jamaica", "Saint Kitts and Nevis",
#                       "Saint Lucia", "Saint Vincent and the Grenadines",
#                       "Suriname", "Trinidad and Tobago")
# 
# carib_country_GDP_select <- carib_country_GDP %>%
#   clean_names() %>%
#   filter(country %in% select_countries) 
# 
# carib_country_GDP_select$gdp_nominal_millions_of_us <- parse_number(carib_country_GDP_select$gdp_nominal_millions_of_us)
# carib_country_GDP_select$gdp_nominal_percapita_us <- parse_number(carib_country_GDP_select$gdp_nominal_percapita_us)
# 
# carib_country_GDP_select <- carib_country_GDP_select %>%
#   mutate(gdp_nominal_billions_of_us = gdp_nominal_millions_of_us/1000)
# 
# # Save the processed dataset
# 
# write_csv(carib_country_GDP_select, "sub_pro_3_carib_economy/datasets/gdp_gdp_per_cap.csv")

# Load the data

carib_country_GDP_select <- read_csv("sub_pro_3_carib_economy/datasets/gdp_gdp_per_cap.csv")

# (D) Plots

## 1) A bar plot demonstrating the GDP of the top 5 economies relative to the rest of the region

ggplot(carib_country_GDP_select, aes(reorder(country, +gdp_nominal_billions_of_us), gdp_nominal_billions_of_us, fill = country)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_nominal_billions_of_us) > 20) + 
  scale_fill_brewer(palette="Reds") +
  labs(x = "Country",
       y = "Nominal GDP (Billions, US$)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, 150, by = 20),
                     minor_breaks = seq(0, 150, by = 10)) +
  theme(axis.title.x =element_text(size = 30),
        axis.title.y =element_text(size = 30),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5, vjust=0),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = -0.75, vjust = 1),
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_carib_economy/images/bar_top_5_carib_gdp_2025.png", width = 12, height = 12, dpi = 300)

## 2) A treemap demonstrating the GDP of the top 5 economies relative to the rest of the continent

# Get the top 5 economies
carib_country_GDP_clean_top_5 <- carib_country_GDP_select[1:5, ]

# Get the bottom 10
carib_country_GDP_clean_bottom <- carib_country_GDP_select[6:15, ]

# Sum the bottom 10
carib_country_GDP_clean_bottom_total <- carib_country_GDP_clean_bottom %>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Rest of the Region'))) %>%
  filter(row_number() %in% 11)

# Combine the top 5 economies with the combined GDP of the other 49 countries
carib_country_GDP_clean_top_5_bottom_total <- rbind(carib_country_GDP_clean_top_5, 
                                                    carib_country_GDP_clean_bottom_total)

# Create new columns to provide labels for the treemap
carib_country_GDP_clean_top_5_bottom_total_new_col <- carib_country_GDP_clean_top_5_bottom_total %>%
  mutate(nominal_gdp_new_1 = paste("$", gdp_nominal_billions_of_us, sep = "")) %>%
  mutate(nominal_gdp_new_2 = paste(nominal_gdp_new_1, "Billion", sep = " ")) 

# Plot the tree map
ggplot(carib_country_GDP_clean_top_5_bottom_total_new_col, 
       aes(area = gdp_nominal_billions_of_us, fill = country, 
           label = paste(country, nominal_gdp_new_2, sep = "\n"))) +
  geom_treemap() +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 25,
                    grow = FALSE) + 
  theme(legend.position = "none",
        plot.title = element_text(size=40, hjust = 0.5, vjust = 1, face="bold"),
        plot.subtitle = element_text(size=15, hjust = 0.5, vjust = 1),
        legend.text = element_text(size = 20),
        plot.caption = element_text(size = 25, hjust = 0, vjust = 1, face = "bold"),
        panel.background = element_rect(fill="#F2F2F2"),
        plot.background = element_rect(fill="#F2F2F2"),
        legend.background = element_rect(fill="#F2F2F2")) +
  scale_fill_brewer(palette = "RdPu")

ggsave("sub_pro_3_carib_economy/images/treemap_top_5_carib_gdp_2025.png", width = 12, height = 12, dpi = 300)

## 3) A bar plot to compare the GDP per capita of Caribbean countries

ggplot(carib_country_GDP_select, aes(reorder(country, +gdp_nominal_percapita_us), gdp_nominal_percapita_us, fill = country)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_nominal_percapita_us) > 20000) + 
  geom_hline(yintercept = 10000, linetype = "dashed") +
  annotate("text", x=5, y=11000, label="GDP per capita = $10000", size = 8, angle=90) +
  geom_hline(yintercept = 20000, linetype = "dashed") +
  annotate("text", x=5, y=21000, label="GDP per capita = $20000", size = 8, angle=90) +
  scale_fill_brewer(palette="BuGn") +
  labs(x = "Country",
       y = "GDP per capita (US$)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma, 
                     breaks = seq(0, 40000, by = 10000),
                     minor_breaks = seq(0, 40000, by = 2500)) +
  theme(axis.title.x =element_text(size = 30),
        axis.title.y =element_text(size = 30),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.minor = element_line(color = "black", size = 5),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25, hjust = 0.5, vjust=0.5),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = -0.75, vjust = 1),
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_carib_economy/images/bar_top_carib_5_gdp_per_cap_2025.png", width = 12, height = 12, dpi = 300)

## 4) Scatter plot to compare GDP per capita, population size, and nominal GDP

# Import the required data from Wikipedia

link_pop <- "https://en.wikipedia.org/wiki/List_of_Caribbean_countries_by_population"
carib_country_pop <- link_pop %>%
  read_html("[class='wikitable sortable']") %>%
  html_table(fill = TRUE)

carib_country_pop_1 <- carib_country_pop[[1]]

# Clean the data, fix columns and county labels

carib_country_pop_1_clean <- carib_country_pop_1 %>%
  clean_names() %>%
  mutate(country_dependency = case_when(
    country_dependency == "Bahamas" ~ "The Bahamas",
    TRUE ~ country_dependency # Keep all other values as they are
  )) %>%
  select(c(country_dependency , officialfigure)) %>%
  filter(country_dependency  %in% select_countries) %>%
  rename("population" = "officialfigure") %>%
  rename("country" = "country_dependency") 

# parsing out the number is very critical as a simple conversion using
# as._____() will not work

# Join the tables

carib_country_GDP_pop <- left_join(carib_country_GDP_select, carib_country_pop_1_clean, by = "country")

carib_country_GDP_pop$population <- parse_number(carib_country_GDP_pop$population)
carib_country_GDP_pop$gdp_nominal_millions_of_us <- parse_number(carib_country_GDP_pop$gdp_nominal_millions_of_us)
carib_country_GDP_pop$gdp_nominal_percapita_us <- parse_number(carib_country_GDP_pop$gdp_nominal_percapita_us)

carib_country_GDP_pop <- carib_country_GDP_pop %>%
  mutate(gdp_nominal_billions_of_us = gdp_nominal_millions_of_us / 1000)

# # Save the processed dataset

write_csv(carib_country_GDP_pop, "sub_pro_3_carib_economy/datasets/carib_gdp_population.csv")

# Load the data

carib_country_GDP_pop <- read_csv("sub_pro_3_carib_economy/datasets/carib_gdp_population.csv")

# Basic bubble plot / ####### Not very useful #########

ggplot(carib_country_GDP_pop, aes(x=gdp_nominal_percapita_us, y=gdp_nominal_billions_of_us)) +
  geom_point(alpha=0.5, size = 4, color = "darkblue") + 
  geom_text_repel(aes(label = ifelse(gdp_nominal_percapita_us >= 10000 | gdp_nominal_millions_of_us >= 5, country, "")), 
                  size = 7.5, max.overlaps = 20, seed = 42, segment.length = 2, segment.size = 1) +
  labs(x = "GDP per capita (US$)",
       y = "Nominal GDP (Billions, US$)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma, 
                     breaks = seq(0, 150, by = 50),
                     minor_breaks = seq(0, 150, by = 50)) +
  theme(axis.title.x =element_text(size = 25),
        axis.title.y =element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25, hjust = 0.5, vjust=0.5),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_carib_economy/images/scatter_gdp_vs_gdp_per_cap_2025.png", width = 12, height = 12, dpi = 300)
