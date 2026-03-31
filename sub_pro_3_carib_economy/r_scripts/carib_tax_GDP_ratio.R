# An analysis of tax revenue to GDP ratio
# Tax as % of GDP (2023)

# Data Source: Our World in Data (UNU-WIDER and World Bank)

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)
library(scales)

# (B) Get the data from OWID via API

# tax_GDP_carib <- read.csv("https://ourworldindata.org/grapher/tax-revenues-vs-gdp-per-capita.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2023&country=ATG~BHS~BRB~BLZ~DMA~DOM~GRD~GUY~HTI~JAM~LCA~KNA~VCT~SUR~TTO")

# Save datasets

# write_csv(tax_GDP_carib, "sub_pro_3_carib_economy/datasets/tax_GDP_carib.csv")

tax_GDP_carib <- read_csv("sub_pro_3_carib_economy/datasets/tax_GDP_carib.csv")

# (C) Wrangle the data and select the correct countries

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

tax_GDP_carib_select <- tax_GDP_carib %>%
  select(entity, tax_inc_sc, ny_gdp_pcap_pp_kd) %>%
  filter(entity %in% select_countries)

# (E) Plots

## 1) A bar plot demonstrating the tax/GDP ratio 

ggplot(tax_GDP_carib_select, aes(reorder(entity, +tax_inc_sc), tax_inc_sc, fill = entity)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(tax_inc_sc) > 25) + 
  geom_hline(yintercept = 25, linetype = "dashed") +
  annotate("text", x=7.5, y=24, label="Tax:GDP ratio = 25%", size = 8, angle=90) +
  scale_fill_brewer(palette="GnBu") +
  labs(x = "Country",
       y = "Tax:GDP Ratio (%)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, 100, by = 10),
                     minor_breaks = seq(0, 100, by = 5)) +
  theme(axis.title.x =element_text(size = 30),
        axis.title.y =element_text(size = 30),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = -0.5, vjust = 1),
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_carib_economy/images/tax_GDP_2023.png", width = 12, height = 12, dpi = 300)

