# ICT in the Caribbean (Part 1)

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

# Load the required datasets

# a) Share of individuals using the internet

# share_net <- read_csv("https://ourworldindata.org/grapher/share-of-individuals-using-the-internet.csv?v=1&csvType=full&useColumnShortNames=true")

# write_csv(share_net, "sub_pro_8_ict_owid/datasets/share_net.csv")

share_net <- read_csv("sub_pro_8_ict_owid/datasets/share_net.csv")

# Clean the datasets

# Only include Carib Countries

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

share_net_select_carib <- share_net %>%
  clean_names() %>%
  filter(entity %in% select_countries)

# Should be 16 countries
unique(share_net_select_carib$entity)

# EDA plots

# 1) Top 5 Carib Countries (Internet Access) in 2023

share_net_select_carib |> 
  rename(share_using_net = "it_net_user_zs") |>
  filter(year == 2023) |>
  arrange(desc(share_using_net)) |>
  #slice_max(share_using_net, n = 5) |>
  ggplot(aes(x=reorder(entity, share_using_net), y = share_using_net, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_color_brewer(palette = "Set3") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 80, linetype = "dashed") +
  annotate("text", x=5, y=79, label="80% Share", size = 7.5, angle=90) +
  labs(x = "Country",
       y = "Share of the population using the Internet (%)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_8_ict_owid/images/share_net_carib.png", width = 12, height = 12, dpi = 300)
