# Olympics and Paralympics (2024) in Caribbean

# Olympics: https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table

# Paralympics: https://www.paralympic.org/en/paris-2024-paralympics/medals

# (A) Load the required libraries

library(readxl)
library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# (B) Download the data

# link <- "https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table"

# olympics_2024 <- link %>%
#   read_html("[class='wikitable sortable']") %>%
#   html_table(fill = TRUE)

# olympics_2024_1 <- olympics_2024[[4]]

# olympics_2024_1 <- olympics_2024_1 |>
#   clean_names() |>
#   rename(country = noc) |>
#   mutate(total = as.numeric(total))

# Save datasets

# write_csv(olympics_2024_1, "sub_pro_9_olympics_wpr_ioc/datasets/olympics_2024")

# (C) Load the required datasets

olympics_2024 <- read_csv("sub_pro_9_olympics_wpr_ioc/datasets/olympics_2024") |>
  clean_names()
paralympics_2024 <- read_excel("sub_pro_9_olympics_wpr_ioc/datasets/paralympics_2024.xlsx") |>
  clean_names()

# Get the carib datasets

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")
  
