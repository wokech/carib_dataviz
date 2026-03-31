# Benjamin Braun Lessons
# https://benjaminbraun.org/tjamaicahing/financial-data/dbnomics_r#/title-slide

# Current Account Balance

# install.packages("rdbnomics")
# install.packages("thematic")

library(tidyverse) #https://www.tidyverse.org/
library(rdbnomics) #https://git.nomics.world/dbnomics/rdbnomics
library(janitor) #https://github.com/sfirke/janitor
library(thematic) #For styling plots: https://rstudio.github.io/thematic/index.html

# 1) Current account
#    Exports of goods and services + Receipts of income on ....-owned assets abroad Minus
#    Imports of goods and services + Payments of income on foreign-owned assets in the 
#    United States + Unilateral current transfers

# Jamaica 

bop_imf_jamaica <- rdb("IMF", "BOP", mask = "A.JM.BCA_BP6_USD") 

bop_imf_jamaica <- as_tibble(bop_imf_jamaica) |>
  clean_names()

# Jamaica Plot 

ggplot(
  data = bop_imf_jamaica,
  aes(x = period, 
      y = value)) +
  geom_col() +
  labs(title = 'CA balance ...') + 
  facet_wrap("ref_area")

# Top Economies Plot

bop_imf_top_econ_carib <- rdb("IMF", "BOP", mask = "A.DO+JM+TT+HT.BCA_BP6_USD") 

bop_imf_top_econ_carib <- as_tibble(bop_imf_top_econ_carib) |>
  clean_names() |>
  filter(period > 1980)

# Caribbean Plot 

ggplot(
  data = bop_imf_top_econ_carib,
  aes(x = period, 
      y = value)) +
  geom_col() +
  labs(title = 'CA balance ...') + 
  facet_wrap("ref_area")
