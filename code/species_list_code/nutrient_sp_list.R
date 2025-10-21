##Get species list of any fish we have nutrient data for 

library(tidyverse)

##read in nutrient data, cleaned, outliers removed
nutrients_sp <- read.csv("data/processed_data/Nutrient_data_all_outliers_removed.csv") %>%
  select(sci_name) %>%
  unique() %>%
  filter(!grepl("spp", sci_name))

##some are only genus, will need to remove any that are just genus 