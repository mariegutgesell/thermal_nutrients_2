##Determining Nutrient Ranges based on InFoods 


library(tidyverse)
library(ggplot2)
library(reshape2)

#2) AnFoods Database -----------
##Import nutrient data for fish
anfoods_1 <- read_excel("data/raw_data/AnFooD2.0_read_only.xlsx", sheet = "09 Fish & Shellfish") %>%
  mutate(`ASFIS Scientific name` = coalesce(`ASFIS Scientific name`, `Scientific name`)) %>%  ##if ASFIS is NA, fill in name from scientific name, if exists
  dplyr::rename(sci_name = `ASFIS Scientific name`, sampling_location = `Country, region` ) %>%
  filter(Subgroup == "Finfish") %>%
  mutate(sci_name = str_remove_all(sci_name, "\\(|\\)")) %>% ##remove any brackets around a given species name
  select(`Food Item ID`, `3_alpha`, sci_name, `Food name in English`, `ASFIS English name`, sampling_location, Type, Processing, `Comments on data processing/methods`, BiblioID, `CA(mg)`, `FE(mg)`, `ZN(mg)`, `SE(mcg)`, `VITA_RAE(mcg)`,`VITA-(mcg)`, `PROTCNT(g)`, `PROTCNP(g)`, `PROT-(g)`) 



anfoods_2 <- read_excel("data/raw_data/AnFooD2.0_read_only.xlsx", sheet = "09 Fish & Shellfish_fatty acids") %>%
  mutate(`ASFIS Scientific name` = coalesce(`ASFIS Scientific name`, `Scientific name`)) %>%  ##if ASFIS is NA, fill in name from scientific name, if exists
  dplyr::rename(sci_name = `ASFIS Scientific name`, sampling_location = `Country, region`)%>%
  filter(Subgroup == "Finfish") %>%
  mutate(sci_name = str_remove_all(sci_name, "\\(|\\)")) %>% ##remove any brackets around a given species name
  select(`Food Item ID`, `3_alpha`, sci_name, `Food name in English`, `ASFIS English name`, sampling_location, Type, Processing, `Comments on data processing/methods`, BiblioID,  `FACID(g)`, `FAPUN3(g)`, `F20D5N3(g)`, `F22D6N3(g)`) 

##Join two dataframes together, filter to keep only usable samples (e.g., raw, wild-caught)
anfoods_df <- left_join(anfoods_1, anfoods_2, by = c("Food Item ID", "3_alpha", "sci_name", "Food name in English", "ASFIS English name", "sampling_location", "Type", "Processing", "Comments on data processing/methods", "BiblioID")) %>%
  mutate(source_df = "AnFoods") %>%
  
  mutate(sample_origin = case_when(
    grepl("W", Type) ~ "Wild",
    grepl("F", Type) ~ "Farmed",
  )) %>%
  mutate(body_part = case_when(
    grepl("skinless fillet", `Food name in English`) ~ "muscle_skinless",
    grepl("skinless, boneless fillet", `Food name in English`) ~ "muscle_skinless",
    grepl("fillet", `Food name in English`) ~ "muscle",
    grepl("muscle", `Food name in English`) ~ "muscle",
    grepl("meat", `Food name in English`) ~ "muscle",
    grepl("whole", `Food name in English`) ~ "whole",
    grepl("flesh", `Food name in English`) ~ "muscle",
    grepl("skin", `Food name in English`) ~ "skin",
    grepl("egg", `Food name in English`) ~ "eggs",
    grepl("cleaned parts", `Food name in English`) ~ "cleaned_parts",
    grepl("edible parts", `Food name in English`) ~ "whole",
    grepl("body tissue", `Food name in English`) ~ "muscle",
    grepl("steak", `Food name in English`) ~ "muscle",
    grepl("Caviar", `Food name in English`) ~ "eggs",
  ))  %>%
  dplyr::rename(prep_form = "Processing", sample_id = "Food Item ID", sample_info = "Food name in English", common_name = "ASFIS English name", ref_id = "BiblioID", data_processing_comments = "Comments on data processing/methods") %>%
  select(source_df, sample_id, sci_name, sample_info, common_name, sampling_location, sample_origin, prep_form, body_part, ref_id, data_processing_comments, `CA(mg)`, `FE(mg)`, `ZN(mg)`, `SE(mcg)`, `FACID(g)`, `FAPUN3(g)`, `F20D5N3(g)`, `F22D6N3(g)`, `VITA_RAE(mcg)`,`VITA-(mcg)`, `PROTCNT(g)`, `PROTCNP(g)`, `PROT-(g)`) %>%
  filter(prep_form == "r", sample_origin %in% c("Wild", NA)) %>%
  reshape2::melt(id.vars = c("source_df", "sample_id", "sci_name", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "sample_info", "ref_id", "data_processing_comments"), variable.name = "Nutrient") %>%
  filter(!is.na(value)) %>%
  mutate(value = str_remove_all(value, "\\(|\\)")) %>%
  mutate(value = str_remove_all(value, "\\[|\\]"))

anfoods_df$value <- as.numeric(anfoods_df$value)

test <- anfoods_df %>%
  filter(is.na(value))


anfoods_df <- anfoods_df %>%
  filter(value < 10000)

ggplot(anfoods_df, aes(x = body_part, y = value)) +
  geom_boxplot() + 
  facet_wrap(~Nutrient, scale = "free")

anfoods_range  <- anfoods_df %>%
  group_by(Nutrient) %>%
  summarise_at(vars(value), list(max_value = max, min_value = min))
  
  
