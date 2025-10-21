##Join existing nutrient databases together 
##Keeping all possible data -- will later be easier to select/filter species in Great Lakes, but also broader species Lists 
##April 10, 2024
##Marie Gutgesell 

##NOTE: DEC 9 -- Look into Hicks nutrient data (in raw data file)
library(tidyverse)
library(readxl)
library(data.table)

#1) uFish Database ------------
##Import and clean uFish data
ufish_sp_list <- read_excel("data/raw_data/uFiSh1.0.xlsx", sheet = "02 Overview Species", skip = 3) %>%
  dplyr::rename(sci_name = `SCIENTIFIC NAME, AUTHOR`) %>%
  slice(1:25) ##this only keeps rows that have fish, removes any non-fish species
ufish <- read_excel("data/raw_data/uFiSh1.0.xlsx", sheet = "04 NV_sum (per 100 g EP)") %>%
  dplyr::rename(`3-ALPHA` = "3-Alpha")

##Clean up ufish species list to make suitable for joining
ufish_sp_list <- ufish_sp_list[,c(2:10)] %>%
  filter(!is.na(FAMILY)) %>% ##filters out blank rows (due to how sheet is set up)
  separate(sci_name, c("Genus", "Species"), " ") %>%
  unite(sci_name, c(Genus, Species), sep = " ", remove = FALSE) %>% ##note: this only works for the fish species, but just beware that this doesn't work for all the shellfish as there are sometimes long descriptions in the scientifc name column, come back to this if you are interesd in shellfish
  dplyr::select(`3-ALPHA`, FAMILY, Genus, sci_name, `ENGLISH NAME`) %>%
  filter(!grepl("Includes", sci_name)) ##this removes a combo of species, all of which are inverts (shrimp, octopus, clams, crabs etc. so not using)
  
ufish_sp_list$sci_name <-   gsub(",", "", ufish_sp_list$sci_name)
ufish_sp_list$sci_name <-   gsub("∆", "", ufish_sp_list$sci_name)
ufish_sp_list$FAMILY <-   gsub("∆", "", ufish_sp_list$FAMILY)

##Join species list to nutrient data, clean/extract relevant info, filter to usable samples 
ufish_df <- left_join(ufish_sp_list, ufish, by = "3-ALPHA") %>%
  dplyr::rename(sample_info = "Food name in English", prep_form = "State of food", sample_id = "Food Item ID", ref_id = "RefID") %>%
  select(sample_id, sci_name, sample_info,`Habitat`, prep_form, ref_id,  `CA(mg)`, `FE(mg)`, `ZN(mg)`, `SE(mcg)`, `FACID(g)`, `FAPUN3(g)`, `F20D5N3(g)`, `F22D6N3(g)`) %>%##what about cis version of EPA and DHA? currently not the cis one, but could add together? think about 
  mutate(sample_origin = case_when(
    grepl("W", Habitat) ~ "Wild",
    grepl("F", Habitat) ~ "Farmed",
  )) %>%
  mutate(body_part = case_when(
    grepl("fillet w/o skin", sample_info) ~ "muscle_skinless",
    grepl("dressed w/ head", sample_info) ~ "whole",
    grepl("fillet", sample_info) ~ "muscle", 
    grepl("muscle", sample_info) ~ "muscle",
    grepl("meat", sample_info) ~ "muscle",
    grepl("whole", sample_info) ~ "whole",
    grepl("flesh", sample_info) ~ "muscle",
    grepl("tail", sample_info) ~ "muscle", ##is this the right coding for a tail part?
    grepl("raw", sample_info) ~ "unknown_raw", ##just says catfishes and raw, not sure if whole or muscle
  )) %>%
  select(sample_id, sci_name, sample_info, sample_origin, prep_form, body_part, ref_id, `CA(mg)`, `FE(mg)`, `ZN(mg)`, `SE(mcg)`, `FACID(g)`, `FAPUN3(g)`, `F20D5N3(g)`, `F22D6N3(g)`) %>%
  filter(prep_form == "r", sample_origin %in% c("Wild", NA))%>%##select only raw and wild-caught samples
  mutate(source_df = "uFish", sampling_location = NA, common_name = NA, data_processing_comments = NA) %>%
  reshape2::melt(id.vars = c("source_df", "sample_id", "sci_name", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "sample_info", "ref_id", "data_processing_comments"), variable.name = "Nutrient")
 

##Lots of sample origin is NA...for now keeping NA and wild, i.e., removing ones that are specifically indicated as farmed
test1 <- ufish_df %>%
  filter(!is.na(value)) %>%
  filter(is.na(body_part))
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
  reshape2::melt(id.vars = c("source_df", "sample_id", "sci_name", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "sample_info", "ref_id", "data_processing_comments"), variable.name = "Nutrient")

##Lots of sample origin is NA...for now keeping NA and wild, i.e., removing ones that are specifically indicated as farmed
##note: where the name is NA, there is also no associated diet data, so don't worry about 

test2 <- anfoods_df %>%
  filter(!is.na(value)) %>%
  filter(is.na(body_part))
#3) FishBase Nutrient data --------------
library(rfishbase)
#devtools::install_version("duckdb", version = "1.1.3")
library(duckdb)
available_releases()

fb_tables(server = c("fishbase"), version = "latest") ##note, some packages seem to mess w/ fishbase, try restarting r session and only load fishbase if having issues

fb_nutrientdata <- fb_tbl("nutrients", version = "latest")
fb_nutrientdata_meta <- fb_tbl("nutrientssummary")

test <- fb_nutrientdata %>%
  filter(Nutrient == "Vitamin_A")
##join nutrient code to spec code
fb_df<- left_join(fb_nutrientdata, fb_nutrientdata_meta, by = "NutrientsCode") %>%
  mutate(sci_name = if_else(is.na(SpeciesUsed), 
                                   GenusUsed, 
                                   str_c(GenusUsed, SpeciesUsed, sep = " "))) %>%
  dplyr::rename(sample_id = "NutrientsCode", ref_id = "NutrientRefNo", body_part = "SampleForm", weight_type = "PrepForm") %>%
  mutate(sample_origin = "Unknown", sampling_location = NA, source_df = "FishBase", data_processing_comments = NA, common_name = NA, sample_info = NA, prep_form = NA) %>%
  filter(weight_type != "dry") %>% ##remove dry weight samples
  select(source_df, sample_id, sci_name, sample_info, common_name, sampling_location, sample_origin, prep_form, body_part, ref_id, data_processing_comments, Nutrient, Value)
  
test3 <- fb_df %>%
  filter(!is.na(Value)) %>%
  filter(is.na(body_part))



  
#4) Data from Bernhardt & O'Connor ------------
bo <- read.csv("data/raw_data/global-seafood-nutrient-dataset-raw.csv") %>%
  dplyr::rename(sci_name = "taxon_name") %>%
  filter(subgroup == "Finfish")

bo_df <- bo %>%
  mutate(prep_form = NA, source_df = "Bernhardt & O'Connor, 2019", sample_origin = NA) %>%
  dplyr::rename(sample_id = "obs_id", sampling_location = "location", ref_id = "biblio_id", data_processing_comments = "comments_on_data_processing_methods") %>%
  dplyr::select(source_df, sample_id, sci_name, sampling_location, sample_origin, prep_form, body_part, common_name, sample_info, ref_id, data_processing_comments, ca_mg:protein) %>%
  reshape2::melt(id.vars = c("source_df", "sample_id", "sci_name", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "sample_info", "ref_id", "data_processing_comments"), variable.name = "Nutrient") %>%
  filter(!is.na(value))

test4 <- bo_df %>%
  filter(!is.na(value)) %>%
  filter(is.na(body_part))

ca <- bo_df %>%
  filter(grepl("ca_mg", Nutrient))
#5) Great Lakes Nutrient Data ---------------
##Import nutrient data from lit search, cleaned and converted to proper units
glfnd <- read.csv("data/intermediate_data/Great_Lakes_Fish_Nutrient_Data_converted.csv")

##clean and filter out original units not using 
glfnd_df <- glfnd %>%
  filter(!Unit %in% c("%")) %>%
  dplyr::rename(sample_id = "fish_sample_id", Value = "value_converted", data_processing_comments = "Unit", ref_id = "pdf_id") %>%
  mutate(source_df = "GL_Lit_Search") %>%
  dplyr::select(source_df, sample_id, sci_name, sample_info, common_name, sampling_location, sample_origin, prep_form, body_part, ref_id, data_processing_comments, Nutrient, Value)

test5 <- glfnd_df %>%
  filter(!is.na(Value)) %>%
  filter(is.na(body_part))
#6) Joining all 5 databases together

##clean up environment
rm(list=setdiff(ls(), c("ufish_df", "anfoods_df", "fb_df", "bo_df", "glfnd_df")))

##make sure all df have the same columns in right order, remove any brackets from numbers, all nutrients have same naming format 
ufish_df <- ufish_df %>%
  dplyr::rename(Value = "value") %>%
  select(source_df, sample_id, sci_name, sample_info, common_name, sampling_location, sample_origin, prep_form, body_part, ref_id, data_processing_comments, Nutrient, Value) %>%
  mutate(Value = str_replace_all(Value, "\\[|\\]", ""))

anfoods_df <- anfoods_df %>%
  dplyr::rename(Value = "value") %>%
  select(source_df, sample_id, sci_name, sample_info, common_name, sampling_location, sample_origin, prep_form, body_part, ref_id, data_processing_comments, Nutrient, Value)

fb_df<- fb_df %>%
  select(source_df, sample_id, sci_name, sample_info, common_name, sampling_location, sample_origin, prep_form, body_part, ref_id, data_processing_comments, Nutrient, Value) %>%
  mutate(Nutrient = case_when(
    startsWith(Nutrient, "Iron") ~ "FE(mg)",
    startsWith(Nutrient, "Zinc") ~ "ZN(mg)",
    startsWith(Nutrient, "Calcium") ~ "CA(mg)",
    startsWith(Nutrient, "Selen") ~ "SE(mcg)",
    startsWith(Nutrient, "Omega") ~ "FAPUN3(g)",
    startsWith(Nutrient, "Prot") ~ "Protein",
    startsWith(Nutrient, "Vita") ~ "Vitamin_A"
  ))
##units found on FishBase

bo_df$Nutrient <- as.character(bo_df$Nutrient)
bo_df <- bo_df %>%
  dplyr::rename(Value = "value") %>%
  select(source_df, sample_id, sci_name, sample_info, common_name, sampling_location, sample_origin, prep_form, body_part, ref_id, data_processing_comments, Nutrient, Value) %>%
  mutate(Nutrient = case_when(
    startsWith(Nutrient, "fe_mg") ~ "FE(mg)",
    startsWith(Nutrient, "zn_mg") ~ "ZN(mg)",
    startsWith(Nutrient, "ca_mg") ~ "CA(mg)",
    startsWith(Nutrient, "prot") ~ "Protein",
    startsWith(Nutrient, "epa") ~ "EPA(g)",
    startsWith(Nutrient, "dha") ~ "DHA(g)",
    startsWith(Nutrient, "fat") ~ "FAT(mg)"
  ))

glfnd_df <- glfnd_df %>%
  select(source_df, sample_id, sci_name, sample_info, common_name, sampling_location, sample_origin, prep_form, body_part, ref_id, data_processing_comments, Nutrient, Value)
  
##Join all nutrient data together
nutrient_all <- rbind(ufish_df, anfoods_df, fb_df, bo_df, glfnd_df) 
nutrient_all$Value <- as.numeric(nutrient_all$Value)
nutrient_all$Nutrient <- as.character(nutrient_all$Nutrient)

nutrient_all_df <- nutrient_all %>%
  mutate(Nutrient_Name = case_when(
    startsWith(Nutrient, "iron") ~ "Iron (mg)",
    startsWith(Nutrient, "FE") ~ "Iron (mg)",
    startsWith(Nutrient, "zinc") ~ "Zinc (mg)",
    startsWith(Nutrient, "ZN") ~ "Zinc (mg)",
    startsWith(Nutrient, "calc") ~ "Calcium (mg)",
    startsWith(Nutrient, "CA") ~ "Calcium (mg)",
    startsWith(Nutrient, "sele") ~ "Selenium (ug)",
    startsWith(Nutrient, "SE") ~ "Selenium (ug)",
    startsWith(Nutrient, "pufa") ~ "PUFA (g)",
    startsWith(Nutrient, "FAPUN") ~ "PUFA (g)",
    startsWith(Nutrient, "mean") ~ "Total Fat (g)",
    startsWith(Nutrient, "omega") ~ "Omega 3 FA (g)",
    startsWith(Nutrient, "fa_epa") ~ "EPA (g)",
    startsWith(Nutrient, "EPA") ~ "EPA (g)",
    startsWith(Nutrient, "DHA") ~ "DHA (g)",
    startsWith(Nutrient, "F20") ~ "EPA (g)",
    startsWith(Nutrient, "F22") ~ "DHA (g)",
    startsWith(Nutrient, "fa_dha") ~ "DHA (g)",
    startsWith(Nutrient, "protein") ~ "Protein (g)",
    startsWith(Nutrient, "Protein") ~ "Protein (g)",
    startsWith(Nutrient, "FAC") ~ "Total FA (g)",
    startsWith(Nutrient, "FAT") ~ "Total Fat (g)",
    startsWith(Nutrient, "Vit") ~ "Vitamin A (ug)",
    startsWith(Nutrient, "PRO") ~ "Protein (g)",
    startsWith(Nutrient, "VIT") ~ "Vitamin (ug)",
  ))
test <- nutrient_all_df %>%
  filter(!is.na(Value)) %>%
  filter(is.na(body_part))



body_part <- nutrient_all_df %>%
  filter(!is.na(Value)) %>%
  dplyr::select(body_part) %>%
  unique()
  
##Clean up body part names 
nutrient_all_df <- nutrient_all_df %>%
  mutate(body_part = case_when(
    startsWith(body_part, "edible_portion") ~ "edible_portion",
    startsWith(body_part, "edible portion") ~ "edible_portion",
    startsWith(body_part, "cleaned") ~ "cleaned_parts",
    startsWith(body_part, "muscle (skinless") ~ "muscle_skinless",
    startsWith(body_part, "muscle_skinless") ~ "muscle_skinless",
    startsWith(body_part, "muscle_organs") ~ "muscle_organs",
    startsWith(body_part, "muscle") ~ "muscle",
    startsWith(body_part, "whole_parts") ~ "whole",
    startsWith(body_part, "whole") ~ "whole",
    startsWith(body_part, "unknown_raw") ~ "whole",
    startsWith(body_part, "eggs") ~ "eggs",
    startsWith(body_part, "unknown") ~ "unknown",
    startsWith(body_part, "liver") ~ "liver",
    startsWith(body_part, "skin") ~ "skin",
    startsWith(body_part, "oil") ~ "oil",
    startsWith(body_part, "esophagus") ~ "esophagus",
    startsWith(body_part, "viscera") ~ "viscera",
    startsWith(body_part, "NA") ~ "unknown",
))

nutrient_all_df <- nutrient_all_df %>%
  filter(!is.na(Value))

write.csv(nutrient_all_df, "data/processed_data/Nutrient_data_all.csv")

