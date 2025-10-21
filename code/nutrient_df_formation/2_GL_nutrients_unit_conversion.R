##Convert nutrient data gathered from lit to proper standard units 
##April 10, 2024
##Marie Gutgesell 

library(tidyverse)
library(reshape2)

##Read in nutrient data with standardized unit names
source("code/nutrient_df_formation/1_GL_nutrients_unit_standardization.R") 
##Standard FAO units:
##For fatty acids (PUFA, omega3, EPA, DHA, total fat) and protein = g/100g
##For 3 micronutrients (iron, calcium and zinc = mg/100g)
##selenium:  ug/100g

sample_form <- glfnd %>%
  dplyr::select(sample_form) %>%
  unique()
#1) Clean dataframe, remove unusable samples (e.g., dry weight/misc samples, cooked/processed, non-muscle samples)
glfnd <- glfnd %>%
  dplyr::rename(sample_origin = "type", prep_form = "processing",  sampling_location = "location", sample_info = "other", common_name = "common_name_english") %>%
  mutate(body_part = case_when(
    grepl("fillet with skin", sample_form) ~ "muscle",
    grepl("fillet with flesh and skin", sample_form) ~ "muscle",
    grepl("skin on fillet", sample_form) ~ "muscle",
    grepl("fillet no skin", sample_form) ~ "muscle_skinless",
    grepl("skinned fillet", sample_form) ~ "muscle_skinless",
    grepl("skinless fillet", sample_form) ~ "muscle_skinless",
    grepl("filleted, skinned", sample_form) ~ "muscle_skinless",
    grepl("Skinless fish fillets", sample_form) ~ "muscle_skinless",
    grepl("boneless, skinless fillet", sample_form) ~ "muscle_skinless",
    grepl("belly", sample_form) ~ "muscle", ##in bernhardt and o'connor they had belly flaps included in fillets as muscle
    grepl("fillet", sample_form) ~ "muscle",
    grepl("Fillet", sample_form) ~ "muscle",
    grepl("skinless tissue", sample_form) ~ "muscle_skinless",
    grepl("muscle", sample_form) ~ "muscle",
    grepl("meat", sample_form) ~ "muscle",
    grepl("flesh", sample_form) ~ "muscle",
    grepl("whole minus stomach", sample_form) ~ "muscle_organs",
    grepl("whole", sample_form) ~ "whole",
    grepl("epaxial musculature", sample_form) ~ "muscle",
    grepl("liver", sample_form) ~ "liver",
    grepl("edible portion", sample_form) ~"edible portion",
  ))

glfnd_usable <- glfnd %>%
  filter(!body_part %in% c("liver")) %>%
  filter(weight_type == "ww")  ##this also filters out pdf 20

test <- glfnd_usable %>%
  filter(is.na(body_part))
##1) Micronutrients (iron, calcium, and zinc) unit conversion - all standardize to mg/100g --------------
##for now, taking out error, but need to think about how to incorporate this and how to convert it... 
##1a) Simplifying data set to columns want to keep
test_micro <- glfnd_usable %>%
  select(fish_sample_id, scientific_name, taxonomic_level, sampling_location, sample_origin, prep_form, body_part, common_name, weight_type, sample_info, pdf_id, iron, iron_new_unit_name, zinc, zinc_new_unit_name, calcium, calcium_new_unit)

##1b) Convert to long form data -- need to do separately for each nutrient because of how data is structured, and then just joined back together
iron_long <- test_micro %>%
  select(fish_sample_id:pdf_id, iron, iron_new_unit_name) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "iron_new_unit_name"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = iron_new_unit_name) %>%
  filter(!is.na(value))

zinc_long <- test_micro %>%
  select(fish_sample_id:pdf_id, zinc, zinc_new_unit_name) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "zinc_new_unit_name"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = zinc_new_unit_name)%>%
  filter(!is.na(value))

cal_long <- test_micro %>%
  select(fish_sample_id:pdf_id, calcium, calcium_new_unit) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "calcium_new_unit"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = calcium_new_unit) %>%
  filter(!is.na(value)) %>%
  filter(value != "na") ##this row had na written in so have just removed separately

##join all long forms together
micro_long <- rbind(iron_long, zinc_long, cal_long)
##remove df not needed anymore
#rm(cal_long, sel_long, zinc_long, iron_long, test_micro)

##1c) Identify all of the types of units that need to be converted
micro_units <- micro_long %>%
  select(Unit) %>%
  distinct()

##% unit types



##1d) Convert units
##Function to convert units

micro_conversion_func <- function(x){
  mg_100g <- x %>%
    filter(Unit == "mg/100g") %>%
    mutate(value_converted = value*1)
  ug_100g <- x %>%
    filter(Unit == "ug/100g") %>%
    mutate(value_converted = value*10^-3)
  ug_g <- x %>%
    filter(Unit == "ug/g") %>%
    mutate(value_converted = value*10^-1) ##changed from x10^-5 to fix conversion 
  mg_kg <- x %>%
    filter(Unit == "mg/kg") %>%
    mutate(value_converted = value*10^-1)
  ppm <- x %>%
    filter(Unit == "ppm") %>%
    mutate(value_converted = value*10^-1)
  converted_df <- rbind(mg_100g, ug_100g, ug_g, mg_kg, ppm) %>%
    mutate(Unit_converted = "mg/100g")
}

micro_long$value <- as.numeric(micro_long$value)
##Apply conversion function
micro_long_converted <- micro_conversion_func(micro_long)
##Change scientific notation format
micro_long_converted$value_converted <- format(micro_long_converted$value_converted, scientific = F)

micro_long_converted <- micro_long_converted %>%
  select(fish_sample_id:pdf_id, Nutrient, Unit, value, Unit_converted, value_converted)

##2) Micronutrients (selenium) unit conversion -- standardize to ug/100g --------------

##2 a and b) only one micronutrient so can do combined
sel_long <- glfnd_usable %>%
  select(fish_sample_id, scientific_name, taxonomic_level, sampling_location, sample_origin, prep_form, body_part, common_name, weight_type, sample_info, pdf_id, selenium, selenium_new_unit_name) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "selenium_new_unit_name"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = selenium_new_unit_name) %>%
  filter(!is.na(value))

##2c) Identify all of the types of units that need to be converted
sel_units <- sel_long %>%
  select(Unit) %>%
  distinct()


##2d) Convert units
##Function to convert units

sel_conversion_func <- function(x){
  umol <- x %>%
    filter(Unit == "umol") %>%
    mutate(value_converted = value*78.96*10^-1) ##note in the conversion table this is umol/kg, is this the same as just umol? - yes
  ug_100g <- x %>%
    filter(Unit == "ug/100g") %>%
    mutate(value_converted = value*1)
  ug_g <- x %>%
    filter(Unit == "ug/g") %>%
    mutate(value_converted = value*10^2)
  mg_kg <- x %>%
    filter(Unit == "mg/kg") %>%
    mutate(value_converted = value*10^2)
  ppm <- x %>%
    filter(Unit == "ppm") %>%
    mutate(value_converted = value*10^2)
  ng_g <- x %>%
    filter(Unit == "ng/g") %>%
    mutate(value_converted = value/10)
  ppb <- x %>%
    filter(Unit == "ppb") %>%
    mutate(value_converted = value*10^-1)
  converted_df <- rbind(umol, ug_100g, ug_g, mg_kg, ppm, ng_g, ppb) %>%
    mutate(Unit_converted = "ug/100g")
}

sel_long$value <- as.numeric(sel_long$value)
##Apply conversion function
sel_long_converted <- sel_conversion_func(sel_long)
##Change scientific notation format
sel_long_converted$value_converted <- format(sel_long_converted$value_converted, scientific = F)

sel_long_converted <- sel_long_converted %>%
  select(fish_sample_id:pdf_id, Nutrient, Unit, value, Unit_converted, value_converted)


##3) Fat
##3a) Simpligying dataset to columns want to keep
fat_long <- glfnd_usable %>%
  select(fish_sample_id, scientific_name, taxonomic_level, sampling_location, sample_origin, prep_form, body_part, common_name, weight_type, sample_info, pdf_id, mean_total_fat_data_available, fat_new_unit) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "fat_new_unit"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = fat_new_unit) %>%
  filter(!is.na(value)) %>%
  filter(value != "na")

##3c) Identify all of the types of units that need to be converted
fat_units <- fat_long %>%
  select(Unit) %>%
  distinct()

##3d) Convert units
##Function to convert units

fat_conversion_func <- function(x){
  g_100g <- x %>%
    filter(Unit == "g/100g") %>%
    mutate(value_converted = value*1) 
  mg_100g <- x %>%
    filter(Unit == "mg/100g") %>%
    mutate(value_converted = value*10^-3)
  per <- x %>%
    filter(Unit == "%") %>%
    mutate(value_converted = value*1)
  converted_df <- rbind(g_100g, mg_100g, per) %>%
    mutate(Unit_converted = "g/100g")
}

fat_long$value <- as.numeric(fat_long$value)
##Apply conversion function
fat_long_converted <- fat_conversion_func(fat_long)
##this also removes any dry weights

##Change scientific notation format
fat_long_converted$value_converted <- format(fat_long_converted$value_converted, scientific = F)

fat_long_converted <- fat_long_converted %>%
  select(fish_sample_id:pdf_id, Nutrient, Unit, value, Unit_converted, value_converted)

##4) Fat, fatty acids, and protein ------------
##4a) Simplifying data set to columns want to keep
test_macro <- glfnd_usable %>%
  select(fish_sample_id, scientific_name, taxonomic_level, sampling_location, sample_origin, prep_form, body_part, common_name, weight_type, sample_info, pdf_id, pufa_value, pufa_new_unit, omega_3_fa_mean, omega3_new_unit, fa_epa, epa_new_unit, fa_dha, dha_new_unit, protein_data_available, protein_new_unit)

##4b) Convert to long form data -- need to do separately for each nutrient because of how data is structured, and then just joined back together
pufa_long <- test_macro %>%
  select(fish_sample_id:pdf_id, pufa_value, pufa_new_unit) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "pufa_new_unit"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = pufa_new_unit) %>%
  filter(!is.na(value))

omega3_long <- test_macro %>%
  select(fish_sample_id:pdf_id, omega_3_fa_mean, omega3_new_unit) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "omega3_new_unit"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = omega3_new_unit) %>%
  filter(!is.na(value))

epa_long <- test_macro %>%
  select(fish_sample_id:pdf_id, fa_epa, epa_new_unit) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "epa_new_unit"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = epa_new_unit) %>%
  filter(!is.na(value))

dha_long <- test_macro %>%
  select(fish_sample_id:pdf_id, fa_dha, dha_new_unit) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "dha_new_unit"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = dha_new_unit) %>%
  filter(!is.na(value))

protein_long <- test_macro %>%
  select(fish_sample_id:pdf_id, protein_data_available, protein_new_unit) %>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "protein_new_unit"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = protein_new_unit) %>%
  filter(!is.na(value))

total_fa_long <- glfnd_usable %>%
  select(fish_sample_id, scientific_name, taxonomic_level, sampling_location, sample_origin, prep_form, body_part, common_name, weight_type, sample_info, pdf_id, total_fa, total_fa_unit)%>%
  melt(id.vars = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id", "total_fa_unit"), variable.name = "Nutrient") %>%
  dplyr::rename(Unit = total_fa_unit, Total_FA_g_100g = value) %>%
  filter(!is.na(Total_FA_g_100g)) 
  


macro_long <- rbind(pufa_long, omega3_long, epa_long, dha_long, protein_long) %>%
  filter(Unit != "na") %>%
  filter(!value %in% c("na", "tr", "<0.1")) %>% ##<0.1 is not a number so can't use, and not sure what tr stands for.. 
  left_join(fat_long_converted, by = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id")) %>% ##add total fat in g/100g to be used for unit conversions where needed
  left_join(total_fa_long%>% select(fish_sample_id, scientific_name, taxonomic_level, sampling_location, sample_origin, prep_form, body_part, common_name, weight_type, sample_info, pdf_id, Total_FA_g_100g), by = c("fish_sample_id", "scientific_name", "taxonomic_level", "sampling_location", "sample_origin", "prep_form", "body_part", "common_name", "weight_type", "sample_info", "pdf_id")) %>%
   select(fish_sample_id:value.x, value_converted, Total_FA_g_100g) %>%
  dplyr::rename(Total_fat_g_100g = "value_converted") %>%
  dplyr::rename(Unit = "Unit.x") %>%
  dplyr::rename(Nutrient = "Nutrient.x") %>%
  dplyr::rename(value = "value.x") 


  
##4c) Identify all of the types of units that need to be converted
macro_units <- macro_long %>%
  select(Unit) %>%
  distinct()

##units not sure how to convert:
##(wt% of total FA) -- is this the same as %_fa? 


##which rows need total FA ?
test <- macro_long %>%
  filter(Unit %in% c("g_100g_fa", "%_fa"))
test2 <- macro_long %>%
  filter(Unit == "(wt% of total FA)")

##4d) Convert units
##Function to convert units
macro_conversion_func <- function(x){
  g_100g <- x %>%
    filter(Unit == "g/100g") %>%
    mutate(value_converted = value*1) ##note in the conversion table this is umol/kg, is this the same as just umol? 
  mg_100g <- x %>%
    filter(Unit == "mg/100g") %>%
    mutate(value_converted = value*10^-3)
  g_100g_fa <- x %>%
    filter(Unit == "g/100g_fa" & Total_FA_g_100g != "na") %>%
    mutate(value_converted = value*(10^-2)*Total_FA_g_100g) 
  g_100g_fat <- x %>%
    filter(Unit == "g/100g_fat") %>%
    mutate(value_converted = value*(10^-2)*Total_fat_g_100g)
  per_fa <- x %>%
    filter(Unit == "%_fa" & Total_FA_g_100g != "na") %>%
    mutate(value_converted = value*(10^-2)*Total_FA_g_100g) 
  ug_mg <- x %>%
    filter(Unit == "ug/mg") %>%
    mutate(value_converted = value*10^-1)
  mg_g <- x %>%
    filter(Unit == "mg/g") %>%
    mutate(value_converted = value*10^-5)
  per_fat <- x %>%
    filter(Unit == "%_fat") %>%
    mutate(value_converted = value*(10^-2)*Total_fat_g_100g)
  per <- x %>%
    filter(Unit == "%") %>%
    mutate(value_converted = value*1)
  mg_kg <- x %>%
    filter(Unit == "mg/kg") %>%
    mutate(value_converted = value*10^-4)
  converted_df <- rbind(g_100g, mg_100g, g_100g_fat, ug_mg, mg_g, per_fat, per, mg_kg) %>% ##need to add g_100g_fa and per_fa to this list once figured out 
    mutate(Unit_converted = "g/100g")
}

macro_long$value <- as.numeric(macro_long$value)
macro_long$Total_fat_g_100g <- as.numeric(macro_long$Total_fat_g_100g)
macro_long$Total_FA_g_100g <- as.numeric(macro_long$Total_FA_g_100g)
##Apply conversion function
macro_long_converted <- macro_conversion_func(macro_long)
##Change scientific notation format
macro_long_converted$value_converted <- format(macro_long_converted$value_converted, scientific = F)

macro_long_converted <- macro_long_converted %>%
  select(fish_sample_id:pdf_id, Nutrient, Unit, value, Unit_converted, value_converted)

##Join all files together to get final dataframe of all converted nutrient data -------------

glfnd_converted <- rbind(micro_long_converted, sel_long_converted, fat_long_converted, macro_long_converted) %>%
  rename(sci_name = "scientific_name") %>%
  filter(taxonomic_level == "Species") ##for now only keep nutrient data for species level (only 28/1438 observations not at species level)

write.csv(glfnd_converted, "data/intermediate_data/Great_Lakes_Fish_Nutrient_Data_converted.csv")


