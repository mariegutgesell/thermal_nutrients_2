##Density plots of nutrient values and CTmax 

library(tidyverse)
library(ggplot2)

##1) Import the CTmax data estimated for acclimation for species specific ranges (see Allsp_NLMEM_CTmax_predicitons.R code for how these predicted CTmax were generated)
ctmax <- read.csv("data/processed_data/CTmax_fw_sst_est_summer_mean_sprange_ALL.csv") %>%
  dplyr::rename(sci_name = "Species_GL")

##Need to select only 1 methodology for CTmax estimates - select dynamic if present
dyn <- ctmax %>%
  filter(Methodology == "dynamic")

static <- ctmax %>%
  filter(Methodology == "static") %>%
  filter(!sci_name %in% dyn$sci_name)

ctmax_df <- rbind(dyn, static) %>%
  select(sci_name, TL_p_fw_mean)

head(ctmax_df)
##2) Import all nutrient data -- with outliers removed (see Testing_Nutrient_Data_Outliers_Extremes.R)
nutrient_df <- read.csv("data/processed_data/Nutrient_data_all_outliers_removed.csv") %>%
  # filter(!body_part %in% c("liver", "egg", "esophagus", "skin", "viscera")) %>%
  filter(!grepl("spp", sci_name)) %>%
  filter(str_count(sci_name, "\\S+") == 2) %>%
  separate(sci_name, into = c("Genus", "Species"), remove = TRUE) %>%
  unite("sci_name", Genus, Species, sep = " ") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = FALSE) %>%
  unique() %>%
  select(sci_name, body_part_2, Nutrient_Name, Value)
nutrient_df$Value <- as.numeric(nutrient_df$Value)

test <- nutrient_df %>%
  dplyr::select(body_part_2) %>%
  distinct()

head(nutrient_df)
##3) Import species list 
sp_all <- read.csv("data/species_list/master_sp_list_clean.csv") 



##convert 3 columns (fresh, brackish and salt) into a single column factor
##1 = fresh, 1.5 = fresh+brackish, 2 = brackish, 2.5 = brackish+salt, 3 = salt, 4 = fresh+brackish+salt 
sp_all_traits <- sp_all %>%
  mutate(habitat_score = case_when(
    Fresh == 1 & Brack == 0 & Saltwater == 0 ~ 1,
    Fresh == 1 & Brack == 1 & Saltwater == 0 ~ 1.5,
    Fresh == 0 & Brack == 1 & Saltwater == 0 ~ 2,
    Fresh == 0 & Brack == 1 & Saltwater == 1 ~ 2.5,
    Fresh == 0 & Brack == 0 & Saltwater == 1 ~ 3,
    Fresh == 1 & Brack == 1 & Saltwater == 1 ~ 4,
    TRUE ~ NA_real_
  )) %>%
  mutate(habitat_specific = case_when(
    Fresh == 1 & Brack == 0 & Saltwater == 0 ~ "freshwater",
    Fresh == 1 & Brack == 1 & Saltwater == 0 ~ "freshwater_brackish",
    Fresh == 0 & Brack == 1 & Saltwater == 0 ~ "brackish",
    Fresh == 0 & Brack == 1 & Saltwater == 1 ~ "brackish_salt",
    Fresh == 0 & Brack == 0 & Saltwater == 1 ~ "saltwater",
    Fresh == 1 & Brack == 1 & Saltwater == 1 ~ "freshwater_brackish_saltwater",
  )) %>%
  mutate(habitat = case_when(
    Fresh == 1 & Brack == 0 & Saltwater == 0 ~ "freshwater",
    Fresh == 1 & Brack == 1 & Saltwater == 0 ~ "freshwater",
    Fresh == 0 & Brack == 1 & Saltwater == 0 ~ "saltwater",
    Fresh == 0 & Brack == 1 & Saltwater == 1 ~ "saltwater",
    Fresh == 0 & Brack == 0 & Saltwater == 1 ~ "saltwater",
    Fresh == 1 & Brack == 1 & Saltwater == 1 ~ "freshwater_brackish_saltwater",
  )) %>%
  group_by(sci_name, habitat) %>%
  select(sci_name, LongevityWild, Vulnerability, Length, CommonLength, FoodTroph, K_mean, habitat)

head(sp_all_traits)



##Calculate means for traits 
traits_sp <- sp_all_traits %>%
  group_by(sci_name) %>%
  summarise(
    LongevityWild = mean(LongevityWild, na.rm = TRUE),
    # Vulnerability = mean(Vulnerability, na.rm = TRUE),
    Length = mean(Length, na.rm = TRUE),
    #  CommonLength = mean(CommonLength, na.rm = TRUE),
    FoodTroph = mean(FoodTroph, na.rm = TRUE),
    K_mean = mean(K_mean, na.rm = TRUE),
    habitat = first(habitat)   # if categorical
  )



##create dataframe of CTmax and traits 
ctmax_trait_df <- ctmax_df %>%
  left_join(traits_sp, by = "sci_name") %>%
  filter(sci_name %in% nutrient_df$sci_name)

nutrient_trait_df <- nutrient_df %>%
  left_join(traits_sp, by = "sci_name") 


##test out plotting 
ctmax_plot <- ctmax_trait_df %>%
  filter(!is.na(habitat)) %>% 
  ggplot(aes(x = TL_p_fw_mean, group = habitat, fill = habitat)) +
  geom_density(alpha = 0.6) +
  theme_classic() +
  labs(x = "CTmax (˚C)")
ctmax_plot


nutrient_plot <- nutrient_trait_df %>%
  filter(!is.na(habitat)) %>% 
  filter(Nutrient_Name %in% c("Calcium (mg)", "DHA (g)", "EPA (g)", "Iron (mg)", "Protein (g)", "Selenium (ug)", "Total Fat (g)", "Vitamin A (ug)", "Zinc (mg)")) %>%
  ggplot(aes(x = log(Value), group = habitat, fill = habitat)) +
  geom_density(alpha = 0.6) +
  theme_classic() +
  facet_wrap(~Nutrient_Name, scale = "free", nrow = 9, ncol = 1)
nutrient_plot


