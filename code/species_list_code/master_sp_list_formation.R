##make master species list 

##Get species list of any fish we have nutrient data for 

library(tidyverse)
library(readxl)
library(taxize)

##read in nutrient data, cleaned, outliers removed
nutrients_sp <- read.csv("data/processed_data/Nutrient_data_all_outliers_removed.csv") %>%
  dplyr::select(sci_name) %>%
  unique() %>%
  filter(!grepl("spp", sci_name)) %>%
  filter(str_count(sci_name, "\\S+") == 2) %>%
  mutate(nutrient_data = "Y") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = TRUE) %>%
  unite("sci_name", Genus, Species, sep = " ") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = FALSE) %>%
  unique()
  
##will need to sort out hte genus etc. 

##read in GL species list (want to indicate if GL species or not)
gl_sp_list <- read.csv("data/species_list/Great_Lakes_sp/GL_species_list_final.csv") %>%
  dplyr::select(Family:Fish_Type) %>%
  mutate(gl_fish = "Y") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = TRUE) %>%
  unite("sci_name", Genus, Species, sep = " ") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = FALSE) %>%
  unique()
  


##read in compte & olden data and get species list 
co_sp_list <- read_excel("data/raw_data/Comte_Olden_CTmax_Data.xlsx", sheet = "Original_data") %>%
  dplyr::select(Species) %>%
  unique() %>%
  mutate(ctmax_estimate = "Y") %>%
  rename(sci_name = "Species") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = TRUE) %>%
  unite("sci_name", Genus, Species, sep = " ") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = FALSE) %>%
  unique()
  


sp_all <- full_join(nutrients_sp, co_sp_list, by = c("Genus", "Species", "sci_name")) %>%
  full_join(gl_sp_list, by = c("Genus", "Species", "sci_name")) %>%
  dplyr::select(Genus, Species, sci_name, nutrient_data, ctmax_estimate, gl_fish) %>%
  unique()

##the extra pieces are when the second species part was repeated -- so i think not an issue 

###Get family and genus for each species
fams <- sp_all %>%
  select(Genus) %>%
  unique()

#sp_fams <- tax_name(fams$Genus, get = "family", db = "itis") 

#sp_fams_2 <- sp_fams %>%
#  rename(Genus2= "Family") %>%
#  rename(Family = "Genus" ) %>%
#  rename(Genus = "Genus2")
#sp_all <- left_join(sp_all, sp_fams_2, by = "Genus")

sp_fams <- read.csv("data/species_list/master_sp_list.csv") %>%
  dplyr::select(Family, Genus, Species) %>%
  unique()



sp_all_2 <- left_join(sp_all, sp_fams, by = c("Genus", "Species")) %>%
  dplyr::select(Family, Genus, Species, sci_name, nutrient_data:gl_fish)



##to fix:
#Chelinus undulates ##fix this one
#Brama

##Get trait data for each species 
library(rfishbase)
library(duckdb)
##Import tables of fish trait data
available_releases()
fb_tables(server = c("fishbase"), version = "latest") ##note, some packages seem to mess w/ fishbase, try restarting r session and only load fishbase if having issues

##Extract Fish Morphometric data
fb_morph <- fb_tbl("species") %>%
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name %in% sp_all_2$sci_name) %>%
  dplyr::select(SpecCode, Genus, Species, sci_name, SpeciesRefNo, FBname, Fresh,Brack, Saltwater, LongevityWild, Vulnerability, Length, LTypeMaxM, CommonLength, LTypeComM, Weight, Importance)
##has morph data for 176/178 GL species -- fb missing: Moxostoma duquesnii, Notropis buccatus
missing_sp_morph <- anti_join(gl_sp_all, fb_morph, by = c("sci_name"))

##Extract trophic data 
gl_tp <- ecology(species_list = sp_all_2$sci_name, fields = NULL,server = getOption("FISHBASE_API", "fishbase")) %>%
  dplyr::select(SpecCode, Species, DietTroph, DietSeTroph, DietRemark, FoodTroph, FoodSeTroph, FoodRemark) %>%
  dplyr::rename(sci_name = "Species")

sp_all_3 <- left_join(sp_all_2, fb_morph, by = c("Genus", "Species", "sci_name")) %>%
  left_join(gl_tp, by = "sci_name")


write.csv(sp_all_3, "data/species_list/master_sp_list_clean.csv")

##need to check some duplicates for the trophic position stuff
