##Great Lakes Species List Dataframe formation and name cleaning
##April 10, 2024 -- modified Dec 2, 2024
##Marie Gutgesell

library(tidyverse)
library(readxl)
library(taxize)
library(rfishbase)

#1A) Import and clean names using taxize of Great Lakes commercial species list --- note: skip these steps and just read in cleaned names if don't want to rerun taxize ------------
##Import list of commercial Great Lakes Species
####source: Great Lakes Fishery Commission: http://www.glfc.org/great-lakes-databases.php
#gl_sp_comm <- read.csv("data/Great_Lakes_sp/species_list/GL_Fish_Species_Commercial.csv") %>%
#  dplyr::rename(Species = "Species.Name")

##obtain family using taxize
#gl_commercial_famlist <- gl_sp_comm %>%
#  separate(Species, into = c("Genus", "Species")) %>%
#  select(-Species) %>%
#  left_join(gl_sp_comm, by = "Common.Name") 

#gl_commercial_fams <- tax_name(gl_commercial_famlist$Genus, get = "family", db = "itis") 

##join family back to original species list
#gl_sp_comm <- cbind(gl_commercial_famlist, gl_commercial_fams) %>%
#  select(-db, -query)
#write.csv(gl_sp_comm, "data/species_list/Great_Lakes_sp/GL_Fish_Species_Commercial_clean.csv")
  
#1B) Read in Great Lakes commercial species list with cleaned names (start here if don't want to re-run taxize) ---------
gl_sp_comm <- read.csv("data/species_list/Great_Lakes_sp/GL_Fish_Species_Commercial_clean.csv") %>%
  dplyr::rename(sci_name = "Species")

#2) Read in list of all Great Lakes fish species --------
gl_sp_all <- read_excel("data/species_list/Great_Lakes_sp/GL_Fish_Species_All.xlsx")  |>
  dplyr::select(Class, Family, Species) |>
  dplyr::rename(sci_name = "Species") |>
  separate(sci_name, c("Genus", "Species"), sep = " ", remove = FALSE) %>%
  dplyr::select(Family, Genus, sci_name) |>
  mutate(Family = gsub("�", "", Family)) 


#3) Create final species list dataframe -- indicating which species are commercial/non-commercial --------
##For commercial species where they are only identified to genus, get species names for each of those species in that genus/family that are present in the great lakes
gl_sp_comm_sp <- gl_sp_comm %>%
  filter(!grepl( "spp.", sci_name)) %>%
  filter(!grepl("Cyprinidae", sci_name)) %>%
  dplyr::select(family, Genus, sci_name) %>%
  dplyr::rename(Family = "family")

gl_sp_comm_genus <- gl_sp_comm %>%
  filter(grepl( "spp.", sci_name))

gl_sp_genus <- gl_sp_all %>%
  filter(Genus %in% gl_sp_comm_genus$Genus) %>%
  dplyr::select(Family, Genus, sci_name)

gl_sp_cyp <- gl_sp_all %>%
  filter(Family == "Cyprinidae") %>%
  dplyr::select(Family, Genus, sci_name)

##bind back together and indicate which species in great lakes are commercial or not 
gl_sp_comm <- rbind(gl_sp_comm_sp, gl_sp_genus, gl_sp_cyp) %>%
  unique() %>%
  mutate(Fish_Type = "Commercial")

gl_sp_all <- left_join(gl_sp_all, gl_sp_comm, by = c("Family", "Genus", "sci_name")) %>%
  mutate(Fish_Type = ifelse(is.na(Fish_Type), "Non_Commercial", Fish_Type))

#4) Save species list for use in subsequent analyses --------------------
write.csv(gl_sp_all, "data/species_list/Great_Lakes_sp/GL_species_list_final.csv")
