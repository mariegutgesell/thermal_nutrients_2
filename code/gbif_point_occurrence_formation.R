##Get gbif point occurrences 

library(dplyr)
library(readxl)
library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(exactextractr)
library(ncdf4)
library(raster)

##read in sp list w/ iucn polygons and determine which species need gbif poly
iucn1 <- read.csv("data/species_list/fish_slc_ter_1_sp_list.csv")
iucn2 <- read.csv("data/species_list/fish_slc_ter_2_sp_list.csv")
iucn3 <- read.csv("data/species_list/fish_slc_ter_3_sp_list.csv")

iucn <- rbind(iucn1, iucn2, iucn3)
sp_all <- read.csv("data/species_list/master_sp_list.csv")

gbif_sp <- sp_all %>%
  filter(!sci_name %in% iucn$sci_name) %>%
  filter(nutrient_data == "Y")


##For these, use occurance data from GBIF (this is what was done in Compte & Olden) (note - don't need to re-run these lines, just need to load RDS)
sp_nopoly <- gbif_sp
library(rgbif)

# Define a list of species names
species_list <- sp_nopoly$sci_name
species_list
rm(list = ls()[!ls() %in% c("species_list")])

sp1 <- species_list[1:100]
# Create an empty list to store occurrence data for each species
occurrence_data_list_1 <- list()

# Loop through the species list and retrieve occurrence data
for (sci_name in sp1) {
occurrence_data <- occ_search( scientificName = sci_name, decimalLatitude = '-90,90', decimalLongitude = '-180,180', limit = 1000)
  occurrence_data_list_1[[sci_name]] <- occurrence_data
}
saveRDS(occurrence_data_list_1, "data/large-files/occurance_data_list_1.rds")

gc()
rm(list = ls()[!ls() %in% c("species_list")])
sp2 <- species_list[101:200]
# Create an empty list to store occurrence data for each species
occurrence_data_list_2 <- list()

# Loop through the species list and retrieve occurrence data
for (sci_name in sp2) {
  occurrence_data <- occ_search( scientificName = sci_name, decimalLatitude = '-90,90', decimalLongitude = '-180,180', limit = 1000)
  occurrence_data_list_2[[sci_name]] <- occurrence_data
}
saveRDS(occurrence_data_list_2, "data/large-files/occurance_data_list_2.rds")


gc()
rm(list = ls()[!ls() %in% c("species_list")])
sp3 <- species_list[201:300]
# Create an empty list to store occurrence data for each species
occurrence_data_list_3 <- list()

# Loop through the species list and retrieve occurrence data
for (sci_name in sp3) {
  occurrence_data <- occ_search( scientificName = sci_name, decimalLatitude = '-90,90', decimalLongitude = '-180,180', limit = 1000)
  occurrence_data_list_3[[sci_name]] <- occurrence_data
}
saveRDS(occurrence_data_list_3, "data/large-files/occurance_data_list_3.rds")

gc()
rm(list = ls()[!ls() %in% c("species_list")])
sp4 <- species_list[301:391]
# Create an empty list to store occurrence data for each species
occurrence_data_list_4 <- list()

# Loop through the species list and retrieve occurrence data
for (sci_name in sp4) {
  occurrence_data <- occ_search( scientificName = sci_name, decimalLatitude = '-90,90', decimalLongitude = '-180,180', limit = 1000)
  occurrence_data_list_4[[sci_name]] <- occurrence_data
}
saveRDS(occurrence_data_list_4, "data/large-files/occurance_data_list_4.rds")






##1) START HERE -- Extract lat/long from gbif occurrence points  -------------------
##Get sf from occurrance list
gbif1 <- readRDS("data/large-files/occurance_data_list_1.rds")
gbif2 <- readRDS("data/large-files/occurance_data_list_2.rds")
gbif3 <- readRDS("data/large-files/occurance_data_list_3.rds")
gbif4 <- readRDS("data/large-files/occurance_data_list_4.rds")

occurrence_data_list <- c(gbif1, gbif2, gbif3, gbif4)
##Extract latitude/longitude of GBIF occurrence points 
# List to store extracted data frames
extracted_data_list <- list()

# Loop through species names and extract "data" component
for (sci_name in names(occurrence_data_list)) {
  extracted_data_list[[sci_name]] <- occurrence_data_list[[sci_name]][["data"]]
}
# Columns to select from each data frame
selected_columns <- c("family", "genus", "species", "decimalLatitude", "decimalLongitude")

# Select specified columns from each data frame
selected_data_list <- lapply(extracted_data_list, function(df) {
  df[, selected_columns, drop = FALSE]
})

# Combine selected data frames
all_occurrence_df <- do.call(rbind, selected_data_list) %>%
  dplyr::rename(longitude = "decimalLongitude") %>%
  dplyr::rename(latitude = "decimalLatitude") 


write.csv(all_occurrence_df, "data/intermediate_data/all_gbif_occurrence_df.csv")

##convert these occurances to a spatial polygon
all_occurrence_sf <- st_as_sf(all_occurrence_df, coords = c("longitude", "latitude"), crs = 4326)


