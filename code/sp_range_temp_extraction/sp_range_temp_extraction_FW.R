##species range temp extraction -- FW

library(dplyr)
library(readxl)
library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(exactextractr)
library(ncdf4)
library(raster)

##read in gbif and iucn polygons
##extract FW temperature from summer means 


## do same for the SST in separate script
##then in another script, make df of  mean range temps, and then 30 temp selection from mean/sd 

##IUCN Polygons
iucn1 <- readRDS("data/large-files/fish_slc_ter_1.rds")
iucn2 <- readRDS("data/large-files/fish_slc_ter_2.rds")
iucn3 <- readRDS("data/large-files/fish_slc_ter_3.rds")

fish_slc_ter_all <- rbind(iucn1, iucn2, iucn3)
##GBIF Occurrence points
all_occurrence_df <- read.csv("data/intermediate_data/all_gbif_occurrence_df.csv")
##convert these occurances to a spatial polygon
all_occurrence_sf <- st_as_sf(all_occurrence_df, coords = c("longitude", "latitude"), crs = 4326)



##Importing mean summer temperatures
tmp_fw_summer_southern_mean <- readRDS("data/intermediate_data/tmp_fw_summer_southern_mean.rds")
tmp_fw_summer_southern_mean <- unwrap(tmp_fw_summer_southern_mean)
tmp_fw_summer_southern_stdev <- readRDS("data/intermediate_data/tmp_fw_summer_southern_stdev.rds")
tmp_fw_summer_southern_stdev <- unwrap(tmp_fw_summer_southern_stdev)
tmp_fw_summer_southern_max <- readRDS("data/intermediate_data/tmp_fw_summer_southern_max.rds")
tmp_fw_summer_southern_max <- unwrap(tmp_fw_summer_southern_max)

tmp_fw_summer_northern_mean <- readRDS("data/intermediate_data/tmp_fw_summer_northern_mean.rds")
tmp_fw_summer_northern_mean <- unwrap(tmp_fw_summer_northern_mean)
tmp_fw_summer_northern_stdev <- readRDS("data/intermediate_data/tmp_fw_summer_northern_stdev.rds")
tmp_fw_summer_northern_stdev <- unwrap(tmp_fw_summer_northern_stdev)
tmp_fw_summer_northern_max <- readRDS("data/intermediate_data/tmp_fw_summer_northern_max.rds")
tmp_fw_summer_northern_max <- unwrap(tmp_fw_summer_northern_max)

##Join the northern and southern hemispheres back together
tmp_fw_summer_all_mean <- terra::merge(tmp_fw_summer_northern_mean, tmp_fw_summer_southern_mean)
tmp_fw_summer_all_stdev <- merge(tmp_fw_summer_northern_stdev, tmp_fw_summer_southern_stdev)
tmp_fw_summer_all_max <- merge(tmp_fw_summer_northern_max, tmp_fw_summer_southern_max)


###Extraction from mean, stdev, and max temps layers for species ranges/occurrences ----------------
## Using exactextractr for extraction (not we could have used `stars` instead)
##extracting temps in range from species that have range polygons in IUCN
iucn_poly <- sf::st_as_sf(fish_slc_ter_all)
iucn_species <- iucn_poly$sci_name

iucn_max_temps <- exactextractr::exact_extract(tmp_fw_summer_all_max, iucn_poly)
iucn_max_temps <- setNames(iucn_max_temps, iucn_species)

iucn_max_temps <- bind_rows(lapply(names(iucn_max_temps), function(iucn_species) {
  data <- iucn_max_temps[[iucn_species]]
  data$sci_name <- iucn_species
  return(data)
})) %>%
  dplyr::rename(Max_Temp_K = "value") %>%
  dplyr::select(sci_name, Max_Temp_K)

iucn_mean_temps <- exactextractr::exact_extract(tmp_fw_summer_all_mean, iucn_poly)
iucn_mean_temps <- setNames(iucn_mean_temps, iucn_species)

iucn_mean_temps <- bind_rows(lapply(names(iucn_mean_temps), function(iucn_species) {
  data <- iucn_mean_temps[[iucn_species]]
  data$sci_name <- iucn_species
  return(data)
})) %>%
  dplyr::rename(Mean_Temp_K = "value") %>%
  dplyr::select(Mean_Temp_K)

iucn_stdev_temps <- exactextractr::exact_extract(tmp_fw_summer_all_stdev, iucn_poly)
iucn_stdev_temps <- setNames(iucn_stdev_temps, iucn_species)

iucn_stdev_temps <- bind_rows(lapply(names(iucn_stdev_temps), function(iucn_species) {
  data <- iucn_stdev_temps[[iucn_species]]
  data$sci_name <- iucn_species
  return(data)
})) %>%
  dplyr::rename(Stdev_Temp = "value") %>%
  dplyr::select(Stdev_Temp)

iucn_temps <-  cbind(iucn_max_temps, iucn_mean_temps, iucn_stdev_temps) %>%
 # dplyr::select(sci_name, Max_Temp_K, Mean_Temp_K, Stdev_Temp) %>%
  mutate(Location_Source = "IUCN") 

##quick test for NAs
test1 <- iucn_temps %>%
  filter(is.na(Mean_Temp_K))
##i think NAs for the temperatures would come because there is a part of their polygon/range that does not have freshwater temp associated
rm(list = ls()[!ls() %in% c("iucn_temps", "all_occurrence_sf", "tmp_fw_summer_all_max", "tmp_fw_summer_all_mean", "tmp_fw_summer_all_stdev")])
##extracting temps at point occurrences in GBIF for species that did not have a IUCN range polygon
gbif_points <- sf::st_as_sf(all_occurrence_sf)
gbif_max_temps <- terra::extract(tmp_fw_summer_all_max, gbif_points)
gbif_max_temps <- cbind(all_occurrence_df, gbif_max_temps)

gbif_mean_temps <- terra::extract(tmp_fw_summer_all_mean, gbif_points)
gbif_mean_temps <- cbind(all_occurrence_df, gbif_mean_temps)

gbif_stdev_temps <- terra::extract(tmp_fw_summer_all_stdev, gbif_points)
gbif_stdev_temps <- cbind(all_occurrence_df, gbif_stdev_temps)


gbif_temps <-  left_join(gbif_max_temps, gbif_mean_temps, by = c("family", "genus", "species", "latitude", "longitude", "ID")) %>%
  left_join(gbif_stdev_temps, by = c("family", "genus", "species", "latitude", "longitude", "ID"))%>%
  dplyr::rename(Family = "family") %>%
  dplyr::rename(Genus = "genus") %>%
  dplyr::rename(sci_name = "species") %>%
  dplyr::rename(Max_Temp_K = "max") %>%
  dplyr::rename(Mean_Temp_K = "mean") %>%
  dplyr::rename(Stdev_Temp = "std") %>%
  dplyr::select( sci_name, Max_Temp_K, Mean_Temp_K, Stdev_Temp) %>%
  mutate(Location_Source = "GBIF")



##Combine temperatures for ranges/occurrences and then convert from Kelvins to degrees C
fw_temp_all <- rbind(iucn_temps, gbif_temps) %>%
  mutate(Max_Temp_C = Max_Temp_K - 273.15) %>%
  mutate(Mean_Temp_C = Mean_Temp_K - 273.15)  %>%
  filter(!is.na(Mean_Temp_C)) %>%
  mutate(Temp_Type = "FW")


##Final temperature data contains the average and maximum of average summer temps (warmest quarter) for each year 1958-2001 within each species range/point occurrances (same process as Comte and Olden)
write.csv(fw_temp_all, "data/intermediate_data/fw_temp_all.csv")
##

