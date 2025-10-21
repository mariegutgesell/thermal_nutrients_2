##species range temp extraction -- SST
##Marie Gutgesell
##Dec 4, 2024


library(dplyr)
library(readxl)
library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(exactextractr)
library(ncdf4)
library(raster)


##IUCN Polygons
iucn1 <- readRDS("data/large-files/fish_slc_ter_1.rds")
iucn2 <- readRDS("data/large-files/fish_slc_ter_2.rds")
iucn3 <- readRDS("data/large-files/fish_slc_ter_3.rds")

iucn_poly <- rbind(iucn1, iucn2, iucn3)
##GBIF Occurrence points
all_occurrence_df <- read.csv("data/intermediate_data/all_gbif_occurrence_df.csv")
##convert these occurances to a spatial polygon
all_occurrence_sf <- st_as_sf(all_occurrence_df, coords = c("longitude", "latitude"), crs = 4326)


tmp_sst_summer_southern_mean <- readRDS("data/intermediate_data/tmp_sst_summer_southern_mean.rds")
tmp_sst_summer_southern_mean <- unwrap(tmp_sst_summer_southern_mean)
tmp_sst_summer_southern_stdev <- readRDS("data/intermediate_data/tmp_sst_summer_southern_stdev.rds")
tmp_sst_summer_southern_stdev <- unwrap(tmp_sst_summer_southern_stdev)
tmp_sst_summer_southern_max <- readRDS("data/intermediate_data/tmp_sst_summer_southern_max.rds")
tmp_sst_summer_southern_max <- unwrap(tmp_sst_summer_southern_max)

tmp_sst_summer_northern_mean <- readRDS("data/intermediate_data/tmp_sst_summer_northern_mean.rds")
tmp_sst_summer_northern_mean <- unwrap(tmp_sst_summer_northern_mean)
tmp_sst_summer_northern_stdev <- readRDS("data/intermediate_data/tmp_sst_summer_northern_stdev.rds")
tmp_sst_summer_northern_stdev <- unwrap(tmp_sst_summer_northern_stdev)
tmp_sst_summer_northern_max <- readRDS("data/intermediate_data/tmp_sst_summer_northern_max.rds")
tmp_sst_summer_northern_max <- unwrap(tmp_sst_summer_northern_max)

##Join the northern and southern hemispheres back together
tmp_sst_summer_all_mean <- terra::merge(tmp_sst_summer_northern_mean, tmp_sst_summer_southern_mean)
tmp_sst_summer_all_stdev <-terra:: merge(tmp_sst_summer_northern_stdev, tmp_sst_summer_southern_stdev)
tmp_sst_summer_all_max <- terra::merge(tmp_sst_summer_northern_max, tmp_sst_summer_southern_max)



###Extraction from mean, stdev, and max temps layers for species ranges/occurrences ----------------
rm(list = ls()[!ls() %in% c("tmp_sst_summer_all_max", "iucn_poly", "all_occurrence_sf","all_occurrence_df","tmp_sst_summer_all_mean","tmp_sst_summer_all_stdev")])

## Using exactextractr for extraction (not we could have used `stars` instead)
##extracting temps in range from species that have range polygons in IUCN
iucn_poly <- sf::st_as_sf(iucn_poly)
iucn_species <- iucn_poly$sci_name

iucn_max_temps <- exactextractr::exact_extract(tmp_sst_summer_all_max, iucn_poly)
iucn_max_temps <- setNames(iucn_max_temps, iucn_species)

iucn_max_temps <- bind_rows(lapply(names(iucn_max_temps), function(iucn_species) {
  data <- iucn_max_temps[[iucn_species]]
  data$sci_name <- iucn_species
  return(data)
})) %>%
  dplyr::rename(Max_Temp_C = "value") %>%
  dplyr::select(sci_name, Max_Temp_C)

iucn_mean_temps <- exactextractr::exact_extract(tmp_sst_summer_all_mean, iucn_poly)
iucn_mean_temps <- setNames(iucn_mean_temps, iucn_species)

iucn_mean_temps <- bind_rows(lapply(names(iucn_mean_temps), function(iucn_species) {
  data <- iucn_mean_temps[[iucn_species]]
  data$sci_name <- iucn_species
  return(data)
})) %>%
  dplyr::rename(Mean_Temp_C = "value") %>%
  dplyr::select(Mean_Temp_C)

iucn_stdev_temps <- exactextractr::exact_extract(tmp_sst_summer_all_stdev, iucn_poly)
iucn_stdev_temps <- setNames(iucn_stdev_temps, iucn_species)

iucn_stdev_temps <- bind_rows(lapply(names(iucn_stdev_temps), function(iucn_species) {
  data <- iucn_stdev_temps[[iucn_species]]
  data$sci_name <- iucn_species
  return(data)
})) %>%
  dplyr::rename(Stdev_Temp = "value") %>%
  dplyr::select(Stdev_Temp)

iucn_temps <- cbind(iucn_max_temps, iucn_mean_temps, iucn_stdev_temps) %>%
  dplyr::select(sci_name, Max_Temp_C, Mean_Temp_C, Stdev_Temp) %>%
  mutate(Location_Source = "IUCN")

##quick test for NAs
test1 <- iucn_temps %>%
  filter(is.na(Mean_Temp_C))

##i think NAs for the temperatures would come because there is a part of their polygon/range that does not have marine temp associated

##extracting temps at point occurrences in GBIF for species that did not have a IUCN range polygon
gbif_points <- sf::st_as_sf(all_occurrence_sf)
gbif_max_temps <- terra::extract(tmp_sst_summer_all_max, gbif_points)
gbif_max_temps <- cbind(all_occurrence_df, gbif_max_temps)

gbif_mean_temps <- terra::extract(tmp_sst_summer_all_mean, gbif_points)
gbif_mean_temps <- cbind(all_occurrence_df, gbif_mean_temps)

gbif_stdev_temps <- terra::extract(tmp_sst_summer_all_stdev, gbif_points)
gbif_stdev_temps <- cbind(all_occurrence_df, gbif_stdev_temps)


gbif_temps <-  left_join(gbif_max_temps, gbif_mean_temps, by = c("family", "genus", "species", "latitude", "longitude", "ID")) %>%
  left_join(gbif_stdev_temps, by = c("family", "genus", "species", "latitude", "longitude", "ID"))%>%
  dplyr::rename(Family = "family") %>%
  dplyr::rename(Genus = "genus") %>%
  dplyr::rename(sci_name = "species") %>%
  dplyr::rename(Max_Temp_C = "max") %>%
  dplyr::rename(Mean_Temp_C = "mean") %>%
  dplyr::rename(Stdev_Temp = "std") %>%
  dplyr::select(sci_name,Max_Temp_C, Mean_Temp_C, Stdev_Temp) %>%
  mutate(Location_Source = "GBIF")



##Combine temperatures for ranges/occurrences and then convert from Kelvins to degrees C
sst_temp_all <- rbind(iucn_temps, gbif_temps) %>%
  mutate(Temp_Type = "SST") %>%
  filter(!is.na(Mean_Temp_C))

##Final temperature data contains the average and maximum of average summer temps (warmest quarter) for each year 1958-2001 within each species range/point occurrances (same process as Comte and Olden)
write.csv(sst_temp_all, "data/intermediate_data/hadisst_temp_all.csv")


