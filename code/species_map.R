##Species map 



sp_all <- read.csv("data/species_list/master_sp_list_clean.csv") 

ctmax <- read.csv("data/processed_data/CTmax_fw_sst_est_summer_mean_sprange_ALL.csv") %>%
  dplyr::rename(sci_name = "Species_GL")
dyn <- ctmax %>%
  filter(Methodology == "dynamic")

static <- ctmax %>%
  filter(Methodology == "static") %>%
  filter(!sci_name %in% dyn$sci_name)

ctmax_df <- rbind(dyn, static)

mean_temp_sd <- read.csv("data/processed_data/All_sp_temp_mean_sd_HadISST_FW.csv") %>%
  dplyr::select(sci_name, mean_temp, sd_temp)

##2) Import all nutrient data -- with outliers removed (see Testing_Nutrient_Data_Outliers_Extremes.R)
nutrient_df <- read.csv("data/processed_data/Nutrient_data_all_outliers_removed.csv") %>%
  # filter(!body_part %in% c("liver", "egg", "esophagus", "skin", "viscera")) %>%
  filter(!grepl("spp", sci_name)) %>%
  filter(str_count(sci_name, "\\S+") == 2) %>%
  separate(sci_name, into = c("Genus", "Species"), remove = TRUE) %>%
  unite("sci_name", Genus, Species, sep = " ") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = FALSE) %>%
  unique() #%>%

##create df that says for each species where there is nutrient data, is there CTmax data and mean temp, or just mean temp - this will color coordinate for map


##all temp data: 
all_temp_data <-  read.csv("data/intermediate_data/hadisst_temp_all.csv")


##import lat/long of polygons etc 
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


##then will want to extract the mid lat/long of each range or from gbif occurance data 