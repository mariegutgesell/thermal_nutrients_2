###Get IUCN Polygons for any species we have

##October 24, 2024
##Marie Gutgesell

library(dplyr)
library(readxl)
library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(exactextractr)
library(ncdf4)
library(raster)

#1) Extracting IUCN range polygons and GBIF point occurrences for All Fish species w/ ctmax and nutrient overlap ----------------
#https://www.iucnredlist.org/resources/spatial-data-download
##IUCN Fish Range polygons (with RDS don't need to run this again)
setwd("/Volumes/LACIE SHARE/Thermal_Nutrients_Repo/")
fish1 <- rbind(
  st_read("large-files/FW_FISH/FW_FISH_PART1.shp"),
  st_read("large-files/FW_FISH/FW_FISH_PART2.shp")
)


##read in great lakes species list 
setwd("~/Desktop/thermal_nutrients_2/")
sp_all <- read.csv("data/species_list/master_sp_list.csv")

##Extract range polygons from IUCN shape files for species list
## switching off spherical geometry (not a big deal for what we are doing here)
## below we create one polygon per species takes 2min
sf_use_s2(FALSE)
fish_slc_1 <- fish1 |>
  dplyr::filter(sci_name %in% sp_all$sci_name) |>
  group_by_at(all_of(names(fish1)[c(1:2, 17:26)])) |>
  summarise(
    geometry = st_union(geometry),
    n_sources = n()
  ) 

##
fish_slc_ter_1 <- terra::vect(fish_slc_1) |>
  project("epsg:4326")

setwd("~/Desktop/thermal_nutrients_2/")
saveRDS(fish_slc_ter_1, "data/large-files/fish_slc_ter_1.rds")

##make df of species in first group
attributes_df_1 <- as.data.frame(fish_slc_ter_1)
sci_name_df_1 <- attributes_df_1["sci_name"]
write.csv(sci_name_df_1, "data/species_list/fish_slc_ter_1_sp_list.csv")

##2nd group of fish -------------------
rm(list = ls())
gc()

setwd("~/Desktop/thermal_nutrients_2/")
sp_all <- read.csv("data/species_list/master_sp_list.csv")
batch1 <- read.csv("data/species_list/fish_slc_ter_1_sp_list.csv")
sp_2 <- sp_all %>%
  filter(!sci_name %in% batch1$sci_name)

setwd("/Volumes/LACIE SHARE/Thermal_Nutrients_Repo/")
fish2 <- rbind(
  st_read("large-files/MARINEFISH/MARINEFISH_PART1.shp"),
  st_read("large-files/MARINEFISH/MARINEFISH_PART2.shp"),
  st_read("large-files/MARINEFISH/MARINEFISH_PART3.shp")
)

validity_check <- fish2 |> 
  dplyr::mutate(valid_geom = sf::st_is_valid(geometry))

fish_slc_2 <- fish2 |>
  dplyr::filter(sci_name %in% sp_2$sci_name) |>
  group_by_at(all_of(names(fish2)[c(1:2, 17:26)])) |>
  dplyr::filter(sf::st_is_valid(geometry)) |> 
  summarise(
    geometry = st_union(geometry),
    n_sources = n()
  ) 

##
fish_slc_ter_2 <- terra::vect(fish_slc_2) |>
  project("epsg:4326")

setwd("~/Desktop/thermal_nutrients_2/")
saveRDS(fish_slc_ter_2, "data/large-files/fish_slc_ter_2.rds")

##make df of species in first group
attributes_df_2 <- as.data.frame(fish_slc_ter_2)
sci_name_df_2 <- attributes_df_2["sci_name"]
write.csv(sci_name_df_2, "data/species_list/fish_slc_ter_2_sp_list.csv")

##3rd group of fish -----------------------------
rm(list = ls())
gc()

setwd("~/Desktop/thermal_nutrients_2/")
sp_all <- read.csv("data/species_list/master_sp_list.csv")
batch1 <- read.csv("data/species_list/fish_slc_ter_1_sp_list.csv")
batch2 <- read.csv("data/species_list/fish_slc_ter_2_sp_list.csv")

sp_3 <- sp_all %>%
  filter(!sci_name %in% batch1$sci_name) %>%
  filter(!sci_name %in% batch2$sci_name) 

setwd("/Volumes/LACIE SHARE/Thermal_Nutrients_Repo/")
fish3 <- rbind(
  st_read("large-files/MARINEFISH/MARINEFISH_PART4.shp"),
  st_read("large-files/MARINEFISH/MARINEFISH_PART5.shp"),
  st_read("large-files/MARINEFISH/MARINEFISH_PART6.shp")
)


fish_slc_3 <- fish3 |>
  dplyr::filter(sci_name %in% sp_3$sci_name) |>
  group_by_at(all_of(names(fish3)[c(1:2, 17:26)])) |>
  dplyr::filter(sf::st_is_valid(geometry)) |> 
  summarise(
    geometry = st_union(geometry),
    n_sources = n()
  ) 

##
fish_slc_ter_3 <- terra::vect(fish_slc_3) |>
  project("epsg:4326")
#setwd("/Volumes/LACIE SHARE/Thermal_Nutrients_Repo/")
setwd("~/Desktop/thermal_nutrients_2/")
saveRDS(fish_slc_ter_3, "data/large-files/fish_slc_ter_3.rds")

##make df of species in first group
attributes_df_3 <- as.data.frame(fish_slc_ter_3)
sci_name_df_3 <- attributes_df_3["sci_name"]
write.csv(sci_name_df_3, "data/species_list/fish_slc_ter_3_sp_list.csv")


###Examining certain species ranges -----------------
iucn1 <- readRDS("data/large-files/fish_slc_ter_1.rds")
iucn2 <- readRDS("data/large-files/fish_slc_ter_2.rds")
iucn3 <- readRDS("data/large-files/fish_slc_ter_3.rds")

iucn_poly <- rbind(iucn1, iucn2, iucn3)

iucn_poly_sf <- sf::st_as_sf(iucn_poly)
iucn_poly_sf%>%
  filter(sci_name == "Coregonus lavaretus") |> mapview()

iucn_poly_sf%>%
  filter(sci_name == "Rasbora tawarensis") |> mapview()

##species w/ no SD for temps...
#Coregonus lavaretus -- range is only in 1 small lake in france, so would likely only have 1 temp point (only in one lat/long grid space) -- considered endemic to this lake
#Rasbora tawarensis -- range is only 1 small lake in indonesia -- endemic to 1 lake 

iucn_poly_sf%>%
  filter(sci_name == "Couesius plumbeus") |> mapview()

