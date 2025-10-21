##Get gbif points and create gbif polygons 

library(tidyverse)
library(sf)
library(terra)
library(raster)
library(mapview)
library(rgbif)

##read in sp list w/ iucn polygons and determine which species need gbif poly
iucn1 <- read.csv("data/species_list/fish_slc_ter_1_sp_list.csv")
iucn2 <- read.csv("data/species_list/fish_slc_ter_2_sp_list.csv")
iucn3 <- read.csv("data/species_list/fish_slc_ter_3_sp_list.csv")

iucn <- rbind(iucn1, iucn2, iucn3)
sp_all <- read.csv("data/species_list/master_sp_list.csv")

gbif_sp <- sp_all %>%
  filter(!sci_name %in% iucn$sci_name) %>%
  filter(nutrient_data == "Y")

##run gbif functions
source("code/gbif.range/R/get_gbif.R")
source("code/gbif.range/R/get_range.R")
source("code/gbif.range/R/make_tiles.R")


gbif_sp_list <- gbif_sp$sci_name




##get gbif points 
gbif_sp_1 <- gbif_sp_list[1:50]

gbif_pts <- lapply(gbif_sp_1, get_gbif)
gbif_pts_all <- do.call(rbind, gbif_pts)
