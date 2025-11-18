##Species map 



library(dplyr)
library(readxl)
library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(exactextractr)
library(raster)
library(leaflet)
library(webshot2)

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

##species traits -- could only include species where have all traits 
sp_all <- read.csv("data/species_list/master_sp_list_clean.csv") 

test <- sp_all %>%
  filter(nutrient_data == "Y")
test2 <- sp_all %>%
  filter(ctmax_estimate=="Y")

test3 <- sp_all %>%
  filter(nutrient_data == "Y" & ctmax_estimate == "Y") %>%
  dplyr::select(sci_name) %>%
  unique()

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
  mutate(habitat = case_when(
    Fresh == 1 & Brack == 0 & Saltwater == 0 ~ "freshwater",
    Fresh == 1 & Brack == 1 & Saltwater == 0 ~ "freshwater_brackish",
    Fresh == 0 & Brack == 1 & Saltwater == 0 ~ "brackish",
    Fresh == 0 & Brack == 1 & Saltwater == 1 ~ "brackish_salt",
    Fresh == 0 & Brack == 0 & Saltwater == 1 ~ "saltwater",
    Fresh == 1 & Brack == 1 & Saltwater == 1 ~ "freshwater_brackish_saltwater",
  )) %>%
  group_by(sci_name, habitat) %>%
  dplyr::select(sci_name, LongevityWild, Vulnerability, Length, CommonLength, FoodTroph, K_mean, habitat)


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
  ) %>%
  na.omit()

##create df that says for each species where there is nutrient data, is there CTmax data and mean temp, or just mean temp - this will color coordinate for map
ctmax_sp_list = ctmax_df %>%
  dplyr::select(sci_name) %>%
  distinct() %>%
  mutate(has_ctmax = TRUE)

mean_temp_species = mean_temp_sd %>%
  dplyr::select(sci_name) %>%
  distinct() %>%
  mutate(has_temp = TRUE)

nutrient_sp = nutrient_df %>%
 dplyr:: select(sci_name) %>%
  distinct()

nutrient_sp <- nutrient_sp %>%
  left_join(ctmax_sp_list, by = "sci_name") %>%
  left_join(mean_temp_species, by = "sci_name") %>%
  mutate(
    has_ctmax = ifelse(is.na(has_ctmax), FALSE, TRUE),
    has_temp  = ifelse(is.na(has_temp), FALSE, TRUE)
  )

nutrient_sp <- nutrient_sp %>%
  mutate(temp_data = case_when(
    has_ctmax ~ "CTmax",
    !has_ctmax & has_temp ~ "Species Range Temperature",
    TRUE ~ "No Temperature Data"
  )) %>%
  left_join(traits_sp, by = "sci_name")





##import lat/long of polygons etc 
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


# IUCN polygons (already sf, but make sure CRS is WGS84)
        # often a good idea

##get on good projection
iucn_poly_ea <- project(iucn_poly, "EPSG:4326")  

# Dissolve all polygons per species
#iucn_agg <- aggregate(iucn_poly_ea, by = "sci_name")  
# This merges all polygons of a species into one geometry per sci_name

# Get centroids of those merged polygons
iucn_centroids_ea <- centroids(iucn_agg)

# Back to WGS84 for lat/long
iucn_centroids_df <- st_as_sf(iucn_centroids_ea) %>%
  dplyr::select(sci_name, geometry) %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )


##for gbif occurrence data

occ_centroids <- all_occurrence_sf %>%
  group_by(species) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%  # union of points
  st_centroid() %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>%
  rename(sci_name = "species") 
  


occ_centroids_latlong <- occ_centroids %>%
  mutate()

##lets see if can join to nutrients data 
##join to nutrient species list 
df_final <- left_join(nutrient_sp, iucn_centroids_df, by = "sci_name") %>%
  left_join(occ_centroids, by = c("sci_name"))


df_final <- df_final %>%
  mutate(
    lon = coalesce(lon.x, lon.y),
    lat = coalesce(lat.x, lat.y)
  ) %>%
  dplyr::select(sci_name, temp_data:habitat, lon, lat)

##so 99 species of 888 do not have temp data of any kind for 


##plot out map 
df_final_2 <- df_final %>%
  filter(temp_data != "No Temperature Data")
##still have 792 species w/ nutrient data, this is pretty good .. 

sites_coord <- st_as_sf(df_final_2, coords = c("lon", "lat"), crs = 4326)
site_map <- mapview(sites_coord, map.types = "Esri.WorldTopoMap",zcol = "temp_data", legend = TRUE,  alpha = 1, alpha.regions = 1, cex = 4)
site_map
##so this are species w/ any type of temp and nutrient data 

##okay now this is the map for species w/ all traits and temp data and nutrient data -- as these are the only ones we could include in full model
df_final_3 <- df_final_2 %>%
  na.omit()
##295 species with nutrient, temperature, and all traits
sites_coord_2 <- st_as_sf(df_final_3, coords = c("lon", "lat"), crs = 4326)
my_cols <- c(
  "CTmax" = "#1b9e77",
  "Species Range Temperature" = "#7570b3"
)
site_map_2 <- mapview(sites_coord_2, map.types = "Esri.WorldTopoMap",zcol = "temp_data", col.regions = my_cols, legend = TRUE,  alpha = 1, alpha.regions = 1, cex = 4)
site_map_2

site_map_leaflet <- site_map_2@map 
site_map_leaflet

mapshot(
  site_map_leaflet,
  file   = "figures/site_map_2.png",
  vwidth = 1600,
  vheight = 1200
)

