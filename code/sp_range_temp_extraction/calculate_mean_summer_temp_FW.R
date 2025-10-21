##Calculate mean summer temp -- FW 

library(dplyr)
library(readxl)
library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(exactextractr)
library(ncdf4)
library(raster)

#2) Freshwater Temperature Data -----------------
##each layer is a day, and it shows the simulated temperature across each 0.5x0.5 lat/lon grid 
##coverage fraction tells you how much of a square in the raster grid is covered (so takes data still even if range doesn't cover all of raster grid)
##so 44 years because has all of 2001, so essentially 2002 (2001 is included), which is 16060, and then 11 leap years? would that make sense? that would be the 16071 columns... and then each row is a pixel in the range

rm(list = ls()[!ls() %in% c("all_occurrence_sf", "fish_slc_ter_all")])
##1) Read in file of freshwater water temperature 
setwd("/Volumes/LACIE SHARE/Thermal_Nutrients_Repo/")
filenames <- file.path(
  "large-files",
  paste0("watertemperature_wfd_historical_1958-2001", ".nc")
)

tmp_fw <- terra::rast(filenames) #|>
#  project("epsg:4326") ##not sure if this is necessary
print(tmp_fw)

##2) Split raster into southern and northern hemisphere
# Define extent for the southern hemisphere (-90 to 0 degrees latitude)
extent_southern <- extent(-180, 180, -90, 0)

# Define extent for the northern hemisphere (0 to 90 degrees latitude)
extent_northern <- extent(-180, 180, 0, 90)

# Crop the raster for the southern hemisphere
setwd("~/Desktop/thermal_nutrients_2/")
#tmp_fw_southern <- crop(tmp_fw, extent_southern)
#saveRDS(tmp_fw_southern, "data/intermediate_files/tmp_fw_southern.rds")

tmp_fw_southern<- readRDS("data/intermediate_files/tmp_fw_southern.rds")
tmp_fw_southern
# Crop the raster for the northern hemisphere

#tmp_fw_northern <- crop(tmp_fw, extent_northern)
#saveRDS(tmp_fw_northern, "data/intermediate_files/tmp_fw_northern.rds")
tmp_fw_northern<- readRDS("data/intermediate_files/tmp_fw_northern.rds")
tmp_fw_northern

##3) Calculate average of warmest quarter 
##Northern Hemisphere 
##Extract layers for June, July, August, and then calculate yearly average of summer mean temps 
years <- terra::time(tmp_fw_northern) |> format("%Y") |> unique()

# Create an empty list to store filtered layers
filtered_layers <- list()

# Loop through each year
for (i in 1:length(years)) {
  # Extract the year
  year <- years[i]
  
  # Define the start and end dates for June 1 and August 31 of the current year
  start_date <- paste0(year, "-06-01")
  end_date <- paste0(year, "-08-31")
  
  # Subset the raster stack for the current year and the specified time range
  subset_layer <- subset(tmp_fw_northern, time(tmp_fw_northern) >= as.Date(start_date) & time(tmp_fw_northern) < as.Date(end_date))
  
  ##Calculate mean temp across the summer months 
  subset_layer_mean <- mean(subset_layer)
  
  # Store the subset layer in the list
  filtered_layers[[i]] <- subset_layer_mean
}

# Combine the filtered layers into a single SpatRaster object
tmp_fw_summer_northern <- rast(filtered_layers)

##Then average across all 44 years .
#tmp_fw_summer_northern_mean <- mean(tmp_fw_summer_northern)
#saveRDS(tmp_fw_summer_northern_mean, "data/intermediate_files/tmp_fw_summer_northern_mean.rds")

#tmp_fw_summer_northern_stdev <- stdev(tmp_fw_summer_northern)
#saveRDS(tmp_fw_summer_northern_stdev, "data/intermediate_files/tmp_fw_summer_northern_stdev.rds")

#tmp_fw_summer_northern_max <- max(tmp_fw_summer_northern)
#saveRDS(tmp_fw_summer_northern_max, "data/intermediate_files/tmp_fw_summer_northern_max.rds")

##Southern Hemisphere 
##Extract layers for December, January, February and then calculate yearly average of summer mean temps 
years <- terra::time(tmp_fw_southern) |> format("%Y") |> unique()

# Create an empty list to store filtered layers
filtered_layers_2 <- list()

# Loop through each year -- december
for (i in 1:length(years)) {
  # Extract the year
  year <- years[i]
  
  # Define the start and end dates for December of the current year
  start_date_1 <- paste0(year, "-12-01")
  end_date_1 <- paste0(year, "-12-31")
  
  # Subset the raster stack for the current year and the specified time range
  subset_layer_1 <- subset(tmp_fw_southern, time(tmp_fw_southern) >= as.Date(start_date_1) & time(tmp_fw_southern) < as.Date(end_date_1))
  
  # Define the start and end dates for January and February of the current year
  start_date_2 <- paste0(year, "-01-01")
  end_date_2 <- paste0(year, "-03-01")
  
  # Subset the raster stack for the current year and the specified time range
  subset_layer_2 <- subset(tmp_fw_southern, time(tmp_fw_southern) >= as.Date(start_date_2) & time(tmp_fw_southern) < as.Date(end_date_2))
  
  subset_layer <- merge(subset_layer_1, subset_layer_2)
  
  ##Calculate mean temp across the summer months 
  subset_layer_mean <- mean(subset_layer)
  
  # Store the subset layer in the list
  filtered_layers_2[[i]] <- subset_layer_mean
}


# Combine the filtered layers into a single SpatRaster object
tmp_fw_summer_southern <- rast(filtered_layers_2)

##Then calculate mean over 44 years 
#tmp_fw_summer_southern_mean <- mean(tmp_fw_summer_southern)
#saveRDS(tmp_fw_summer_southern_mean, "data/intermediate_files/tmp_fw_summer_southern_mean.rds")

#tmp_fw_summer_southern_stdev <- stdev(tmp_fw_summer_southern)
#saveRDS(tmp_fw_summer_southern_stdev, "data/intermediate_files/tmp_fw_summer_southern_stdev.rds")

#tmp_fw_summer_southern_max <- max(tmp_fw_summer_southern)
#saveRDS(tmp_fw_summer_southern_max, "data/intermediate_files/tmp_fw_summer_southern_max.rds")



##2) START HERE TO AVOID RE-RUNNING TEMP STEPS -- READ IN AVERAGE TEMPERATURE DATA -----------------
tmp_fw_summer_southern_mean <- readRDS("data/intermediate_files/tmp_fw_summer_southern_mean.rds")
tmp_fw_summer_southern_mean <- unwrap(tmp_fw_summer_southern_mean)
tmp_fw_summer_southern_stdev <- readRDS("data/intermediate_files/tmp_fw_summer_southern_stdev.rds")
tmp_fw_summer_southern_stdev <- unwrap(tmp_fw_summer_southern_stdev)
tmp_fw_summer_southern_max <- readRDS("data/intermediate_files/tmp_fw_summer_southern_max.rds")
tmp_fw_summer_southern_max <- unwrap(tmp_fw_summer_southern_max)

tmp_fw_summer_northern_mean <- readRDS("data/intermediate_files/tmp_fw_summer_northern_mean.rds")
tmp_fw_summer_northern_mean <- unwrap(tmp_fw_summer_northern_mean)
tmp_fw_summer_northern_stdev <- readRDS("data/intermediate_files/tmp_fw_summer_northern_stdev.rds")
tmp_fw_summer_northern_stdev <- unwrap(tmp_fw_summer_northern_stdev)
tmp_fw_summer_northern_max <- readRDS("data/intermediate_files/tmp_fw_summer_northern_max.rds")
tmp_fw_summer_northern_max <- unwrap(tmp_fw_summer_northern_max)

##Join the northern and southern hemispheres back together
tmp_fw_summer_all_mean <- terra::merge(tmp_fw_summer_northern_mean, tmp_fw_summer_southern_mean)
tmp_fw_summer_all_stdev <- merge(tmp_fw_summer_northern_stdev, tmp_fw_summer_southern_stdev)
tmp_fw_summer_all_max <- merge(tmp_fw_summer_northern_max, tmp_fw_summer_southern_max)

