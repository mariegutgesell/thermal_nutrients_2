####Calculate mean summer temp -- SST

library(dplyr)
library(readxl)
library(sf)
library(terra)
library(tidyverse)
library(exactextractr)
library(raster)


##Read in HadISST data -- need to calculate means based of warmest month in each hemisphere

##Read in saved yearly SST avg 
# Extraction
## using the saved raster
#setwd("~/Desktop/Thermal Nutrients/")

filenames <- file.path(
  "data", "large-files",
  paste0("HadISST_sst", ".nc")
)

tmp_sst <- terra::rast(filenames) 


print(tmp_sst)


units <- units(tmp_sst)
uu <- unique(units)

##First, select the proper year range 1958-2001 to make comparable to freshwater data 
time <- time(tmp_sst)
print(time)
# Convert time to years
years <- as.numeric(format(time, "%Y"))
months <- as.numeric(format(time, "%m"))

# Find indices for 1958 to 2001
start_index <- which.min(abs(years - 1958))
end_index <- max(which(years == 2001))  # Ensure all months in 2001 are included


tmp_sst_sub <- tmp_sst[[start_index:end_index]]
print(tmp_sst_sub)
##replace any -1000 values, or -999 values with NA (as these indicate missing/insufficient data)
tmp_sst_sub[tmp_sst_sub == -1000] <- NA

units <- units(tmp_sst_sub)
uu <- unique(units)
#plot(tmp_sst_sub)
##2) Split raster into southern and northern hemisphere
# Define extent for the southern hemisphere (-90 to 0 degrees latitude)
extent_southern <- extent(-180, 180, -90, 0)

# Define extent for the northern hemisphere (0 to 90 degrees latitude)
extent_northern <- extent(-180, 180, 0, 90)


# Crop the raster for the southern hemisphere
#setwd("~/Desktop/Thermal Nutrients/")
#tmp_sst_southern <- crop(tmp_sst_sub, extent_southern)
#saveRDS(tmp_sst_southern, "data/intermediate_files/tmp_sst_southern.rds")

tmp_sst_southern<- readRDS("data/intermediate_files/tmp_sst_southern.rds")
tmp_sst_southern

min_s <- minmax(tmp_sst_southern)[1, ]
min_s
##something is wrong with the minimum values, a whole bunch have a minimum of -1000.00 C -- that is impossible
max_s <- minmax(tmp_sst_southern)[2, ]
max_s
##max values look okay 
# Crop the raster for the northern hemisphere
#tmp_sst_northern <- crop(tmp_sst_sub, extent_northern)
#saveRDS(tmp_sst_northern, "data/intermediate_files/tmp_sst_northern.rds")
tmp_sst_northern<- readRDS("data/intermediate_files/tmp_sst_northern.rds")
tmp_sst_northern

min_n <- minmax(tmp_sst_northern)[1, ]
min_n
##something is wrong with the minimum values, all have a minimum of -1000.00 C -- that is impossible
max_n <- minmax(tmp_sst_northern)[2, ]
max_n
##max values look okay 
##3) Calculate average of warmest quarter  -------
##Northern Hemisphere 
##Extract layers for June, July, August, and then calculate yearly average of summer mean temps 
years <- terra::time(tmp_sst_northern) |> format("%Y") |> unique()

# Create an empty list to store filtered layers
filtered_layers <- list()

# Loop through each year
for (i in 1:length(years)) {
  # Extract the year
  year <- years[i]
  
  # Define the start and end dates for June 1 and August 31 of the current year
  start_date <- paste0(year, "-06-16")
  end_date <- paste0(year, "-08-16")
  
  # Subset the raster stack for the current year and the specified time range
  subset_layer <- subset(tmp_sst_northern, time(tmp_sst_northern) >= as.Date(start_date) & time(tmp_sst_northern) < as.Date(end_date))
  
  ##Calculate mean temp across the summer months 
  subset_layer_mean <- mean(subset_layer)
  
  # Store the subset layer in the list
  filtered_layers[[i]] <- subset_layer_mean
}

# Combine the filtered layers into a single SpatRaster object
tmp_sst_summer_northern <- rast(filtered_layers)

##Then average across all 44 years .
tmp_sst_summer_northern_mean <- mean(tmp_sst_summer_northern)
saveRDS(tmp_sst_summer_northern_mean, "data/intermediate_files/tmp_sst_summer_northern_mean.rds")

tmp_sst_summer_northern_stdev <- stdev(tmp_sst_summer_northern)
saveRDS(tmp_sst_summer_northern_stdev, "data/intermediate_files/tmp_sst_summer_northern_stdev.rds")

tmp_sst_summer_northern_max <- max(tmp_sst_summer_northern)
saveRDS(tmp_sst_summer_northern_max, "data/intermediate_files/tmp_sst_summer_northern_max.rds")

##Southern Hemisphere 
##Extract layers for December, January, February and then calculate yearly average of summer mean temps 
years <- terra::time(tmp_sst_southern) |> format("%Y") |> unique()

# Create an empty list to store filtered layers
filtered_layers_2 <- list()

# Loop through each year -- december
for (i in 1:length(years)) {
  # Extract the year
  year <- years[i]
  
  # Define the start and end dates for December of the current year
  date_1 <- paste0(year, "-12-16")
  
  # Subset the raster stack for the current year and the specified time range
  subset_layer_1 <- subset(tmp_sst_southern, time(tmp_sst_southern) == as.Date(date_1))
  
  # Define the start and end dates for January and February of the current year
  start_date_2 <- paste0(year, "-01-16")
  end_date_2 <- paste0(year, "-03-16")
  
  # Subset the raster stack for the current year and the specified time range
  subset_layer_2 <- subset(tmp_sst_southern, time(tmp_sst_southern) >= as.Date(start_date_2) & time(tmp_sst_southern) < as.Date(end_date_2))
  
  subset_layer <- merge(subset_layer_1, subset_layer_2)
  
  ##Calculate mean temp across the summer months 
  subset_layer_mean <- mean(subset_layer)
  
  # Store the subset layer in the list
  filtered_layers_2[[i]] <- subset_layer_mean
}


# Combine the filtered layers into a single SpatRaster object
tmp_sst_summer_southern <- rast(filtered_layers_2)
tmp_sst_summer_southern
##Then calculate mean over 44 years 
tmp_sst_summer_southern_mean <- mean(tmp_sst_summer_southern)
saveRDS(tmp_sst_summer_southern_mean, "data/intermediate_files/tmp_sst_summer_southern_mean.rds")

tmp_sst_summer_southern_stdev <- stdev(tmp_sst_summer_southern)
saveRDS(tmp_sst_summer_southern_stdev, "data/intermediate_files/tmp_sst_summer_southern_stdev.rds")

tmp_sst_summer_southern_max <- max(tmp_sst_summer_southern)
saveRDS(tmp_sst_summer_southern_max, "data/intermediate_files/tmp_sst_summer_southern_max.rds")



##2) START HERE TO AVOID RE-RUNNING TEMP STEPS -- READ IN AVERAGE TEMPERATURE DATA -----------------
tmp_sst_summer_southern_mean <- readRDS("data/intermediate_files/tmp_sst_summer_southern_mean.rds")
tmp_sst_summer_southern_mean <- unwrap(tmp_sst_summer_southern_mean)
tmp_sst_summer_southern_stdev <- readRDS("data/intermediate_files/tmp_sst_summer_southern_stdev.rds")
tmp_sst_summer_southern_stdev <- unwrap(tmp_sst_summer_southern_stdev)
tmp_sst_summer_southern_max <- readRDS("data/intermediate_files/tmp_sst_summer_southern_max.rds")
tmp_sst_summer_southern_max <- unwrap(tmp_sst_summer_southern_max)

tmp_sst_summer_northern_mean <- readRDS("data/intermediate_files/tmp_sst_summer_northern_mean.rds")
tmp_sst_summer_northern_mean <- unwrap(tmp_sst_summer_northern_mean)
tmp_sst_summer_northern_stdev <- readRDS("data/intermediate_files/tmp_sst_summer_northern_stdev.rds")
tmp_sst_summer_northern_stdev <- unwrap(tmp_sst_summer_northern_stdev)
tmp_sst_summer_northern_max <- readRDS("data/intermediate_files/tmp_sst_summer_northern_max.rds")
tmp_sst_summer_northern_max <- unwrap(tmp_sst_summer_northern_max)

##Join the northern and southern hemispheres back together
tmp_sst_summer_all_mean <- terra::merge(tmp_sst_summer_northern_mean, tmp_sst_summer_southern_mean)
tmp_sst_summer_all_stdev <-terra:: merge(tmp_sst_summer_northern_stdev, tmp_sst_summer_southern_stdev)
tmp_sst_summer_all_max <- terra::merge(tmp_sst_summer_northern_max, tmp_sst_summer_southern_max)

