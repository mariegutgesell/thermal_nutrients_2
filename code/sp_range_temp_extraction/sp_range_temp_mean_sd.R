##Calculate mean and SD of FW+SST data, and then do Monte Carlo selection of 3 random points for ctmax 

library(tidyverse)


##Combine SST and FW temp -- 
sst_temp_all <- read.csv("data/intermediate_data/hadisst_temp_all.csv") %>%
  dplyr::select(sci_name, Mean_Temp_C, Location_Source, Temp_Type) 

fw_temp_all <- read.csv("data/intermediate_data/fw_temp_all.csv") %>%
  dplyr::select(sci_name, Mean_Temp_C, Location_Source, Temp_Type) 


temp_df <- rbind(fw_temp_all, sst_temp_all)

sp_all <- read.csv("data/species_list/master_sp_list_clean.csv") %>%
  dplyr::select(Family:gl_fish) %>%
  distinct()


temp_df_mean_sd <- temp_df %>%
  group_by(sci_name) %>%
  filter(!is.na(Mean_Temp_C))%>%
  summarise_at(vars(Mean_Temp_C), list(mean_temp = mean, sd_temp = sd)) %>%
  left_join(sp_all, by = "sci_name") %>%
  dplyr::select(Family, Genus, sci_name, nutrient_data, ctmax_estimate, gl_fish, mean_temp, sd_temp)

write.csv(temp_df_mean_sd, "data/processed_data/All_sp_temp_mean_sd_HadISST_FW.csv")

test <- temp_df_mean_sd %>%
  filter(is.na(sd_temp))
##Create a normal distribution using mean and SD for each fish, and then use monte carlo simulation to extract 30 temps for each fish
library(truncnorm)
set.seed(123)
simulation_results <- lapply(1:nrow(temp_df_mean_sd), function(i) {
  species <- temp_df_mean_sd$sci_name[i]
  mean_temp <- temp_df_mean_sd$mean_temp[i]
  sd_temp <- temp_df_mean_sd$sd_temp[i]
  
  ##create bounds so not sampling from extremes of normal distribution
  lower <- qnorm(0.05, mean_temp, sd_temp)
  upper <- qnorm(0.95, mean_temp, sd_temp)
  
  # Simulate 30 temperatures
  simulated_temps <- rtruncnorm(30, a = lower, b = upper, mean = mean_temp, sd = sd_temp)
  
  # Create a data frame for this species
  data.frame(species = species, temperature = simulated_temps)
})

# Combine all species data into one data frame
temp_est_final <- do.call(rbind, simulation_results) %>%
  rename(sci_name = "species")


##Join these back to other data so that can keep some info about the fish and location source

temp_est_final_2 <- left_join(temp_est_final, sp_all, by = "sci_name")


write.csv(temp_est_final_2, "data/processed_data/All_sp_acclim_temp_estimates_HadISST_FW.csv")



