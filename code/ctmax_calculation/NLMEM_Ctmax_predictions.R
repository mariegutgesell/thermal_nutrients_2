##NLMEM and CTmax predictions for global species set -- based on 30 MC draws from mean+sd standard distribution of temps across range
##April 10, 2024 - updated Nov 11, 2024 -- updated Dec 4, 2024
##Marie Gutgesell

library(tidyverse)
library(readxl)
library(nlme)
library(purrr)

#1) NLMEM to create species-specific equations used to estimate CTmax -------------
##Import thermal data (source: Compte & Olden 2017, original data)
thermal_data_orig <- read_excel("data/raw_data/Comte_Olden_CTmax_Data.xlsx", sheet = "Original_data")

thermal_data_orig <- thermal_data_orig %>%
  separate(Species, into = c("Genus", "Species")) %>% ##generate separate column of genus, to help join to lists where only genus is known
  dplyr::select(-Species) %>%
  cbind(thermal_data_orig)

thermal_data_orig <- thermal_data_orig[,c(1,2, 26, 3:24)] %>%##reduce to columns of interest
  dplyr::rename(Thermal_Limit_C = `Thermal limit (°C)`, Temp_Acclim_C = `Temperature of acclimation (°C)`)
##Visualize temperature acclimation plots for each species
hist(thermal_data_orig$Thermal_Limit_C)

ctmax_acclim_all <- ggplot(thermal_data_orig, aes(x= Temp_Acclim_C, y = Thermal_Limit_C, group = Methodology, colour = Methodology)) +
  geom_point() +
  geom_smooth() +
  #facet_wrap(~Species) +
  theme_classic()
ctmax_acclim_all

str(thermal_data_orig)
##Non-linear mixed effects model between temperature of acclimation and upper thermal limits
##Random effect: species identity -- accounting for interspecific differences in acclimation ability
##Fixed effect: methodology (static or dynamic) 

##nlme() linearizes the model to estimate fixed effects
##nlmr() uses a Laplace approximation to estimate fixed effects

##NLME
tn_grp1 <- groupedData(Thermal_Limit_C ~ Temp_Acclim_C | Methodology, data = thermal_data_orig)

tn1.nlme <- nlme(Thermal_Limit_C~ SSasymp(Temp_Acclim_C, Asym, R0, lrc),
                 data = tn_grp1,
                 fixed = Asym + R0 + lrc ~ 1,
                 start = c(Asym = 55 , R0 = 20, lrc = -3.5))
tn1.nlme


tn2.nlme <- update(tn1.nlme,
                   random = list(Methodolgy = lrc ~ 1, 
                                 Species = Asym + R0 ~ 1),
                   groups = ~Methodology/Species)
##no errors or warnings presented!! :D 
tn2.nlme
summary(tn2.nlme)
fixef(tn2.nlme)
ranef(tn2.nlme)
tn2.coef <- coef(tn2.nlme)


##p-value of 0, this feels odd but i guess technically possible? lets try generating the species specific asymptotes plot and see what it is looking like
##correlated vs. uncorrelated random effects
tn3.nlme <- update(tn1.nlme,
                   random = list(Methodolgy = pdDiag(lrc ~ 1), 
                                 Species = pdDiag(Asym + R0 ~ 1)), ##uncorrelated random effect structure
                   groups = ~Methodology/Species)

tn3.nlme
summary(tn3.nlme)
fixef(tn3.nlme)
ranef(tn3.nlme)
tn3.coef <- coef(tn3.nlme)
#which model (correlated vs. uncorrelated random effect structure) has better fit? 
c(corr = AIC(tn2.nlme), uncorr = AIC(tn3.nlme))
##basically the same, but uncorrelated is slightly lower, so maybe better to use that?

##predicted vs. actual estimates
ggplot(tn_grp1, aes(y = predict(tn3.nlme), x = Thermal_Limit_C, group = Methodology, colour = Methodology)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(y = 'Predicted Thermal Limit (˚C)', x = 'Observed Thermal Limit (˚C)') +
  theme_classic()

fit_test <- lm(Thermal_Limit_C ~ predict(tn3.nlme), data = tn_grp1)
summary(fit_test)

##Generating species specific equations -- fixed slope depending on methodology, random asymptote and intercept for each species 
##general equation: Thermal_Limit_C(Temp_Acclim_C) = Asym + (R0 - Asym)e^(-lrc*Temp_Acclim_C) 
##for each species want to generate range of Temp_Acclim_C +/- 2.5C the range of Temp_Acclim_C in the dataset
##then, want to generate predicted Thermal_Limit_C across that whole range, using the method:species specific coefficients (i.e., specific equation for each species)

#generate species specific estimate range based on thermal acclimation temps +/2.5
##want to create data frame of 
tn3.coef <- tibble::rownames_to_column(tn3.coef, "Group") %>%
  separate(Group, c("Methodology", "Species"), '/') 


sp_num <- tn3.coef %>%
  dplyr::select(Species) %>%
  distinct()

tn3 <- thermal_data_orig %>%
  group_by(Methodology, Species) %>%
  summarise_at(vars(Temp_Acclim_C), list(TA_min = min, TA_max = max)) %>%
  mutate(TA_min_2.5 = TA_min - 2.5) %>%
  mutate(TA_max_2.5 = TA_max + 2.5) %>%
  dplyr::select(Methodology, Species, TA_min_2.5, TA_max_2.5) %>%
  left_join(tn3.coef, by = c("Methodology", "Species")) %>%
  group_by(Methodology, Species) %>%
  mutate(Temp_Acclim_C = map2(TA_min_2.5, TA_max_2.5, seq, length.out = 10)) %>%
  unnest(cols = Temp_Acclim_C) 


tn3_pred <- as.data.frame(predict(tn3.nlme,  newdata = tn3)) 
tn3_pred <- cbind(tn3, tn3_pred) %>%
  rename(TL_p = `predict(tn3.nlme, newdata = tn3)`)

d_plot <- tn3_pred %>%
  filter(Methodology == "dynamic") %>%
  ggplot(aes(x = Temp_Acclim_C, y = TL_p, group = Species, colour = Species)) +
  geom_line() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Acclimation Temperature (˚C)", y = "Thermal Limit (˚C)", title = "Dynamic")
d_plot

s_plot <- tn3_pred %>%
  filter(Methodology == "static") %>%
  ggplot(aes(x = Temp_Acclim_C, y = TL_p, group = Species, colour = Species)) +
  geom_line() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Acclimation Temperature (˚C)", y = "Thermal Limit (˚C)", title = "Static")

s_plot


##Correlated RE matrix model 
tn2.coef <- tibble::rownames_to_column(tn2.coef, "Group") %>%
  separate(Group, c("Methodology", "Species"), '/') 


tn2 <- thermal_data_orig %>%
  group_by(Methodology, Species) %>%
  summarise_at(vars(Temp_Acclim_C), list(TA_min = min, TA_max = max)) %>%
  mutate(TA_min_2.5 = TA_min - 2.5) %>%
  mutate(TA_max_2.5 = TA_max + 2.5) %>%
  dplyr::select(Methodology, Species, TA_min_2.5, TA_max_2.5) %>%
  left_join(tn3.coef, by = c("Methodology", "Species")) %>%
  group_by(Methodology, Species) %>%
  mutate(Temp_Acclim_C = map2(TA_min_2.5, TA_max_2.5, seq, length.out = 10)) %>%
  unnest(cols = Temp_Acclim_C) 


tn2_pred <- as.data.frame(predict(tn2.nlme,  newdata = tn2)) 
tn2_pred <- cbind(tn2, tn2_pred) %>%
  rename(TL_p = `predict(tn2.nlme, newdata = tn2)`)

d_plot <- tn2_pred %>%
  filter(Methodology == "dynamic") %>%
  ggplot(aes(x = Temp_Acclim_C, y = TL_p, group = Species, colour = Species)) +
  geom_line() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Acclimation Temperature (˚C)", y = "Thermal Limit (˚C)", title = "Dynamic")
d_plot

s_plot <- tn2_pred %>%
  filter(Methodology == "static") %>%
  ggplot(aes(x = Temp_Acclim_C, y = TL_p, group = Species, colour = Species)) +
  geom_line() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Acclimation Temperature (˚C)", y = "Thermal Limit (˚C)", title = "Static")

s_plot


#2) Generate CTmax estimates based on 100 random draws of mean summer temperatures from species ranges using species specific NLMEM models

##clean up environment
rm(list = ls()[! ls() %in% c("tn3.coef", "tn3.nlme")])

##UPDATE  CODE BELOW HERE FOR GLOBAL SPECIES ANALYSIS ==================
#Read in 30 temps for each species, based on monte-carlo draws from normal distribution using meand and sd of temp across species range  ==============
temp_all <- read.csv("data/processed_data/All_sp_acclim_temp_estimates_HadISST_FW.csv") %>% #####see end of All_sp_extract_water_temp_SST.R and sp_range_temp_mean_sd.R for code to generate this
  filter(nutrient_data == "Y" & ctmax_estimate == "Y") %>%
  dplyr::select(sci_name, temperature) %>%
  dplyr::rename(Species = "sci_name") 

test <- temp_all %>%
  dplyr::select(Species) %>%
  unique()
##Predict CTmax based on the 30 acclimation temperature

fw_mean_pred <- inner_join(tn3.coef, temp_all, by = "Species") %>%
  dplyr::rename(Temp_Acclim_C = "temperature")

##checking species overlap
fw_mean_pred_sp <- fw_mean_pred %>%
  distinct(Species)

fw_sp <- temp_all %>%
  dplyr::select(Species) %>%
  distinct()
##somehow lost 4 species... come back here.. nte: nov 13, 2024

##Generate predicted CTmax based on species specific equations for each of the temperature estimates for each species
##possible for 115 species

tn3_fw_mean_pred <- as.data.frame(predict(tn3.nlme,  newdata = fw_mean_pred))

tn3_fw_mean_pred <- cbind(fw_mean_pred, tn3_fw_mean_pred) %>%
  rename(TL_p_fw_mean = `predict(tn3.nlme, newdata = fw_mean_pred)`) %>%
  left_join(fw_sp, by = c("Species")) %>%
  mutate(Temp_Estimate_Source = "Compte Olden Species NLMEM", Species_CO = Species) %>%
  dplyr::rename(Species_GL = "Species") %>%
  dplyr::select(Methodology, Species_CO, Species_GL, Asym, R0, lrc, Temp_Acclim_C, TL_p_fw_mean, Temp_Estimate_Source) 



hist(tn3_fw_mean_pred$TL_p_fw_mean)


##some of these values look really fucking weird... 
test <- read.csv("data/processed_data/All_sp_acclim_temp_estimates_HadISST_FW.csv")  %>%
  filter(sci_name == "Couesius plumbeus")

test <- read.csv("data/intermediate_data/fw_temp_all.csv") %>%
  filter(sci_name == "Couesius plumbeus")
##write csv 
write.csv(tn3_fw_mean_pred, "data/processed_data/CTmax_fw_sst_est_summer_mean_sprange_ALL.csv")




##IF WANT TO USE IMPUTED DATA -------------------
##For commercial species where don't have species-specific equation, impute estimated CTmax using species specific temp ranges but equation parameters from other commercial species in the same genus
##Predict CTmax using parameters from other commercial species in genus from Great Lakes (excluding cyprinidae)
##create df of 12 commercial species w/o raw CO data (not including cyprinids)
is <- fw_temp_all %>%
  filter(!Species %in% tn3_fw_mean_pred$Species_GL) %>%
  filter(Fish_Type == "Commercial") %>%
  filter(Family != "Cyprinidae")

test <- is %>%
  dplyr::select(Species) %>%
  distinct()

is_sp <- is %>%
  dplyr::select(Family, Genus, Species, Fish_Type, Location_Source) %>%
  distinct()
##randomly select 100 values for each species - start w/ mean temp 
# Group the original data frame by 'species'
grouped <- is %>% group_by(Species) %>% filter(!is.na(Mean_Temp_C))

# Sample 100 values for each species
set.seed(4)
sampled_df <- grouped %>% slice_sample(n = 100, replace = FALSE) 

##Now need to predict for the 100 values
sampled_df_mean <- sampled_df %>%
  dplyr::select(Genus, Species, Mean_Temp_C)

na_test <- sampled_df_mean %>%
  filter(is.na(Mean_Temp_C))


test <- sampled_df_mean %>%
  dplyr::select(Species) %>%
  distinct()

##get NLMEM coefficients for species of other genus for the missing species
co_is_genus <- tn3.coef %>%
  separate(Species, c("Genus", "Species2"), sep = " ", remove = FALSE) %>%
  filter(Genus %in% sampled_df_mean$Genus)


is_mean_pred <- inner_join(co_is_genus, sampled_df_mean, by = "Genus") %>%
  dplyr::rename(Temp_Acclim_C = "Mean_Temp_C")


is_pred_df <- is_mean_pred %>%
  dplyr::select(Methodology, Species.x, Asym, R0, lrc, Temp_Acclim_C) %>%
  dplyr::rename(Species = "Species.x")


tn3_fw_mean_is <- as.data.frame(predict(tn3.nlme,  newdata = is_pred_df))

tn3_fw_mean_is <- cbind(is_pred_df, tn3_fw_mean_is) %>%
  rename(TL_p_fw_mean = `predict(tn3.nlme, newdata = is_pred_df)`) 

tn3_fw_mean_is <- tn3_fw_mean_is %>%
  separate(Species, c("Genus", "Species2"), sep = " ", remove = FALSE) %>%
  left_join(is_sp, by = "Genus") %>%
  dplyr::rename(Species_CO = "Species.x", Species_GL = "Species.y") %>%
  mutate(Temp_Estimate_Source = "Compte Olden NLMEM Genus Imputed") %>%
  dplyr::select(Methodology, Species_CO, Species_GL, Asym, R0, lrc, Temp_Acclim_C, TL_p_fw_mean, Fish_Type, Location_Source, Temp_Estimate_Source) 



ggplot(tn3_fw_mean_is, aes(x = Species_GL, y = TL_p_fw_mean, group = Species_CO, color = Species_GL)) +
  geom_boxplot() +
  theme_classic()


##Join imputed and predicted CTmax estimates together -- should have 100 estimates for each species 
##join together 
tn3_fw_mean_all <- rbind(tn3_fw_mean_pred, tn3_fw_mean_is)
