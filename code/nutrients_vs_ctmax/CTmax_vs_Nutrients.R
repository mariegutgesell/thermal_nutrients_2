##CTmax estimates vs. Nutrient Content -- for All possible species with nutrient + thermal data
##April 10, 2024


library(tidyverse)
library(readxl)
library(data.table)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(MuMIn)

library(visreg)
library(car)
##1) Import the CTmax data estimated for acclimation for species specific ranges (see Allsp_NLMEM_CTmax_predicitons.R code for how these predicted CTmax were generated)
ctmax <- read.csv("data/processed_data/CTmax_fw_sst_est_summer_mean_sprange_ALL.csv") %>%
  dplyr::rename(sci_name = "Species_GL")

##Need to select only 1 methodology for CTmax estimates - select dynamic if present
dyn <- ctmax %>%
  filter(Methodology == "dynamic")

static <- ctmax %>%
  filter(Methodology == "static") %>%
  filter(!sci_name %in% dyn$sci_name)

ctmax_df <- rbind(dyn, static)


##2) Import all nutrient data -- with outliers removed (see Testing_Nutrient_Data_Outliers_Extremes.R)
nutrient_df <- read.csv("data/processed_data/Nutrient_data_all_outliers_removed.csv") %>%
 # filter(!body_part %in% c("liver", "egg", "esophagus", "skin", "viscera")) %>%
  filter(!grepl("spp", sci_name)) %>%
  filter(str_count(sci_name, "\\S+") == 2) %>%
  separate(sci_name, into = c("Genus", "Species"), remove = TRUE) %>%
  unite("sci_name", Genus, Species, sep = " ") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = FALSE) %>%
  unique()
nutrient_df$Value <- as.numeric(nutrient_df$Value)

test <- nutrient_df %>%
  dplyr::select(body_part) %>%
  distinct()
##3) Import species list 
sp_all <- read.csv("data/species_list/master_sp_list_clean.csv")

##Calculate mean values and join df together
ctmax_mean <- ctmax_df %>%
  dplyr::select(Methodology, sci_name,  TL_p_fw_mean, Temp_Estimate_Source) %>%
  dplyr::rename(CT_max = "TL_p_fw_mean") %>%
  group_by(Methodology, sci_name) %>%
  summarise(across(.cols = CT_max,list(mean = mean, sd = sd))) %>%
  #mutate(Source = "Species Range NLMEM") %>%
  dplyr::select(sci_name, CT_max_mean, CT_max_sd, Methodology)


nutrients_mean <- nutrient_df %>%
  filter(!is.na(Value)) %>%
  group_by(sci_name, Nutrient_Name, body_part_2) %>% ##change to just body_part if want to keep more specific body groupings
  summarise(mean_value = mean(Value), sd_value = sd(Value)) 

##convert 3 columns (fresh, brackish and salt) into a single column factor
##1 = fresh, 1.5 = fresh+brackish, 2 = brackish, 2.5 = brackish+salt, 3 = salt, 4 = fresh+brackish+salt 
sp_all_traits_mean <- sp_all %>%
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
  summarise_at(vars(Length, CommonLength, LongevityWild, Vulnerability, Weight, DietTroph, FoodTroph, habitat_score, TLinfinity_mean, K_mean), list(mean = mean))

##may need to come back and filter out NAs before calculating means, but will be slightly complicated because only some columns will have NAs - come back to this 
##join together
ctmax_nutrient_df_mean <- left_join(ctmax_mean, sp_all_traits_mean, by = "sci_name") %>%
  left_join(nutrients_mean, by = c("sci_name")) %>%
  mutate(mean_value_log = log(mean_value))

test <- ctmax_nutrient_df_mean %>%
  dplyr::select(sci_name) %>%
  unique()

hist(ctmax_nutrient_df_mean$CT_max_mean)
hist(ctmax_df$TL_p_fw_mean)
#write.csv(ctmax_nutrient_df_mean, "data/processed_data/GL_mean_ctmax_nutrient_df.csv")

###Testing relationship between mean CTmax estimates and imputed CTmax estimates
imputed_df <- read_excel("data/raw_data/Comte_Olden_CTmax_Data.xlsx", sheet = "Imputed_data") %>%
  rename(sci_name = "Species")

test_ctmax <- left_join(ctmax_mean, imputed_df, by = "sci_name")

ggplot(test_ctmax, aes(x = CTmax, y = CT_max_mean)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "red", linewidth = 1) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(y = "Mean Estimated CTmax (˚C)", x = "Compte & Olden Imputed CTmax")


##some imputed values missing for species they had thermal data for? that is odd.. but is true
##for 6 of the ones we have ctmax estimate for, don't have an imputed CTmax 

##see if have metabolic rate or growth rate in fishbase , diet pathway (benthic, pelagic etc.) - max length vs. mean length? 

##explanatory variables: CT_max_mean, methodology, habitat, CommonLength_mean, LongevityWild_mean, Vulnerability_mean, FoodTroph_mean, K_mean_mean, body_part_2
ctmax_nutrient_df_mean <- ctmax_nutrient_df_mean %>%
  select(sci_name, Methodology, habitat, CT_max_mean, Length_mean, CommonLength_mean:Vulnerability_mean,FoodTroph_mean,  K_mean_mean, body_part_2,Nutrient_Name, mean_value)
str(ctmax_nutrient_df_mean)
##standardize (z-score) variables - mean 0 and sd of 1 
ctmax_nutrient_df_mean_scaled <- ctmax_nutrient_df_mean %>%
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))

##Make correlation plot of numerical explanatory variables
exp_var <- ctmax_nutrient_df_mean_scaled %>%
  ungroup() %>%
  select(CT_max_mean, Length_mean, LongevityWild_mean, FoodTroph_mean, K_mean_mean) %>%
  unique()
str(exp_var)

corr_mat <- cor(exp_var, use = "pairwise.complete.obs")

# visualize
corrplot(corr_mat, method = "color", type = "upper",  addCoef.col = "black",
         tl.col = "black", tl.srt = 45)

##don't have significant correlations -- only choose one of common length or max length 



##3) Simple Linear Regressions -- using mean values ===========
options(na.action = "na.fail")
##3.1) All Possible Species ------------------

#Plotting Relationships
gl_all_plot_2 <- ctmax_nutrient_df_mean_scaled %>%
  ggplot(aes(x = CT_max_mean, y = log(mean_value))) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  #  geom_errorbar(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), width = .2, position = position_dodge(0.05)) + 
  facet_wrap(~Nutrient_Name, scales = "free") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title =  "Mean CTmax vs. Nutrient Content - All Possible Species")
gl_all_plot_2


##Multiple linear regressions 
##Calcium 
ca_mean <- ctmax_nutrient_df_mean%>%
  filter(Nutrient_Name == "Calcium (mg)") %>%
  na.omit()  %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
##full model
ca_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean  + K_mean_mean + habitat + body_part_2, data = ca_mean)
summary(ca_lm)

##run AIC on all models using dredging approach -- compares all possible nest models constrained to those w/ CTmax
ca_model_set <- dredge(ca_lm, subset = CT_max_mean)
ca_model_set ##there are 2 models within 2 AIC score - how do you deal w/ this?

##since there are multiple best models, need to check carefully which they are and why 
##how do they differ? which model is most parsimonious?


##potential averagin approach: 
ca_avg_mod <- model.avg(ca_model_set, subset = delta < 2)
summary(ca_avg_mod)


##select the best model - don't just do this if more than 1 good model 
ca_best_model_1 <- get.models(ca_model_set, 1)[[1]]
summary(ca_best_model_1)

ca_best_model_2 <- get.models(ca_model_set, 2)[[1]]
summary(ca_best_model_2)

##Model 1 is the most parsimonious

##
v <- visreg(ca_best_model_1, "CT_max_mean", partial = TRUE, plot = FALSE)
ca_plot <- ggplot() +
  geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Calcium (mg/100g)")
ca_plot


##DHA 
dha_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "DHA (g)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
##full model
dha_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = dha_mean)
summary(dha_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
dha_model_set <- dredge(dha_lm, subset = CT_max_mean)
dha_model_set ##no models within 2 AIC score

##select the best model
dha_best_model <- get.models(dha_model_set, 1)[[1]]
summary(dha_best_model)


v <- visreg(dha_best_model, "CT_max_mean", partial = TRUE, plot = FALSE)
dha_plot <- ggplot() +
  geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log DHA (g/100g)")
dha_plot

###EPA 
epa_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "EPA (g)") %>%
  na.omit()  %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
epa_lm <- lm(log(mean_value) ~CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean  + K_mean_mean + habitat + body_part_2, data = epa_mean)
summary(epa_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
epa_model_set <- dredge(epa_lm, subset = CT_max_mean)
epa_model_set ##2 models within 2 AIC score

##potential averagin approach: 
epa_avg_mod <- model.avg(epa_model_set, subset = delta < 2)
summary(epa_avg_mod)

##select the best model
epa_best_model_1 <- get.models(epa_model_set, 1)[[1]]
summary(epa_best_model_1)

epa_best_model_2 <- get.models(epa_model_set, 2)[[1]]
summary(epa_best_model_2)

##For EPA, model 1 is most parsimonious 

v <- visreg(epa_best_model_1, "CT_max_mean", partial = TRUE, plot = FALSE)
epa_plot <- ggplot() +
  geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log EPA (g/100g)")
epa_plot

###Iron 
fe_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Iron (mg)") %>%
  na.omit()  %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
fe_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = fe_mean)
summary(fe_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
fe_model_set <- dredge(fe_lm, subset = CT_max_mean)
fe_model_set ##3 models within 2 AIC score - CTmax not part of it when not restricted to ct max

##potential averagin approach: 
fe_avg_mod <- model.avg(fe_model_set, subset = delta < 2)
summary(fe_avg_mod)

##select the best model -- in this case, the one with the lowest df (most parsimonious)
fe_best_model <- get.models(fe_model_set, 1)[[1]]
summary(fe_best_model)

v <- visreg(fe_best_model, "CT_max_mean", partial = TRUE, plot = FALSE)
fe_plot <- ggplot() +
 # geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
 # geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Iron (mg/100g)")
fe_plot


##Protein
pro_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Protein (g)") %>%
  na.omit()  %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
pro_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = pro_mean)
summary(pro_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
pro_model_set <- dredge(pro_lm, subset = CT_max_mean)
pro_model_set ##2 models within 2 AIC score -- first model where 0 is not w/ ct max, but some models within 2 AIC do have CT max


##potential averagin approach: 
pro_avg_mod <- model.avg(pro_model_set, subset = delta < 2)
summary(pro_avg_mod)

##select the best model - model 2 is most parsimonious 
pro_best_model <- get.models(pro_model_set, 2)[[1]]
summary(pro_best_model)

v <- visreg(pro_best_model, "CT_max_mean", partial = TRUE, plot = FALSE)
pro_plot <- ggplot() +
 #  geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
#   geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Protein (g/100g)")
pro_plot

##Selenium
sel_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Selenium (ug)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
sel_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = sel_mean)
summary(sel_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
sel_model_set <- dredge(sel_lm, subset = CT_max_mean)
sel_model_set ##10 models within 2 AIC score --

##potential averagin approach: 
sel_avg_mod <- model.avg(sel_model_set, subset = delta < 2)
summary(sel_avg_mod)

##select the best model - model 1 is the most parsimonious 
sel_best_model <- get.models(sel_model_set, 1)[[1]]
summary(sel_best_model)

v <- visreg(sel_best_model, "CT_max_mean", partial = TRUE, plot = FALSE)
sel_plot <- ggplot() +
 # geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
#  geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Selenium (mg/100g)")
sel_plot

##Fat
fat_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Total Fat (g)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
fat_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = fat_mean)
summary(fat_lm)


##run AIC on all models using dredging approach -- compares all possible nest models
fat_model_set <- dredge(fat_lm, subset = CT_max_mean)
fat_model_set ##7 models within 2 AIC score 

##potential averagin approach: 
fat_avg_mod <- model.avg(fat_model_set, subset = delta < 2)
summary(fat_avg_mod)

##select the best model - model 6 is the most parsimonious
fat_best_model <- get.models(fat_model_set, 6)[[1]]
summary(fat_best_model)

v <- visreg(fat_best_model, "CT_max_mean", partial = TRUE, plot = FALSE)
fat_plot <- ggplot() +
 #  geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
#    geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Total Fat (g/100g)")
fat_plot

##Zinc 
zn_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Zinc (mg)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
zn_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = zn_mean)
summary(zn_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
zn_model_set <- dredge(zn_lm, subset = CT_max_mean)
zn_model_set ##3 models within 2 AIC score 

##potential averagin approach: 
zn_avg_mod <- model.avg(zn_model_set, subset = delta < 2)
summary(zn_avg_mod)

##select the best model - model 1 is the most parsimonious
zn_best_model <- get.models(zn_model_set, 1)[[1]]
summary(zn_best_model)

v <- visreg(zn_best_model, "CT_max_mean", partial = TRUE, plot = FALSE)
zn_plot <- ggplot() +
#  geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
#  geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Zinc (mg/100g)")
zn_plot

##Vitamin A 
va_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Vitamin A (ug)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(CT_max_mean, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
va_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = va_mean)
summary(va_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
va_model_set <- dredge(va_lm, subset = CT_max_mean)
va_model_set ##3 models within 2 AIC score - ctmax not part of it 

##potential averagin approach: 
va_avg_mod <- model.avg(va_model_set, subset = delta < 2)
summary(va_avg_mod)

##select the best model - model 1 is most parsimonious
va_best_model <- get.models(va_model_set, 1)[[1]]
summary(va_best_model)

v <- visreg(va_best_model, "CT_max_mean", partial = TRUE, plot = FALSE)
va_plot <- ggplot() +
  #  geom_line(data = v$fit, aes(x = CT_max_mean, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = CT_max_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CT_max_mean, y = visregRes), size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Vitamin A (mg/100g)")
va_plot


###Playing around with potential figures for MS




macro_plot <- ggarrange(pro_plot, fat_plot, epa_plot, dha_plot, nrow = 2, ncol = 2, labels = c("a)", "b)", "c)", "d)"), font.label = list(colour = "black", size = 14))
macro_plot

plot1 <- ggarrange(sel_plot, zn_plot, va_plot, nrow = 1, ncol = 3, labels = c("c)", "d)", "e)"),  font.label = list(colour = "black", size = 14))
plot2 <- ggarrange(ca_plot, fe_plot,nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14))

micro_plot <-  ggarrange(plot2, plot1, nrow=2, ncol =1 )
micro_plot




##mutrients not loooking at :


omg3_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Omega 3 FA (g)")
omg3_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ habitat_score_mean+ LongevityWild_mean, data = omg3_mean)
summary(omg3_lm)
#par(mfrow = c(2, 2))
#plot(omg3_lm)
pufa_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "PUFA (g)")
pufa_lm <- lm(log(mean_value) ~ CT_max_mean + Length_mean + FoodTroph_mean+ habitat_score_mean + LongevityWild_mean, data = pufa_mean)
summary(pufa_lm)
#par(mfrow = c(2, 2))
#plot(pufa_lm)


###Linear Mixed Effects Model ----------
library(lme4)
#install.packages("Matrix")
ctmax_nutrient_df_all <- inner_join(ctmax_df, nutrient_df, by = "sci_name") #%>%
  #inner_join(sp_all, by = "sci_name")


##calcium
ca <- ctmax_nutrient_df_all %>%
  filter(Nutrient_Name == "Calcium (mg)")

ca_lme <- lmer(log(Value) ~ TL_p_fw_mean+ (1|sci_name), ca)
summary(ca_lme)

##dha
dha <- ctmax_nutrient_df_all %>%
  filter(Nutrient_Name == "DHA (g)")

dha_lme <- lmer(Value ~ TL_p_fw_mean + (1|sci_name), dha)
summary(dha_lme)
dha_lme
