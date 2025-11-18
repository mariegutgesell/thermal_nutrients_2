##Comparing mean temperature across range and CTmax -- then comparing mean temp vs. nutrient conditions 

##Nov 22, 2024

library(tidyverse)
library(readxl)
library(data.table)
library(ggplot2)
library(lme4)
library(lmerTest)
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

##2) Importe mean/sd of temp range
mean_temp_sd <- read.csv("data/processed_data/All_sp_temp_mean_sd_HadISST_FW.csv") %>%
  dplyr::select(sci_name:sd_temp)



##2) Import all nutrient data -- with outliers removed (see Testing_Nutrient_Data_Outliers_Extremes.R)
nutrient_df <- read.csv("data/processed_data/Nutrient_data_all_outliers_removed.csv") %>%
 # filter(!body_part %in% c("liver", "egg", "esophagus", "skin", "viscera")) %>%
  filter(!grepl("spp", sci_name)) %>%
  filter(str_count(sci_name, "\\S+") == 2) %>%
  separate(sci_name, into = c("Genus", "Species"), remove = TRUE) %>%
  unite("sci_name", Genus, Species, sep = " ") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = FALSE) %>%
  unique() #%>%
  #filter(source_df != "Bernhardt & O'Connor, 2019")


nutrient_df$Value <- as.numeric(nutrient_df$Value)

test <- nutrient_df %>%
  dplyr::select(body_part) %>%
  distinct()
##3) Import species list 
sp_all <- read.csv("data/species_list/master_sp_list_clean.csv") %>%
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
  ))%>%
  group_by(sci_name, habitat) %>%
  summarise_at(vars(Length, CommonLength, LongevityWild, Vulnerability, Weight, DietTroph, FoodTroph, TLinfinity_mean, K_mean), list(mean = mean))


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
  group_by(sci_name, Nutrient_Name, body_part_2) %>%
  summarise(mean_value = mean(Value), sd_value = sd(Value)) 


##join together
temp_nutrient_df_mean <- left_join(ctmax_mean, sp_all, by = "sci_name") %>%
  left_join(nutrients_mean, by = c("sci_name")) %>%
  left_join(mean_temp_sd, by = "sci_name")

test <- temp_nutrient_df_mean %>%
  dplyr::select(sci_name) %>%
  unique()

hist(temp_nutrient_df_mean$CT_max_mean)

hist(ctmax_df$TL_p_fw_mean)
hist(temp_nutrient_df_mean$CT_max_sd)
#write.csv(temp_nutrient_df_mean, "data/processed_data/GL_mean_ctmax_nutrient_df.csv")

###Testing relationship between mean CTmax estimates and mean temp of range
ggplot(temp_nutrient_df_mean, aes(x = mean_temp, y = CT_max_mean)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "red", linewidth = 1) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(y = "Mean Estimated CTmax (˚C)", x = "Mean Temperature of Species Range (˚C)")



##Now just join mean temp data and nutrients
temp_nutrient_df_mean <- left_join(nutrients_mean, mean_temp_sd, by = "sci_name") %>%
  left_join(sp_all, by = "sci_name") 

sp_mean_temp_nutrients <- temp_nutrient_df_mean %>%
  ungroup() %>%
  dplyr::select(sci_name) %>%
  unique() ##888 species! 


##explanatory variables: mean_temp, methodology, habitat, CommonLength_mean, LongevityWild_mean, Vulnerability_mean, FoodTroph_mean, K_mean_mean, body_part_2
temp_nutrient_df_mean <- temp_nutrient_df_mean %>%
  ungroup() %>%
  select(sci_name, habitat, mean_temp, Length_mean, CommonLength_mean:Vulnerability_mean,FoodTroph_mean,  K_mean_mean, body_part_2,Nutrient_Name, mean_value) %>%
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean,
                  LongevityWild_mean, FoodTroph_mean, K_mean_mean, mean_value),
                ~ as.numeric(.x))) 

str(temp_nutrient_df_mean)
##standardize (z-score) variables - mean 0 and sd of 1 


##Make correlation plot of numerical explanatory variables
exp_var <- temp_nutrient_df_mean %>%
  ungroup() %>%
  select(mean_temp, Length_mean, LongevityWild_mean, FoodTroph_mean, K_mean_mean) %>%
  unique()
str(exp_var)

corr_mat <- cor(exp_var, use = "pairwise.complete.obs")

# visualize
corrplot(corr_mat, method = "color", type = "upper",  addCoef.col = "black",
         tl.col = "black", tl.srt = 45)

##don't have significant correlations -- only choose one of common length or max length 



##3) Simple Linear Regressions -- using mean values ===========

##3.1) All Possible Species ------------------
options(na.action = "na.fail") ##need to turn this on so that model testing only done where all are not NA across all predictor variables 

#Plotting Relationships
gl_all_plot_2 <- temp_nutrient_df_mean %>%
  ggplot(aes(x = mean_temp, y = log(mean_value))) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  #  geom_errorbar(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), width = .2, position = position_dodge(0.05)) + 
  facet_wrap(~Nutrient_Name, scales = "free") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title =  "Mean CTmax vs. Nutrient Content - All Possible Species")
gl_all_plot_2


##Multiple linear regressions 
##Calcium 
ca_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Calcium (mg)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
##full model
ca_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = ca_mean)
summary(ca_lm)

##run AIC on all models using dredging approach -- compares all possible nest models constrained to those w/ CTmax
ca_model_set <- dredge(ca_lm, subset = mean_temp)
ca_model_set ##there are 6 models within 2 AIC score - how do you deal w/ this?

##potential averagin approach: 
ca_avg_mod <- model.avg(ca_model_set, subset = delta < 2)
summary(ca_avg_mod)


##select the best model - don't just do this if more than 1 good model 
ca_best_model_1 <- get.models(ca_model_set, 1)[[1]]
summary(ca_best_model_1)

ca_best_model_2 <- get.models(ca_model_set, 2)[[1]]
summary(ca_best_model_2)

ca_best_model_3 <- get.models(ca_model_set, 3)[[1]]
summary(ca_best_model_3)

##Model 2 and 3  is the most parsimonious - how to decide?

##
v <- visreg(ca_best_model_2, "mean_temp", partial = TRUE, plot = FALSE)
ca_plot <- ggplot() +
 # geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
#  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Temperature across Range (˚C)", y = "Log Calcium (mg/100g)")
ca_plot


##DHA 
dha_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "DHA (g)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
##full model
dha_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = dha_mean)
summary(dha_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
dha_model_set <- dredge(dha_lm, subset = mean_temp)
dha_model_set ##no models within 2 AIC score

##potential averagin approach: 

##select the best model
dha_best_model <- get.models(dha_model_set, 1)[[1]]
summary(dha_best_model)


v <- visreg(dha_best_model, "mean_temp", partial = TRUE, plot = FALSE)
dha_plot <- ggplot() +
  #geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  #geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Temperature across Range (˚C)", y = "Log DHA (g/100g)")
dha_plot

###EPA 
epa_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "EPA (g)") %>%
  na.omit()%>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
epa_lm <- lm(log(mean_value) ~mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean  + K_mean_mean + habitat + body_part_2, data = epa_mean)
summary(epa_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
epa_model_set <- dredge(epa_lm, subset = mean_temp)
epa_model_set ##2 models within 2 AIC score

##since there are multiple best models, need to check carefully which they are and why 
##how do they differ? which model is most parsimonious?
##also - when looking across all nutrients, how do these models vary and in terms of which parameters explain them?

##potential averagin approach: 
epa_avg_mod <- model.avg(epa_model_set, subset = delta < 2)
summary(epa_avg_mod)

##select the best model
epa_best_model_1 <- get.models(epa_model_set, 1)[[1]]
summary(epa_best_model_1)

epa_best_model <- get.models(epa_model_set, 5)[[1]]
summary(epa_best_model)

##For EPA, model 5 is most parsimonious 

v <- visreg(epa_best_model, "mean_temp", partial = TRUE, plot = FALSE)
epa_plot <- ggplot() +
  geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log EPA (g/100g)")
epa_plot

###Iron 
fe_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Iron (mg)") %>%
  na.omit()%>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
fe_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = fe_mean)
summary(fe_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
fe_model_set <- dredge(fe_lm, subset = mean_temp)
fe_model_set ##4 models within 2 AIC score - CTmax not part of it when not restricted to ct max


##potential averagin approach: 
fe_avg_mod <- model.avg(fe_model_set, subset = delta < 2)
summary(fe_avg_mod)

##select the best model -- in this case, the one with the lowest df (most parsimonious)
fe_best_model <- get.models(fe_model_set, 2)[[1]]
summary(fe_best_model)

v <- visreg(fe_best_model, "mean_temp", partial = TRUE, plot = FALSE)
fe_plot <- ggplot() +
  # geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  # geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Iron (mg/100g)")
fe_plot


##Protein
pro_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Protein (g)") %>%
  na.omit()%>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
pro_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = pro_mean)
summary(pro_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
pro_model_set <- dredge(pro_lm, subset = mean_temp)
pro_model_set ##6 models within 2 AIC score -- first model where 0 is not w/ ct max, but some models within 2 AIC do have CT max


##potential averagin approach: 
pro_avg_mod <- model.avg(pro_model_set, subset = delta < 2)
summary(pro_avg_mod)

##select the best model - model 5 is most parsimonious 
pro_best_model <- get.models(pro_model_set, 5)[[1]]
summary(pro_best_model)

v <- visreg(pro_best_model, "mean_temp", partial = TRUE, plot = FALSE)
pro_plot <- ggplot() +
  geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Protein (g/100g)")
pro_plot

##Selenium
sel_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Selenium (ug)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
sel_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = sel_mean)
summary(sel_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
sel_model_set <- dredge(sel_lm, subset = mean_temp)
sel_model_set ##6 models within 2 AIC score --

##potential averagin approach: 
sel_avg_mod <- model.avg(sel_model_set, subset = delta < 2)
summary(sel_avg_mod)

##select the best model - model 5 is the most parsimonious 
sel_best_model <- get.models(sel_model_set, 3)[[1]]
summary(sel_best_model)

v <- visreg(sel_best_model, "mean_temp", partial = TRUE, plot = FALSE)
sel_plot <- ggplot() +
  # geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Selenium (mg/100g)")
sel_plot

##Fat
fat_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Total Fat (g)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
fat_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = fat_mean)
summary(fat_lm)


##run AIC on all models using dredging approach -- compares all possible nest models
fat_model_set <- dredge(fat_lm, subset = mean_temp)
fat_model_set ##3 models within 2 AIC score 

##potential averagin approach: 
fat_avg_mod <- model.avg(fat_model_set, subset = delta < 2)
summary(fat_avg_mod)

##select the best model - model 1 is the most parsimonious
fat_best_model <- get.models(fat_model_set, 1)[[1]]
summary(fat_best_model)

v <- visreg(fat_best_model, "mean_temp", partial = TRUE, plot = FALSE)
fat_plot <- ggplot() +
 # geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  #geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Total Fat (g/100g)")
fat_plot

##Zinc 
zn_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Zinc (mg)") %>%
  na.omit()%>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
zn_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = zn_mean)
summary(zn_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
zn_model_set <- dredge(zn_lm, subset = mean_temp)
zn_model_set ##6 models within 2 AIC score 

##potential averagin approach: 
zn_avg_mod <- model.avg(zn_model_set, subset = delta < 2)
summary(zn_avg_mod)

##select the best model - model 1 is the most parsimonious
zn_best_model <- get.models(zn_model_set, 6)[[1]]
summary(zn_best_model)

v <- visreg(zn_best_model, "mean_temp", partial = TRUE, plot = FALSE)
zn_plot <- ggplot() +
    geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
    geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Zinc (mg/100g)")
zn_plot

##Vitamin A 
va_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Vitamin A (ug)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
va_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = va_mean)
summary(va_lm)
##only 3 fish species where we have all of these data 

##run AIC on all models using dredging approach -- compares all possible nest models
va_model_set <- dredge(va_lm, subset = mean_temp)
va_model_set ##3 models within 2 AIC score - ctmax not part of it 

##potential averagin approach: 
va_avg_mod <- model.avg(va_model_set, subset = delta < 2)
summary(va_avg_mod)

##select the best model - model 1 is most parsimonious
va_best_model <- get.models(va_model_set, 1)[[1]]
summary(va_best_model)

v <- visreg(va_best_model, "mean_temp", partial = TRUE, plot = FALSE)
va_plot <- ggplot() +
    geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
    geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
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


##Analysis without including body part -- just selecting muscle or whole -----------------
temp_nutrient_df_mean <- temp_nutrient_df_mean %>%
  filter(body_part_2 %in% c("muscle (with and without organs)", "whole"))

test <- temp_nutrient_df_mean %>%
  select(body_part_2) %>%
  unique()
##Multiple linear regressions 
##Calcium 
ca_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Calcium (mg)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
##full model
ca_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat, data = ca_mean)
summary(ca_lm)

##run AIC on all models using dredging approach -- compares all possible nest models constrained to those w/ CTmax
ca_model_set <- dredge(ca_lm, subset = mean_temp)
ca_model_set ##there are 8 models within 2 AIC score - how do you deal w/ this?

##potential averagin approach: 
ca_avg_mod <- model.avg(ca_model_set, subset = delta < 2)
summary(ca_avg_mod)


##select the best model - don't just do this if more than 1 good model 
ca_best_model <- get.models(ca_model_set, 4)[[1]]
summary(ca_best_model)



##
v <- visreg(ca_best_model, "mean_temp", partial = TRUE, plot = FALSE)
ca_plot <- ggplot() +
  # geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Temperature across Range (˚C)", y = "Log Calcium (mg/100g)")
ca_plot


##DHA 
dha_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "DHA (g)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
##full model
dha_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat, data = dha_mean)
summary(dha_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
dha_model_set <- dredge(dha_lm, subset = mean_temp)
dha_model_set ##no models within 2 AIC score

##potential averagin approach: 

##select the best model
dha_best_model <- get.models(dha_model_set, 1)[[1]]
summary(dha_best_model)


v <- visreg(dha_best_model, "mean_temp", partial = TRUE, plot = FALSE)
dha_plot <- ggplot() +
  #geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  #geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Temperature across Range (˚C)", y = "Log DHA (g/100g)")
dha_plot

###EPA 
epa_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "EPA (g)") %>%
  na.omit()%>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
epa_lm <- lm(log(mean_value) ~mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean  + K_mean_mean + habitat, data = epa_mean)
summary(epa_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
epa_model_set <- dredge(epa_lm, subset = mean_temp)
epa_model_set ##2 models within 2 AIC score

##since there are multiple best models, need to check carefully which they are and why 
##how do they differ? which model is most parsimonious?
##also - when looking across all nutrients, how do these models vary and in terms of which parameters explain them?

##potential averagin approach: 
epa_avg_mod <- model.avg(epa_model_set, subset = delta < 2)
summary(epa_avg_mod)

##select the best model
epa_best_model_1 <- get.models(epa_model_set, 1)[[1]]
summary(epa_best_model_1)

epa_best_model <- get.models(epa_model_set, 5)[[1]]
summary(epa_best_model)

##For EPA, model 5 is most parsimonious 

v <- visreg(epa_best_model, "mean_temp", partial = TRUE, plot = FALSE)
epa_plot <- ggplot() +
  geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Temperature across Range (˚C)", y = "Log EPA (g/100g)")
epa_plot

###Iron 
fe_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Iron (mg)") %>%
  na.omit()%>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
fe_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat, data = fe_mean)
summary(fe_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
fe_model_set <- dredge(fe_lm, subset = mean_temp)
fe_model_set ##4 models within 2 AIC score - CTmax not part of it when not restricted to ct max


##potential averagin approach: 
fe_avg_mod <- model.avg(fe_model_set, subset = delta < 2)
summary(fe_avg_mod)

##select the best model -- in this case, the one with the lowest df (most parsimonious)
fe_best_model <- get.models(fe_model_set, 7)[[1]]
summary(fe_best_model)

v <- visreg(fe_best_model, "mean_temp", partial = TRUE, plot = FALSE)
fe_plot <- ggplot() +
   geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1, linetype = "dashed") +
   geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Temperature across Range (˚C)", y = "Log Iron (mg/100g)")
fe_plot


##Protein
pro_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Protein (g)") %>%
  na.omit()%>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
pro_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat, data = pro_mean)
summary(pro_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
pro_model_set <- dredge(pro_lm, subset = mean_temp)
pro_model_set ##13 models within 2 AIC score -- first model where 0 is not w/ ct max, but some models within 2 AIC do have CT max


##potential averagin approach: 
pro_avg_mod <- model.avg(pro_model_set, subset = delta < 2)
summary(pro_avg_mod)

##select the best model - model 5 is most parsimonious 
pro_best_model <- get.models(pro_model_set, 3)[[1]]
summary(pro_best_model)

v <- visreg(pro_best_model, "mean_temp", partial = TRUE, plot = FALSE)
pro_plot <- ggplot() +
  geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Protein (g/100g)")
pro_plot

##Selenium
sel_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Selenium (ug)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
sel_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat, data = sel_mean)
summary(sel_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
sel_model_set <- dredge(sel_lm, subset = mean_temp)
sel_model_set ##6 models within 2 AIC score --

##potential averagin approach: 
sel_avg_mod <- model.avg(sel_model_set, subset = delta < 2)
summary(sel_avg_mod)

##select the best model - model 5 is the most parsimonious 
sel_best_model <- get.models(sel_model_set, 1)[[1]]
summary(sel_best_model)

v <- visreg(sel_best_model, "mean_temp", partial = TRUE, plot = FALSE)
sel_plot <- ggplot() +
  # geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Temperature across Range (˚C)", y = "Log Selenium (mg/100g)")
sel_plot

##Fat
fat_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Total Fat (g)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
fat_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat, data = fat_mean)
summary(fat_lm)


##run AIC on all models using dredging approach -- compares all possible nest models
fat_model_set <- dredge(fat_lm, subset = mean_temp)
fat_model_set ##3 models within 2 AIC score 

##potential averagin approach: 
fat_avg_mod <- model.avg(fat_model_set, subset = delta < 2)
summary(fat_avg_mod)

##select the best model - model 1 is the most parsimonious
fat_best_model <- get.models(fat_model_set, 5)[[1]]
summary(fat_best_model)

v <- visreg(fat_best_model, "mean_temp", partial = TRUE, plot = FALSE)
fat_plot <- ggplot() +
  # geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  #geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Temperature across Range (˚C)", y = "Log Total Fat (g/100g)")
fat_plot

##Zinc 
zn_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Zinc (mg)") %>%
  na.omit()%>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
zn_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat, data = zn_mean)
summary(zn_lm)

##run AIC on all models using dredging approach -- compares all possible nest models
zn_model_set <- dredge(zn_lm, subset = mean_temp)
zn_model_set ##6 models within 2 AIC score 

##potential averagin approach: 
zn_avg_mod <- model.avg(zn_model_set, subset = delta < 2)
summary(zn_avg_mod)

##select the best model - model 4 is the most parsimonious
zn_best_model <- get.models(zn_model_set, 4)[[1]]
summary(zn_best_model)

v <- visreg(zn_best_model, "mean_temp", partial = TRUE, plot = FALSE)
zn_plot <- ggplot() +
#  geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
#  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = "Log Zinc (mg/100g)")
zn_plot

##Vitamin A 
va_mean <- temp_nutrient_df_mean %>%
  filter(Nutrient_Name == "Vitamin A (ug)") %>%
  na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
  mutate(across(c(mean_temp, Length_mean, CommonLength_mean, LongevityWild_mean,  FoodTroph_mean, K_mean_mean),
                ~ scale(.x)[,1]))
va_lm <- lm(log(mean_value) ~ mean_temp + Length_mean + FoodTroph_mean+ LongevityWild_mean + K_mean_mean + habitat + body_part_2, data = va_mean)
summary(va_lm)
##only 3 fish species where we have all of these data 

##run AIC on all models using dredging approach -- compares all possible nest models
va_model_set <- dredge(va_lm, subset = mean_temp)
va_model_set ##3 models within 2 AIC score - ctmax not part of it 

##potential averagin approach: 
va_avg_mod <- model.avg(va_model_set, subset = delta < 2)
summary(va_avg_mod)

##select the best model - model 1 is most parsimonious
va_best_model <- get.models(va_model_set, 2)[[1]]
summary(va_best_model)

v <- visreg(va_best_model, "mean_temp", partial = TRUE, plot = FALSE)
va_plot <- ggplot() +
  geom_line(data = v$fit, aes(x = mean_temp, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = mean_temp, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = mean_temp, y = visregRes), size = 2) +
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
