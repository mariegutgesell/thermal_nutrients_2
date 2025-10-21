##CTmax estimates vs. Nutrient Content -- for All possible species with nutrient + thermal data
##April 10, 2024


library(tidyverse)
library(readxl)
library(data.table)
library(ggplot2)
library(ggpubr)

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
  filter(!body_part %in% c("liver", "egg", "esophagus", "skin", "viscera")) %>%
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
  group_by(sci_name, Nutrient_Name) %>%
  summarise(mean_value = mean(Value), sd_value = sd(Value)) 


##join together
ctmax_nutrient_df_mean <- inner_join(ctmax_mean, sp_all, by = "sci_name") %>%
  inner_join(nutrients_mean, by = c("sci_name")) %>%
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
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Our estimated mean CTmax") +
  xlab("Compte & Olden Imputed CTmax")

##some imputed values missing for species they had thermal data for? that is odd.. but is true
##for 6 of the ones we have ctmax estimate for, don't have an imputed CTmax 

##Relationships b/w mean temp ranges and other potential variables
ggplot(ctmax_nutrient_df_mean, aes(y = Length, x = CT_max_mean)) + 
  geom_point()
lm1 <- lm(Length ~ CT_max_mean, ctmax_nutrient_df_mean)
summary(lm1)
ggplot(ctmax_nutrient_df_mean, aes(y = CommonLength, x = CT_max_mean)) + 
  geom_point()
lm2 <- lm(CommonLength ~ CT_max_mean, ctmax_nutrient_df_mean)
summary(lm2)
ggplot(ctmax_nutrient_df_mean, aes(y = FoodTroph, x = CT_max_mean)) + 
  geom_point()
lm3 <- lm(FoodTroph~ CT_max_mean, ctmax_nutrient_df_mean)
summary(lm3)
ggplot(ctmax_nutrient_df_mean, aes(y = FoodTroph, x = CommonLength)) + 
  geom_point()
lm4 <- lm(FoodTroph ~ CommonLength, ctmax_nutrient_df_mean)
summary(lm4)
##3) Simple Linear Regressions -- using mean values ===========

##3.1) All Possible Species ------------------

#Plotting Relationships
gl_all_plot <- ctmax_nutrient_df_mean %>%
  ggplot(aes(x = CT_max_mean, y = log(mean_value))) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_errorbar(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), width = .2, position = position_dodge(0.05)) +
  geom_errorbar(aes(xmin = CT_max_mean-CT_max_sd, xmax = CT_max_mean+CT_max_sd), width = .2, position = position_dodge(0.05)) +
  facet_wrap(~Nutrient_Name, scales = "free") +
  theme_classic() +
  ylab("Nutrient_Value")
gl_all_plot



gl_all_plot_2 <- ctmax_nutrient_df_mean %>%
  ggplot(aes(x = CT_max_mean, y = log(mean_value))) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  #  geom_errorbar(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), width = .2, position = position_dodge(0.05)) + 
  facet_wrap(~Nutrient_Name, scales = "free") +
  theme_classic() +
  labs(y = "Mean Nutrient Value", x = "Mean Estimated CTmax (˚C)", title =  "Mean CTmax vs. Nutrient Content - All Possible Species")
gl_all_plot_2


##Simple linear regressions 
ca_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Calcium (mg)")
ca_lm <- lm(log(mean_value) ~ CT_max_mean + Length + FoodTroph+ Fresh + Brack + Saltwater + LongevityWild, data = ca_mean)
summary(ca_lm)

ca_lm_2 <- lm(log(mean_value) ~ CT_max_mean, data = ca_mean)
summary(ca_lm)

#par(mfrow = c(2, 2))
#plot(ca_lm)

dha_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "DHA (g)")
dha_lm <- lm(log(mean_value) ~ CT_max_mean + Length + FoodTroph+ Fresh + Brack + Saltwater + LongevityWild, data = dha_mean)
summary(dha_lm)
#par(mfrow = c(2, 2))
#plot(dha_lm)

epa_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "EPA (g)")
epa_lm <- lm(log(mean_value) ~ CT_max_mean + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = epa_mean)
summary(epa_lm)
#par(mfrow = c(2, 2))
#plot(epa_lm)

fe_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Iron (mg)") %>%
  filter(mean_value < 1000) ##removing the one large outlier
fe_lm <- lm(log(mean_value) ~ CT_max_mean +Length + FoodTroph+ Fresh + Brack + Saltwater + LongevityWild, data = fe_mean)
summary(fe_lm)
#par(mfrow = c(2, 2))
#plot(fe_lm)

omg3_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Omega 3 FA (g)")
omg3_lm <- lm(log(mean_value) ~ CT_max_mean+ Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = omg3_mean)
summary(omg3_lm)
#par(mfrow = c(2, 2))
#plot(omg3_lm)

pro_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Protein (g)")
pro_lm <- lm(log(mean_value) ~ CT_max_mean+ Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = pro_mean)
summary(pro_lm)
#par(mfrow = c(2, 2))
#plot(pro_lm)

pufa_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "PUFA (g)")
pufa_lm <- lm(log(mean_value) ~ CT_max_mean+ Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = pufa_mean)
summary(pufa_lm)
#par(mfrow = c(2, 2))
#plot(pufa_lm)

sel_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Selenium (ug)")
sel_lm <- lm(log(mean_value) ~ CT_max_mean+ Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = sel_mean)
summary(sel_lm)

#par(mfrow = c(2, 2))
#plot(sel_lm)

fat_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Total Fat (g)")
fat_lm <- lm(log(mean_value) ~ CT_max_mean+ Length + FoodTroph + + Fresh + Brack + Saltwater + LongevityWild, data = fat_mean)
summary(fat_lm)
#par(mfrow = c(2, 2))
#plot(fat_lm)

zn_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Zinc (mg)")
zn_lm <- lm(log(mean_value) ~ CT_max_mean+ Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = zn_mean)
summary(zn_lm)
#par(mfrow = c(2, 2))
#plot(zn_lm)

va_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Vitamin A (ug)")
va_lm <- lm(log(mean_value) ~ CT_max_mean + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = va_mean)
summary(va_lm)
#par(mfrow = c(2, 2))
#plot(va_lm)

###Playing around with potential figures for MS
pro_plot <- ggplot(pro_mean, aes(x = CT_max_mean, y = mean_value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value", x = "Mean Estimated CTmax (˚C)", title = "Protein (g)")
pro_plot

fat_plot <- ggplot(fat_mean, aes(x = CT_max_mean, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "Total Fat (g)")
fat_plot
  
epa_plot <- ggplot(epa_mean, aes(x = CT_max_mean, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "EPA (g)")
epa_plot

dha_plot <- ggplot(dha_mean, aes(x = CT_max_mean, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "DHA (g)")
dha_plot

omg3_plot <- ggplot(omg3_mean, aes(x = CT_max_mean, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "Omega3 (g)")
omg3_plot


plot1 <- ggarrange(omg3_plot, epa_plot, dha_plot, nrow = 1, ncol = 3, labels = c("c)", "d)", "e)"),  font.label = list(colour = "black", size = 14, family = "Times New Roman"))
  
plot2 <- ggarrange(pro_plot, fat_plot,nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))

macro_plot <-  ggarrange(plot2, plot1, nrow=2, ncol =1 )
macro_plot


##micronutrient plot 
ca_plot <- ggplot(ca_mean, aes(x = CT_max_mean, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "Calcium (mg)")
ca_plot


fe_plot <- ggplot(fe_mean, aes(x = CT_max_mean, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "Iron (mg)")
fe_plot

sel_plot <- ggplot(sel_mean, aes(x = CT_max_mean, y = mean_value_log)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "Selenium (ug)")
sel_plot

va_plot <- ggplot(va_mean, aes(x = CT_max_mean, y = mean_value_log)) +
  geom_point() +
#  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "Vitamin A (ug)")
va_plot

zn_plot <- ggplot(zn_mean, aes(x = CT_max_mean, y = mean_value_log)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Estimated CTmax (˚C)", title = "Zinc (mg)")
zn_plot


plot1 <- ggarrange(sel_plot, zn_plot, va_plot, nrow = 1, ncol = 3, labels = c("c)", "d)", "e)"),  font.label = list(colour = "black", size = 14, family = "Times New Roman"))

plot2 <- ggarrange(ca_plot, fe_plot,nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))

micro_plot <-  ggarrange(plot2, plot1, nrow=2, ncol =1 )
micro_plot



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
