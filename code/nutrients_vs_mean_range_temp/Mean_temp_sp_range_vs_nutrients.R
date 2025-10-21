##Comparing mean temperature across range and CTmax -- then comparing mean temp vs. nutrient conditions 

##Nov 22, 2024

library(tidyverse)
library(readxl)
library(data.table)
library(ggplot2)


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
temp_mean_sd <- read.csv("data/processed_data/All_sp_temp_mean_sd_HadISST_FW.csv") %>%
  dplyr::select(sci_name:sd_temp)



##2) Import all nutrient data -- with outliers removed (see Testing_Nutrient_Data_Outliers_Extremes.R)
nutrient_df <- read.csv("data/processed_data/Nutrient_data_all_outliers_removed.csv") %>%
  filter(!body_part %in% c("liver", "egg", "esophagus", "skin", "viscera")) %>%
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
  left_join(temp_mean_sd, by = "sci_name")

test <- ctmax_nutrient_df_mean %>%
  dplyr::select(sci_name) %>%
  unique()

hist(ctmax_nutrient_df_mean$CT_max_mean)

hist(ctmax_df$TL_p_fw_mean)
hist(ctmax_nutrient_df_mean$CT_max_sd)
#write.csv(ctmax_nutrient_df_mean, "data/processed_data/GL_mean_ctmax_nutrient_df.csv")

###Testing relationship between mean CTmax estimates and mean temp of range
ggplot(ctmax_nutrient_df_mean, aes(x = mean_temp, y = CT_max_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Mean Temperature of Species Range (˚C)") +
  ylab("Mean CTmax (˚C)")


##Now just join mean temp data and nutrients
ctmax_nutrient_df_mean <- inner_join(nutrients_mean, temp_mean_sd, by = "sci_name") %>%
  inner_join(sp_all, by = "sci_name") %>%
  mutate(mean_temp_log = log10(mean_temp))

sp_mean_temp_nutrients <- ctmax_nutrient_df_mean %>%
  dplyr::select(sci_name) %>%
  unique()

##Relationships b/w mean temp ranges and other potential variables
ggplot(ctmax_nutrient_df_mean, aes(y = Length, x = mean_temp)) + 
  geom_point()

ggplot(ctmax_nutrient_df_mean, aes(y = CommonLength, x = mean_temp)) + 
  geom_point()

ggplot(ctmax_nutrient_df_mean, aes(y = FoodTroph, x = mean_temp)) + 
  geom_point()

ggplot(ctmax_nutrient_df_mean, aes(y = FoodTroph, x = CommonLength)) + 
  geom_point()

hist(ctmax_nutrient_df_mean$mean_temp_log)

##3) Multiple Linear Regressions -- using mean values ===========

##3.1) All Possible Species ------------------

#Plotting Relationships
gl_all_plot_2 <- ctmax_nutrient_df_mean %>%
  ggplot(aes(x = mean_temp, y = log(mean_value))) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  #  geom_errorbar(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), width = .2, position = position_dodge(0.05)) + 
  facet_wrap(~Nutrient_Name, scales = "free") +
  theme_classic() +
  labs(y = "Mean Nutrient Value", x = "Mean Temperature of Species Range (˚C)", title =  "Mean Sp. Range Temp vs. Nutrient Content - All Possible Species")
gl_all_plot_2

gl_all_plot_3 <- ctmax_nutrient_df_mean %>%
  ggplot(aes(x = mean_temp, y = mean_value)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  #  geom_errorbar(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), width = .2, position = position_dodge(0.05)) + 
  facet_wrap(~Nutrient_Name, scales = "free") +
  theme_classic() +
  labs(y = "Mean Nutrient Value", x = "Mean Temperature of Species Range (˚C)", title =  "Mean Sp. Range Temp vs. Nutrient Content - All Possible Species")
gl_all_plot_3


##Simple linear regressions 
ca_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Calcium (mg)") %>%
  mutate(mean_value_log = log(mean_value))
ca_lm <- lm(log(mean_value) ~ mean_temp+ Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = ca_mean)
summary(ca_lm)
ca_lm2 <- lm(log(mean_value) ~ mean_temp, data = ca_mean)
summary(ca_lm2)
par(mfrow = c(2, 2))
plot(ca_lm)

dha_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "DHA (g)")
dha_lm <- lm(log(mean_value) ~ mean_temp + Length + FoodTroph+ Fresh + Brack + Saltwater + LongevityWild, data = dha_mean)
summary(dha_lm)
dha_lm2 <- lm(log(mean_value) ~ mean_temp, data = dha_mean)
summary(dha_lm2)
par(mfrow = c(2, 2))
plot(dha_lm)

epa_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "EPA (g)")
epa_lm <- lm(log(mean_value) ~ mean_temp + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = epa_mean)
summary(epa_lm) 
epa_lm2 <- lm(log(mean_value) ~ mean_temp, data = epa_mean)
summary(epa_lm2)
#par(mfrow = c(2, 2))
#plot(epa_lm)

fe_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Iron (mg)") %>%
  filter(mean_value < 1000)
fe_lm <- lm(log(mean_value) ~ mean_temp + Length + FoodTroph+ Fresh + Brack + Saltwater + LongevityWild, data = fe_mean)
summary(fe_lm)
fe_lm2 <- lm(log(mean_value) ~ mean_temp, data = fe_mean)
summary(fe_lm2)
#par(mfrow = c(2, 2))
#plot(fe_lm)

omg3_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Omega 3 FA (g)")
omg3_lm <- lm(mean_value ~ mean_temp + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = omg3_mean)
summary(omg3_lm)
omg3_lm2 <- lm(log(mean_value) ~ mean_temp, data = omg3_mean)
summary(omg3_lm2)
#par(mfrow = c(2, 2))
#plot(omg3_lm)

pro_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Protein (g)") %>%
  filter(mean_value < 60)
pro_lm <- lm(mean_value ~ mean_temp + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = pro_mean)
summary(pro_lm)
pro_lm2 <- lm(mean_value ~ mean_temp, data = pro_mean)
summary(pro_lm2)
#par(mfrow = c(2, 2))
#plot(pro_lm)

pufa_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "PUFA (g)")
pufa_lm <- lm(mean_value ~ mean_temp + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = pufa_mean)
summary(pufa_lm)
pufa_lm <- lm(mean_value ~ mean_temp, data = pufa_mean)
summary(pufa_lm)
#par(mfrow = c(2, 2))
#plot(pufa_lm)

sel_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Selenium (ug)")
sel_lm <- lm(log(mean_value) ~ mean_temp + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = sel_mean)
summary(sel_lm)
sel_lm2 <- lm(log(mean_value) ~ mean_temp, data = sel_mean)
summary(sel_lm2)
#par(mfrow = c(2, 2))
#plot(sel_lm)

fat_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Total Fat (g)")
fat_lm <- lm(log(mean_value) ~ mean_temp + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = fat_mean)
summary(fat_lm)
fat_lm2 <- lm(log(mean_value) ~ mean_temp, data = fat_mean)
summary(fat_lm2)
#par(mfrow = c(2, 2))
#plot(fat_lm)

zn_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Zinc (mg)")
zn_lm <- lm(log(mean_value) ~ mean_temp + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = zn_mean)
summary(zn_lm)
zn_lm2 <- lm(log(mean_value) ~ mean_temp, data = zn_mean)
summary(zn_lm2)
#par(mfrow = c(2, 2))
#plot(zn_lm)

va_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Vitamin A (ug)")
va_lm <- lm(log(mean_value) ~ mean_temp + Length + FoodTroph + Fresh + Brack + Saltwater + LongevityWild, data = va_mean)
summary(va_lm)
va_lm2 <- lm(log(mean_value) ~ mean_temp, data = va_mean)
summary(va_lm2)
#par(mfrow = c(2, 2))
#plot(va_lm)


###Playing around with potential figures for MS
pro_plot <- ggplot(pro_mean, aes(x = mean_temp, y = mean_value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value", x = "Mean Temperature of Species Range (˚C)", title = "Protein (g)")
pro_plot

fat_plot <- ggplot(fat_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
 # geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "Total Fat (g)")
fat_plot

epa_plot <- ggplot(epa_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "EPA (g)")
epa_plot

dha_plot <- ggplot(dha_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "DHA (g)")
dha_plot

omg3_plot <- ggplot(omg3_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "Omega3 (g)")
omg3_plot


plot1 <- ggarrange(omg3_plot, epa_plot, dha_plot, nrow = 1, ncol = 3, labels = c("c)", "d)", "e)"),  font.label = list(colour = "black", size = 14, family = "Times New Roman"))

plot2 <- ggarrange(pro_plot, fat_plot,nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))

macro_plot <-  ggarrange(plot2, plot1, nrow=2, ncol =1 )
macro_plot


##micronutrient plot 
ca_plot <- ggplot(ca_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "Calcium (mg)")
ca_plot


fe_plot <- ggplot(fe_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "Iron (mg)")
fe_plot

sel_plot <- ggplot(sel_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
#  geom_smooth(method = "lm", linetype = "dashed") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "Selenium (ug)")
sel_plot

va_plot <- ggplot(va_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
    geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "Vitamin A (ug)")
va_plot

zn_plot <- ggplot(zn_mean, aes(x = mean_temp, y = log(mean_value))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(y = "Mean Nutrient Value (log)", x = "Mean Temperature of Species Range (˚C)", title = "Zinc (mg)")
zn_plot


plot1 <- ggarrange(sel_plot, zn_plot, va_plot, nrow = 1, ncol = 3, labels = c("c)", "d)", "e)"),  font.label = list(colour = "black", size = 14, family = "Times New Roman"))

plot2 <- ggarrange(ca_plot, fe_plot,nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))

micro_plot <-  ggarrange(plot2, plot1, nrow=2, ncol =1 )
micro_plot






####MEAN TEMP VS ALL NUTRIENTs --- trouble. shooting 

##All nutrient values vs mean temp -- looking at differences in values from different sources
nutrients_mean_temp <- left_join(nutrient_df, temp_mean_sd, by = "sci_name") 


gl_all_plot_2 <- nutrients_mean_temp %>%
  ggplot(aes(x = mean_temp, y = Value, group = source_df, color = source_df)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  #  geom_errorbar(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value), width = .2, position = position_dodge(0.05)) + 
  facet_wrap(~Nutrient_Name, scales = "free") +
  theme_classic() +
  labs(y = "Nutrient Value", x = "Mean Temp across Species Range")
gl_all_plot_2


##Simple linear regressions 
ca_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Calcium (mg)")
ca_lm <- lm(mean_value ~ lat, data = ca_mean)
summary(ca_lm)
par(mfrow = c(2, 2))
#plot(ca_lm)

dha_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "DHA (g)")
dha_lm <- lm(mean_value ~ lat, data = dha_mean)
summary(dha_lm)
par(mfrow = c(2, 2))
#plot(dha_lm)

epa_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "EPA (g)")
epa_lm <- lm(mean_value ~ lat, data = epa_mean)
summary(epa_lm)
par(mfrow = c(2, 2))
#plot(epa_lm)

fe_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Iron (mg)")
fe_lm <- lm(mean_value ~ lat, data = fe_mean)
summary(fe_lm)
par(mfrow = c(2, 2))
#plot(fe_lm)

omg3_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Omega 3 FA (g)")
omg3_lm <- lm(mean_value ~ lat, data = omg3_mean)
summary(omg3_lm)
par(mfrow = c(2, 2))
#plot(omg3_lm)

pro_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Protein (g)")
pro_lm <- lm(mean_value ~ lat, data = pro_mean)
summary(pro_lm)
par(mfrow = c(2, 2))
#plot(pro_lm)

pufa_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "PUFA (g)")
pufa_lm <- lm(mean_value ~ lat, data = pufa_mean)
summary(pufa_lm)
par(mfrow = c(2, 2))
#plot(pufa_lm)

sel_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Selenium (ug)")
sel_lm <- lm(mean_value ~ lat, data = sel_mean)
summary(sel_lm)
par(mfrow = c(2, 2))
#plot(sel_lm)

fat_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Total Fat (g)")
fat_lm <- lm(mean_value ~ lat, data = fat_mean)
summary(fat_lm)
par(mfrow = c(2, 2))
#plot(fat_lm)

zn_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Zinc (mg)")
zn_lm <- lm(mean_value ~ lat, data = zn_mean)
summary(zn_lm)
par(mfrow = c(2, 2))
#plot(zn_lm)

va_mean <- ctmax_nutrient_df_mean %>%
  filter(Nutrient_Name == "Vitamin A")
va_lm <- lm(mean_value ~ lat, data = va_mean)
summary(va_lm)
par(mfrow = c(2, 2))

