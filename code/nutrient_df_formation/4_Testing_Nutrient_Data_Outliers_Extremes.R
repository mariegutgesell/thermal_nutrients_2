##Testing nutrient conversions -- determining outliers/weird values


library(ggplot2)
library(tidyverse)

##Read in nutrient data
nutrients <- read.csv("data/processed_data/Nutrient_data_all.csv") %>%
 # filter(!body_part %in% c("liver", "egg", "esophagus", "skin", "viscera", "eggs", "oil", "unknown")) %>%
  filter(!is.na(Value)) %>%
  dplyr::select(source_df:Nutrient_Name)

nutrients <- nutrients %>%
  mutate(Unit_converted = case_when(
    startsWith(Nutrient, "iron") ~ "mg/100g",
    startsWith(Nutrient, "FE") ~ "mg/100g",
    startsWith(Nutrient, "zinc") ~ "mg/100g",
    startsWith(Nutrient, "ZN") ~ "mg/100g",
    startsWith(Nutrient, "calc") ~ "mg/100g",
    startsWith(Nutrient, "CA") ~ "mg/100g",
    startsWith(Nutrient, "sele") ~ "ug/100g",
    startsWith(Nutrient, "SE") ~ "ug/100g",
    startsWith(Nutrient, "pufa") ~ "g/100g",
    startsWith(Nutrient, "FAPUN") ~ "g/100g",
    startsWith(Nutrient, "mean") ~ "g/100g",
    startsWith(Nutrient, "omega") ~ "g/100g",
    startsWith(Nutrient, "fa_epa") ~ "g/100g",
    startsWith(Nutrient, "F20") ~ "g/100g",
    startsWith(Nutrient, "F22") ~ "g/100g",
    startsWith(Nutrient, "fa_dha") ~ "g/100g",
    startsWith(Nutrient, "protein") ~ "g/100g",
    startsWith(Nutrient, "Protein") ~ "g/100g",
    startsWith(Nutrient, "FAC") ~ "g/100g",
    startsWith(Nutrient, "Vit") ~ "ug/100g",
  ))  %>%
  mutate(body_part_2 = case_when(
    startsWith(body_part, "cleaned_parts") ~ "muscle (with and without organs)",
    startsWith(body_part, "edible_portion")~ "muscle (with and without organs)",
    startsWith(body_part, "muscle")~ "muscle (with and without organs)",
    startsWith(body_part, "muscle_organs")~ "muscle (with and without organs)",
    startsWith(body_part, "muscle_skinless")~ "muscle (with and without organs)",
    startsWith(body_part, "eggs")~ "eggs",
    startsWith(body_part, "liver")~ "liver",
    startsWith(body_part, "oil")~ "oil",
    startsWith(body_part, "skin")~ "skin",
    startsWith(body_part, "whole")~ "whole",
    startsWith(body_part, "unknown")~ "unknown",
    startsWith(body_part, "viscera")~ "viscera",
    startsWith(body_part, "esophagus")~ "esophagus",
    is.na(body_part)~ "unknown",
  ))
  

test <- nutrients %>%
  filter(is.na(Value))

test2 <- nutrients %>%
  filter(is.na(body_part_2))

va <- nutrients %>%
  filter(Nutrient == "Vitamin_A")

##outlier filtering version 1 (not using)
nutrients_all_noout <- nutrients %>%
  mutate(outlier_calcium = ifelse(Nutrient_Name == "Calcium (mg)" & Value > 200, "outlier", "no")) %>%
  mutate(outlier_iron = ifelse(Nutrient_Name == "Iron (mg)" & Value > 10, "outlier", "no")) %>%
  mutate(outlier_selenium = ifelse(Nutrient_Name == "Selenium (ug)" & Value > 200, "outlier", "no"))  %>%
  mutate(outlier_dha = ifelse(Nutrient_Name == "DHA (g)" & Value > 5, "outlier", "no")) %>%
  mutate(outlier_epa = ifelse(Nutrient_Name == "EPA (g)" & Value > 5, "outlier", "no")) %>%
  mutate(outlier_protein = ifelse(Nutrient_Name == "Protein (g)" & Value < 5, "outlier", "no")) %>%
  mutate(outlier_vitA = ifelse(Nutrient_Name == "Vitamin A (ug)" & Value < 30000, "outlier", "no")) %>%  ##values should only be this high if like a concetrated liver or something.. 
  filter(outlier_calcium == "no" & outlier_iron == "no" & outlier_selenium == "no" & outlier_epa == "no" & outlier_dha == "no", outlier_protein == "no")

nutrients_all_noout$sci_name <- gsub("  +", " ", nutrients_all_noout$sci_name)

nutrients_all_noout <- nutrients_all_noout %>%
  unique()

##trying outliers by removing any values outside of the FAO/INFOODS range for finfish - filtering within 1 order of magnitude of FAO range 
nutrients_all_noout_2 <- nutrients %>%
  mutate(outlier_calcium = ifelse(Nutrient_Name == "Calcium (mg)" & Value > 1000, "outlier", "no")) %>%
  mutate(outlier_calcium2 = ifelse(Nutrient_Name == "Calcium (mg)" & Value < 0.00001, "outlier", "no")) %>%
  mutate(outlier_iron = ifelse(Nutrient_Name == "Iron (mg)" & Value > 100, "outlier", "no")) %>%
  mutate(outlier_iron2 = ifelse(Nutrient_Name == "Iron (mg)" & Value < 0.1, "outlier", "no")) %>%
  mutate(outlier_selenium = ifelse(Nutrient_Name == "Selenium (ug)" & Value > 1000, "outlier", "no"))  %>%
  mutate(outlier_selenium2 = ifelse(Nutrient_Name == "Selenium (ug)" & Value < 1, "outlier", "no"))  %>%
  mutate(outlier_dha = ifelse(Nutrient_Name == "DHA (g)" & Value > 10, "outlier", "no")) %>%
  mutate(outlier_dha2 = ifelse(Nutrient_Name == "DHA (g)" & Value < 0.001, "outlier", "no")) %>%
  mutate(outlier_epa = ifelse(Nutrient_Name == "EPA (g)" & Value > 10, "outlier", "no")) %>%
  mutate(outlier_epa2 = ifelse(Nutrient_Name == "EPA (g)" & Value < 0.01, "outlier", "no")) %>%
  mutate(outlier_pufa = ifelse(Nutrient_Name == "PUFA (g)" & Value < 0.01, "outlier", "no")) %>%
  mutate(outlier_pufa2 = ifelse(Nutrient_Name == "PUFA (g)" & Value > 10, "outlier", "no")) %>%
  mutate(outlier_protein1 = ifelse(Nutrient_Name == "Protein (g)" & Value < 1, "outlier", "no")) %>% 
  mutate(outlier_protein2 = ifelse(Nutrient_Name == "Protein (g)" & Value > 100, "outlier", "no")) %>% 
  mutate(outlier_zinc = ifelse(Nutrient_Name == "Zinc (mg)" & Value < 0.01, "outlier", "no")) %>%  ## one order of magnitude outside of FAO range 
  mutate(outlier_zinc1 = ifelse(Nutrient_Name == "Zinc (mg)" & Value > 100, "outlier", "no")) %>% 
  mutate(outlier_vitA = ifelse(Nutrient_Name == "Vitamin A (ug)" & Value > 1000, "outlier", "no")) %>% 
  mutate(outlier_vitA2 = ifelse(Nutrient_Name == "Vitamin A (ug)" & Value < 1, "outlier", "no")) %>% ##values should only be this high if like a concetrated liver or something.. 
  filter(outlier_calcium == "no" & outlier_iron == "no" & outlier_selenium == "no" & outlier_epa == "no" & outlier_dha == "no", outlier_protein1 == "no", outlier_protein2 == "no", outlier_zinc == "no", outlier_vitA == "no", outlier_selenium2 == "no", outlier_zinc1 == "no",
         outlier_calcium2 == "no", outlier_dha2 == "no", outlier_epa2 == "no",  outlier_pufa == "no", outlier_pufa2 == "no", outlier_vitA2 == "no", outlier_iron2 == "no"
      )

nutrients_all_noout_2$sci_name <- gsub("  +", " ", nutrients_all_noout_2$sci_name)

write.csv(nutrients_all_noout_2, "data/processed_data/Nutrient_data_all_outliers_removed.csv")

##df of outliers (same as above, just keeps the outliers)
outliers <- nutrients %>%
  mutate(outlier_calcium = ifelse(Nutrient_Name == "Calcium (mg)" & Value > 1000, "outlier", "no")) %>%
  mutate(outlier_calcium2 = ifelse(Nutrient_Name == "Calcium (mg)" & Value < 0.00001, "outlier", "no")) %>%
  mutate(outlier_iron = ifelse(Nutrient_Name == "Iron (mg)" & Value > 100, "outlier", "no")) %>%
  mutate(outlier_iron2 = ifelse(Nutrient_Name == "Iron (mg)" & Value < 0.1, "outlier", "no")) %>%
  mutate(outlier_selenium = ifelse(Nutrient_Name == "Selenium (ug)" & Value > 1000, "outlier", "no"))  %>%
  mutate(outlier_selenium2 = ifelse(Nutrient_Name == "Selenium (ug)" & Value < 1, "outlier", "no"))  %>%
  mutate(outlier_dha = ifelse(Nutrient_Name == "DHA (g)" & Value > 10, "outlier", "no")) %>%
  mutate(outlier_dha2 = ifelse(Nutrient_Name == "DHA (g)" & Value < 0.001, "outlier", "no")) %>%
  mutate(outlier_epa = ifelse(Nutrient_Name == "EPA (g)" & Value > 10, "outlier", "no")) %>%
  mutate(outlier_epa2 = ifelse(Nutrient_Name == "EPA (g)" & Value < 0.01, "outlier", "no")) %>%
  mutate(outlier_pufa = ifelse(Nutrient_Name == "PUFA (g)" & Value < 0.01, "outlier", "no")) %>%
  mutate(outlier_pufa2 = ifelse(Nutrient_Name == "PUFA (g)" & Value > 10, "outlier", "no")) %>%
  mutate(outlier_protein1 = ifelse(Nutrient_Name == "Protein (g)" & Value < 1, "outlier", "no")) %>% 
  mutate(outlier_protein2 = ifelse(Nutrient_Name == "Protein (g)" & Value > 100, "outlier", "no")) %>% 
  mutate(outlier_zinc = ifelse(Nutrient_Name == "Zinc (mg)" & Value < 0.01, "outlier", "no")) %>%  ## one order of magnitude outside of FAO range 
  mutate(outlier_zinc1 = ifelse(Nutrient_Name == "Zinc (mg)" & Value > 100, "outlier", "no")) %>% 
  mutate(outlier_vitA = ifelse(Nutrient_Name == "Vitamin A (ug)" & Value > 1000, "outlier", "no")) %>% 
  mutate(outlier_vitA2 = ifelse(Nutrient_Name == "Vitamin A (ug)" & Value < 1, "outlier", "no")) %>% ##values should only be this high if like a concetrated liver or something.. 
  filter(outlier_calcium == "outlier" | outlier_iron == "outlier" | outlier_selenium == "outlier" | outlier_epa == "outlier" | outlier_dha == "outlier" | outlier_protein1 == "outlier" | outlier_protein2 == "outlier" | outlier_zinc == "outlier" | outlier_vitA == "outlier" | outlier_selenium2 == "outlier" | outlier_zinc1 == "outlier" |
         outlier_calcium2 == "outlier"| outlier_dha2 == "outlier"| outlier_epa2 == "outlier"|  outlier_pufa == "outlier"| outlier_pufa2 == "outlier"| outlier_vitA2 == "outlier" |outlier_iron2 == "outlier"
  )

test <- anti_join(nutrients, nutrients_all_noout_2, by = "sample_id")
#nutrients_all$SampleForm <- toupper(nutrients_all$SampleForm)
nutrients$Value <- as.numeric(nutrients$Value)
nutrients_all_noout$Value <- as.numeric(nutrients_all_noout$Value)

##Influence of Tissue Type on Nutritional Content
ggplot(nutrients, aes(x = source_df, y = Value, group = body_part, color = body_part)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~Nutrient_Name, scale = "free")

##Largest variation is within tissue type..so i think it is a conversion issue, not a body part issue
ggplot(nutrients, aes(x = body_part, y = Value, group = body_part)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Nutrient Value Converted") +
  facet_wrap(~Nutrient_Name, scale = "free")

ggplot(nutrients_all_noout, aes(x = body_part, y = Value, group = body_part)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Nutrient Value Converted") +
  facet_wrap(~Nutrient_Name, scale = "free")

ggplot(nutrients_all_noout_2, aes(x = body_part_2, y = Value, group = body_part_2)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Nutrient Value Converted") +
  facet_wrap(~Nutrient_Name, scale = "free")

##Influence of original unit on Nutritional content
ggplot(nutrients, aes(x = source_df, y = Value, group = Unit_converted, color = Unit_converted)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~Nutrient_Name, scale = "free")

ggplot(nutrients, aes(x = Unit_converted, y = Value, group = Unit_converted)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Nutrient Value Converted") +
  facet_wrap(~Nutrient_Name, scale = "free")

##Calcium
nutrients %>%
  filter(Nutrient_Name == "Calcium (mg)") %>%
  ggplot(aes(x = body_part, y = Value, group = body_part)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~source_df, scale = "free")

##DHA
nutrients_all_noout %>%
  filter(Nutrient_Name == "DHA (g)") %>%
  ggplot(aes(x = SampleForm, y = Value, group = SampleForm)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~Source_df, scale = "free")

##selenium
nutrients_all %>%
filter(Nutrient_Name == "Selenium (ug)") %>%
  ggplot(aes(x = SampleForm, y = Value, group = SampleForm)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~Source_df, scale = "free")

sel <- nutrients_all %>%
  filter(Nutrient_Name == "Selenium (ug)")

zn <- nutrients_all_noout_2 %>%
  filter(Nutrient_Name == "Zinc (mg)")

fe <- nutrients 
##From GL_Nutient_Data -- change glfnd_converted_2 to nutrients lit 
