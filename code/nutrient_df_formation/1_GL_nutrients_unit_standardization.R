##Standardize name of units in nutrient data gathered from the lit 
#August 25th 2023, -- updated April 10, 2024
##Ijeoma Nwafor, Marie Gutgesell 

#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("readxl")
#install.packages("dplyr")

##Load packages
library(tidyverse)
library(janitor)
library(readxl)
library(dplyr)


#1) Import raw nutrient data -- check it is latest version
glfnd <- read.csv("data/raw_data/Great_Lakes_Fish_Nutrient_Data_March_13_2024.csv") %>%
  clean_names()# %>%
  #view()
##remove first row that only has header descriptions, no data
glfnd <- glfnd[-c(1),]

#2) Standardize Unit Names
##Iron
glfnd <- glfnd %>% 
  mutate(iron_new_unit_name = case_when(
    iron_unit == 'mg/100g' ~ 'mg/100g',
    iron_unit == 'mg/100 g' ~ 'mg/100g',
    iron_unit == 'μmol (Se/kg)' ~ 'umol',
    iron_unit == 'ppm' ~ 'ppm',
    iron_unit == 'μg/100 g ww' ~ 'ug/100g',
    iron_unit == 'ppm ww' ~ 'ppm',
    iron_unit == 'µg g-1' ~ 'ug/g',
    iron_unit == 'mg/100 g ww' ~ 'mg/100g',
    iron_unit == 'ng/g ww' ~ 'ng/g',
    iron_unit == 'ug/g' ~ 'ug/g',
    iron_unit == 'μg/g ww' ~ 'ug/g',
    iron_unit == 'mg/kg' ~ 'mg/kg',
    iron_unit == '(ppm, wet weight) (μg/g)' ~ 'ppm',
    iron_unit == 'ppb' ~ 'ppb',
    iron_unit == 'ug/g ww' ~ 'ug/g',
    iron_unit == 'μg g–1 wet weight' ~ 'ug/g',
    iron_unit == 'ppb ww' ~ 'ppb',
    iron_unit == 'ug/g ww' ~ 'ug/g',
   TRUE ~ iron_unit #keep unchanged values
  )
  )

##Selenium
glfnd <- glfnd %>% 
  mutate(selenium_new_unit_name = case_when(
    selenium_unit == 'mg/100g' ~ 'mg/100g',
    selenium_unit == 'μmol (Se/kg)' ~ 'umol',
    selenium_unit == 'ppm' ~ 'ppm',
    selenium_unit == 'μg/100 g ww' ~ 'ug/100g',
    selenium_unit == 'ppm ww' ~ 'ppm',
    selenium_unit == 'µg g-1' ~ 'ug/g',
    selenium_unit == 'mg/100 g ww' ~ 'mg/100g',
    selenium_unit == 'ng/g ww' ~ 'ng/g',
    selenium_unit == 'ug/g' ~ 'ug/g',
    selenium_unit == 'μg/g ww' ~ 'ug/g',
    selenium_unit == 'mg/kg' ~ 'mg/kg',
    selenium_unit == '(ppm, wet weight) (μg/g)' ~ 'ppm',
    selenium_unit == 'ppb' ~ 'ppb',
    selenium_unit == 'ug/g ww' ~ 'ug/g',
    selenium_unit == 'μg g–1 wet weight' ~ 'ug/g',
    selenium_unit == 'ppb ww' ~ 'ppb',
    selenium_unit == 'ug/g ww' ~ 'ug/g',
    selenium_unit ==  'μg g−1' ~ 'ug/g',
    selenium_unit ==  'μg/g' ~ 'ug/g',
    TRUE ~ selenium_unit #keep unchanged values'
  )
  ) 

##Zinc
glfnd <- glfnd %>% 
  mutate(zinc_new_unit_name = case_when(
    zinc_unit == 'mg/100g' ~ 'mg/100g',
    zinc_unit == 'mg/100 g' ~ 'mg/100g',
    zinc_unit == 'μmol (Se/kg)' ~ 'umol',
    zinc_unit == 'ppm' ~ 'ppm',
    zinc_unit == 'μg/100 g ww' ~ 'ug/100g',
    zinc_unit == 'ppm ww' ~ 'ppm',
    zinc_unit == 'µg g-1' ~ 'ug/g',
    zinc_unit == 'mg/100 g ww' ~ 'mg/100g',
    zinc_unit == 'ng/g ww' ~ 'ng/g',
    zinc_unit == 'ug/g' ~ 'ug/g',
    zinc_unit == 'μg/g ww' ~ 'ug/g',
    zinc_unit == 'μg/g' ~ 'ug/g',
    zinc_unit == 'mg/kg' ~ 'mg/kg',
    zinc_unit == '(ppm, wet weight) (μg/g)' ~ 'ppm',
    zinc_unit == 'ppb' ~ 'ppb',
    zinc_unit == 'ug/g ww' ~ 'ug/g',
    zinc_unit == 'μg g–1 wet weight' ~ 'ug/g',
    zinc_unit == 'ppb ww' ~ 'ppb',
    zinc_unit == 'ug/g ww' ~ 'ug/g',
    TRUE ~ zinc_unit #keep unchanged values
  )
  )

##Calcium 
glfnd <- glfnd %>% 
  mutate(calcium_new_unit = case_when(
    calcium_unit == 'mg/100g' ~ 'mg/100g',
    calcium_unit == 'μmol (Se/kg)' ~ 'umol',
    calcium_unit == 'ppm' ~ 'ppm',
    calcium_unit == 'μg/100 g ww' ~ 'ug/100g',
    calcium_unit == 'ppm ww' ~ 'ppm',
    calcium_unit == 'µg g-1' ~ 'ug/g',
    calcium_unit == 'mg/100 g ww' ~ 'mg/100g',
    calcium_unit == 'ng/g ww' ~ 'ng/g',
    calcium_unit == 'ug/g' ~ 'ug/g',
    calcium_unit == 'μg/g ww' ~ 'ug/g',
    calcium_unit == 'mg/kg' ~ 'mg/kg',
    calcium_unit == '(ppm, wet weight) (μg/g)' ~ 'ppm',
    calcium_unit == 'ppb' ~ 'ppb',
    calcium_unit == 'ug/g ww' ~ 'ug/g',
    calcium_unit == 'μg g–1 wet weight' ~ 'ug/g',
    calcium_unit == 'ppb ww' ~ 'ppb',
    calcium_unit == 'ug/g ww' ~ 'ug/g',
    TRUE ~ calcium_unit #keep unchanged values
  )
  )

##Protein 
glfnd <- glfnd %>% 
  mutate(protein_new_unit = case_when(
    protein_unit == ('g/100 g fresh raw sample') ~ 'g/100g',
    protein_unit == ('mg / g wet wt') ~ 'mg/g',
    protein_unit == ('proximate composition') ~ '%',
    protein_unit == ('g/100g edible portion') ~ 'g/100g',
    protein_unit == ('% mean') ~ '%',
    protein_unit == ("%") ~ '%',
    protein_unit == ("g/100g") ~ 'g/100g',
    protein_unit == ("g/100 g") ~ 'g/100g',
    TRUE ~ protein_unit #keep unchanged values
    )
  )

##Fat
## didnt do pdf_52
glfnd <- glfnd %>% 
  mutate(fat_new_unit = case_when(
    total_fat_data_unit == ('g/100g') ~ 'g/100g',
    total_fat_data_unit == ('(mg fatty acid/100 g fish).') ~ 'mg/100g',
    total_fat_data_unit == ('percentage (inc. protein dat)') ~ '%',
    total_fat_data_unit == ('mg/100 g') ~ 'mg/100g',
    total_fat_data_unit == ('Total Fat g/100 g fish') ~ 'g/100g',
    total_fat_data_unit == ('g lipid/100 g fish') ~ 'g/100g',
    total_fat_data_unit == ('g/100 g ww') ~ 'g/100g',
    total_fat_data_unit == ('% composition ww') ~ '%',
    total_fat_data_unit == ('% ww') ~ '%',
    total_fat_data_unit == ('% of freeze dried tissue') ~ '%',
    total_fat_data_unit == ('(g/100 g)') ~ 'g/100g',
    total_fat_data_unit == ('percent') ~ '%',
    total_fat_data_unit == ('g/100 g') ~ 'g/100g',
    total_fat_data_unit == ('%') ~ '%',
    total_fat_data_unit == ('g/100 g fresh raw sample') ~ 'g/100g',
    total_fat_data_unit == ('mg/100 g') ~ 'mg/100g',
    total_fat_data_unit == ('g/100 g') ~ 'g/100g',
    total_fat_data_unit == ('g/100g body') ~ 'g/100g',
    total_fat_data_unit == ('g/100 g serving') ~ 'g/100g',
    total_fat_data_unit == ('g/100 g fillet') ~ 'g/100g',
    total_fat_data_unit == ('g/100g') ~ 'g/100g',
    total_fat_data_unit == ('g/100 g ww') ~ 'g/100g',
    total_fat_data_unit == ('g/100g edible portion') ~ 'g/100g',
    total_fat_data_unit == ('% lipid') ~ '%',
    total_fat_data_unit == ('% mean, lipid') ~ '%',
    total_fat_data_unit == ('% wet mass') ~ '%',
    total_fat_data_unit == ('proximate composition') ~ '%',
    #pdf52 total_fat_data_unit == ('% Dry mass in muscle') ~ '%',
    TRUE ~ total_fat_data_unit #keep unchanged values
  )
  )

# EPA
glfnd <- glfnd %>%
  mutate(epa_new_unit = case_when(
    fa_epa_unit == ('g/100g') ~ 'g/100g',
    fa_epa_unit == ('(mg fatty acid/100 g fish).') ~ 'mg/100g',
    fa_epa_unit == ('(mg 100 g1).') ~ 'mg/100g',
    fa_epa_unit == ('mg/kg wet weight fish tissue') ~ 'mg/kg',
    fa_epa_unit == ('mg per100 g of fish sample, wet weight,') ~ 'mg/100g',
    fa_epa_unit == ('mg/100 g') ~ 'mg/100g',
    fa_epa_unit == ('g/100 g') ~ 'g/100g',
    fa_epa_unit == ('g/100 g serving') ~ 'g/100g',
    fa_epa_unit == ('mg/100 g') ~ 'mg/100g',
    fa_epa_unit == ('% of fa detected') ~ '%_fa',
    fa_epa_unit == ('g/100 g fatty acids') ~ 'g/100g_fa',
    fa_epa_unit == ('mg/100 g fish') ~ 'mg/100g',
    fa_epa_unit == ('mg/g fish') ~ 'mg/g',
    fa_epa_unit == ('% of total fatty acids') ~ '%_fa',
    fa_epa_unit == ('mg/100 g w.w') ~ 'mg/100g',
    fa_epa_unit == ('relative percentages') ~ '%_fa',
    fa_epa_unit == ('% of total fatty acids') ~ '%_fa',
    fa_epa_unit == ('μg/mg') ~ 'ug/mg',
    fa_epa_unit == ('µg/mg freeze dried whole body tissue') ~ 'ug/mg',
    fa_epa_unit == ('μg per mg tissue dry weight') ~ 'ug/mg',
    fa_epa_unit == ('(mg fatty acid/100 g raw tissue') ~ 'mg/100g',
    fa_epa_unit == ('concentration (relative concentration)') ~ '%_fa',
    fa_epa_unit == ('% detected') ~ '%',
    fa_epa_unit == ('w/weight %') ~ '%_fa',
    fa_epa_unit == ('weight percent of total fatty acids') ~ '%_fa',
    fa_epa_unit == ('mg/100g') ~ 'mg/100g',
    fa_epa_unit == ('g/100g ww') ~ 'g/100g',
    fa_epa_unit == ('mg/100g') ~ 'mg/100g',
    fa_epa_unit == ('wt% of total FA') ~ '%_fa',
    fa_epa_unit == ('g/100 g serving') ~ 'g/100g',
    fa_epa_unit == ('wt %') ~ '%',
    fa_epa_unit == ('mg/100g fish') ~ 'mg/100g',
    fa_epa_unit == ('g/100g FA') ~ 'g/100g_fa',
    fa_epa_unit == ('Relative percent fatty acid composition of total fat') ~ '%_fat',
    fa_epa_unit == ('g/100 g lipid') ~ 'g/100g_fat',
    fa_epa_unit == ('mg/100g') ~ 'mg/100g',
    fa_epa_unit == ('%') ~ '%',
    fa_epa_unit == ('% wet mass') ~ '%',
    TRUE ~ fa_epa_unit
  )
  )

#DHA
glfnd <- glfnd %>%
  mutate(dha_new_unit = case_when(
    fa_dha_unit == ('g/100g') ~ 'g/100g',
    fa_dha_unit == ('(mg fatty acid/100 g fish).') ~ 'mg/100g',
    fa_dha_unit == ('(mg 100 g1).') ~ 'mg/100g',
    fa_dha_unit == ('mg/kg wet weight fish tissue') ~ 'mg/kg',
    fa_dha_unit == ('mg per100 g of fish sample, wet weight,') ~ 'mg/100g',
    fa_dha_unit == ('mg/100 g') ~ 'mg/100g',
    fa_dha_unit == ('g/100 g') ~ 'g/100g',
    fa_dha_unit == ('g/100 g serving') ~ 'g/100g',
    fa_dha_unit == ('mg/100 g') ~ 'mg/100g',
    fa_dha_unit == ('% of fa detected') ~ '%_fa',
    fa_dha_unit == ('g/100 g fatty acids') ~ 'g/100g_fa',
    fa_dha_unit == ('mg/100 g fish') ~ 'mg/100g',
    fa_dha_unit == ('mg/g fish') ~ 'mg/g',
    fa_dha_unit == ('% of total fatty acids') ~ '%_fa',
    fa_dha_unit == ('mg/100 g w.w') ~ 'mg/100g',
    fa_dha_unit == ('relative percentages') ~ '%_fa',
    fa_dha_unit == ('% of total fatty acids') ~ '%_fa',
    fa_dha_unit == ('μg/mg') ~ 'ug/mg',
    fa_dha_unit == ('µg/mg freeze dried whole body tissue') ~ 'ug/mg',
    fa_dha_unit == ('μg per mg tissue dry weight') ~ 'ug/mg',
    fa_dha_unit == ('(mg fatty acid/100 g raw tissue') ~ 'mg/100g',
    fa_dha_unit == ('concentration (relative concentration)') ~ '%_fa',
    fa_dha_unit == ('% detected') ~ '%',
    fa_dha_unit == ('w/weight %') ~ '%_fa',
    fa_dha_unit == ('weight percent of total fatty acids') ~ '%_fa',
    fa_dha_unit == ('mg/100g') ~ 'mg/100g',
    fa_dha_unit == ('g/100g ww') ~ 'g/100g',
    fa_dha_unit == ('mg/100g') ~ 'mg/100g',
    fa_dha_unit == ('wt% of total FA') ~ '%_fa',
    fa_dha_unit == ('g/100 g serving') ~ 'g/100g',
    fa_dha_unit == ('wt %') ~ '%',
    fa_dha_unit == ('mg/100g fish') ~ 'mg/100g',
    fa_dha_unit == ('g/100g FA') ~ 'g/100g_fa',
    fa_dha_unit == ('Relative percent fatty acid composition of total fat') ~ '%_fat',
    fa_dha_unit == ('g/100 g lipid') ~ 'g/100g_fat',
    fa_dha_unit == ('mg/100g') ~ 'mg/100g',
    fa_dha_unit == ('%') ~ '%',
    fa_dha_unit == ('% wet mass') ~ '%',
    TRUE ~ fa_dha_unit
  )
  )


#Omega 3
glfnd <- glfnd %>%
  mutate(omega3_new_unit = case_when(
    omega_3_fa_unit == ('(mg fatty acid/100 g fish).') ~ 'mg/100g',
    omega_3_fa_unit == ('g/100g') ~ 'g/100g',
    omega_3_fa_unit == ('mg/100g') ~ 'mg/100g',
    omega_3_fa_unit == ('mg/100 g') ~ 'mg/100g',
    omega_3_fa_unit == ('mg/100 g') ~ 'mg/100g',
    omega_3_fa_unit == ('g/100 g') ~ 'g/100g',
    omega_3_fa_unit == ('% of total fat content') ~ '%_fat',
    omega_3_fa_unit == ('mg/100 g') ~ 'mg/100g',
    omega_3_fa_unit == ('g/100 g fatty acids') ~ 'g/100g_fa',
    omega_3_fa_unit == ('mg/100 g fish') ~ 'mg/100g',
    omega_3_fa_unit == ('g per 100 g lipid in fish') ~ 'g/100g_fat',
    omega_3_fa_unit == ('percentage of total fatty acids detected') ~ '%_fa',
    omega_3_fa_unit == ('µg/mg freeze dried whole body tissue') ~ 'ug/mg',
    omega_3_fa_unit == ('(mg/100 g)') ~ 'mg/100g',
    omega_3_fa_unit == ('ug·mg dry mass−1') ~ 'ug/mg',
    omega_3_fa_unit == ('weight percent of total fatty acids') ~ '%_fa',
    omega_3_fa_unit == ('mg/100g ww') ~ 'mg/100g',
    omega_3_fa_unit == ('mg . 100 g-1') ~ 'mg/100g',
    omega_3_fa_unit == ('mg/100g') ~ 'mg/100g',
    omega_3_fa_unit == ('g/100g') ~ 'g/100g',
    omega_3_fa_unit == ('mg/g body weight') ~ 'mg/g',
    omega_3_fa_unit == ('g/100g') ~ 'g/100g',
    omega_3_fa_unit == ('% of total fat content') ~ '%_fat',
    omega_3_fa_unit == ('wt %') ~ '%',
    omega_3_fa_unit == ('g/100g') ~ 'g/100g',
    omega_3_fa_unit == ('mg/100g') ~ 'mg/100g',
    omega_3_fa_unit == ('g/100 g ww') ~ 'g/100g',
    omega_3_fa_unit == ('mg/100g') ~ 'mg/100g',
    omega_3_fa_unit == ('%') ~ '%',
    omega_3_fa_unit == ('(wt% of total FA)') ~ '%_fa',
  TRUE ~ omega_3_fa_unit
)
)

#View(glfnd)

#PUFAS
glfnd <- glfnd %>%
  mutate(pufa_new_unit = case_when(
    pufa_units == ('values for 100-g edible portion') ~ 'g/100g',
    pufa_units == ('g/100g') ~ 'g/100g',
    TRUE ~ pufa_units
  ))

