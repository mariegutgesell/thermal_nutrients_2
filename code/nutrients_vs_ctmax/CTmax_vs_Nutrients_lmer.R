##CTmax estimates vs. Nutrient Content -- for All possible species with nutrient + thermal data - LMER 
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
library(lme4)
library(lmerTest)
##1) Import the CTmax data estimated for acclimation for species specific ranges (see Allsp_NLMEM_CTmax_predicitons.R code for how these predicted CTmax were generated)
ctmax <- read.csv("data/processed_data/CTmax_fw_sst_est_summer_mean_sprange_ALL.csv") %>%
  dplyr::rename(sci_name = "Species_GL")

##Need to select only 1 methodology for CTmax estimates - select dynamic if present
dyn <- ctmax %>%
  filter(Methodology == "dynamic")

static <- ctmax %>%
  filter(Methodology == "static") %>%
  filter(!sci_name %in% dyn$sci_name)

ctmax_df <- rbind(dyn, static) %>%
  select(sci_name, TL_p_fw_mean)

head(ctmax_df)
##2) Import all nutrient data -- with outliers removed (see Testing_Nutrient_Data_Outliers_Extremes.R)
nutrient_df <- read.csv("data/processed_data/Nutrient_data_all_outliers_removed.csv") %>%
  # filter(!body_part %in% c("liver", "egg", "esophagus", "skin", "viscera")) %>%
  filter(!grepl("spp", sci_name)) %>%
  filter(str_count(sci_name, "\\S+") == 2) %>%
  separate(sci_name, into = c("Genus", "Species"), remove = TRUE) %>%
  unite("sci_name", Genus, Species, sep = " ") %>%
  separate(sci_name, into = c("Genus", "Species"), remove = FALSE) %>%
  unique() %>%
  select(sci_name, body_part_2, Nutrient_Name, Value)
nutrient_df$Value <- as.numeric(nutrient_df$Value)

test <- nutrient_df %>%
  dplyr::select(body_part_2) %>%
  distinct()

head(nutrient_df)
##3) Import species list 
sp_all <- read.csv("data/species_list/master_sp_list_clean.csv") 



##convert 3 columns (fresh, brackish and salt) into a single column factor
##1 = fresh, 1.5 = fresh+brackish, 2 = brackish, 2.5 = brackish+salt, 3 = salt, 4 = fresh+brackish+salt 
sp_all_traits <- sp_all %>%
  mutate(habitat_score = case_when(
    Fresh == 1 & Brack == 0 & Saltwater == 0 ~ 1,
    Fresh == 1 & Brack == 1 & Saltwater == 0 ~ 1.5,
    Fresh == 0 & Brack == 1 & Saltwater == 0 ~ 2,
    Fresh == 0 & Brack == 1 & Saltwater == 1 ~ 2.5,
    Fresh == 0 & Brack == 0 & Saltwater == 1 ~ 3,
    Fresh == 1 & Brack == 1 & Saltwater == 1 ~ 4,
    TRUE ~ NA_real_
  )) %>%
  mutate(habitat_specific = case_when(
    Fresh == 1 & Brack == 0 & Saltwater == 0 ~ "freshwater",
    Fresh == 1 & Brack == 1 & Saltwater == 0 ~ "freshwater_brackish",
    Fresh == 0 & Brack == 1 & Saltwater == 0 ~ "brackish",
    Fresh == 0 & Brack == 1 & Saltwater == 1 ~ "brackish_salt",
    Fresh == 0 & Brack == 0 & Saltwater == 1 ~ "saltwater",
    Fresh == 1 & Brack == 1 & Saltwater == 1 ~ "freshwater_brackish_saltwater",
  )) %>%
  mutate(habitat = case_when(
    Fresh == 1 & Brack == 0 & Saltwater == 0 ~ "freshwater",
    Fresh == 1 & Brack == 1 & Saltwater == 0 ~ "freshwater",
    Fresh == 0 & Brack == 1 & Saltwater == 0 ~ "brackish",
    Fresh == 0 & Brack == 1 & Saltwater == 1 ~ "saltwater",
    Fresh == 0 & Brack == 0 & Saltwater == 1 ~ "saltwater",
    Fresh == 1 & Brack == 1 & Saltwater == 1 ~ "freshwater_brackish_saltwater",
  )) %>%
  group_by(sci_name, habitat) %>%
  select(sci_name, LongevityWild, Vulnerability, Length, CommonLength, FoodTroph, K_mean, habitat)

head(sp_all_traits)


##Data is hierarchical in structure: 
#Level 1: individual measurements (nutrient, ctmax)
#Level 2: species-level traits

##Calculate means for traits and ctmax, so have species level predictors 
ctmax_sp <- ctmax_df %>%
  group_by(sci_name) %>%
  summarise(CTmax = mean(TL_p_fw_mean, na.rm = TRUE))

traits_sp <- sp_all_traits %>%
  group_by(sci_name) %>%
  summarise(
    LongevityWild = mean(LongevityWild, na.rm = TRUE),
   # Vulnerability = mean(Vulnerability, na.rm = TRUE),
    Length = mean(Length, na.rm = TRUE),
  #  CommonLength = mean(CommonLength, na.rm = TRUE),
    FoodTroph = mean(FoodTroph, na.rm = TRUE),
    K_mean = mean(K_mean, na.rm = TRUE),
    habitat = first(habitat)   # if categorical
  )

##keep individual level nutrient values but means across species for predictor variables
analysis_df <- nutrient_df %>%
  left_join(ctmax_sp, by = "sci_name") %>%
  left_join(traits_sp, by = "sci_name")



##some imputed values missing for species they had thermal data for? that is odd.. but is true
##for 6 of the ones we have ctmax estimate for, don't have an imputed CTmax 

##see if have metabolic rate or growth rate in fishbase , diet pathway (benthic, pelagic etc.) - max length vs. mean length? 

##explanatory variables: CTmax, methodology, habitat, CommonLength, LongevityWild, Vulnerability, FoodTroph, K_mean, body_part_2


##Make correlation plot of numerical explanatory variables
exp_var <- analysis_df %>%
  ungroup() %>%
  select(CTmax, Length, LongevityWild, FoodTroph, K_mean) %>%
  unique()
str(exp_var)

corr_mat <- cor(exp_var, use = "pairwise.complete.obs")

# visualize
corrplot(corr_mat, method = "color", type = "upper",  addCoef.col = "black",
         tl.col = "black", tl.srt = 45)

##don't have significant correlations -- only choose one of common length or max length 


###Hierarchical Linear Mixed Effects Model -- constrained to models w/ ctmax---------------------------
options(na.action = "na.fail")

lmer_function <- function(x, nutrient) {
  df <- x %>%
    filter(Nutrient_Name == nutrient) %>%
    na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
   mutate(across(c(CTmax, Length,  LongevityWild,  FoodTroph, K_mean),
                ~ scale(.x)[,1]))%>%
    filter(body_part_2 %in% c("muscle (with and without organs)", "whole"))

  ##full model
  lmer_full <- lmer(log(Value) ~ CTmax + Length + LongevityWild + K_mean + habitat+  (1|sci_name), data = df, REML = FALSE)
  summary(lmer_full)

  ##run AIC on all models using dredging approach -- compares all possible nest models constrained to those w/ CTmax
  model_set <- dredge(lmer_full, subset = ~ CTmax)
  model_set 

  ##potential averagin approach: 
 # avg_mod <- model.avg(model_set, subset = delta < 2)
#  summary(avg_mod)

  ##select the best model - don't just do this if more than 1 good model 
  top_models <- subset(model_set, delta < 2)
  top_models
  # index of the most parsimonious model (smallest df)
  best_idx <- which.min(top_models$df)

  # get all models in the ΔAIC < 2 set:
  top_model_list <- get.models(model_set, subset = delta < 2)

  # pick the most parsimonious one:
  best_model <- top_model_list[[best_idx]]
  best_model_summary <- summary(best_model)
  best_model


  v <- visreg(best_model, "CTmax", partial = TRUE, plot = FALSE)
  plot_sig <- ggplot() +
  geom_line(data = v$fit, aes(x = CTmax, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = CTmax, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
 
  plot_marginal_sig <- ggplot() +
    geom_line(data = v$fit, aes(x = CTmax, y = visregFit), color = "red", linewidth = 1, linetype = "dashed") +
    geom_ribbon(data = v$fit, aes(x = CTmax, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
    geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
    theme_classic() +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
    labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
  plot_no_sig <- ggplot() +
    geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
    theme_classic() +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
    labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
  return(list(
    data = df,
    model_set = model_set,
    top_models = top_models,
    best_model = best_model,
    best_model_summary = best_model_summary,
    plot_sig = plot_sig,
    plot_marginal_sig = plot_marginal_sig,
    plot_no_sig = plot_no_sig
  ))

}



dha = lmer_function(analysis_df, "DHA (g)")
dha$best_model_summary ##56 sp
dha_plot <- dha$plot_sig
dha_plot

epa = lmer_function(analysis_df, "EPA (g)")
epa$best_model_summary ##56 sp
epa_plot <- epa$plot_sig
epa_plot

pro = lmer_function(analysis_df, "Protein (g)")
pro$best_model_summary ##64 sp
pro_plot <- pro$plot_sig
pro_plot


fat = lmer_function(analysis_df, "Total Fat (g)")
fat$best_model_summary ##63 ind
fat_plot <- fat$plot_no_sig
fat_plot


calcium = lmer_function(analysis_df, "Calcium (mg)")
calcium$best_model_summary
cal_plot <- calcium$plot_sig
cal_plot

fe= lmer_function(analysis_df, "Iron (mg)")
fe$best_model_summary
fe_plot <- fe$plot_sig
fe_plot


zn= lmer_function(analysis_df, "Zinc (mg)")
zn$best_model_summary
zn_plot <- zn$plot_sig
zn_plot

sel= lmer_function(analysis_df, "Selenium (ug)")
sel$best_model_summary
sel_plot <- sel$plot_sig
sel_plot


va= lmer_function(analysis_df, "Vitamin A (ug)")
va$best_model_summary
va_plot <- va$plot_no_sig
va_plot

###Playing around with potential figures for MS

macro_plot <- ggarrange(pro_plot, fat_plot, epa_plot, dha_plot, nrow = 2, ncol = 2, labels = c("a)", "b)", "c)", "d)"), font.label = list(colour = "black", size = 14))
macro_plot

plot1 <- ggarrange(sel_plot, zn_plot, va_plot, nrow = 1, ncol = 3, labels = c("c)", "d)", "e)"),  font.label = list(colour = "black", size = 14))
plot2 <- ggarrange(cal_plot, fe_plot,nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14))

micro_plot <-  ggarrange(plot2, plot1, nrow=2, ncol =1 )
micro_plot


##Make summary table of results 
extract_lmer_summary <- function(model, response_name) {
  smry <- summary(model)
  
  # AIC and residual df
  aic_val  <- round(AIC(model), 2)
  df_resid <- df.residual(model)
  
  # Marginal and conditional R2 (if MuMIn is available)
  R2_m <- R2_c <- NA_real_
  if (requireNamespace("MuMIn", quietly = TRUE)) {
    r2_vals <- MuMIn::r.squaredGLMM(model)
    R2_m <- r2_vals[1, "R2m"]
    R2_c <- r2_vals[1, "R2c"]
  }
  
  # Fixed-effects table
  coefs <- as.data.frame(smry$coefficients)
  coefs$Term <- rownames(coefs)
  rownames(coefs) <- NULL
  
  # If there is no p-value column (plain lme4), approximate from t-values
  if (!"Pr(>|t|)" %in% names(coefs)) {
    if ("t value" %in% names(coefs)) {
      coefs$`Pr(>|t|)` <- 2 * (1 - pnorm(abs(coefs$`t value`)))
    } else if ("t-value" %in% names(coefs)) {
      coefs$`Pr(>|t|)` <- 2 * (1 - pnorm(abs(coefs$`t-value`)))
    } else {
      coefs$`Pr(>|t|)` <- NA_real_
    }
  }
  
  fixed <- coefs %>%
    dplyr::transmute(
      Response       = response_name,
      Term           = Term,
      Estimate       = round(Estimate, 3),
      Std_Error      = round(`Std. Error`, 3),
      Stat_Value     = round(`t value`, 3),
      P_Value        = round(`Pr(>|t|)`, 3),
      R2_marginal    = "",
      R2_conditional = "",
      AIC            = "",
      DF_resid       = ""
    )
  
  # Put model-level metrics in the first row
  fixed$R2_marginal[1]    <- if (!is.na(R2_m)) round(R2_m, 3) else ""
  fixed$R2_conditional[1] <- if (!is.na(R2_c)) round(R2_c, 3) else ""
  fixed$AIC[1]            <- aic_val
  fixed$DF_resid[1]       <- df_resid
  
  # Convert everything to character for easy binding / CSV export
  fixed <- fixed %>%
    dplyr::mutate(across(everything(), as.character))
  
  fixed
}


models_lmer <- list(
  Protein = pro$best_model,
  TotalFat = fat$best_model,
  EPA     = epa$best_model,
  DHA     = dha$best_model,
  Calcium = calcium$best_model,
  Iron = fe$best_model,
  Zinc    = zn$best_model,
  Selenium = sel$best_model,
  VitaminA = va$best_model
  
  
)
models_lmer


library(purrr)
library(dplyr)

lmer_results <- map_dfr(names(models_lmer), function(nm) {
  extract_lmer_summary(models_lmer[[nm]], response_name = nm)
})

# Optional: replace NA/blank with "" for clean CSV
lmer_results_clean <- lmer_results %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

# Write to CSV
write.csv(lmer_results_clean, "tables/lmer_best_models_summary_ctmax.csv", row.names = FALSE)


###Hierarchical Linear Mixed Effects Model -- not constrained to models w/ ctmax---------------------------
options(na.action = "na.fail")

lmer_function_unconstrained <- function(x, nutrient) {
  df <- x %>%
    filter(Nutrient_Name == nutrient) %>%
    na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
    mutate(across(c(CTmax, Length,  LongevityWild,  FoodTroph, K_mean),
                  ~ scale(.x)[,1]))%>%
    filter(body_part_2 %in% c("muscle (with and without organs)", "whole"))
  
  ##full model
  lmer_full <- lmer(log(Value) ~ CTmax + Length + LongevityWild + K_mean + habitat+  (1|sci_name), data = df, REML = FALSE)
  summary(lmer_full)
  
  ##run AIC on all models using dredging approach -- compares all possible nest models constrained to those w/ CTmax
  model_set <- dredge(lmer_full)
  model_set 
  
  ##potential averagin approach: 
  # avg_mod <- model.avg(model_set, subset = delta < 2)
  #  summary(avg_mod)
  
  ##select the best model - don't just do this if more than 1 good model 
  top_models <- subset(model_set, delta < 2)
  top_models
  # index of the most parsimonious model (smallest df)
  best_idx <- which.min(top_models$df)
  
  # get all models in the ΔAIC < 2 set:
  top_model_list <- get.models(model_set, subset = delta < 2)
  
  # pick the most parsimonious one:
  best_model <- top_model_list[[best_idx]]
  best_model_summary <- summary(best_model)
  best_model
  
  
 # v <- visreg(best_model, "CTmax", partial = TRUE, plot = FALSE)
#  plot_sig <- ggplot() +
  #  geom_line(data = v$fit, aes(x = CTmax, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = CTmax, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
   # geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
  #  theme_classic() +
  #  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
   # labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
  
 # plot_marginal_sig <- ggplot() +
  #  geom_line(data = v$fit, aes(x = CTmax, y = visregFit), color = "red", linewidth = 1, linetype = "dashed") +
  #  geom_ribbon(data = v$fit, aes(x = CTmax, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  #  geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
  #  theme_classic() +
  #  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  #  labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
  #plot_no_sig <- ggplot() +
   # geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
  #  theme_classic() +
   # theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  #  labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
  return(list(
    data = df,
    model_set = model_set,
    top_models = top_models,
    best_model = best_model,
    best_model_summary = best_model_summary
   # plot_sig = plot_sig,
  #  plot_marginal_sig = plot_marginal_sig,
   # plot_no_sig = plot_no_sig
  ))
  
}



dha = lmer_function_unconstrained(analysis_df, "DHA (g)")
dha$best_model_summary ##56 sp
#dha_plot <- dha$plot_sig
#dha_plot

epa = lmer_function_unconstrained(analysis_df, "EPA (g)")
epa$best_model_summary ##56 sp
#epa_plot <- epa$plot_sig
#epa_plot

pro = lmer_function_unconstrained(analysis_df, "Protein (g)")
pro$best_model_summary ##64 sp
#pro_plot <- pro$plot_sig
#pro_plot


fat = lmer_function_unconstrained(analysis_df, "Total Fat (g)")
fat$best_model_summary ##63 ind
#fat_plot <- fat$plot_no_sig
#fat_plot


calcium = lmer_function_unconstrained(analysis_df, "Calcium (mg)")
calcium$best_model_summary
#cal_plot <- calcium$plot_sig
#cal_plot

fe= lmer_function_unconstrained(analysis_df, "Iron (mg)")
fe$best_model_summary
#fe_plot <- fe$plot_sig
#fe_plot


zn= lmer_function_unconstrained(analysis_df, "Zinc (mg)")
zn$best_model_summary
#zn_plot <- zn$plot_sig
#zn_plot

sel= lmer_function_unconstrained(analysis_df, "Selenium (ug)")
sel$best_model_summary
#sel_plot <- sel$plot_sig
#sel_plot


va= lmer_function_unconstrained(analysis_df, "Vitamin A (ug)")
va$best_model_summary
#va_plot <- va$plot_no_sig
#va_plot



##Make summary table of results 
extract_lmer_summary <- function(model, response_name) {
  smry <- summary(model)
  
  # AIC and residual df
  aic_val  <- round(AIC(model), 2)
  df_resid <- df.residual(model)
  
  # Marginal and conditional R2 (if MuMIn is available)
  R2_m <- R2_c <- NA_real_
  if (requireNamespace("MuMIn", quietly = TRUE)) {
    r2_vals <- MuMIn::r.squaredGLMM(model)
    R2_m <- r2_vals[1, "R2m"]
    R2_c <- r2_vals[1, "R2c"]
  }
  
  # Fixed-effects table
  coefs <- as.data.frame(smry$coefficients)
  coefs$Term <- rownames(coefs)
  rownames(coefs) <- NULL
  
  # If there is no p-value column (plain lme4), approximate from t-values
  if (!"Pr(>|t|)" %in% names(coefs)) {
    if ("t value" %in% names(coefs)) {
      coefs$`Pr(>|t|)` <- 2 * (1 - pnorm(abs(coefs$`t value`)))
    } else if ("t-value" %in% names(coefs)) {
      coefs$`Pr(>|t|)` <- 2 * (1 - pnorm(abs(coefs$`t-value`)))
    } else {
      coefs$`Pr(>|t|)` <- NA_real_
    }
  }
  
  fixed <- coefs %>%
    dplyr::transmute(
      Response       = response_name,
      Term           = Term,
      Estimate       = round(Estimate, 3),
      Std_Error      = round(`Std. Error`, 3),
      Stat_Value     = round(`t value`, 3),
      P_Value        = round(`Pr(>|t|)`, 3),
      R2_marginal    = "",
      R2_conditional = "",
      AIC            = "",
      DF_resid       = ""
    )
  
  # Put model-level metrics in the first row
  fixed$R2_marginal[1]    <- if (!is.na(R2_m)) round(R2_m, 3) else ""
  fixed$R2_conditional[1] <- if (!is.na(R2_c)) round(R2_c, 3) else ""
  fixed$AIC[1]            <- aic_val
  fixed$DF_resid[1]       <- df_resid
  
  # Convert everything to character for easy binding / CSV export
  fixed <- fixed %>%
    dplyr::mutate(across(everything(), as.character))
  
  fixed
}


models_lmer <- list(
  Protein = pro$best_model,
  TotalFat = fat$best_model,
  EPA     = epa$best_model,
  DHA     = dha$best_model,
  Calcium = calcium$best_model,
  Iron = fe$best_model,
  Zinc    = zn$best_model,
  Selenium = sel$best_model,
  VitaminA = va$best_model
  
  
)
models_lmer


library(purrr)
library(dplyr)

lmer_results <- map_dfr(names(models_lmer), function(nm) {
  extract_lmer_summary(models_lmer[[nm]], response_name = nm)
})

# Optional: replace NA/blank with "" for clean CSV
lmer_results_clean <- lmer_results %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

# Write to CSV
write.csv(lmer_results_clean, "tables/lmer_best_models_summary_ctmax_unconstrained.csv", row.names = FALSE)



###Hierarchical Linear Mixed Effects Model -- ONLY FOR CTMAX AND NUTRIENTS ----------------------

options(na.action = "na.fail")

lmer_function_ctmax_only <- function(x, nutrient) {
  df <- x %>%
    filter(Nutrient_Name == nutrient) %>%
    dplyr::select(sci_name, CTmax, body_part_2, Nutrient_Name, Value) %>%
    mutate(across(c(CTmax),
                  ~ scale(.x)[,1]))%>%
    na.omit() %>%##need to run model where all predictor variables are present, this does reduce 
    filter(body_part_2 %in% c("muscle (with and without organs)", "whole"))
  
  ##full model
  lmer_full <- lmer(log(Value) ~ CTmax + (1|sci_name), data = df, REML = FALSE)
  lmer_full_summary <- summary(lmer_full)

  
  v <- visreg(lmer_full, "CTmax", partial = TRUE, plot = FALSE)
  plot_sig <- ggplot() +
    geom_line(data = v$fit, aes(x = CTmax, y = visregFit), color = "red", linewidth = 1) +
    geom_ribbon(data = v$fit, aes(x = CTmax, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
    geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
    theme_classic() +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
    labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
  
  plot_marginal_sig <- ggplot() +
    geom_line(data = v$fit, aes(x = CTmax, y = visregFit), color = "red", linewidth = 1, linetype = "dashed") +
    geom_ribbon(data = v$fit, aes(x = CTmax, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
    geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
    theme_classic() +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
    labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
  plot_no_sig <- ggplot() +
    geom_point(data = v$res, aes(x = CTmax, y = visregRes), size = 2) +
    theme_classic() +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
    labs(x = "Mean Estimated CTmax (˚C)", y = paste("Log", nutrient))
  
  return(list(
    data = df,
    lmer_full_summary = lmer_full_summary,
    plot_sig = plot_sig,
    plot_marginal_sig = plot_marginal_sig,
    plot_no_sig = plot_no_sig
  ))
  
}



dha = lmer_function_ctmax_only(analysis_df, "DHA (g)")
dha$lmer_full_summary ##68 ind
dha_plot <- dha$plot_sig
dha_plot

epa = lmer_function_ctmax_only(analysis_df, "EPA (g)")
epa$lmer_full_summary ##68 ind
epa_plot <- epa$plot_sig
epa_plot

pro = lmer_function_ctmax_only(analysis_df, "Protein (g)")
pro$lmer_full_summary ##93 sp
pro_plot <- pro$plot_sig
pro_plot


fat = lmer_function_ctmax_only(analysis_df, "Total Fat (g)")
fat$lmer_full_summary ##81 sp
fat_plot <- fat$plot_sig
fat_plot


calcium = lmer_function_ctmax_only(analysis_df, "Calcium (mg)")
calcium$lmer_full_summary ##73 sp
cal_plot <- calcium$plot_sig
cal_plot

fe= lmer_function_ctmax_only(analysis_df, "Iron (mg)")
fe$lmer_full_summary ##76 sp
fe_plot <- fe$plot_sig
fe_plot


zn= lmer_function_ctmax_only(analysis_df, "Zinc (mg)")
zn$lmer_full_summary ##73 sp
zn_plot <- zn$plot_sig
zn_plot

sel= lmer_function_ctmax_only(analysis_df, "Selenium (ug)")
sel$lmer_full_summary ##61 sp
sel_plot <- sel$plot_sig
sel_plot


va= lmer_function_ctmax_only(analysis_df, "Vitamin A (ug)")
va$lmer_full_summary ##24 sp
va_plot <- va$plot_no_sig
va_plot

###Playing around with potential figures for MS

macro_plot <- ggarrange(pro_plot, fat_plot, epa_plot, dha_plot, nrow = 2, ncol = 2, labels = c("a)", "b)", "c)", "d)"), font.label = list(colour = "black", size = 14))
macro_plot

plot1 <- ggarrange(sel_plot, zn_plot, va_plot, nrow = 1, ncol = 3, labels = c("c)", "d)", "e)"),  font.label = list(colour = "black", size = 14))
plot2 <- ggarrange(cal_plot, fe_plot,nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14))

micro_plot <-  ggarrange(plot2, plot1, nrow=2, ncol =1 )
micro_plot










###TESTING BAYESIAN HIERARCHICAL MODEL THAT PRESERVES VARIATION IN CTMAX AS WELL 
## nutrient rows: have log_nutrient, but CTmax is NA
nutrient_long <- nutrient_df %>%
  left_join(traits_sp, by = "sci_name") %>%
  mutate(
    log_nutrient = log(Value),
    CTmax        = NA_real_,       # not observed here
    resp_id      = "nutrient"
  )

## ctmax rows: have CTmax, but log_nutrient is NA
ctmax_long <- ctmax_df %>%
  left_join(traits_sp, by = "sci_name") %>%
  mutate(
    CTmax        = TL_p_fw_mean,
    log_nutrient = NA_real_,       # not observed here
    body_part_2  = NA_character_,  # not used for CTmax rows
    Nutrient_Name= NA_character_,
    resp_id      = "ctmax"
  )

combined_df <- bind_rows(nutrient_long, ctmax_long)



library(brms)

## Formula for CTmax (response 1)
bf_ctmax <- bf(
  CTmax ~ 1 + (1 | sci_name),
  family = gaussian()
)

## Formula for log nutrient (response 2)
bf_nutrient <- bf(
  log_nutrient ~ 1 + body_part_2 + habitat + Length + LongevityWild + K_mean +
    (1 | sci_name),
  family = gaussian()
)

## Fit joint model
fit_mv <- brm(
  bf_ctmax + bf_nutrient + set_rescor(FALSE),  # no residual correlation; RE can still correlate
  data   = combined_df,
  cores  = 4,
  chains = 4,
  iter   = 4000,
  control = list(adapt_delta = 0.95)
)
