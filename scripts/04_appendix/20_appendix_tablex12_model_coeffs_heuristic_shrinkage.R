#####################################################################################

#Appendix

#Supplementary Table 12:

#Model coefficients

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(naniar)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
load("m4.RData")
load("m1_optimism.RData")
load("m2_optimism.RData")
load("m3_optimism.RData")
load("m4_optimism.RData")
#Complete case model data -------------------------------------------------------------
##Define variables in models12 & models3------------------------------------------------
### Models12 ------------------------------------------------------------------------
#Continuous variables
varlist_12 = c("AgeatDiagnosis",
               "bmi_model",
               "HbA1c_at_diagnosis_v1"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat_12 = c(
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  "famhisnoninsdiab",
  #"famhisauto",
  "num_anti"
)
all_vars_12 <- c(varlist_12, varlist_cat_12)
### Models3 ------------------------------------------------------------------------
#Continuous variables
varlist_3 = c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1",
              "T1DGRS2_z"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat_3 = c(
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  "famhisnoninsdiab",
  #"famhisauto",
  "num_anti"
)
all_vars_3 <- c(varlist_3, varlist_cat_3)
##Make complete case datasets ----------------------------------------------------------
SR_SRout_ccc<- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)
#Model12
SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
#Model3
SR_m3_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3))

#Heuristic shrinkage ----------------------------------------------------------------------
##Model 1 -----------------------------------------------------------------------------------
##Step 1: extract linear predictor (log odds), intercept and shrinkage factor
#Shrinkage factor = optimism-adjusted calibration slope
lp_full <- predict(m1)

intercept <- coef(m1)[1]
coefs <- coef(m1)
betas_orig <- coefs[-1]

lp_nocons <- lp_full - intercept

shrinkage <- mean_optimism_m1 %>%
  filter(`Performance metric` == "Slope") %>%
  dplyr::select(`Full:corrected`) %>%
  pull()
##Step 2: Apply shrinkage factor to beta coefficients (without intercept)
lp_nocons_shrunk <- shrinkage*lp_nocons
##Step 3: Re-estimate intercept with offset
glm_offset <- glm(SRoutcome ~offset(lp_nocons_shrunk), family = "binomial", data = SR_m12_data)
new_intercept <- coef(glm_offset)[1]
betas_shrunk <- shrinkage*betas_orig
##Step 4: Reconstruct final lp (with new intercept)
lp_final_shrunk <- lp_nocons_shrunk + new_intercept
##Step 5: Summarise and compare original vs shrunkedn LP
summary(lp_full)
summary(lp_final_shrunk)
coef_shrunk <- c("(Intercept)" = new_intercept, betas_shrunk)
print(coef_shrunk)

coefs_m1 <- as.data.frame(coef_shrunk)
coefs_m1$variable <- as.character(rownames(coefs_m1))

##Model 2 -----------------------------------------------------------------------------------
##Step 1: extract linear predictor (log odds), intercept and shrinkage factor
#Shrinkage factor = optimism-adjusted calibration slope
lp_full <- predict(m2)

intercept <- coef(m2)[1]
coefs <- coef(m2)
betas_orig <- coefs[-1]

lp_nocons <- lp_full - intercept

shrinkage <- mean_optimism_m2 %>%
  filter(`Performance metric` == "Slope") %>%
  dplyr::select(`Full:corrected`) %>%
  pull()
##Step 2: Apply shrinkage factor to beta coefficients (without intercept)
lp_nocons_shrunk <- shrinkage*lp_nocons
##Step 3: Re-estimate intercept with offset
glm_offset <- glm(SRoutcome ~offset(lp_nocons_shrunk), family = "binomial", data = SR_m12_data)
new_intercept <- coef(glm_offset)[1]
betas_shrunk <- shrinkage*betas_orig
##Step 4: Reconstruct final lp (with new intercept)
lp_final_shrunk <- lp_nocons_shrunk + new_intercept
##Step 5: Summarise and compare original vs shrunkedn LP
summary(lp_full)
summary(lp_final_shrunk)
coef_shrunk <- c("(Intercept)" = new_intercept, betas_shrunk)
print(coef_shrunk)

coefs_m2 <- as.data.frame(coef_shrunk)
coefs_m2$variable <- as.character(rownames(coefs_m2))

##Model 3 -----------------------------------------------------------------------------------
##Step 1: extract linear predictor (log odds), intercept and shrinkage factor
#Shrinkage factor = optimism-adjusted calibration slope
lp_full <- predict(m3)

intercept <- coef(m3)[1]
coefs <- coef(m3)
betas_orig <- coefs[-1]

lp_nocons <- lp_full - intercept

shrinkage <- mean_optimism_m3 %>%
  filter(`Performance metric` == "Slope") %>%
  dplyr::select(`Full:corrected`) %>%
  pull()
##Step 2: Apply shrinkage factor to beta coefficients (without intercept)
lp_nocons_shrunk <- shrinkage*lp_nocons
##Step 3: Re-estimate intercept with offset
glm_offset <- glm(SRoutcome ~offset(lp_nocons_shrunk), family = "binomial", data = SR_m3_data)
new_intercept <- coef(glm_offset)[1]
betas_shrunk <- shrinkage*betas_orig
##Step 4: Reconstruct final lp (with new intercept)
lp_final_shrunk <- lp_nocons_shrunk + new_intercept
##Step 5: Summarise and compare original vs shrunkedn LP
summary(lp_full)
summary(lp_final_shrunk)
coef_shrunk <- c("(Intercept)" = new_intercept, betas_shrunk)
print(coef_shrunk)

coefs_m3 <- as.data.frame(coef_shrunk)
coefs_m3$variable <- as.character(rownames(coefs_m3))


##Model 4 -----------------------------------------------------------------------------------
##Step 1: extract linear predictor (log odds), intercept and shrinkage factor
#Shrinkage factor = optimism-adjusted calibration slope
lp_full <- predict(m4)

intercept <- coef(m4)[1]
coefs <- coef(m4)
betas_orig <- coefs[-1]

lp_nocons <- lp_full - intercept

shrinkage <- mean_optimism_m4 %>%
  filter(`Performance metric` == "Slope") %>%
  dplyr::select(`Full:corrected`) %>%
  pull()
##Step 2: Apply shrinkage factor to beta coefficients (without intercept)
lp_nocons_shrunk <- shrinkage*lp_nocons
##Step 3: Re-estimate intercept with offset
glm_offset <- glm(SRoutcome ~offset(lp_nocons_shrunk), family = "binomial", data = SR_m3_data)
new_intercept <- coef(glm_offset)[1]
betas_shrunk <- shrinkage*betas_orig
##Step 4: Reconstruct final lp (with new intercept)
lp_final_shrunk <- lp_nocons_shrunk + new_intercept
##Step 5: Summarise and compare original vs shrunkedn LP
summary(lp_full)
summary(lp_final_shrunk)
coef_shrunk <- c("(Intercept)" = new_intercept, betas_shrunk)
print(coef_shrunk)

coefs_m4 <- as.data.frame(coef_shrunk)
coefs_m4$variable <- as.character(rownames(coefs_m4))


#Create table ----------------------------------------------------------------------------------
clean_and_order_coefs <- function(coefs, variable_order) {
  coefs %>%
    mutate(
      variable_clean = case_when(
        variable == "(Intercept).(Intercept)" ~ "Intercept",
        variable == "AgeatDiagnosis" ~ "Age at diagnosis (years)",
        variable == "bmi_model" ~ "BMI at diagnosis (kg/m2)",
        variable == "HbA1c_at_diagnosis_v1" ~ "HbA1c at diagnosis (mmol/mol)",
        variable == "Gender_v1Male" ~ "Male sex",
        variable == "famhisnoninsdiabYes" ~ "Parent history of non-insulin-treated diabetes",
        variable == "DKA1" ~ "Presence of DKA",
        variable == "Unintentional_weight_loss_v1Yes" ~ "Unintentional weight-loss",
        variable == "osmotic1" ~ "Presence of osmotic symptoms",
        #variable == "relevel(factor(Eth_5cat), ref = \\\"White\\\")South Asian" ~ "South Asian ethnicity",
        str_detect(variable, "relevel\\(factor\\(Eth_4cat\\), ref = \\\"White\\\"\\)South Asian") ~ "South Asian ethnicity",
        str_detect(variable, "relevel\\(factor\\(Eth_4cat\\), ref = \\\"White\\\"\\)Black") ~ "Black ethnicity",
        str_detect(variable, "relevel\\(factor\\(Eth_4cat\\), ref = \\\"White\\\"\\)Other/Mixed") ~ "Other/Mixed ethnicity",
        #str_detect(variable, "relevel\\(factor\\(Eth_5cat\\), ref = \\\"White\\\"\\)Mixed") ~ "Mixed ethnicity",
        variable == "autoimmune1" ~ "Presence of other autoimmune disease",
        variable == "num_anti1" ~ "Single positive antibody",
        variable == "num_anti2" ~ "Two positive antibodies",
        variable == "num_anti3" ~ "Three positive antibodies",
        variable == "T1DGRS2_z" ~ "T1D GRS",
        TRUE ~ variable
      )
    ) %>%
    mutate(variable_clean = factor(variable_clean, levels = variable_order)) %>%
    arrange(variable_clean)
}

# Define the desired variable order
variable_order <- c(
  "Intercept",
  "Age at diagnosis (years)",
  "BMI at diagnosis (kg/m2)",
  "HbA1c at diagnosis (mmol/mol)",
  "Male sex",
  "Parent history of non-insulin-treated diabetes",
  "Presence of DKA",
  "Unintentional weight-loss",
  "Presence of osmotic symptoms",
  "Black ethnicity",
  #"Mixed ethnicity",
  "Other/Mixed ethnicity",
  "South Asian ethnicity",
  "Presence of other autoimmune disease",
  "Single positive antibody",
  "Two positive antibodies",
  "Three positive antibodies",
  "T1D GRS"
)

# Clean and order coefficients for each model
coefs_ordered_m1 <- clean_and_order_coefs(coefs_m1, variable_order)
coefs_ordered_m2 <- clean_and_order_coefs(coefs_m2, variable_order)
coefs_ordered_m3 <- clean_and_order_coefs(coefs_m3, variable_order)
coefs_ordered_m4 <- clean_and_order_coefs(coefs_m4, variable_order)

# Create coefficient tables
create_coef_table <- function(coefs_ordered, model_number) {
  beta_col_name <- paste0("Model ", model_number, " Beta coefficient")

  coefs_ordered %>%
    #filter(var_type != "break") %>%
    dplyr::select(variable_clean, coef_shrunk) %>%
    rename(
      `Model Feature` = variable_clean,
      !!beta_col_name := coef_shrunk,
    )
}

coef_table_m1 <- create_coef_table(coefs_ordered_m1, 1)
coef_table_m2 <- create_coef_table(coefs_ordered_m2, 2)
coef_table_m3 <- create_coef_table(coefs_ordered_m3, 3)
coef_table_m4 <- create_coef_table(coefs_ordered_m4, 4)

# Combine all coefficient tables
model_coef_table <- coef_table_m3 %>%
  left_join(coef_table_m2, by = "Model Feature") %>%
  left_join(coef_table_m1, by = "Model Feature") %>%
  left_join(coef_table_m4, by = "Model Feature")
# Reorder columns so that Model 1 columns come first, followed by Model 2, Model 3, and Model 4
model_coef_table <- model_coef_table %>%
  dplyr::select(
    `Model Feature`,
    `Model 1 Beta coefficient`,
    `Model 2 Beta coefficient`,
    `Model 3 Beta coefficient`,
    `Model 4 Beta coefficient`
  )

# Write to Excel
write_xlsx(model_coef_table, "tables/Supp_Table12_Model_coeffs_shrunk.xlsx")
########################################################################################
#How to I translate this back into a glm object?
########################################################################################
# m1_shrunk <- update(m1)
# m1_shrunk <- glm(SRoutcome ~offset(lp_nocons_shrunk), family = "binomial", data = SR_m12_data)
# summary(m1_shrunk)
