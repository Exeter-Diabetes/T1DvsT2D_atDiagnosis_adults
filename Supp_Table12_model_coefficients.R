#####################################################################################

#Paper figures

#Supplementary Table 14

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
# ##Models -------------------------------------------------------------------------------
# ###Model 1
# m1 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
#             scale(bmi_model) +
#             scale(HbA1c_at_diagnosis_v1) +
#             #scale(c_peptide_v1) +
#             Gender_v1 +
#             osmotic +
#             autoimmune +
#             Unintentional_weight_loss_v1 +
#             DKA +
#             famhisnoninsdiab,
#           #famhisauto,
#           data = SR_m12_data,
#           family = binomial)
# save(m1, file = "m1.RData")
#
# m1_alt <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
#                 scale(bmi_model) +
#                 scale(HbA1c_at_diagnosis_v1) +
#                 #scale(c_peptide_v1) +
#                 Gender_v1 +
#                 osmotic +
#                 autoimmune +
#                 Unintentional_weight_loss_v1 +
#                 DKA +
#                 famhisdiab,
#               #famhisauto,
#               data = SR_m12_data,
#               family = binomial)
# save(m1_alt, file = "m1_alt.RData")
#
# SR_12_dd <- SR_m12_data %>%
#   dplyr::select(all_of(all_vars_12), "SRoutcome") %>%
#   as.data.frame()
# dd <- datadist(SR_12_dd); options(datadist = 'dd')
# m1_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) +
#                 scale(bmi_model) +
#                 scale(HbA1c_at_diagnosis_v1) +
#                 #scale(c_peptide_v1) +
#                 Gender_v1 +
#                 osmotic +
#                 autoimmune +
#                 Unintentional_weight_loss_v1 +
#                 DKA +
#                 famhisnoninsdiab,
#               #famhisauto,
#               data = SR_12_dd,
#               x=TRUE,
#               y = TRUE)
#
# ###Model 2
# m2 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
#             scale(bmi_model) +
#             scale(HbA1c_at_diagnosis_v1) +
#             #scale(c_peptide_v1) +
#             Gender_v1 +
#             osmotic +
#             autoimmune +
#             Unintentional_weight_loss_v1 +
#             DKA +
#             famhisnoninsdiab +
#             #famhisauto +
#             num_anti,
#           data = SR_m12_data,
#           family = binomial)
#
# save(m2, file = "m2.RData")
#
# m2_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) +
#                 scale(bmi_model) +
#                 scale(HbA1c_at_diagnosis_v1) +
#                 #scale(c_peptide_v1) +
#                 Gender_v1 +
#                 osmotic +
#                 autoimmune +
#                 Unintentional_weight_loss_v1 +
#                 DKA +
#                 famhisnoninsdiab +
#                 #famhisauto +
#                 num_anti,
#               data = SR_12_dd,
#               x=TRUE,
#               y = TRUE)
#
# ###Model 3
# m3 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
#             scale(bmi_model) +
#             scale(HbA1c_at_diagnosis_v1) +
#             #scale(c_peptide_v1) +
#             Gender_v1 +
#             osmotic +
#             autoimmune +
#             Unintentional_weight_loss_v1 +
#             DKA +
#             famhisnoninsdiab +
#             #famhisauto +
#             num_anti +
#             scale(T1DGRS2_z),
#           data = SR_m3_data,
#           family = binomial)
# save(m3, file = "m3.RData")
#
# SR_3_dd <- SR_m3_data %>%
#   dplyr::select(all_of(all_vars_3), "SRoutcome") %>%
#   as.data.frame()
# dd3 <- datadist(SR_3_dd); options(datadist = 'dd')
# m3_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) +
#                 scale(bmi_model) +
#                 scale(HbA1c_at_diagnosis_v1) +
#                 #scale(c_peptide_v1) +
#                 Gender_v1 +
#                 osmotic +
#                 autoimmune +
#                 Unintentional_weight_loss_v1 +
#                 DKA +
#                 famhisnoninsdiab +
#                 #famhisauto +
#                 num_anti +
#                 scale(T1DGRS2_z),
#               data = SR_3_dd,
#               x=TRUE,
#               y = TRUE)

##Display item---------------------------------------------------------------------------
###Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m1,
           test_data = SR_m12_data,
           outcome = "SRoutcome",
           saving_name = "03_04_sm1",
           manual_plotting = TRUE,
           manual_plot_name = "m1")
###Model 2 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m2,
           test_data = SR_m12_data,
           outcome = "SRoutcome",
           saving_name = "03_04_sm2",
           manual_plotting = TRUE,
           manual_plot_name = "m2")
###Model 3 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m3,
           test_data = SR_m3_data,
           outcome = "SRoutcome",
           saving_name = "03_04_sm3",
           manual_plotting = TRUE,
           manual_plot_name = "m3")
###Model 4 ROW --------------------------------------------------------------------
model_info(model = m4,
           test_data = SR_m3_data,
           outcome = "SRoutcome",
           saving_name = "03_04_sm4",
           manual_plotting = TRUE,
           manual_plot_name = "m4")

clean_and_order_coefs <- function(coefs, variable_order) {
  coefs %>%
    mutate(
      variable_clean = case_when(
        variable == "(Intercept)" ~ "Intercept",
        variable == "scale(AgeatDiagnosis)" ~ "Age at diagnosis (years)",
        variable == "scale(bmi_model)" ~ "BMI at diagnosis (kg/m2)",
        variable == "scale(HbA1c_at_diagnosis_v1)" ~ "HbA1c at diagnosis (mmol/mol)",
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
        variable == "scale(T1DGRS2_z)" ~ "T1D GRS",
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
  beta_col_name <- paste0("Model ", model_number, " Beta coefficient (95% CI)")
  or_col_name <- paste0("Model ", model_number, " Odds Ratio (95% CI)")
  pval_col_name <- paste0("Model ", model_number, " p-value")

  coefs_ordered %>%
    mutate(
      coef_95 = paste0(estimate, " (", round(lower, 2), ", ", round(upper, 2), ")"),
      OR_95 = paste0(OR, " (", round(OR_lower, 2), ", ", round(OR_upper, 2), ")")
    ) %>%
    #filter(var_type != "break") %>%
    dplyr::select(variable_clean, coef_95, OR_95, p_value) %>%
    rename(
      `Model Feature` = variable_clean,
      !!beta_col_name := coef_95,
      !!or_col_name := OR_95,
      !!pval_col_name := p_value
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
  select(
    `Model Feature`,
    `Model 1 Beta coefficient (95% CI)`, `Model 1 Odds Ratio (95% CI)`, `Model 1 p-value`,
    `Model 2 Beta coefficient (95% CI)`, `Model 2 Odds Ratio (95% CI)`, `Model 2 p-value`,
    `Model 3 Beta coefficient (95% CI)`, `Model 3 Odds Ratio (95% CI)`, `Model 3 p-value`,
    `Model 4 Beta coefficient (95% CI)`, `Model 4 Odds Ratio (95% CI)`, `Model 4 p-value`
  )

# Write to Excel
write_xlsx(model_coef_table, "tables/Supp_Table14_Model_coeffs.xlsx")
