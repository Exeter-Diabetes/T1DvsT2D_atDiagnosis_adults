#####################################################################################

#StartRight Prime Secondary Outcome Characteristics Table

#Supp Table 5

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/var_characteristics_1.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_ro_cc_1_2025.RData")

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
  "famhisnoninsdiab",
  "DKA", 
  "Unintentional_weight_loss_v1", 
  "osmotic", 
  "autoimmune", 
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
  "famhisnoninsdiab",
  "DKA", 
  "Unintentional_weight_loss_v1", 
  "osmotic", 
  "autoimmune", 
  "num_anti"
)
all_vars_3 <- c(varlist_3, varlist_cat_3)
##Make complete case datasets ----------------------------------------------------------
SR_ro_cc<- SR_ro_cc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
  )
SR_ro_cc$robust_outcome <- ifelse(SR_ro_cc$robust_outcome == "T1D", 1, 0)
SR_ro_cc$DKA <- as.character(SR_ro_cc$DKA)
SR_ro_cc$osmotic <- as.character(SR_ro_cc$osmotic)
SR_ro_cc$autoimmune <- as.character(SR_ro_cc$autoimmune)
#Model12
SR_m12_data <- SR_ro_cc %>%
  drop_na(all_of(all_vars_12))

SR_m12_t1d <- SR_m12_data %>%
  filter(robust_outcome == 1)
SR_m12_t2d <- SR_m12_data %>%
  filter(robust_outcome == 0)
#Model3
SR_m3_data <- SR_ro_cc %>%
  drop_na(all_of(all_vars_3))

SR_m3_t1d <- SR_m3_data %>%
  filter(robust_outcome == 1)
SR_m3_t2d <- SR_m3_data %>%
  filter(robust_outcome == 0)
#Characteristics tables -------------------------------------------------------------
##Whole_SR ----------------------------------------------------------------------------
varlist_12 = c("AgeatDiagnosis", 
               "bmi_model", 
               "HbA1c_at_diagnosis_v1", 
               "dur_diab_yr_v1", 
               "dur_diab_wks_v1", 
               "dur_diab_joined_v4"
)
varlist_3 = c("AgeatDiagnosis", 
              "bmi_model", 
              "HbA1c_at_diagnosis_v1",
              "T1DGRS2_z",
              "dur_diab_yr_v1", 
              "dur_diab_wks_v1", 
              "dur_diab_joined_v4"
)
var_characteristics(varlist = varlist_3, 
                    varlist_cat = varlist_cat_3,
                    dataset = SR_ro_cc,
                    numeric_option = "medianIQR",
                    group = "robust_outcome",
                    table_name = "tables/Sup_table_2_SR_medianIQR",
                    p_value_testing = FALSE)

var_characteristics(varlist = varlist_3, 
                    varlist_cat = varlist_cat_3,
                    dataset = SR_ro_cc,
                    numeric_option = "medianIQR",
                    table_name = "tables/Sup_table_2_SR_medianIQR_all",
                    p_value_testing = FALSE)

##Model12 ----------------------------------------------------------------------------
var_characteristics(varlist = varlist_12, 
                    varlist_cat = varlist_cat_12,
                    dataset = SR_m12_data,
                    numeric_option = "medianIQR",
                    group = "robust_outcome",
                    table_name = "tables/Sup_table_2_m12_medianIQR",
                    p_value_testing = FALSE, 
                    missingness = FALSE)

var_characteristics(varlist = varlist_12, 
                    varlist_cat = varlist_cat_12,
                    dataset = SR_m12_data,
                    numeric_option = "medianIQR",
                    table_name = "tables/Sup_table_2_m12_medianIQR_all",
                    p_value_testing = FALSE, 
                    missingness = FALSE)
#Include antibody breakdown to table
varlist_cat_12_anti = c(
  "Gender_v1",
  "famhisnoninsdiab",
  "DKA", 
  "Unintentional_weight_loss_v1", 
  "osmotic", 
  "autoimmune", 
  "num_anti",
  "GAD_bin_v1", 
  "IA2_bin_v1", 
  "ZNT8_bin_v1"
)
var_characteristics(varlist = varlist_12, 
                    varlist_cat = varlist_cat_12_anti,
                    dataset = SR_m12_data,
                    numeric_option = "medianIQR",
                    group = "robust_outcome",
                    table_name = "tables/Sup_table_2_m12_anti_medianIQR",
                    p_value_testing = FALSE, 
                    missingness = TRUE)

var_characteristics(varlist = varlist_12, 
                    varlist_cat = varlist_cat_12_anti,
                    dataset = SR_m12_t1d,
                    numeric_option = "medianIQR",
                    group = "Gender_v1",
                    table_name = "tables/Sup_table_2_m12_T1D_anti_medianIQR",
                    p_value_testing = FALSE, 
                    missingness = FALSE)
var_characteristics(varlist = varlist_12, 
                    varlist_cat = varlist_cat_12_anti,
                    dataset = SR_m12_t2d,
                    numeric_option = "medianIQR",
                    group = "Gender_v1",
                    table_name = "tables/Sup_table_2_m12_T2D_anti_medianIQR",
                    p_value_testing = FALSE, 
                    missingness = FALSE)
##Model3 -----------------------------------------------------------------------------
var_characteristics(varlist = varlist_3, 
                    varlist_cat = varlist_cat_3,
                    dataset = SR_m3_data,
                    numeric_option = "medianIQR",
                    group = "robust_outcome",
                    table_name = "tables/Sup_table_2_m3_medianIQR",
                    p_value_testing = FALSE, 
                    missingness = FALSE)
var_characteristics(varlist = varlist_3, 
                    varlist_cat = varlist_cat_3,
                    dataset = SR_m3_data,
                    numeric_option = "medianIQR",
                    table_name = "tables/Sup_table_2_m3_medianIQR_all",
                    p_value_testing = FALSE, 
                    missingness = FALSE)
var_characteristics(varlist = varlist_3, 
                    varlist_cat = varlist_cat_12_anti,
                    dataset = SR_m3_t1d,
                    numeric_option = "medianIQR",
                    group = "Gender_v1",
                    table_name = "tables/Sup_table_2_m3_T1D_anti_medianIQR",
                    p_value_testing = FALSE, 
                    missingness = FALSE)
var_characteristics(varlist = varlist_3, 
                    varlist_cat = varlist_cat_12_anti,
                    dataset = SR_m3_t2d,
                    numeric_option = "medianIQR",
                    group = "Gender_v1",
                    table_name = "tables/Sup_table_2_m3_T2D_anti_medianIQR",
                    p_value_testing = FALSE, 
                    missingness = FALSE)