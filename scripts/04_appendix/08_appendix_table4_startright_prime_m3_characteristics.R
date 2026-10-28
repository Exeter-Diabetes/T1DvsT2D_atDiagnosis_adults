#####################################################################################

#StartRight Prime Primary Outcome Characteristics Table

#Supplementary Table 4

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/var_characteristics_1.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")

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
SR_50_SRout_ccc<- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         define_3y_dur = ifelse(is.na(dur_diab_joined_v4),
                                dur_diab_yr_v3,
                                dur_diab_joined_v4)
  )
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
#Model12
SR_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12))

SR_m12_t1d <- SR_m12_data %>%
  filter(SRoutcome == 1)
SR_m12_t2d <- SR_m12_data %>%
  filter(SRoutcome == 0)
#Model3
SR_m3_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_3))

SR_m3_t1d <- SR_m3_data %>%
  filter(SRoutcome == 1)
SR_m3_t2d <- SR_m3_data %>%
  filter(SRoutcome == 0)
#Characteristics tables -------------------------------------------------------------
##Whole_SR ----------------------------------------------------------------------------
varlist_cat_12 = c(
  "Gender_v1",
  "Eth_5cat",
  "clinical_diagnosis_v1",
  "famhisnoninsdiab",
  "DKA",
  "Unintentional_weight_loss_v1",
  "osmotic",
  "autoimmune",
  "num_anti"
)
varlist_12 = c("AgeatDiagnosis",
               "bmi_model",
               "HbA1c_at_diagnosis_v1",
               "c_peptide_v4",
               #"dur_diab_yr_v1",
               "dur_diab_wks_v1",
               "define_3y_dur"
)
varlist_3 = c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1",
              "c_peptide_v4",
              "T1DGRS2_z",
              #"dur_diab_yr_v1",
              "dur_diab_wks_v1",
              "define_3y_dur"
)
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12,
#                     dataset = SR_50_SRout_ccc,
#                     numeric_option = "medianIQR",
#                     group = "SRoutcome",
#                     table_name = "tables/Sup_table_3_SR_medianIQR",
#                     p_value_testing = FALSE)
#
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12,
#                     dataset = SR_50_SRout_ccc,
#                     numeric_option = "medianIQR",
#                     table_name = "tables/Sup_table_3_SR_medianIQR_all",
#                     p_value_testing = FALSE)

##Model12 ----------------------------------------------------------------------------
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12,
#                     dataset = SR_m12_data,
#                     numeric_option = "medianIQR",
#                     group = "SRoutcome",
#                     table_name = "tables/Sup_table_4_m12_medianIQR",
#                     p_value_testing = FALSE,
#                     missingness = FALSE)
#
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12,
#                     dataset = SR_m12_data,
#                     numeric_option = "medianIQR",
#                     table_name = "tables/Sup_table_4_m12_medianIQR_all",
#                     p_value_testing = FALSE,
#                     missingness = FALSE)
#Include antibody breakdown to table
# varlist_cat_12_anti = c(
#   "Gender_v1",
#   "famhisnoninsdiab",
#   "DKA",
#   "Unintentional_weight_loss_v1",
#   "osmotic",
#   "autoimmune",
#   "num_anti",
#   "GAD_bin_v1",
#   "IA2_bin_v1",
#   "ZNT8_bin_v1"
# )
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12_anti,
#                     dataset = SR_m12_data,
#                     numeric_option = "medianIQR",
#                     group = "SRoutcome",
#                     table_name = "tables/Sup_table_4_m12_anti_medianIQR",
#                     p_value_testing = FALSE,
#                     missingness = TRUE)
#
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12_anti,
#                     dataset = SR_m12_t1d,
#                     numeric_option = "medianIQR",
#                     group = "Gender_v1",
#                     table_name = "tables/Sup_table_4_m12_T1D_anti_medianIQR",
#                     p_value_testing = FALSE,
#                     missingness = FALSE)
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12_anti,
#                     dataset = SR_m12_t2d,
#                     numeric_option = "medianIQR",
#                     group = "Gender_v1",
#                     table_name = "tables/Sup_table_4_m12_T2D_anti_medianIQR",
#                     p_value_testing = FALSE,
#                     missingness = FALSE)
##Model3 -----------------------------------------------------------------------------
var_characteristics(varlist = varlist_3,
                    varlist_cat = varlist_cat_3,
                    dataset = SR_m3_data,
                    numeric_option = "medianIQR",
                    group = "SRoutcome",
                    table_name = "tables/Sup_table_4_m3_medianIQR",
                    p_value_testing = FALSE,
                    missingness = FALSE)
var_characteristics(varlist = varlist_3,
                    varlist_cat = varlist_cat_3,
                    dataset = SR_m3_data,
                    numeric_option = "medianIQR",
                    table_name = "tables/Sup_table_4_m3_medianIQR_all",
                    p_value_testing = FALSE,
                    missingness = FALSE)
# var_characteristics(varlist = varlist_3,
#                     varlist_cat = varlist_cat_12_anti,
#                     dataset = SR_m3_t1d,
#                     numeric_option = "medianIQR",
#                     group = "Gender_v1",
#                     table_name = "tables/Sup_table_4_m3_T1D_anti_medianIQR",
#                     p_value_testing = FALSE,
#                     missingness = FALSE)
# var_characteristics(varlist = varlist_3,
#                     varlist_cat = varlist_cat_12_anti,
#                     dataset = SR_m3_t2d,
#                     numeric_option = "medianIQR",
#                     group = "Gender_v1",
#                     table_name = "tables/Sup_table_4_m3_T2D_anti_medianIQR",
#                     p_value_testing = FALSE,
#                     missingness = FALSE)
