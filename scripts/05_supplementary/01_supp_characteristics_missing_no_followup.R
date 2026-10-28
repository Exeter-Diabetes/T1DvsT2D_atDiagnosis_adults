#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/var_characteristics_1.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")

#Complete case model data -------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_30_6_2025.RData")
#1) Filtering --------------------------------------------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------------
#CREATE SR 18-50s
SR_SRout <- SR %>%
  filter(AgeatDiagnosis <= 50) %>%
  #filter(Date_Visit_v1 <= "2018-10-31") %>%
  filter(!grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                Other_type_of_diabetes_v1)) %>%
  #filter(!is.na(SRoutcome)) %>%
  mutate(missing3y = ifelse(is.na(SRoutcome) | !(define %in% c("T1D Cpep v4",
                                                               "T2D Cpep v4 ",
                                                               "T2D no insulin v3",
                                                               "T2D no insulin v4")),
                            "missing 3 year followup",
                            "included with outcome")) %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi_calc_v1,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         define_3y_dur = ifelse(is.na(dur_diab_joined_v4),
                                dur_diab_yr_v3,
                                dur_diab_joined_v4)
  )
table(SR_SRout$missing3y, useNA = "ifany")
SR_SRout %>%
  filter(is.na(bmi_diag)) %>%
  group_by(missing3y) %>%
  count()
##Make complete case datasets ----------------------------------------------------------
SR_SRout_ccc<- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         define_3y_dur = ifelse(is.na(dur_diab_joined_v4),
                                dur_diab_yr_v3,
                                dur_diab_joined_v4)
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)

SR_SRout_ccc %>%
  filter(is.na(bmi_model) | is.na(AgeatDiagnosis)) %>%
  count()


SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(missing_def = ifelse(is.na(bmi_model), "missing bmi",
                              ifelse(is.na(HbA1c_at_diagnosis_v1), "missing HbA1c",
                                     ifelse(is.na(osmotic), "missing osmotic", "included")))) %>%
  mutate(missing = ifelse(str_detect(missing_def, "missing"), "missing", missing_def))
table(SR_SRout_ccc$missing_def, useNA = "ifany")
table(SR_SRout_ccc$missing, useNA = "ifany")

SR_SRout_ccc %>%
  filter(is.na(bmi_diag)) %>%
  group_by(missing) %>%
  count()
#Characteristics tables -------------------------------------------------------------
##Whole_SR ----------------------------------------------------------------------------
varlist_cat_3 = c(
  "Gender_v1",
  "Eth_5cat",
  "clinical_diagnosis_v1",
  "famhisnoninsdiab",
  "DKA",
  "Unintentional_weight_loss_v1",
  "osmotic",
  "autoimmune",
  "num_anti",
  "famhisauto",
  "SRoutcome"
)

varlist_3 = c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1",
              "c_peptide_v4",
              "T1DGRS2_z",
              #"dur_diab_yr_v1",
              "dur_diab_wks_v1",
              "define_3y_dur",
              "wh_ratio_v1"
)

##Model3 -----------------------------------------------------------------------------
var_characteristics(varlist = varlist_3,
                    varlist_cat = varlist_cat_3,
                    dataset = SR_SRout_ccc,
                    numeric_option = "medianIQR",
                    group = "missing",
                    table_name = "tables/App_table_inc_vs_missing_characteristics",
                    p_value_testing = TRUE,
                    stat_test_cont = "Wilcox",
                    stat_test_cat = "Chi_square",
                    missingness = TRUE)
var_characteristics(varlist = varlist_3,
                    varlist_cat = varlist_cat_3,
                    dataset = SR_SRout,
                    group = "missing3y",
                    numeric_option = "medianIQR",
                    table_name = "tables/App_table_inc_vs_noSRoutcome_characteristics",
                    p_value_testing = TRUE,
                    stat_test_cont = "Wilcox",
                    stat_test_cat = "Chi_square",
                    missingness = TRUE)
