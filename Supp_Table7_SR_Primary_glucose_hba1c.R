############################################################################################
#
#Comparison between HbA1c and glucose at diagnosis

#This does this in StartRight 18-50s

#Supplementary Table 8


#############################################################################################
#load libraries
library(tidyverse)
#library(writexl)

#load functions
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
source("functions/model_comparison2.R")
#load data-------------------------------------------------------------------------
#load dataset from 01_11
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
#load("~/PhD/StartRight_paper/data/SR_ro_cc_1_2025.RData")

#Prep variables ---------------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(
  bmi_model = ifelse(is.na(bmi_diag),
                     bmi,
                     bmi_diag),
  famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
  ) 

table(SR_SRout_ccc$ketosis_no_acidosis_v1, useNA = "ifany")
table(SR_SRout_ccc$ketosis_v1, useNA = "ifany")
table(SR_SRout_ccc$DKA, useNA = "ifany")
#A) For SRoutcome in SR_SRout ----------------------------------------------------
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)

## Suplementary Table 3: HbA1c and glucose checks -----------------------------------------------------------

varlist = c(
  "HbA1c_at_diagnosis_v1",
  "Glucose_at_diagnosis_v1" 
)


model_continuous(varlist, 
                 dataset = SR_SRout_ccc, 
                 outcome = "SRoutcome",
                 saving_name = "tables/SuppTable8_glucose_hba1c",
                 complete_case = TRUE, 
                 plots = FALSE)

# model_comparisons(varlist = varlist, 
#                   varlist_cat = varlist_cat,
#                   base_model_vars = base_model_vars, 
#                   dataset = SR_SRout_ccc,
#                   outcome = "SRoutcome",
#                   saving_name = "02_01_SuppTable3_glucose_hba1c",
#                   complete_case = TRUE
# )
# 
# SR_hba1c <- SR_SRout_ccc %>%
#   drop_na(AgeatDiagnosis, bmi, HbA1c_at_diagnosis_v1, Glucose_at_diagnosis_v1)
# model_hba1c <- glm(SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi) + scale(HbA1c_at_diagnosis_v1), 
#              data = SR_hba1c, 
#              family = binomial)
# 
# 
# model_glucose <- glm(SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi) + scale(Glucose_at_diagnosis_v1), 
#                    data = SR_hba1c, 
#                    family = binomial)
# 
# lrtest(model_hba1c, model_glucose)