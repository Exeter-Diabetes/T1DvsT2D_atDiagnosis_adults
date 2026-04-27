#####################################################################################

#Paper figures

#Supplementary Table 11:
#StartRight 18-50
#Primary outcome
###################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
#source("functions/model_info1.R")
#source("functions/model_comparison2.R")
source("functions/model_comparison3.R")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
load("m4.RData")
#Complete case model data -------------------------------------------------------------
##Define variables in models12 & models3------------------------------------------------
### Models3 ------------------------------------------------------------------------
#Continuous variables
varlist_3 = c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1",
              "T1DGRS2_z"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat_3 = c(
  "Eth_5cat",
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
SR_SRout_ccc$Eth_4cat <- factor(SR_SRout_ccc$Eth_4cat, levels = c("White", "Black", "South Asian", "Other/Mixed"))
#Model3
SR_m3_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3))
#Develop model 4
# m4 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
#             scale(bmi_model) +
#             scale(HbA1c_at_diagnosis_v1) +
#             #scale(c_peptide_v1) +
#             Gender_v1 +
#             osmotic +
#             autoimmune +
#             Unintentional_weight_loss_v1 +
#             DKA +
#             famhisnoninsdiab +
#             scale(T1DGRS2_z),
#           #famhisauto,
#           data = SR_m3_data,
#           family = binomial)
# save(m4, file = "m4.RData")

##Put together table -----------------------------------------------------------------
###Comparison to Model 1 ----------------------------------------------------------------
base_model_vars1 = c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1"
)
#create varlist_cat (categorical variables of interest names)
base_model_vars_cat1 = c(
  "Eth_4cat",
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  "famhisnoninsdiab"
)
varlist1 = c("T1DGRS2_z"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat1 = c(
  "num_anti")

SR_m3_data$Eth_4cat <- factor(SR_m3_data$Eth_4cat, levels = c("White", "Black", "South Asian", "Other/Mixed"))
#SR_SRout_ccc$SRoutcome <- ifelse(SR_SRout_ccc$SRoutcome == "Type 1", 1, 0)
model_comparisons(varlist = varlist1,
                  varlist_cat = varlist_cat1,
                  base_model_vars = base_model_vars1,
                  base_model_vars_cat = base_model_vars_cat1,
                  dataset = SR_m3_data,
                  outcome = "SRoutcome",
                  saving_name = "tables/Supp_Table10a",
                  complete_case = TRUE,
                  plot = FALSE
)
# model_comparisons(varlist = varlist1,
#                   varlist_cat = varlist_cat1,
#                   base_model_vars = base_model_vars1,
#                   base_model_vars_cat = base_model_vars1_CAT,
#                   dataset = SR_SRout_ccc,
#                   outcome = "SRoutcome",
#                   saving_name = "tables/Supp_Table5",
#                   complete_case = FALSE,
#                   plots = FALSE)
###Comparison to Model 2 ---------------------------------------------------------------
# base_model_vars = c("AgeatDiagnosis",
#                     "bmi_model",
#                     "HbA1c_at_diagnosis_v1"
# )
#create varlist_cat (categorical variables of interest names)
base_model_vars_cat2 = c(
  "Eth_4cat",
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  "famhisnoninsdiab",
  "num_anti"
)
# varlist = c("T1DGRS2_z"
# )

model_comparisons(varlist = varlist1,
                  #varlist_cat = varlist_cat,
                  base_model_vars = base_model_vars1,
                  base_model_vars_cat = base_model_vars_cat2,
                  dataset = SR_m3_data,
                  outcome = "SRoutcome",
                  saving_name = "tables/Supp_Table10b",
                  complete_case = TRUE,
                  plot = FALSE
)
