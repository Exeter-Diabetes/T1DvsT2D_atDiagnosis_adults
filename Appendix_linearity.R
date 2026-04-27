#######################################################################################
#STARTRIGHT SIMPLE CLINICAL MODEL: AGES 18-50

#Produces simple clinical model from Age, BMI and sig. different variables
#from 02_11 that add independly to age and BMI (03_11):

#Model development
#Discrimination
#Calibration
#Bootstrapping
#######################################################################################

#load libraries ------------------------------------------------------------------------
library(tidyverse)

#load functions ------------------------------------------------------------------
#source("functions/model_info.R")
#source("functions/model_info1.R")
#source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")

#load data-------------------------------------------------------------------------
#load dataset from 01_11
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")

#get data in right format ------------------------------------------------------------
#Create variable lists -----------------------------------------------------------------
## Continuous variables
varlist = c("AgeatDiagnosis",
            "bmi_model",
            "Waist_v1",
            "HbA1c_at_diagnosis_v1"
)
## create varlist_cat (categorical variables of interest names)
varlist_cat = c(
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  "famhisdiab"
)

##Make complete case datasets ----------------------------------------------------------
SR_SRout_ccc<- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)
#Produce complete case datasets ----------------------------------------------------------
all_vars <- c(varlist, varlist_cat)
##StartRight
SR_SRout_ccc <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars))


# Checks ---------------------------------------------------------------------------
## 1) Distribution of continuous traits
## 2) Relationship between continuous predictors
## 3) Outcome variable
### 1) Frequency of outcome variable
### 2) Distributions of continuous predictors based on outcome
model_continuous(varlist,
                 dataset = SR_SRout_ccc,
                 outcome = "SRoutcome",
                 saving_name = "figures/Appendix_linearity_checks",
                 complete_case = TRUE,
                 plots = TRUE)
### 3) Distributions of categorical predictors based on outcome
# cat_contingency(varlist_cat,
#                 dataset = SR_SRout_ccc,
#                 outcome = "SRoutcome",
#                 saving_name = "03_02_cat_plots",
#                 complete_case = TRUE,
#                 plots = TRUE)




# tbl <- xtabs(~SRoutcome + Gender_v1 + DKA, SR_SRout_ccc)
# ftable(tbl)
# library(vcd)
# mosaic(tbl)
# library(epiDisplay)
#
# count_iR <- tab1(SR_SRout_ccc$SRoutcome,
#                  cum.percent = TRUE,
#                  main = "Insulin Required")
# #proportions of whether require insulin (1) or not (0)
# c <- SR_SRout_ccc %>%
#   count(SRoutcome, name = "n") %>%
#   mutate(prop = n/sum(n)) %>%
#   filter(SRoutcome == "1") %>%
#   mutate(odds = prop/(1 - prop),
#          log_odds = log(prop/(1 - prop)))
# c

###b) Spline model - splines on all continuous vars

###c) Regularization

