#####################################################################################

#Paper figures

#Table 5
#Coefficients and ORs for Scoring table
#Unstandardised

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)

#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/model_continuous1.R")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
load("m1.RData")

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
#Model12
SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
##StartRight Prime --------------------------------------------------------------------
SR_50_SRout_ccc<- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
  )
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
#Model12
SR_50_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12))

##Scoring Table -----------------------------------------------------------------------
#StartRight----------------------------------------------------------------------------------
SR_mD_data <- SR_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, num_anti) %>%
  mutate(
    m1_pp = predict(m1, SR_m12_data, type = "response"),
    agedx = case_when(
      AgeatDiagnosis <37 ~ 2,
      AgeatDiagnosis <40 ~ 1,
      AgeatDiagnosis <=50 ~ 0
    ),
    bmi = case_when(
      bmi_model < 25 ~ 4,
      bmi_model <= 31 ~ 2,
      bmi_model > 31 ~ 0
    ),
    hba1c = ifelse(
      HbA1c_at_diagnosis_v1 < 58, 0, 1
    ),
    sex = ifelse(Gender_v1 == "Female", 1, 0),
    osmotic = ifelse(osmotic == "1", 1, 0),
    autoimmune = ifelse(autoimmune == "1", 1, 0),
    weightloss = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0),
    dka = ifelse(DKA == "1", 1, 0),
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    pos_anti = ifelse(num_anti >= 1, 1, 0)
  )

SR_mD_data <- SR_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex,
         osmotic, autoimmune, weightloss, dka, pardm, pos_anti) %>%
  mutate(ScoreD = select(., agedx:pardm) %>% rowSums(na.rm = TRUE))
varlist = c("ScoreD")
model_continuous(varlist,
                 dataset = SR_mD_data,
                 outcome = "SRoutcome",
                 saving_name = "tables/Supp_Table15",
                 complete_case = TRUE,
                 plots = TRUE)
#StartRight Prime----------------------------------------------------------------------------------
SR_50_mD_data <- SR_50_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, num_anti) %>%
  mutate(
    m1_pp = predict(m1, SR_50_m12_data, type = "response"),
    agedx = case_when(
      AgeatDiagnosis <37 ~ 2,
      AgeatDiagnosis <40 ~ 1,
      AgeatDiagnosis >=50 ~ 0
    ),
    bmi = case_when(
      bmi_model < 25 ~ 4,
      bmi_model <= 31 ~ 2,
      bmi_model > 31 ~ 0
    ),
    hba1c = ifelse(
      HbA1c_at_diagnosis_v1 < 58, 0, 1
    ),
    sex = ifelse(Gender_v1 == "Female", 1, 0),
    osmotic = ifelse(osmotic == "1", 1, 0),
    autoimmune = ifelse(autoimmune == "1", 1, 0),
    weightloss = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0),
    dka = ifelse(DKA == "1", 1, 0),
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    pos_anti = ifelse(num_anti >= 1, 1, 0)
  )

SR_50_mD_data <- SR_50_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex,
         osmotic, autoimmune, weightloss, dka, pardm, pos_anti) %>%
  mutate(ScoreD = select(., agedx:pardm) %>%
           rowSums(na.rm = TRUE))
varlist = c("ScoreD")
model_continuous(varlist,
                 dataset = SR_50_mD_data,
                 outcome = "SRoutcome",
                 saving_name = "tables/Supp_Table15_Prime",
                 complete_case = TRUE,
                 plots = TRUE)


SR_mD_data %>%
  filter(ScoreD >= 7) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  filter(ScoreD < 7) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  filter(ScoreD >= 6) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  filter(ScoreD < 6) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )




