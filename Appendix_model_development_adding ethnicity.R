#########################################################################################
# StartRight PAPER

#Model development
#Forest plots
#########################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(naniar)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
#source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")

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
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
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
##Models -------------------------------------------------------------------------------
###Model 1
m1 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 +
            osmotic +
            autoimmune +
            Unintentional_weight_loss_v1 +
            DKA +
            relevel(factor(Eth_5cat), ref = "White") +
            famhisnoninsdiab,
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
#save(m1, file = "m1.RData")

m1_alt <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
                scale(bmi_model) +
                scale(HbA1c_at_diagnosis_v1) +
                #scale(c_peptide_v1) +
                Gender_v1 +
                osmotic +
                autoimmune +
                Unintentional_weight_loss_v1 +
                DKA +
                #relevel(factor(Eth_5cat), ref = "White") +
                famhisnoninsdiab,
              #famhisauto,
              data = SR_m12_data,
              family = binomial)
#save(m1_alt, file = "m1_alt.RData")

#Likelihood ratio test
lrtest(m1_alt, m1)
#####################################################################################
#Adding iterative features to age, bmi, ethnicity
#To see when south asian flips



###################################################################################
#Model plots of m1_alt model probabilities histograms by ethnicity
###Model 1
m_basic <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            relevel(factor(Eth_5cat), ref = "White"),
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
summary(m_basic)

##
###Model hab1c
m_hba1c <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            #Gender_v1 +
            #osmotic +
            #autoimmune +
            #Unintentional_weight_loss_v1 +
            #DKA +
            relevel(factor(Eth_5cat), ref = "White"), #+
            #famhisnoninsdiab,
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
summary(m_basic)
###Model 1
m_sex <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 +
            #osmotic +
            #autoimmune +
            #Unintentional_weight_loss_v1 +
            #DKA +
            relevel(factor(Eth_5cat), ref = "White"), #+
            #famhisnoninsdiab,
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
summary(m_basic)
###Model omsotic
m_osmo <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 +
            osmotic +
            #autoimmune +
            #Unintentional_weight_loss_v1 +
            #DKA +
            relevel(factor(Eth_5cat), ref = "White"), #+
            #famhisnoninsdiab,
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
summary(m_basic)
###Model autoimmune
m_auto <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 +
            osmotic +
            autoimmune +
            #Unintentional_weight_loss_v1 +
            #DKA +
            relevel(factor(Eth_5cat), ref = "White"), #+
            #famhisnoninsdiab,
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
summary(m_basic)
###Model weight
m_weight <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 +
            osmotic +
            autoimmune +
            Unintentional_weight_loss_v1 +
            #DKA +
            relevel(factor(Eth_5cat), ref = "White"), #+
            #famhisnoninsdiab,
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
summary(m_basic)
###Model dka
m_dka <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 +
            osmotic +
            autoimmune +
            Unintentional_weight_loss_v1 +
            DKA +
            relevel(factor(Eth_5cat), ref = "White"), #+
            #famhisnoninsdiab,
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
summary(m_basic)
###Model famhis (full) = m1
# m_famhis <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
#             scale(bmi_model) +
#             scale(HbA1c_at_diagnosis_v1) +
#             #scale(c_peptide_v1) +
#             Gender_v1 +
#             osmotic +
#             autoimmune +
#             Unintentional_weight_loss_v1 +
#             DKA +
#             relevel(factor(Eth_5cat), ref = "White") +
#             famhisnoninsdiab,
#           #famhisauto,
#           data = SR_m12_data,
#           family = binomial)
summary(m1)
###################################################################################
