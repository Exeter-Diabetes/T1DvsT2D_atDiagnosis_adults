#####################################################################################

#Paper figures

#To get the two final paper complete case model datasets

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
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
##Additional models ------------------------------------------------------------------
###Clinical diagnosis ------------------------------------------------------------------
m_clinical <- glm(SRoutcome ~ clinical_diagnosis_v1,
                  data = SR_m12_data,
                  family = binomial)
###Antibodies only ------------------------------------------------------------------------
m_antis <- glm(SRoutcome ~ num_anti,
               data = SR_m12_data,
               family = binomial)
###GRS only -------------------------------------------------------------------------------
m_grs <- glm(SRoutcome ~ scale(T1DGRS2_z) ,
             data = SR_m3_data,
             family = binomial)
##Table of ROC, Sens, spec--------------------------------------------------------------
###Get model info for models 1-3 using 50% cut-off ----------------------------------
model_info(model = m1,
           test_data = SR_m12_data,
           outcome = "SRoutcome",
           saving_name = "tables/03_04_sm_1_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
model_info(model = m2,
           test_data = SR_m12_data,
           outcome = "SRoutcome",
           saving_name = "tables/03_04_sm_2_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
model_info(model = m3,
           test_data = SR_m3_data,
           outcome = "SRoutcome",
           saving_name = "tables/03_04_sm_3_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)

###Get model info for antis, clinical & grs only models ---------------------------
#### Clinical diagnosis ------------------------------------------------------------
##### Optimal threshold ---------------------------------------------------------
model_info(model = m_clinical, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "tables/03_04_sm_clinical", 
           manual_plotting = TRUE, 
           manual_plot_name = "m_clinical")

##### Contingency table --------------------------------------------------------------
varlist_cat <- c( 
  "clinical_diagnosis_v1"
)
clinic_m12 <- SR_m12_data %>%
  dplyr::select(SRoutcome, clinical_diagnosis_v1) %>%
  filter(clinical_diagnosis_v1 %in% c("Type 1", "Type 2")) %>%
  mutate(clinical_diagnosis_v1 = ifelse(clinical_diagnosis_v1 == "Type 1", 1, 0))
#Uncertain diagnosis = 63
cat_contingency(varlist_cat, 
                dataset = clinic_m12, 
                outcome = "SRoutcome", 
                saving_name = "tables/03_04_clinicdiag_pred",
                complete_case = TRUE, 
                plots = FALSE)

#### Antibody status ----------------------------------------------------------------
####Optimal threshold ------------------------------------------------------------------
model_info(model = m_antis, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm_antis", 
           manual_plotting = TRUE, 
           manual_plot_name = "m_antis")
#####Contingency table ----------------------------------------------------------------
antis_m12 <- SR_m12_data %>%
  dplyr::select(SRoutcome, num_anti) %>%
  mutate(anticat = ifelse(num_anti == "0", 0, 1))
varlist_cat <- c( 
  "anticat"
)
cat_contingency(varlist_cat, 
                dataset = antis_m12, 
                outcome = "SRoutcome", 
                saving_name = "tables/03_04_ANTIS_pred",
                complete_case = TRUE, 
                plots = FALSE)
####GRS -------------------------------------------------------------------------
#####Optimal threshold -------------------------------------------------------------
model_info(model = m_grs, 
           test_data = SR_m3_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm_grs", 
           manual_plotting = TRUE, 
           manual_plot_name = "m_grs")
#####Contingency table --------------------------------------------------------------
varlist <- c( 
  "T1DGRS2_z"
)
model_continuous(varlist, 
                 dataset = SR_m3_data,
                 outcome = "SRoutcome",
                 saving_name = "tables/03_04_T1DGRS_metrics",
                 complete_case = TRUE, 
                 #thresholds = tbl1_thresholds,
                 plots = FALSE,
                 decimals = 3)


###Put together tables ---------------------------------------------------------------
#### Optimal thresholds ----------------------------------------------------------
# m1_summary <- `03_04_sm1` %>%
#   dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold, 
#                 Sensitivity, Specificity, PPV, NPV, Accuracy) %>%
#   mutate(across(5:10,function(x) {x* 100})) %>%
#   mutate(across(5:10,~ round(.x, digits = 1)))
# m2_summary <- `03_04_sm2` %>%
#   dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold, 
#                 Sensitivity, Specificity, PPV, NPV, Accuracy) %>%
#   mutate(across(5:10,function(x) {x* 100})) %>%
#   mutate(across(5:10,~ round(.x, digits = 1)))
# m3_summary <- `03_04_sm3` %>%
#   dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold, 
#                 Sensitivity, Specificity, PPV, NPV, Accuracy) %>%
#   mutate(across(5:10,function(x) {x* 100})) %>%
#   mutate(across(5:10,~ round(.x, digits = 1)))
# clinical_summary <- `tables/03_04_sm_clinical` %>%
#   dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold, 
#                 Sensitivity, Specificity, PPV, NPV, Accuracy) %>%
#   mutate(across(5:10,function(x) {x* 100})) %>%
#   mutate(across(5:10,~ round(.x, digits = 1)))
# antis_summary <- `03_04_sm_antis` %>%
#   dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold, 
#                 Sensitivity, Specificity, PPV, NPV, Accuracy) %>%
#   mutate(across(5:10,function(x) {x* 100})) %>%
#   mutate(across(5:10,~ round(.x, digits = 1)))
# grs_summary <- `03_04_sm_grs` %>%
#   dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold, 
#                 Sensitivity, Specificity, PPV, NPV, Accuracy) %>%
#   mutate(across(5:10,function(x) {x* 100})) %>%
#   mutate(across(5:10,~ round(.x, digits = 1)))
# 
# model_summary <- rbind(m1_summary, 
#                        m2_summary, 
#                        m3_summary, 
#                        clinical_summary, 
#                        antis_summary,
#                        grs_summary) %>%
#   mutate(`T1D %` = round((`1 n`/`n`)*100,1))
# 
# write_xlsx(model_summary,"tables/Table4_unsetthresholds.xlsx")
#### 50% Thresholds ------------------------------------------------------------------
m1_summary <- `tables/03_04_sm_1_50perc` %>%
  select(model, n, `1 n`, ROC_AUC, Threshold, 
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
#mutate(across(5:10,function(x) {x* 100})) #%>%
#mutate(across(5:10,~ round(.x, digits = 1)))
m2_summary <- `tables/03_04_sm_2_50perc` %>%
  select(model, n, `1 n`, ROC_AUC, Threshold, 
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
m3_summary <- `tables/03_04_sm_3_50perc` %>%
  select(model, n, `1 n`, ROC_AUC, Threshold, 
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
clinical_summary <- `tables/03_04_clinicdiag_pred` %>%
  rename(model = variable) %>%
  mutate(Threshold = "T1D") %>%
  select(model, n, `1 n`, ROC_AUC, Threshold, 
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
antis_summary <- `tables/03_04_ANTIS_pred` %>%
  rename(model = variable) %>%
  mutate(Threshold = "Positive") %>%
  select(model, n, `1 n`, ROC_AUC, Threshold, 
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
grs_summary <- `tables/03_04_T1DGRS_metrics` %>%
  rename(model = variable) %>%
  select(model, n, `1 n`, ROC_AUC, Threshold, 
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))

model_summary <- rbind(m1_summary, 
                       m2_summary, 
                       m3_summary, 
                       clinical_summary, 
                       antis_summary,
                       grs_summary) %>%
  mutate(`T1D %` = round((`1 n`/`n`)*100,1))

write_xlsx(model_summary,"tables/Table4.xlsx")