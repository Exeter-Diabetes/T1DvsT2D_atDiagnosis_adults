#####################################################################################

#Paper figures

#Supplementary Table 13:
#StartRight 18-50s
#Secondary Outcome

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/model_info2.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_ro_cc_1_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_ro_cc_2_2025.RData")
# load("m1.RData")
# load("m2.RData")
# load("m3.RData")
# load("m4.RData")
# Model parameters and formulas -----------------------------------------------------------
## Model 1 -----------------------------------------------------------------------------------
m1_beta <- c(
  "(Intercept)" = 8.64624460772943,
  "AgeatDiagnosis" = -0.0964441683502511,
  "bmi_model" = -0.243555720621392,
  "HbA1c_at_diagnosis_v1" = 0.00341120554236777,
  "Gender_v1Male" = -0.863116972338412,
  "osmotic1" = 0.961134759411358,
  "autoimmune1" = 0.823010855901484,
  "Unintentional_weight_loss_v1Yes" = 1.35018265494132,
  "DKA1" = 0.734312980329503,
  'Eth_4catBlack' = -1.32530674465559,
  'Eth_4catOther/Mixed' = -1.34567865383264,
  'Eth_4catSouth Asian' = -0.793896878252353,
  "famhisnoninsdiabYes" = -1.01442801038726
)
m1_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab
## Model 2 -----------------------------------------------------------------------------------
m2_beta <- c(
  "(Intercept)" = 5.65739369171834,
  "AgeatDiagnosis" = -0.0761310235937549,
  "bmi_model" = -0.205897904461199,
  "HbA1c_at_diagnosis_v1" = 0.00311485068438993,
  "Gender_v1Male" = -0.770769399543392,
  "osmotic1" = 0.897232400482147,
  "autoimmune1" = 0.670223920517065,
  "Unintentional_weight_loss_v1Yes" = 1.20670197330313,
  "DKA1" = 0.645909312453152,
  'Eth_4catBlack' = -0.786936569308031,
  'Eth_4catOther/Mixed' = -0.60231161939665,
  'Eth_4catSouth Asian' = -0.362368506943113,
  "famhisnoninsdiabYes" = -0.640773160200783,
  "num_anti1" = 1.96631270605778,
  "num_anti2" = 2.54455916410863,
  "num_anti3" = 3.46277643855415
)
m2_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  num_anti
## Model 3 -----------------------------------------------------------------------------------
m3_beta <- c(
  "(Intercept)" = 5.59111707576748,
  "AgeatDiagnosis" = -0.0658265444495303,
  "bmi_model" = -0.215588994357022,
  "HbA1c_at_diagnosis_v1" = 0.0010205665084689,
  "Gender_v1Male" = -0.769413515416535,
  "osmotic1" = 0.961696294455552,
  "autoimmune1" = 0.718466592548281,
  "Unintentional_weight_loss_v1Yes" = 0.973882805795106,
  "DKA1" = 0.450769465099517,
  'Eth_4catBlack' = -0.0448130235508751,
  'Eth_4catOther/Mixed' = -1.19595487818422,
  'Eth_4catSouth Asian' = -0.508601030699211,
  "famhisnoninsdiabYes" = -0.649989577718691,
  "num_anti1" = 1.69269869149969,
  "num_anti2" = 2.1106207424025,
  "num_anti3" = 2.9710916181995,
  "T1DGRS2_z" = 0.588101930916182
)
m3_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  num_anti +
  T1DGRS2_z

## Model 4 -----------------------------------------------------------------------------------
m4_beta <- c(
  "(Intercept)" = 7.68913552392495,
  "AgeatDiagnosis" = -0.0792922627586783,
  "bmi_model" = -0.245795596269301,
  "HbA1c_at_diagnosis_v1" = 0.00135050843483768,
  "Gender_v1Male" = -0.745200775272355,
  "osmotic1" = 1.01679460706228,
  "autoimmune1" = 0.94971768216283,
  "Unintentional_weight_loss_v1Yes" = 1.05073780432584,
  "DKA1" = 0.626511986416986,
  'Eth_4catBlack' = -0.13926992184133,
  'Eth_4catOther/Mixed' = -1.67150151029812,
  'Eth_4catSouth Asian' = -0.76869417190735,
  "famhisnoninsdiabYes" = -1.07704095684858,
  "T1DGRS2_z" = 0.922151368226217
)
m4_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  T1DGRS2_z

# User-defined function -------------------------------------------------------------
prepare_prediction_data <- function(dat){

  dat %>%
    mutate(
      Eth_4cat = factor(
        Eth_4cat,
        levels = c(
          "White",
          "Black",
          "Other/Mixed",
          "South Asian"
        )
      ),
      Gender_v1 = factor(
        Gender_v1,
        levels = c("Female", "Male")
      )
    )
}
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
SR_ro_cc<- SR_ro_cc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_ro_cc$DKA <- as.character(SR_ro_cc$DKA)
SR_ro_cc$osmotic <- as.character(SR_ro_cc$osmotic)
SR_ro_cc$autoimmune <- as.character(SR_ro_cc$autoimmune)
SR_ro_cc$robust_outcome <- ifelse(SR_ro_cc$robust_outcome == "T1D", 1, 0)
SR_ro_cc$num_anti <- as.character(SR_ro_cc$num_anti)
#Model12
SR_m12_data <- SR_ro_cc %>%
  drop_na(all_of(all_vars_12))
#Model3
SR_m3_data <- SR_ro_cc %>%
  drop_na(all_of(all_vars_3))
#StartRight Prime -------------------------------------------------------------------
SR_50_ro_cc<- SR_50_ro_cc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_50_ro_cc$DKA <- as.character(SR_50_ro_cc$DKA)
SR_50_ro_cc$osmotic <- as.character(SR_50_ro_cc$osmotic)
SR_50_ro_cc$autoimmune <- as.character(SR_50_ro_cc$autoimmune)
SR_50_ro_cc$robust_outcome <- ifelse(SR_50_ro_cc$robust_outcome == "T1D", 1, 0)
SR_50_ro_cc$num_anti <- as.character(SR_50_ro_cc$num_anti)
#Model12
SR_50_m12_data <- SR_50_ro_cc %>%
  drop_na(all_of(all_vars_12))
#Model3
SR_50_m3_data <- SR_50_ro_cc %>%
  drop_na(all_of(all_vars_3))
##Additional models ------------------------------------------------------------------
###Clinical diagnosis ------------------------------------------------------------------
# m_clinical <- glm(robust_outcome ~ clinical_diagnosis_v1,
#                   data = SR_m12_data,
#                   family = binomial)
#StartRight ------------------------------------------------------------------------
###Antibodies only ------------------------------------------------------------------------
m_antis <- glm(robust_outcome ~ num_anti,
               data = SR_m12_data,
               family = binomial)
###GRS only -------------------------------------------------------------------------------
m_grs <- glm(robust_outcome ~ scale(T1DGRS2_z) ,
             data = SR_m3_data,
             family = binomial)
#StartRight Prime -------------------------------------------------------------------
###Antibodies only ------------------------------------------------------------------------
m_50_antis <- glm(robust_outcome ~ num_anti,
               data = SR_50_m12_data,
               family = binomial)
###GRS only -------------------------------------------------------------------------------
m_50_grs <- glm(robust_outcome ~ scale(T1DGRS2_z) ,
             data = SR_50_m3_data,
             family = binomial)

#Predicted probabilities -----------------------------------------------------------------
##StartRight --------------------------------------------------------------------------
##Model 1
SR_m12_data <- prepare_prediction_data(SR_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_m12_data<- SR_m12_data %>%
  mutate(m1_pp = plogis(linpred))

##Model 2
X <- model.matrix(
  m2_formula,
  data = SR_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m2_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m2_beta), drop = FALSE]
linpred <- as.vector(X %*% m2_beta)

SR_m12_data<- SR_m12_data %>%
  mutate(m2_pp = plogis(linpred))
###Model 3
SR_m3_data <- prepare_prediction_data(SR_m3_data)
X <- model.matrix(
  m3_formula,
  data = SR_m3_data
)
colnames(X)
missing_cols <- setdiff(names(m3_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m3_beta), drop = FALSE]
linpred <- as.vector(X %*% m3_beta)

SR_m3_data<- SR_m3_data %>%
  mutate(m3_pp = plogis(linpred))
###Model 4
X <- model.matrix(
  m4_formula,
  data = SR_m3_data
)
colnames(X)
missing_cols <- setdiff(names(m4_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m4_beta), drop = FALSE]
linpred <- as.vector(X %*% m4_beta)

SR_m3_data<- SR_m3_data %>%
  mutate(m4_pp = plogis(linpred))
##StartRight Prime --------------------------------------------------------------------------
###Model 1
SR_50_m12_data <- prepare_prediction_data(SR_50_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_50_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_50_m12_data<- SR_50_m12_data %>%
  mutate(m1_pp = plogis(linpred))
###Model 2
X <- model.matrix(
  m2_formula,
  data = SR_50_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m2_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m2_beta), drop = FALSE]
linpred <- as.vector(X %*% m2_beta)

SR_50_m12_data<- SR_50_m12_data %>%
  mutate(m2_pp = plogis(linpred))
###Model 3
SR_50_m3_data <- prepare_prediction_data(SR_50_m3_data)
X <- model.matrix(
  m3_formula,
  data = SR_50_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m3_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m3_beta), drop = FALSE]
linpred <- as.vector(X %*% m3_beta)

SR_50_m3_data<- SR_50_m3_data %>%
  mutate(m3_pp = plogis(linpred))
###Model 4
X <- model.matrix(
  m4_formula,
  data = SR_50_m3_data
)
colnames(X)
missing_cols <- setdiff(names(m4_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m4_beta), drop = FALSE]
linpred <- as.vector(X %*% m4_beta)

SR_50_m3_data<- SR_50_m3_data %>%
  mutate(m4_pp = plogis(linpred))
##Table of ROC, Sens, spec--------------------------------------------------------------
###StartRight --------------------------------------------------------------------------
###Get model info for models 1-3 using 50% cut-off
model_info(pred_prob_var = "m1_pp",
           test_data = SR_m12_data,
           outcome = "robust_outcome",
           saving_name = "tables/03_04_sm_1_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
model_info(pred_prob_var = "m2_pp",
           test_data = SR_m12_data,
           outcome = "robust_outcome",
           saving_name = "tables/03_04_sm_2_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
model_info(pred_prob_var = "m3_pp",
           test_data = SR_m3_data,
           outcome = "robust_outcome",
           saving_name = "tables/03_04_sm_3_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
model_info(pred_prob_var = "m4_pp",
           test_data = SR_m3_data,
           outcome = "robust_outcome",
           saving_name = "tables/03_04_sm_4_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
###StartRight Prime --------------------------------------------------------------------------
###Get model info for models 1-3 using 50% cut-off
model_info(pred_prob_var = "m1_pp",
           test_data = SR_50_m12_data,
           outcome = "robust_outcome",
           saving_name = "tables/03_04_sm_50_1_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
model_info(pred_prob_var = "m2_pp",
           test_data = SR_50_m12_data,
           outcome = "robust_outcome",
           saving_name = "tables/03_04_sm_50_2_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
model_info(pred_prob_var = "m3_pp",
           test_data = SR_50_m3_data,
           outcome = "robust_outcome",
           saving_name = "tables/03_04_sm_50_3_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)
model_info(pred_prob_var = "m4_pp",
           test_data = SR_50_m3_data,
           outcome = "robust_outcome",
           saving_name = "tables/03_04_sm_50_4_50perc",
           manual_plotting = FALSE,
           threshold = 0.5)

###Get model info for antis, clinical & grs only models ---------------------------
#### Clinical diagnosis ------------------------------------------------------------
##### Optimal threshold ---------------------------------------------------------
# model_info(model = m_clinical,
#            test_data = SR_m12_data,
#            outcome = "robust_outcome",
#            saving_name = "tables/03_04_sm_clinical",
#            manual_plotting = TRUE,
#            manual_plot_name = "m_clinical")

##### Contingency table --------------------------------------------------------------
# varlist_cat <- c(
#   "clinical_diagnosis_v1"
# )
# clinic_m12 <- SR_m12_data %>%
#   dplyr::select(robust_outcome, clinical_diagnosis_v1) %>%
#   filter(clinical_diagnosis_v1 %in% c("Type 1", "Type 2")) %>%
#   mutate(clinical_diagnosis_v1 = ifelse(clinical_diagnosis_v1 == "Type 1", 1, 0))
# #Uncertain diagnosis = 63
# cat_contingency(varlist_cat,
#                 dataset = clinic_m12,
#                 outcome = "robust_outcome",
#                 saving_name = "tables/03_04_clinicdiag_pred",
#                 complete_case = TRUE,
#                 plots = FALSE)

#### Antibody status ----------------------------------------------------------------
#StartRight ------------------------------------------------------------------------
####Optimal threshold ------------------------------------------------------------------
model_info(model = m_antis,
           test_data = SR_m12_data,
           outcome = "robust_outcome",
           saving_name = "03_04_sm_antis",
           manual_plotting = TRUE,
           manual_plot_name = "m_antis")
#####Contingency table ----------------------------------------------------------------
antis_m12 <- SR_m12_data %>%
  dplyr::select(robust_outcome, num_anti) %>%
  mutate(anticat = ifelse(num_anti == "0", 0, 1))
varlist_cat <- c(
  "anticat"
)
cat_contingency(varlist_cat,
                dataset = antis_m12,
                outcome = "robust_outcome",
                saving_name = "tables/03_04_ANTIS_pred",
                complete_case = TRUE,
                plots = FALSE)
####GRS -------------------------------------------------------------------------
#####Optimal threshold -------------------------------------------------------------
model_info(model = m_grs,
           test_data = SR_m3_data,
           outcome = "robust_outcome",
           saving_name = "03_04_sm_grs",
           manual_plotting = TRUE,
           manual_plot_name = "m_grs")
#####Contingency table --------------------------------------------------------------
varlist <- c(
  "T1DGRS2_z"
)
model_continuous(varlist,
                 dataset = SR_m3_data,
                 outcome = "robust_outcome",
                 saving_name = "tables/03_04_T1DGRS_metrics",
                 complete_case = TRUE,
                 #thresholds = tbl1_thresholds,
                 plots = FALSE,
                 decimals = 3)

#StartRight Prime ------------------------------------------------------------------------
####Optimal threshold ------------------------------------------------------------------
model_info(model = m_antis,
           test_data = SR_50_m12_data,
           outcome = "robust_outcome",
           saving_name = "03_04_sm_50_antis",
           manual_plotting = TRUE,
           manual_plot_name = "m_50_antis")
#####Contingency table ----------------------------------------------------------------
antis_50_m12 <- SR_50_m12_data %>%
  dplyr::select(robust_outcome, num_anti) %>%
  mutate(anticat = ifelse(num_anti == "0", 0, 1))
varlist_cat <- c(
  "anticat"
)
cat_contingency(varlist_cat,
                dataset = antis_50_m12,
                outcome = "robust_outcome",
                saving_name = "tables/03_04_ANTIS_50_pred",
                complete_case = TRUE,
                plots = FALSE)
####GRS -------------------------------------------------------------------------
#####Optimal threshold -------------------------------------------------------------
model_info(model = m_grs,
           test_data = SR_50_m3_data,
           outcome = "robust_outcome",
           saving_name = "03_04_sm_50_grs",
           manual_plotting = TRUE,
           manual_plot_name = "m_50_grs")
#####Contingency table --------------------------------------------------------------
varlist <- c(
  "T1DGRS2_z"
)
model_continuous(varlist,
                 dataset = SR_50_m3_data,
                 outcome = "robust_outcome",
                 saving_name = "tables/03_04_T1DGRS_50_metrics",
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
  dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy)%>%
  rename(ROC_AUC = ROCAUC_CI)
#mutate(across(5:10,function(x) {x* 100})) #%>%
#mutate(across(5:10,~ round(.x, digits = 1)))
m2_summary <- `tables/03_04_sm_2_50perc` %>%
  dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy)%>%
  rename(ROC_AUC = ROCAUC_CI)
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
m3_summary <- `tables/03_04_sm_3_50perc` %>%
  dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy)%>%
  rename(ROC_AUC = ROCAUC_CI)
m4_summary <- `tables/03_04_sm_4_50perc` %>%
  dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy)%>%
  rename(ROC_AUC = ROCAUC_CI)
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
# clinical_summary <- `tables/03_04_clinicdiag_pred` %>%
#   rename(model = variable) %>%
#   mutate(Threshold = "T1D") %>%
#   select(model, n, `1 n`, ROC_AUC, Threshold,
#          Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
antis_summary <- `tables/03_04_ANTIS_pred` %>%
  rename(model = variable) %>%
  mutate(Threshold = "Positive") %>%
  dplyr::select(model, n, `1 n`, ROC_AUC, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
grs_summary <- `tables/03_04_T1DGRS_metrics` %>%
  rename(model = variable) %>%
  dplyr::select(model, n, `1 n`, ROC_AUC, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))

model_summary <- rbind(m1_summary,
                       m2_summary,
                       m3_summary,
                       m4_summary,
                       #clinical_summary,
                       antis_summary,
                       grs_summary) %>%
  mutate(`T1D %` = round((`1 n`/`n`)*100,1))

write_xlsx(model_summary,"tables/Supp_Table13.xlsx")

#StartRight Prime --------------------------------------------------------------------
#### 50% Thresholds ------------------------------------------------------------------
m1_summary <- `tables/03_04_sm_50_1_50perc` %>%
  dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy)%>%
  rename(ROC_AUC = ROCAUC_CI)
#mutate(across(5:10,function(x) {x* 100})) #%>%
#mutate(across(5:10,~ round(.x, digits = 1)))
m2_summary <- `tables/03_04_sm_50_2_50perc` %>%
  dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy)%>%
  rename(ROC_AUC = ROCAUC_CI)
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
m3_summary <- `tables/03_04_sm_50_3_50perc` %>%
  dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy)%>%
  rename(ROC_AUC = ROCAUC_CI)
m4_summary <- `tables/03_04_sm_50_4_50perc` %>%
  dplyr::select(model, n, `1 n`, ROCAUC_CI, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy)%>%
  rename(ROC_AUC = ROCAUC_CI)
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
# clinical_summary <- `tables/03_04_clinicdiag_pred` %>%
#   rename(model = variable) %>%
#   mutate(Threshold = "T1D") %>%
#   select(model, n, `1 n`, ROC_AUC, Threshold,
#          Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
antis_summary <- `tables/03_04_ANTIS_50_pred` %>%
  rename(model = variable) %>%
  mutate(Threshold = "Positive") %>%
  dplyr::select(model, n, `1 n`, ROC_AUC, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))
grs_summary <- `tables/03_04_T1DGRS_50_metrics` %>%
  rename(model = variable) %>%
  dplyr::select(model, n, `1 n`, ROC_AUC, Threshold,
         Sensitivity, Specificity, PPV, NPV, Accuracy) #%>%
# mutate(across(5:10,function(x) {x* 100})) %>%
# mutate(across(5:10,~ round(.x, digits = 1)))

model_summary <- rbind(m1_summary,
                       m2_summary,
                       m3_summary,
                       m4_summary,
                       #clinical_summary,
                       antis_summary,
                       grs_summary) %>%
  mutate(`T1D %` = round((`1 n`/`n`)*100,1))

write_xlsx(model_summary,"tables/Supp_Table13_Prime.xlsx")
