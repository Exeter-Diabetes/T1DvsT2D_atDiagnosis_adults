#####################################################################################
# Supplementary Figure 11
# StartRight and StartRight Prime dataset: lost to follow up data vs SR and SR prime
# Primary Outcome: T1DGRS
#####################################################################################

# Libraries -------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(writexl)

# Load functions --------------------------------------------------------------------
source("functions/model_info1.R")
# Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_noSRout_15_10_2026.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_noSRout_15_10_2026.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
#load("m1.RData")

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
  "Eth_4cat",
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
  "Eth_4cat",
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
#Model12
SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3)) %>%
  mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
#Model3

#StartRight Prime -------------------------------------------------------------------
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$num_anti <- as.character(SR_50_SRout_ccc$num_anti)
#Model12
# SR_50_m12_data <- SR_50_SRout_ccc %>%
#   drop_na(all_of(all_vars_12))
#Model12
SR_50_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12)) %>%
mutate(anti_pos = ifelse(num_anti == "0", 0, 1))

# Prepare complete case datasets ----------------------------------------------------
#StartRight
SR_noSRout <- SR_noSRout %>%
  mutate(
    bmi_model = ifelse(is.na(bmi_diag), bmi_calc_v1, bmi_diag),
    famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat),
    anti_pos = ifelse(num_anti == "0", 0, 1)
  )

#SR_noSRout$SRoutcome <- as.numeric(SR_noSRout$SRoutcome)
SR_noSRout$DKA <- as.character(SR_noSRout$DKA)
SR_noSRout$osmotic <- as.character(SR_noSRout$osmotic)
SR_noSRout$autoimmune <- as.character(SR_noSRout$autoimmune)

SR_no_m12_data <- SR_noSRout %>%
  drop_na(all_of(all_vars_12))
#StartRight Prime
SR_50_noSRout <- SR_50_noSRout %>%
  mutate(
    bmi_model = ifelse(is.na(bmi_diag), bmi_calc_v1, bmi_diag),
    famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat),
    anti_pos = ifelse(num_anti == "0", 0, 1)
  )

#SR_50_noSRout$SRoutcome <- as.numeric(SR_50_noSRout$SRoutcome)
SR_50_noSRout$DKA <- as.character(SR_50_noSRout$DKA)
SR_50_noSRout$osmotic <- as.character(SR_50_noSRout$osmotic)
SR_50_noSRout$autoimmune <- as.character(SR_50_noSRout$autoimmune)

SR_no_50_m12_data <- SR_50_noSRout %>%
  drop_na(all_of(all_vars_12))
#--------------------------------------------------------------------------------------------------------
#Get predicted probabilities ---------------------------------------------------------
#StartRight
###Included
#Model 1
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
#Model 2
#SR_m12_data <- prepare_prediction_data(SR_m12_data)
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
###Excluded
##Model 1
SR_no_m12_data <- prepare_prediction_data(SR_no_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_no_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_no_m12_data<- SR_no_m12_data %>%
  mutate(m1_pp = plogis(linpred))
##Model 2
#SR_no_m12_data <- prepare_prediction_data(SR_no_m12_data)
X <- model.matrix(
  m2_formula,
  data = SR_no_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m2_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m2_beta), drop = FALSE]
linpred <- as.vector(X %*% m2_beta)

SR_no_m12_data<- SR_no_m12_data %>%
  mutate(m2_pp = plogis(linpred))

#StartRight Prime
###Included
#Model 1
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
#Model 2
#SR_50_m12_data <- prepare_prediction_data(SR_50_m12_data)
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
###Excluded
#Model 1
SR_no_50_m12_data <- prepare_prediction_data(SR_no_50_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_no_50_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_no_50_m12_data<- SR_no_50_m12_data %>%
  mutate(m1_pp = plogis(linpred))
#Model 2
#SR_no_50_m12_data <- prepare_prediction_data(SR_no_50_m12_data)
X <- model.matrix(
  m2_formula,
  data = SR_no_50_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m2_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m2_beta), drop = FALSE]
linpred <- as.vector(X %*% m2_beta)

SR_no_50_m12_data<- SR_no_50_m12_data %>%
  mutate(m2_pp = plogis(linpred))
###Calibration prep  ----------------------------------------
##StartRight -------------------------------------------------------------------------
##Included
#Model 1
SR_m12_data <- SR_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0,
                              "0",
                              "1")#,
         #m1_pp = predict(m1, SR_m12_data, type = "response")
         )
brks_SR_nm1 <- quantile(SR_m12_data$m1_pp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE)
brks_SR_nm1[1] <- 0.99 * brks_SR_nm1[1]
brks_SR_nm1[length(brks_SR_nm1)] <- 1.1 * brks_SR_nm1[length(brks_SR_nm1)]
dec_SR_nm1 <- cut(
  SR_m12_data$m1_pp,
  breaks = brks_SR_nm1,
  include_lowest = TRUE
)


dec_SR_nm1 <- data.frame(y = SR_m12_data$T1DGRS2_z,
                         pred = SR_m12_data$m1_pp,
                         dec = dec_SR_nm1) %>%
  group_by(dec) %>%
  mutate(prob_obs = mean(y, na.rm = TRUE),
         n_group = n(),
         mnpred = mean(pred, na.rm = TRUE),
         mean_ci = list(mean_cl_normal(y) %>%
                        rename(mean = y, lci = ymin, uci = ymax))) %>%
  unnest(cols = mean_ci)
#Model 2
# SR_m12_data <- SR_m12_data %>%
#   mutate(anti_status = ifelse(num_anti == 0,
#                               "0",
#                               "1")#,
#          #m2_pp = predict(m1, SR_m12_data, type = "response")
#          )
brks_SR_nm2 <- quantile(SR_m12_data$m2_pp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE)
brks_SR_nm2[1] <- 0.99 * brks_SR_nm2[1]
brks_SR_nm2[length(brks_SR_nm2)] <- 1.1 * brks_SR_nm2[length(brks_SR_nm2)]
dec_SR_nm2 <- cut(
  SR_m12_data$m2_pp,
  breaks = brks_SR_nm2,
  include_lowest = TRUE
)


dec_SR_nm2 <- data.frame(y = SR_m12_data$T1DGRS2_z,
                         pred = SR_m12_data$m2_pp,
                         dec = dec_SR_nm2) %>%
  group_by(dec) %>%
  mutate(prob_obs = mean(y, na.rm = TRUE),
         n_group = n(),
         mnpred = mean(pred, na.rm = TRUE),
         mean_ci = list(mean_cl_normal(y) %>%
                          rename(mean = y, lci = ymin, uci = ymax))) %>%
  unnest(cols = mean_ci)
##Excluded
##Model 1
SR_no_m12_data <- SR_no_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0,
                              "0",
                              "1")#,
         #m1_pp = predict(m1, SR_no_m12_data, type = "response")
         )
brks_SR_nm1 <- quantile(SR_no_m12_data$m1_pp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE)
brks_SR_nm1[1] <- 0.99 * brks_SR_nm1[1]
brks_SR_nm1[length(brks_SR_nm1)] <- 1.1 * brks_SR_nm1[length(brks_SR_nm1)]
dec_no_SR_nm1 <- cut(
  SR_no_m12_data$m1_pp,
  breaks = brks_SR_nm1,
  include_lowest = TRUE
)


dec_no_SR_nm1 <- data.frame(y = SR_no_m12_data$T1DGRS2_z,
                            pred = SR_no_m12_data$m1_pp,
                            dec = dec_no_SR_nm1) %>%
  group_by(dec) %>%
  mutate(prob_obs = mean(y, na.rm = TRUE),
         n_group = n(),
         mnpred = mean(pred, na.rm = TRUE),
         mean_ci = list(mean_cl_normal(y) %>%
                          rename(mean = y, lci = ymin, uci = ymax))) %>%
  unnest(cols = mean_ci)
##Model 2
# SR_no_m12_data <- SR_no_m12_data %>%
#   mutate(anti_status = ifelse(num_anti == 0,
#                               "0",
#                               "1"),
#          m1_pp = predict(m1, SR_no_m12_data, type = "response"))
brks_SR_nm1 <- quantile(SR_no_m12_data$m2_pp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE)
brks_SR_nm1[1] <- 0.99 * brks_SR_nm1[1]
brks_SR_nm1[length(brks_SR_nm1)] <- 1.1 * brks_SR_nm1[length(brks_SR_nm1)]
dec_no_SR_nm2 <- cut(
  SR_no_m12_data$m2_pp,
  breaks = brks_SR_nm1,
  include_lowest = TRUE
)


dec_no_SR_nm2 <- data.frame(y = SR_no_m12_data$T1DGRS2_z,
                            pred = SR_no_m12_data$m2_pp,
                            dec = dec_no_SR_nm2) %>%
  group_by(dec) %>%
  mutate(prob_obs = mean(y, na.rm = TRUE),
         n_group = n(),
         mnpred = mean(pred, na.rm = TRUE),
         mean_ci = list(mean_cl_normal(y) %>%
                          rename(mean = y, lci = ymin, uci = ymax))) %>%
  unnest(cols = mean_ci)
#StartRight Prime -------------------------------------------------------------------
##Included
#Model 1
SR_50_m12_data <- SR_50_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0,
                              "0",
                              "1")#,
         #m1_pp = predict(m1, SR_50_m12_data, type = "response")
         )
brks_50_SR_nm1 <- quantile(SR_50_m12_data$m1_pp,
                           probs = seq(0, 1, by = 0.1),
                           na.rm = TRUE)
brks_50_SR_nm1[1] <- 0.99 * brks_50_SR_nm1[1]
brks_50_SR_nm1[length(brks_50_SR_nm1)] <- 1.1 * brks_50_SR_nm1[length(brks_50_SR_nm1)]
dec_50_SR_nm1 <- cut(
  SR_50_m12_data$m1_pp,
  breaks = brks_50_SR_nm1,
  include_lowest = TRUE
)

dec_50_SR_nm1 <- data.frame(y = SR_50_m12_data$T1DGRS2_z,
                            pred = SR_50_m12_data$m1_pp,
                            dec = dec_50_SR_nm1) %>%
  group_by(dec) %>%
  mutate(prob_obs = mean(y, na.rm = TRUE),
         n_group = n(),
         mnpred = mean(pred, na.rm = TRUE),
         mean_ci = list(mean_cl_normal(y) %>%
                          rename(mean = y, lci = ymin, uci = ymax))) %>%
  unnest(cols = mean_ci)
#Model 2
# SR_50_m12_data <- SR_50_m12_data %>%
#   mutate(anti_status = ifelse(num_anti == 0,
#                               "0",
#                               "1"),
#          m1_pp = predict(m1, SR_50_m12_data, type = "response"))
brks_50_SR_nm1 <- quantile(SR_50_m12_data$m2_pp,
                           probs = seq(0, 1, by = 0.1),
                           na.rm = TRUE)
brks_50_SR_nm1[1] <- 0.99 * brks_50_SR_nm1[1]
brks_50_SR_nm1[length(brks_50_SR_nm1)] <- 1.1 * brks_50_SR_nm1[length(brks_50_SR_nm1)]
dec_50_SR_nm2 <- cut(
  SR_50_m12_data$m2_pp,
  breaks = brks_50_SR_nm1,
  include_lowest = TRUE
)

dec_50_SR_nm2 <- data.frame(y = SR_50_m12_data$T1DGRS2_z,
                            pred = SR_50_m12_data$m2_pp,
                            dec = dec_50_SR_nm2) %>%
  group_by(dec) %>%
  mutate(prob_obs = mean(y, na.rm = TRUE),
         n_group = n(),
         mnpred = mean(pred, na.rm = TRUE),
         mean_ci = list(mean_cl_normal(y) %>%
                          rename(mean = y, lci = ymin, uci = ymax))) %>%
  unnest(cols = mean_ci)

##Excluded
#Model 1
SR_no_50_m12_data <- SR_no_50_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0,
                              "0",
                              "1")#,
         #m1_pp = predict(m1, SR_no_50_m12_data, type = "response")
         )
brks_SR_nm1 <- quantile(SR_no_50_m12_data$m1_pp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE)
brks_SR_nm1[1] <- 0.99 * brks_SR_nm1[1]
brks_SR_nm1[length(brks_SR_nm1)] <- 1.1 * brks_SR_nm1[length(brks_SR_nm1)]
dec_no_50_SR_nm1 <- cut(
  SR_no_50_m12_data$m1_pp,
  breaks = brks_SR_nm1,
  include_lowest = TRUE
)


dec_no_50_SR_nm1 <- data.frame(y = SR_no_50_m12_data$T1DGRS2_z,
                         pred = SR_no_50_m12_data$m1_pp,
                         dec = dec_no_50_SR_nm1) %>%
  group_by(dec) %>%
  mutate(prob_obs = mean(y, na.rm = TRUE),
         n_group = n(),
         mnpred = mean(pred, na.rm = TRUE),
         mean_ci = list(mean_cl_normal(y) %>%
                          rename(mean = y, lci = ymin, uci = ymax))) %>%
  unnest(cols = mean_ci)
#Model 2
# SR_no_50_m12_data <- SR_no_50_m12_data %>%
#   mutate(anti_status = ifelse(num_anti == 0,
#                               "0",
#                               "1"),
#          m1_pp = predict(m1, SR_no_50_m12_data, type = "response"))
brks_SR_nm1 <- quantile(SR_no_50_m12_data$m2_pp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE)
brks_SR_nm1[1] <- 0.99 * brks_SR_nm1[1]
brks_SR_nm1[length(brks_SR_nm1)] <- 1.1 * brks_SR_nm1[length(brks_SR_nm1)]
dec_no_50_SR_nm2 <- cut(
  SR_no_50_m12_data$m2_pp,
  breaks = brks_SR_nm1,
  include_lowest = TRUE
)


dec_no_50_SR_nm2 <- data.frame(y = SR_no_50_m12_data$T1DGRS2_z,
                               pred = SR_no_50_m12_data$m2_pp,
                               dec = dec_no_50_SR_nm2) %>%
  group_by(dec) %>%
  mutate(prob_obs = mean(y, na.rm = TRUE),
         n_group = n(),
         mnpred = mean(pred, na.rm = TRUE),
         mean_ci = list(mean_cl_normal(y) %>%
                          rename(mean = y, lci = ymin, uci = ymax))) %>%
  unnest(cols = mean_ci)

## --- Compute dataset counts for model headings ----------------------------
m12_n_start <- nrow(SR_m12_data)
n12_T1D_start <- SR_m12_data %>%
  filter(SRoutcome == 1) %>%
  nrow()
m12_n_prime <- nrow(SR_50_m12_data)
n12_T1D_prime <- SR_50_m12_data %>%
  filter(SRoutcome == 1) %>%
  nrow()
m12_n_start_no <- nrow(SR_no_m12_data)
m12_n_prime_no <- nrow(SR_no_50_m12_data)

## --- Title grobs (right-aligned) -----------------------------------------
model1_text_start_m1 <- paste0("StartRight: Clinical features only model")
model1_text_prime_m1 <- paste0("StartRight Prime: Clinical features only model")
model1_text_start_m2 <- paste0("StartRight: Clinical features + antibodies model")
model1_text_prime_m2 <- paste0("StartRight Prime: Clinical features + antibodies model")

model1_text_start_with <- paste0("Included: With 3-year C-peptide \n (n=", m12_n_start, "; Type 1=", n12_T1D_start, ")")
model1_text_prime_with <- paste0("Included: With 3-year C-peptide \n (n=", m12_n_prime, "; Type 1=", n12_T1D_prime, ")")
model1_text_start_without <- paste0("Excluded: Without 3-year C-peptide \n (n=", m12_n_start_no, ")")
model1_text_prime_without <- paste0("Excluded: Without 3-year C-peptide \n (n=", m12_n_prime_no, ")")

title_m1_start <- patchwork::wrap_elements(ggpubr::text_grob(model1_text_start_m1, face = "bold", size = 20, hjust = 1))
title_m1_prime <- patchwork::wrap_elements(ggpubr::text_grob(model1_text_prime_m1, face = "bold", size = 20, hjust = 0.85))
title_m2_start <- patchwork::wrap_elements(ggpubr::text_grob(model1_text_start_m2, face = "bold", size = 20, hjust = 0.85))
title_m2_prime <- patchwork::wrap_elements(ggpubr::text_grob(model1_text_prime_m2, face = "bold", size = 20, hjust = 0.75))



#Plot prep --------------------------------------------------------------------------
DESIGN_abc <- "
11111#
  222333
  222333
  222333
  222333
  222333
"
model_display_item1 <- patchwork::wrap_plots(
  title_m1_start,
  #calibration plot for model 1 included
  ggplot(dec_SR_nm1, aes(x = mnpred,
                         y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features model") +
    ylab("Mean T1DGRS (z-score) \n of each decile") +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(-0.5, 1.5)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci
    ),
    width = 0.1,
    size = 0.7) +
    labs(title = model1_text_start_with) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    ),
  #Calibration plot for model 1 exlcuded
  ggplot(dec_no_SR_nm1, aes(x = mnpred,
                         y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features model") +
    ylab("Mean T1DGRS (z-score) \n of each decile") +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    #ylim(c(0, 1)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci
                      ),
                  width = 0.1,
                  size = 0.7) +
    labs(title = model1_text_start_without) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )) + patchwork::plot_layout(design = DESIGN_abc)
model_display_item2 <- patchwork::wrap_plots(
  #StartRight Prime
  title_m1_prime,
  #calibration plot for model 1 included
  ggplot(dec_50_SR_nm1, aes(x = mnpred,
                         y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features model") +
    ylab("Mean T1DGRS (z-score) \n of each decile") +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(-1, 2.25)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci
    ),
    width = 0.1,
    size = 0.7) +
    theme_bw() +
    labs(title = model1_text_prime_with) +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    ),
  #Calibration plot for model 1 excluded
  ggplot(dec_no_50_SR_nm1, aes(x = mnpred,
                         y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features model") +
    ylab("Mean T1DGRS (z-score) \n of each decile") +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    #ylim(c(0, 1)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci
    ),
    width = 0.1,
    size = 0.7) +
    labs(title = model1_text_prime_without) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )) + patchwork::plot_layout(design = DESIGN_abc)

model_display_item3 <- patchwork::wrap_plots(
  title_m2_start,
  #calibration plot for model 1 included
  ggplot(dec_SR_nm2, aes(x = mnpred,
                         y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features + antibodies model") +
    ylab("Mean T1DGRS (z-score) \n of each decile") +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(-0.5, 1.5)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci
    ),
    width = 0.1,
    size = 0.7) +
    labs(title = model1_text_start_with) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    ),
  #Calibration plot for model 1 exlcuded
  ggplot(dec_no_SR_nm2, aes(x = mnpred,
                            y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features + antibodies  model") +
    ylab("Mean T1DGRS (z-score) \n of each decile") +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    #ylim(c(0, 1)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci
    ),
    width = 0.1,
    size = 0.7) +
    labs(title = model1_text_start_without) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )) + patchwork::plot_layout(design = DESIGN_abc)
model_display_item4 <- patchwork::wrap_plots(
  #StartRight Prime
  title_m2_prime,
  #calibration plot for model 1 included
  ggplot(dec_50_SR_nm2, aes(x = mnpred,
                            y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features + antibodies model") +
    ylab("Mean T1DGRS (z-score) \n of each decile") +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(-1, 2.25)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci
    ),
    width = 0.1,
    size = 0.7) +
    theme_bw() +
    labs(title = model1_text_prime_with) +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    ),
  #Calibration plot for model 1 excluded
  ggplot(dec_no_50_SR_nm2, aes(x = mnpred,
                               y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features + antibodies model") +
    ylab("Mean T1DGRS (z-score) \n of each decile") +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    #ylim(c(0, 1)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci
    ),
    width = 0.1,
    size = 0.7) +
    labs(title = model1_text_prime_without) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )) + patchwork::plot_layout(design = DESIGN_abc)

pdf("figures/Supp_Figurex11.pdf", height = 12, width = 28)
model_display_item <- patchwork::wrap_plots(
  model_display_item1,
  model_display_item2,
  model_display_item3,
  model_display_item4,
  ncol = 2, nrow = 2
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "", "C", "D",
                                                   "", "E", "F", "", "G", "H")))
print(model_display_item)
dev.off()
ggsave("figures/Supp_Figurex11.jpeg", model_display_item, height = 12, width = 28)
