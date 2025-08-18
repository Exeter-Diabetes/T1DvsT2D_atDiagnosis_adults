#####################################################################################

#Paper figures

#To get the two final paper complete case model datasets

#Supplementary Table 12:
#Optimism metrics for 
#Model 1
#Model 2
#Model 3

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(writexl)
library(pROC)
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
##Models ----------------------------------------------------------------------------
###Model 1 ---------------------------------------------------------------------------
SR_12_dd <- SR_m12_data %>%
  dplyr::select(all_of(all_vars_12), "SRoutcome") %>%
  as.data.frame()
dd <- datadist(SR_12_dd); options(datadist = 'dd')
m1_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) + 
                scale(bmi_model) + 
                scale(HbA1c_at_diagnosis_v1) +
                #scale(c_peptide_v1) +
                Gender_v1 + 
                osmotic + 
                autoimmune + 
                Unintentional_weight_loss_v1 + 
                DKA + 
                famhisnoninsdiab,
              #famhisauto,
              data = SR_12_dd,
              x=TRUE, 
              y = TRUE)
###Model 2 --------------------------------------------------------------------------
m2_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) + 
                scale(bmi_model) + 
                scale(HbA1c_at_diagnosis_v1) +
                #scale(c_peptide_v1) +
                Gender_v1 + 
                osmotic + 
                autoimmune + 
                Unintentional_weight_loss_v1 + 
                DKA + 
                famhisnoninsdiab +
                #famhisauto +
                num_anti,
              data = SR_12_dd,
              x=TRUE, 
              y = TRUE)

###Model 3 ------------------------------------------------------------------------
SR_3_dd <- SR_m3_data %>%
  dplyr::select(all_of(all_vars_3), "SRoutcome") %>%
  as.data.frame()
dd3 <- datadist(SR_3_dd); options(datadist = 'dd')
m3_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) + 
                scale(bmi_model) + 
                scale(HbA1c_at_diagnosis_v1) +
                #scale(c_peptide_v1) +
                Gender_v1 + 
                osmotic + 
                autoimmune + 
                Unintentional_weight_loss_v1 + 
                DKA + 
                famhisnoninsdiab +
                #famhisauto +
                num_anti +
                scale(T1DGRS2_z),
              data = SR_3_dd,
              x=TRUE, 
              y = TRUE)

##Optimism table-----------------------------------------------------------------------
###Model 1 -------------------------------------------------------------------------------
#### RMS: VALIDATE -------------------------------------------------------------------------
m1_optimism_rms <- rms::validate(m1_rms, 
                                 B = 1000, 
                                 maxit = 20)
m1_optimism <- unclass(m1_optimism_rms)
m1_optimism <- data.frame(`Performance metric` = row.names(m1_optimism), m1_optimism)
colnames(m1_optimism) <- c("Performance metric", 
                           "Full",
                           "Bootstrap:training",
                           "Bootstrap:test",
                           "Optimism",
                           "Full:corrected",
                           "Bootstrap:n")
write_xlsx(m1_optimism,"tables/m1_optimism_table.xlsx")

#### ADDITIONAL OPTIMISM METRICS --------------------------------------------------------
SR_m12_data <- SR_m12_data %>%
  mutate(id = 1:n())

original_predictions1 <- predict(m1, 
                                 newdata = SR_m12_data, 
                                 type = "response") %>%
  cbind(id = SR_m12_data$id)
full_roc_model <- roc(SR_m12_data$SRoutcome, 
                      original_predictions1[,1], 
                      plot = FALSE, 
                      ci = TRUE)
full_calinlarge <- mean(original_predictions1[,1], na.rm = TRUE) - (sum(SR_m12_data$SRoutcome)/nrow(SR_m12_data))
#Extract valuable summary information from ROC object

#Make bootstrap selection of full data samples with replacement
set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m12_data, 100)

#bootstrapped_estimations <- data.frame(training = NULL, test = NULL)
bootstrapped_ROC <- data.frame(training = NA, test = NA)
bootstrapped_meanprob <- data.frame(training = NA, test = NA)
# mean(nm1_boot$t)
# quantile(nm1_boot$t, 0.025)
# quantile(nm1_boot$t, 0.975)


for (i in 1:100) {
  #This is the bootstrap sample
  new_dataset <- SR_m12_data[dataset_bootstrap$strap[[i]]$idx,]
  ROC <- data.frame(training = NA, test = NA)
  mPROB <- data.frame(training = NA, test = NA)
  #Estimate model in bootstrap sample
  model <- glm(SRoutcome ~ scale(AgeatDiagnosis) + 
                 scale(bmi_model) + 
                 scale(HbA1c_at_diagnosis_v1) +
                 Gender_v1 + 
                 osmotic + 
                 autoimmune + 
                 Unintentional_weight_loss_v1 + 
                 DKA + 
                 famhisnoninsdiab +
                 famhisauto, 
               data = new_dataset, 
               family = binomial)
  #Predict bootstrap model to bootstrap sample
  boot_predictions_boot <- predict(model, 
                                   newdata = new_dataset,
                                   type = "response") 
  #Predict bootstrap model to full/original data 
  boot_predictions_full <- predict(model, 
                                   newdata = SR_m12_data,
                                   type = "response") 
  
  training_roc_model <- roc(new_dataset$SRoutcome, 
                            boot_predictions_boot, 
                            plot = FALSE, 
                            ci = TRUE)
  #Extract valuable summary information from ROC object
  
  ROC$training <- training_roc_model$auc
  mPROB$training <- mean(boot_predictions_boot,
                         na.rm = TRUE) - (sum(new_dataset$SRoutcome)/nrow(new_dataset))
  test_roc_model <- roc(SR_m12_data$SRoutcome,
                        boot_predictions_full, 
                        plot = FALSE, 
                        ci = TRUE)
  
  
  ROC$test <- test_roc_model$auc
  mPROB$test <- mean(boot_predictions_full, 
                     na.rm = TRUE) - (sum(SR_m12_data$SRoutcome)/nrow(SR_m12_data))
  #sum_table$ROCAUC <- roc_model$auc
  #sum_table$ROC_CI <- paste0("(", round(roc_model$ci[1], 3), ";", round(roc_model$ci[3], 3), ")")
  bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
  bootstrapped_meanprob <- rbind(bootstrapped_ROC, mPROB)
  
}

m1_optimism_rocs <- data.frame(
  `Performance metric` = "ROCAUC",
  Full = full_roc_model$auc,
  `Bootstrap.training` = mean(bootstrapped_ROC$training, na.rm = TRUE),
  `Bootstrap.test` = mean(bootstrapped_ROC$test, na.rm = TRUE)
) %>%
  mutate(
    Optimism = `Bootstrap.training` - `Bootstrap.test`,
    `Full.corrected`= Full - Optimism,
    `Bootstrap.n` = 1000
  )

m1_optimism_add <- data.frame(
  `Performance metric` = "Calibration-in-the-large",
  Full = full_calinlarge,
  `Bootstrap.training` = mean(bootstrapped_meanprob$training, na.rm = TRUE),
  `Bootstrap.test` = mean(bootstrapped_meanprob$test, na.rm = TRUE)
) %>%
  mutate(
    Optimism = `Bootstrap.training` - `Bootstrap.test`,
    `Full.corrected`= Full - Optimism,
    `Bootstrap.n` = 1000
  ) %>%
  rbind(m1_optimism_rocs)
colnames(m1_optimism_add) <- c("Performance metric", 
                               "Full",
                               "Bootstrap:training",
                               "Bootstrap:test",
                               "Optimism",
                               "Full:corrected",
                               "Bootstrap:n")

m1_optimism <- rbind(m1_optimism, 
                     m1_optimism_add)
write_xlsx(m1_optimism,"tables/Supp_Table12_m1.xlsx")
###Model 2 -------------------------------------------------------------------------------
#### RMS: VALIDATE -------------------------------------------------------------------------
m2_optimism_rms <- rms::validate(m2_rms, 
                                 B = 1000, 
                                 maxit = 20)
m2_optimism <- unclass(m2_optimism_rms)
m2_optimism <- data.frame(`Performance metric` = row.names(m2_optimism), 
                          m2_optimism)
colnames(m2_optimism) <- c("Performance metric", 
                           "Full",
                           "Bootstrap:training",
                           "Bootstrap:test",
                           "Optimism",
                           "Full:corrected",
                           "Bootstrap:n")

#### ADDITIONAL OPTIMISM METRICS --------------------------------------------------------
original_predictions2 <- predict(m2, 
                                 newdata = SR_m12_data, 
                                 type = "response") %>%
  cbind(id = SR_m12_data$id)

full_roc_model <- roc(SR_m12_data$SRoutcome, 
                      original_predictions2[,1], 
                      plot = FALSE, 
                      #print.thres = "best", 
                      #print.auc = TRUE, 
                      ci = TRUE)
full_calinlarge <- mean(original_predictions2[,1], 
                        na.rm = TRUE) - (sum(SR_m12_data$SRoutcome)/nrow(SR_m12_data))
#Extract valuable summary information from ROC object

#Make bootstrap selection of full data samples with replacement
set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m12_data, 100)

#bootstrapped_estimations <- data.frame(training = NULL, test = NULL)
bootstrapped_ROC <- data.frame(training = NA, test = NA)
bootstrapped_meanprob <- data.frame(training = NA, test = NA)


for (i in 1:100) {
  #This is the bootstrap sample
  new_dataset <- SR_m12_data[dataset_bootstrap$strap[[i]]$idx,]
  ROC <- data.frame(training = NA, test = NA)
  mPROB <- data.frame(training = NA, test = NA)
  #Estimate model in bootstrap sample
  model <- glm(SRoutcome ~ scale(AgeatDiagnosis) + 
                 scale(bmi_model) + 
                 scale(HbA1c_at_diagnosis_v1) +
                 Gender_v1 + 
                 osmotic + 
                 autoimmune + 
                 Unintentional_weight_loss_v1 + 
                 DKA + 
                 famhisnoninsdiab +
                 famhisauto +
                 num_anti, 
               data = new_dataset, 
               family = binomial)
  #Predict bootstrap model to bootstrap sample
  boot_predictions_boot <- predict(model, 
                                   newdata = new_dataset,
                                   type = "response") 
  #Predict bootstrap model to full/original data 
  boot_predictions_full <- predict(model, 
                                   newdata = SR_m12_data,
                                   type = "response") #%>%
  
  training_roc_model <- roc(new_dataset$SRoutcome, 
                            boot_predictions_boot, 
                            plot = FALSE, 
                            
                            ci = TRUE)
  
  ROC$training <- training_roc_model$auc
  mPROB$training <- mean(boot_predictions_boot, 
                         na.rm = TRUE) - (sum(new_dataset$SRoutcome)/nrow(new_dataset))
  
  test_roc_model <- roc(SR_m12_data$SRoutcome, 
                        boot_predictions_full, 
                        plot = FALSE, 
                        ci = TRUE)
  
  ROC$test <- test_roc_model$auc
  mPROB$test <- mean(boot_predictions_full, 
                     na.rm = TRUE) - (sum(SR_m12_data$SRoutcome)/nrow(SR_m12_data))
  #sum_table$ROCAUC <- roc_model$auc
  #sum_table$ROC_CI <- paste0("(", round(roc_model$ci[1], 3), ";", round(roc_model$ci[3], 3), ")")
  bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
  bootstrapped_meanprob <- rbind(bootstrapped_ROC, mPROB)
  
}

m2_optimism_rocs <- data.frame(
  `Performance metric` = "ROCAUC",
  Full = full_roc_model$auc,
  `Bootstrap.training` = mean(bootstrapped_ROC$training, na.rm = TRUE),
  `Bootstrap.test` = mean(bootstrapped_ROC$test, na.rm = TRUE)
) %>%
  mutate(
    Optimism = `Bootstrap.training` - `Bootstrap.test`,
    `Full.corrected`= Full - Optimism,
    `Bootstrap.n` = 1000
  )

m2_optimism_add <- data.frame(
  `Performance metric` = "Calibration-in-the-large",
  Full = full_calinlarge,
  `Bootstrap.training` = mean(bootstrapped_meanprob$training, na.rm = TRUE),
  `Bootstrap.test` = mean(bootstrapped_meanprob$test, na.rm = TRUE)
) %>%
  mutate(
    Optimism = `Bootstrap.training` - `Bootstrap.test`,
    `Full.corrected`= Full - Optimism,
    `Bootstrap.n` = 1000
  ) %>%
  rbind(m2_optimism_rocs)
colnames(m2_optimism_add) <- c("Performance metric", 
                               "Full",
                               "Bootstrap:training",
                               "Bootstrap:test",
                               "Optimism",
                               "Full:corrected",
                               "Bootstrap:n")

m2_optimism <- rbind(m2_optimism, 
                     m2_optimism_add)
write_xlsx(m2_optimism,"tables/Supp_Table12_m2.xlsx")
###Model 3 -------------------------------------------------------------------------------
#### RMS: VALIDATE -------------------------------------------------------------------------
m3_optimism_rms <- rms::validate(m3_rms, 
                                 B = 1000, 
                                 maxit = 20)
m3_optimism <- unclass(m3_optimism_rms)
m3_optimism <- data.frame(`Performance metric` = row.names(m3_optimism), 
                          m3_optimism)
colnames(m3_optimism) <- c("Performance metric", 
                           "Full",
                           "Bootstrap:training",
                           "Bootstrap:test",
                           "Optimism",
                           "Full:corrected",
                           "Bootstrap:n")

#### ADDITIONAL OPTIMISM METRICS --------------------------------------------------------
SR_m3_data <- SR_m3_data %>%
  mutate(id = 1:n())


original_predictions3 <- predict(m3, 
                                 newdata = SR_m3_data, 
                                 type = "response") %>%
  cbind(id = SR_m3_data$id)

full_roc_model <- roc(SR_m3_data$SRoutcome, 
                      original_predictions3[,1], 
                      plot = FALSE, 
                      ci = TRUE)
full_calinlarge <- mean(original_predictions3[,1], 
                        na.rm = TRUE) - (sum(SR_m3_data$SRoutcome)/nrow(SR_m3_data))
#Extract valuable summary information from ROC object

#Make bootstrap selection of full data samples with replacement
set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m3_data, 100)

bootstrapped_ROC <- data.frame(training = NA, test = NA)
bootstrapped_meanprob <- data.frame(training = NA, test = NA)


for (i in 1:100) {
  #This is the bootstrap sample
  new_dataset <- SR_m3_data[dataset_bootstrap$strap[[i]]$idx,]
  ROC <- data.frame(training = NA, test = NA)
  mPROB <- data.frame(training = NA, test = NA)
  #Estimate model in bootstrap sample
  model <- glm(SRoutcome ~ scale(AgeatDiagnosis) + 
                 scale(bmi_model) + 
                 scale(HbA1c_at_diagnosis_v1) +
                 Gender_v1 + 
                 osmotic + 
                 autoimmune + 
                 Unintentional_weight_loss_v1 + 
                 DKA + 
                 famhisnoninsdiab +
                 famhisauto +
                 num_anti +
                 scale(T1DGRS2_z), 
               data = new_dataset, 
               family = binomial)
  #Predict bootstrap model to bootstrap sample
  boot_predictions_boot <- predict(model, 
                                   newdata = new_dataset,
                                   type = "response") 
  #Predict bootstrap model to full/original data 
  boot_predictions_full <- predict(model, 
                                   newdata = SR_m3_data,
                                   type = "response") #%>%
  
  training_roc_model <- roc(new_dataset$SRoutcome, 
                            boot_predictions_boot, 
                            plot = FALSE, 
                            ci = TRUE)
  
  ROC$training <- training_roc_model$auc
  mPROB$training <- mean(boot_predictions_boot, 
                         na.rm = TRUE) - (sum(new_dataset$SRoutcome)/nrow(new_dataset))
  
  test_roc_model <- roc(SR_m3_data$SRoutcome, 
                        boot_predictions_full, 
                        plot = FALSE, 
                        ci = TRUE)
  #Extract valuable summary information from ROC object
  
  ROC$test <- test_roc_model$auc
  mPROB$test <- mean(boot_predictions_full, 
                     na.rm = TRUE) - (sum(SR_m3_data$SRoutcome)/nrow(SR_m3_data))
  #sum_table$ROCAUC <- roc_model$auc
  #sum_table$ROC_CI <- paste0("(", round(roc_model$ci[1], 3), ";", round(roc_model$ci[3], 3), ")")
  bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
  bootstrapped_meanprob <- rbind(bootstrapped_ROC, mPROB)
  
}

m3_optimism_rocs <- data.frame(
  `Performance metric` = "ROCAUC",
  Full = full_roc_model$auc,
  `Bootstrap.training` = mean(bootstrapped_ROC$training, na.rm = TRUE),
  `Bootstrap.test` = mean(bootstrapped_ROC$test, na.rm = TRUE)
  #Optimism = `Bootstrap:training` - `Bootstrap:test`,
  #`Full:corrected`= Full - Optimism,
  
) %>%
  mutate(
    Optimism = `Bootstrap.training` - `Bootstrap.test`,
    `Full.corrected`= Full - Optimism,
    `Bootstrap.n` = 1000
  )

m3_optimism_add <- data.frame(
  `Performance metric` = "Calibration-in-the-large",
  Full = full_calinlarge,
  `Bootstrap.training` = mean(bootstrapped_meanprob$training, na.rm = TRUE),
  `Bootstrap.test` = mean(bootstrapped_meanprob$test, na.rm = TRUE)
  #Optimism = `Bootstrap:training` - `Bootstrap:test`,
  #`Full:corrected`= Full - Optimism,
  
) %>%
  mutate(
    Optimism = `Bootstrap.training` - `Bootstrap.test`,
    `Full.corrected`= Full - Optimism,
    `Bootstrap.n` = 1000
  ) %>%
  rbind(m3_optimism_rocs)
colnames(m3_optimism_add) <- c("Performance metric", 
                               "Full",
                               "Bootstrap:training",
                               "Bootstrap:test",
                               "Optimism",
                               "Full:corrected",
                               "Bootstrap:n")

m3_optimism <- rbind(m3_optimism, m3_optimism_add)
write_xlsx(m3_optimism,"tables/Supp_Table12_m3.xlsx")