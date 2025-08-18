############################################################################################
#
#Identifying features at presentation that can differentiate between T1D & T2D
#beyond Age at diagnosis & BMI

#This does this in StartRight 18-50s
#Primary & Secondary outcome

#This produces Supplementary Table 9A

#############################################################################################
#load libraries
library(tidyverse)

#load functions
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")

#load data-------------------------------------------------------------------------
#load dataset from 01_11
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/data/SR_ro_cc_1_2025.RData")

#Add model vars ----------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto)
  ) 
SR_ro_cc <- SR_ro_cc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto)
  ) 



#Create variable lists -----------------------------------------------------------------
## Continuous variables 
varlist = c("AgeatDiagnosis", 
            "bmi_model", 
            #"Waist_v1", 
            "wh_ratio_v1",
            "HbA1c_at_diagnosis_v1"
)
## create varlist_cat (categorical variables of interest names)
varlist_cat = c(
  "Gender_v1",
  "DKA", 
  "Unintentional_weight_loss_v1", 
  "autoimmune", 
  "osmotic", 
  "famhisdiab",
  "famhisnoninsdiab",
  "famhisauto"
)

#Produce complete case datasets ----------------------------------------------------------
all_vars <- c(varlist, varlist_cat)
##StartRight
SR_SRout_ccc <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars))

SR_ro_cc <- SR_ro_cc %>%
  drop_na(all_of(all_vars))
SR_ro_cc$Urea_v1 <- as.numeric(SR_ro_cc$Urea_v1)
SR_ro_cc$LDL_v1 <- as.numeric(SR_ro_cc$LDL_v1)
SR_ro_cc$Haemoglobin_v1 <- as.numeric(SR_ro_cc$Haemoglobin_v1)
SR_ro_cc$white_blood_cell_count_v1 <- as.numeric(SR_ro_cc$white_blood_cell_count_v1)
SR_ro_cc$Platelets_v1 <- as.numeric(SR_ro_cc$Platelets_v1)
SR_ro_cc$num_anti <- as.character(SR_ro_cc$num_anti)
SR_ro_cc$autoimmune <- as.numeric(SR_ro_cc$autoimmune)
SR_ro_cc$osmotic <- as.numeric(SR_ro_cc$osmotic)
SR_ro_cc$DKA <- as.numeric(SR_ro_cc$DKA)
###COMPLETE CASE FOR BOTH OUTCOMES & VARS OF INTEREST
SR_cc <- full_join(SR_SRout_ccc, SR_ro_cc) %>%
  drop_na(SRoutcome, robust_outcome)

### ii) Supplementary Table 4 ------------------------------------------------------------
####i) Univariate on continuous numeric features -----------------------------------------
SR_cc$robust_outcome <- ifelse(SR_cc$robust_outcome == "T1D", 1, 0)
SR_cc$SRoutcome <- as.numeric(SR_cc$SRoutcome)
model_continuous(varlist, 
                 dataset = SR_cc, 
                 outcome = "robust_outcome",
                 saving_name = "tables/Supp_Table9B_cont_robust_THRESHOLDS",
                 complete_case = TRUE, 
                 plots = FALSE)
model_continuous(varlist, 
                 dataset = SR_cc, 
                 outcome = "SRoutcome",
                 saving_name = "tables/Supp_Table9B_cont_SR_THRESHOLDS",
                 complete_case = TRUE, 
                 plots = FALSE)

varlist = c("AgeatDiagnosis", 
            "bmi_model", 
            #"Waist_v1", 
            #"wh_ratio_v1",
            "HbA1c_at_diagnosis_v1"
)
tbl1_thresholds <- c(
  "<37",
  "<28",
  ">80")
model_continuous(varlist, 
                 dataset = SR_cc, 
                 outcome = "robust_outcome",
                 thresholds = tbl1_thresholds,
                 saving_name = "tables/Supp_Table9B_cont_robust",
                 complete_case = TRUE, 
                 plots = FALSE)
tbl1_thresholds <- c(
  "<37",
  "<26",
  ">80")
model_continuous(varlist, 
                 dataset = SR_cc, 
                 outcome = "SRoutcome",
                 thresholds = tbl1_thresholds,
                 saving_name = "tables/Supp_Table9B_cont_SR",
                 complete_case = TRUE, 
                 plots = FALSE)
#####WHR ---------------------------------------------------------------------------
###### robust_outcome -------------------------------------------------------------------------
sum_table <- data.frame(variable = NA, 
                        n = NA,
                        Accuracy = NA, 
                        Sensitivity = NA, 
                        Specificity = NA, 
                        PPV = NA, 
                        NPV = NA)
#assign numeric variable name to row
sum_table$variable <- "wh_ratio_v1"

#print this variable to screen (for debugging purposes)
print(var)
#run logistic regression of defined outcome using numeric variable
model <- glm(robust_outcome ~scale(as.numeric(wh_ratio_v1)), 
             data = SR_cc, 
             family = binomial)
#make new columns in dataset
SR_cc <- SR_cc %>%
  mutate(
    #make predicted probability of model in dataset column 
    model_pp = predict(model, SR_cc, type = "response")
  )
#Run ROC analysis
roc_model <- roc(SR_cc$robust_outcome, 
                 SR_cc$wh_ratio_v1, 
                 plot = TRUE, 
                 print.thres = "best", 
                 print.auc = TRUE, 
                 ci = TRUE)
#Extract valuable summary information from ROC object
model_pr <- coords(roc_model, 
                   x = "best", 
                   ret=c("threshold"), 
                   transpose = FALSE)
threshold <- model_pr$threshold
SR_cc$cut_threshold <- ifelse(SR_cc$wh_ratio_v1 < 0.94,
                                 "1", 
                                 "0")

#Assign model summary information to summary data.frame

sens_spec <- prop.table(table(SR_cc$cut_threshold, 
                              SR_cc$robust_outcome), 
                        margin = 2)
sum_table$sensitivity <- sens_spec[str_detect(row.names(sens_spec), "1"), 
                                   str_detect(colnames(sens_spec), "1")]
sum_table$specificity <- sens_spec[str_detect(row.names(sens_spec), "0"),
                                   str_detect(colnames(sens_spec), "0")]
ppv_npv <- prop.table(table(SR_cc$cut_threshold, 
                            SR_cc$robust_outcome), 
                      margin = 1)
sum_table$PPV <- ppv_npv[str_detect(row.names(ppv_npv), "1"), 
                         str_detect(colnames(ppv_npv), "1")]
sum_table$NPV <- ppv_npv[str_detect(row.names(ppv_npv), "0"),
                         str_detect(colnames(ppv_npv), "0")]
accuracy_table <- table(SR_cc$cut_threshold, 
                        SR_cc$robust_outcome)
sum_table$Accuracy <- (accuracy_table[str_detect(row.names(accuracy_table), "1"), 
                                      str_detect(colnames(accuracy_table), "1")] + 
                         accuracy_table[str_detect(row.names(accuracy_table), "0"), 
                                        str_detect(colnames(accuracy_table), "0")])/nrow(SR_cc)
sum_table$n <- nrow(SR_cc)
auc_ci <-roc_model[["ci"]]
sum_table$ROC_AUC <- paste0(round(roc_model$auc,3),
                            " (", round(auc_ci[1],3), "; ",
                            round(auc_ci[3],3), ")")
sum_table$ROC <- roc_model$auc

`tables/Supp_Table9B_cont_robust` <- full_join(`tables/Supp_Table9B_cont_robust`, sum_table)
write_xlsx(`tables/Supp_Table9B_cont_robust`, "tables/Supp_Table9B_cont_robust.xlsx")

###### SRoutcome -------------------------------------------------------------------
sum_table <- data.frame(variable = NA, 
                        n = NA,
                        Accuracy = NA, 
                        Sensitivity = NA, 
                        Specificity = NA, 
                        PPV = NA, 
                        NPV = NA)
#assign numeric variable name to row
sum_table$variable <- "wh_ratio_v1"

#print this variable to screen (for debugging purposes)
print(var)
#run logistic regression of defined outcome using numeric variable
model <- glm(SRoutcome ~scale(as.numeric(wh_ratio_v1)), 
             data = SR_cc, 
             family = binomial)
#make new columns in dataset
SR_cc <- SR_cc %>%
  mutate(
    #make predicted probability of model in dataset column 
    model_pp = predict(model, SR_cc, type = "response")
  )
#Run ROC analysis
roc_model <- roc(SR_cc$SRoutcome, 
                 SR_cc$wh_ratio_v1, 
                 plot = TRUE, 
                 print.thres = "best", 
                 print.auc = TRUE, 
                 ci = TRUE)
#Extract valuable summary information from ROC object
model_pr <- coords(roc_model, 
                   x = "best", 
                   ret=c("threshold"), 
                   transpose = FALSE)
threshold <- model_pr$threshold
SR_cc$cut_threshold <- ifelse(SR_cc$wh_ratio_v1 < 0.94,
                                 "1", 
                                 "0")

#Assign model summary information to summary data.frame

sens_spec <- prop.table(table(SR_cc$cut_threshold, 
                              SR_cc$SRoutcome), 
                        margin = 2)
sum_table$sensitivity <- sens_spec[str_detect(row.names(sens_spec), "1"), 
                                   str_detect(colnames(sens_spec), "1")]
sum_table$specificity <- sens_spec[str_detect(row.names(sens_spec), "0"),
                                   str_detect(colnames(sens_spec), "0")]
ppv_npv <- prop.table(table(SR_cc$cut_threshold, 
                            SR_cc$SRoutcome), 
                      margin = 1)
sum_table$PPV <- ppv_npv[str_detect(row.names(ppv_npv), "1"), 
                         str_detect(colnames(ppv_npv), "1")]
sum_table$NPV <- ppv_npv[str_detect(row.names(ppv_npv), "0"),
                         str_detect(colnames(ppv_npv), "0")]
accuracy_table <- table(SR_cc$cut_threshold, 
                        SR_cc$SRoutcome)
sum_table$Accuracy <- (accuracy_table[str_detect(row.names(accuracy_table), "1"), 
                                      str_detect(colnames(accuracy_table), "1")] + 
                         accuracy_table[str_detect(row.names(accuracy_table), "0"), 
                                        str_detect(colnames(accuracy_table), "0")])/nrow(SR_cc)
sum_table$n <- nrow(SR_cc)
auc_ci <-roc_model[["ci"]]
sum_table$ROC_AUC <- paste0(round(roc_model$auc,3),
                            " (", round(auc_ci[1],3), "; ",
                            round(auc_ci[3],3), ")")
sum_table$ROC <- roc_model$auc

`tables/Supp_Table9B_cont_SR` <- full_join(`tables/Supp_Table9B_cont_SR`, sum_table)
write_xlsx(`tables/Supp_Table9B_cont_SR`, "tables/Supp_Table9B_cont_SR.xlsx")

####ii) Univariate on categorical features --------------------------------------------------
SR_cc <- SR_cc %>%
  mutate(
    Gender_v1 = ifelse(Gender_v1 == "Female", 1, 0),
    #DKA = as.numeric(DKA),
    Unintentional_weight_loss_v1 = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0),
    autoimmune = as.numeric(autoimmune),
    osmotic = as.numeric(osmotic),
    famhisdiab = ifelse(famhisdiab == "No", 1, 0),
    famhisnoninsdiab = ifelse(famhisnoninsdiab == "No", 1, 0),
    famhisauto = as.numeric(famhisauto)
  )
cat_contingency(varlist_cat, 
                dataset = SR_cc, 
                outcome = "robust_outcome", 
                saving_name = "tables/Supp_Table9B_cat_robust",
                complete_case = TRUE, 
                plots = FALSE, 
                decimals = 3)
cat_contingency(varlist_cat, 
                dataset = SR_cc, 
                outcome = "SRoutcome", 
                saving_name = "tables/Supp_Table9B_cat_SRout",
                complete_case = TRUE, 
                plots = FALSE, 
                decimals = 3)

