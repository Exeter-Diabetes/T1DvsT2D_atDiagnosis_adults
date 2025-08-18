############################################################################################
#
#Identifying features at presentation that can differentiate between T1D & T2D
#beyond Age at diagnosis & BMI

#This does this in StartRight 18-50s & StartRight Prime
#Primary outcome

#This produces Table 3

#############################################################################################
#load libraries
library(tidyverse)

#load functions
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")

#load data-------------------------------------------------------------------------
#load dataset from 01_11
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/data/SR_50_SRout_ccc_20_3_2025.RData")

#Add model vars ----------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto)
  ) 

SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
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

##StartRight Prime
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars))




##1) Table 3 -----------------------------------------------------------
#Just primary outcome
#Using same thresholds in Table 1
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
varlist <- c( 
  "bmi", 
  "HbA1c_at_diagnosis_v1"
)
tbl1_thresholds <- c(
  "<26",
  ">80")

model_continuous(varlist, 
                 dataset = SR_SRout_ccc,
                 outcome = "SRoutcome",
                 saving_name = "tables/Table3_SR_cont",
                 complete_case = TRUE, 
                 thresholds = tbl1_thresholds,
                 plots = FALSE)

model_continuous(varlist, 
                 dataset = SR_50_SRout_ccc, 
                 outcome = "SRoutcome",
                 saving_name = "tables/Table3_Prime_cont",
                 complete_case = TRUE, 
                 thresholds = tbl1_thresholds,
                 plots = FALSE)
varlist <- c( 
  "AgeatDiagnosis",
  "bmi", 
  "HbA1c_at_diagnosis_v1"
)
model_continuous(varlist, 
                 dataset = SR_50_SRout_ccc, 
                 outcome = "SRoutcome",
                 saving_name = "tables/Table3_Prime_cont_nothresholds",
                 complete_case = TRUE, 
                 #thresholds = tbl1_thresholds,
                 plots = FALSE)

####WHR ---------------------------------------------------------------------------------
##### SR -------------------------------------------------------------------------------
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
             data = SR_SRout_ccc, 
             family = binomial)
#make new columns in dataset
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(
    #make predicted probability of model in dataset column 
    model_pp = predict(model, SR_SRout_ccc, type = "response")
  )
#Run ROC analysis
roc_model <- roc(SR_SRout_ccc$SRoutcome, 
                 SR_SRout_ccc$wh_ratio_v1, 
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
SR_SRout_ccc$cut_threshold <- ifelse(SR_SRout_ccc$wh_ratio_v1 < 0.94,
                                 "1", 
                                 "0")

#Assign model summary information to summary data.frame

sens_spec <- prop.table(table(SR_SRout_ccc$cut_threshold, 
                              SR_SRout_ccc$SRoutcome), 
                        margin = 2)
sum_table$sensitivity <- sens_spec[str_detect(row.names(sens_spec), "1"), 
                                   str_detect(colnames(sens_spec), "1")]
sum_table$specificity <- sens_spec[str_detect(row.names(sens_spec), "0"),
                                   str_detect(colnames(sens_spec), "0")]
ppv_npv <- prop.table(table(SR_SRout_ccc$cut_threshold, 
                            SR_SRout_ccc$SRoutcome), 
                      margin = 1)
sum_table$PPV <- ppv_npv[str_detect(row.names(ppv_npv), "1"), 
                         str_detect(colnames(ppv_npv), "1")]
sum_table$NPV <- ppv_npv[str_detect(row.names(ppv_npv), "0"),
                         str_detect(colnames(ppv_npv), "0")]
accuracy_table <- table(SR_SRout_ccc$cut_threshold, 
                        SR_SRout_ccc$SRoutcome)
sum_table$Accuracy <- (accuracy_table[str_detect(row.names(accuracy_table), "1"), 
                                      str_detect(colnames(accuracy_table), "1")] + 
                         accuracy_table[str_detect(row.names(accuracy_table), "0"), 
                                        str_detect(colnames(accuracy_table), "0")])/nrow(SR_SRout_ccc)
sum_table$n <- nrow(SR_SRout_ccc)
auc_ci <-roc_model[["ci"]]
sum_table$ROC_AUC <- paste0(round(roc_model$auc,3),
                            " (", round(auc_ci[1],3), "; ",
                            round(auc_ci[3],3), ")")
sum_table$ROC <- roc_model$auc

`tables/Table3_SR_cont` <- full_join(`tables/Table3_SR_cont`, sum_table)
write_xlsx(`tables/Table3_SR_cont`, "tables/Table3_SR_cont.xlsx")

##### Prime -------------------------------------------------------------------------------
#With set threshold ---------------------------------------------------------------------
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
             data = SR_50_SRout_ccc, 
             family = binomial)
#make new columns in dataset
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(
    #make predicted probability of model in dataset column 
    model_pp = predict(model, SR_50_SRout_ccc, type = "response")
  )
#Run ROC analysis
roc_model <- roc(SR_50_SRout_ccc$SRoutcome, 
                 SR_50_SRout_ccc$wh_ratio_v1, 
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
SR_50_SRout_ccc$cut_threshold <- ifelse(SR_50_SRout_ccc$wh_ratio_v1 < 0.94,
                                 "1", 
                                 "0")

#Assign model summary information to summary data.frame

sens_spec <- prop.table(table(SR_50_SRout_ccc$cut_threshold, 
                              SR_50_SRout_ccc$SRoutcome), 
                        margin = 2)
sum_table$sensitivity <- sens_spec[str_detect(row.names(sens_spec), "1"), 
                                   str_detect(colnames(sens_spec), "1")]
sum_table$specificity <- sens_spec[str_detect(row.names(sens_spec), "0"),
                                   str_detect(colnames(sens_spec), "0")]
ppv_npv <- prop.table(table(SR_50_SRout_ccc$cut_threshold, 
                            SR_50_SRout_ccc$SRoutcome), 
                      margin = 1)
sum_table$PPV <- ppv_npv[str_detect(row.names(ppv_npv), "1"), 
                         str_detect(colnames(ppv_npv), "1")]
sum_table$NPV <- ppv_npv[str_detect(row.names(ppv_npv), "0"),
                         str_detect(colnames(ppv_npv), "0")]
accuracy_table <- table(SR_50_SRout_ccc$cut_threshold, 
                        SR_50_SRout_ccc$SRoutcome)
sum_table$Accuracy <- (accuracy_table[str_detect(row.names(accuracy_table), "1"), 
                                      str_detect(colnames(accuracy_table), "1")] + 
                         accuracy_table[str_detect(row.names(accuracy_table), "0"), 
                                        str_detect(colnames(accuracy_table), "0")])/nrow(SR_50_SRout_ccc)
sum_table$n <- nrow(SR_50_SRout_ccc)
auc_ci <-roc_model[["ci"]]
sum_table$ROC_AUC <- paste0(round(roc_model$auc,3),
                            " (", round(auc_ci[1],3), "; ",
                            round(auc_ci[3],3), ")")
sum_table$ROC <- roc_model$auc

`tables/Table3_Prime_cont` <- full_join(`tables/Table3_Prime_cont`, sum_table)
write_xlsx(`tables/Table3_Prime_cont`, "tables/Table3_Prime_cont.xlsx")

##Categorical variables -----------------------------------------------------------------
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(
    Gender_v1 = ifelse(Gender_v1 == "Female", 1, 0),
    DKA = as.numeric(DKA),
    Unintentional_weight_loss_v1 = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0),
    autoimmune = as.numeric(autoimmune),
    osmotic = as.numeric(osmotic),
    famhisdiab = ifelse(famhisdiab == "No", 1, 0),
    famhisnoninsdiab = ifelse(famhisnoninsdiab == "No", 1, 0),
    famhisauto = as.numeric(famhisauto)
  )
cat_contingency(varlist_cat, 
                dataset = SR_50_SRout_ccc, 
                outcome = "SRoutcome", 
                saving_name = "tables/Table3_Prime_cat",
                complete_case = TRUE, 
                plots = FALSE, 
                decimals = 3)
