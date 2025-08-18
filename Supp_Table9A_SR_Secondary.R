############################################################################################
#
#Identifying features at presentation that can differentiate between T1D & T2D
#beyond Age at diagnosis & BMI

#This does this in StartRight 18-50s
#Secondary outcome

#This produces Supplementary Table 9A

#############################################################################################
#load libraries
library(tidyverse)
library(writexl)

#load functions
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")

#load data-------------------------------------------------------------------------
#load dataset from 01_11
load("~/PhD/StartRight_paper/data/SR_ro_cc_1_2025.RData")

#Add model vars ----------------------------------------------------------------------
SR_ro_cc <- SR_ro_cc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto)
  ) 



#list of vars that add -------------------------------------------------------------


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
SR_ro_cc <- SR_ro_cc %>%
  drop_na(all_of(all_vars))
# SR_ro_cc$Urea_v1 <- as.numeric(SR_ro_cc$Urea_v1)
# SR_ro_cc$LDL_v1 <- as.numeric(SR_ro_cc$LDL_v1)
# SR_ro_cc$Haemoglobin_v1 <- as.numeric(SR_ro_cc$Haemoglobin_v1)
# SR_ro_cc$white_blood_cell_count_v1 <- as.numeric(SR_ro_cc$white_blood_cell_count_v1)
# SR_ro_cc$Platelets_v1 <- as.numeric(SR_ro_cc$Platelets_v1)
SR_ro_cc$num_anti <- as.character(SR_ro_cc$num_anti)
SR_ro_cc$autoimmune <- as.numeric(SR_ro_cc$autoimmune)
SR_ro_cc$osmotic <- as.numeric(SR_ro_cc$osmotic)

table(SR_ro_cc$famhisnoninsdiab)


#A) StartRight -----------------------------------------------------------------------
##2) For robust_outcome in SR_ro - Supplementary Table 5 --------------------------------
###i) Univariate on continuous numeric features -----------------------------------------
SR_ro_cc$robust_outcome <- ifelse(SR_ro_cc$robust_outcome == "T1D", 1, 0)
varlist = c("AgeatDiagnosis", 
            "bmi_model", 
            #"Waist_v1", 
            #"wh_ratio_v1",
            "HbA1c_at_diagnosis_v1"
)


model_continuous(varlist, 
                 dataset = SR_ro_cc, 
                 outcome = "robust_outcome",
                 saving_name = "tables/Supp_Table9_cont",
                 complete_case = TRUE, 
                 plots = FALSE)
tbl1_thresholds <- c(
  "<37",
  "<28",
  ">78")
model_continuous(varlist,
                 dataset = SR_ro_cc, 
                 outcome = "robust_outcome",
                 thresholds = tbl1_thresholds,
                 saving_name = "tables/Supp_Table9_cont",
                 complete_case = TRUE,
                 plots = FALSE)
###ii) Univariate on categorical features --------------------------------------------------
SR_ro_cc <- SR_ro_cc %>%
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
                dataset = SR_ro_cc, 
                outcome = "robust_outcome", 
                saving_name = "tables/Supp_Table9_cat",
                complete_case = TRUE, 
                plots = FALSE, 
                decimals = 3)




####WHR
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
             data = SR_ro_cc, 
             family = binomial)
#make new columns in dataset
SR_ro_cc <- SR_ro_cc %>%
  mutate(
    #make predicted probability of model in dataset column 
    model_pp = predict(model, SR_ro_cc, type = "response")
  )
#Run ROC analysis
roc_model <- roc(SR_ro_cc$robust_outcome, 
                 SR_ro_cc$wh_ratio_v1, 
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
SR_ro_cc$cut_threshold <- ifelse(SR_ro_cc$wh_ratio_v1 < 0.94,
                                     "1", 
                                     "0")

#Assign model summary information to summary data.frame

sens_spec <- prop.table(table(SR_ro_cc$cut_threshold, 
                              SR_ro_cc$robust_outcome), 
                        margin = 2)
sum_table$sensitivity <- sens_spec[str_detect(row.names(sens_spec), "1"), 
                                   str_detect(colnames(sens_spec), "1")]
sum_table$specificity <- sens_spec[str_detect(row.names(sens_spec), "0"),
                                   str_detect(colnames(sens_spec), "0")]
ppv_npv <- prop.table(table(SR_ro_cc$cut_threshold, 
                            SR_ro_cc$robust_outcome), 
                      margin = 1)
sum_table$PPV <- ppv_npv[str_detect(row.names(ppv_npv), "1"), 
                         str_detect(colnames(ppv_npv), "1")]
sum_table$NPV <- ppv_npv[str_detect(row.names(ppv_npv), "0"),
                         str_detect(colnames(ppv_npv), "0")]
accuracy_table <- table(SR_ro_cc$cut_threshold, 
                        SR_ro_cc$robust_outcome)
sum_table$Accuracy <- (accuracy_table[str_detect(row.names(accuracy_table), "1"), 
                                      str_detect(colnames(accuracy_table), "1")] + 
                         accuracy_table[str_detect(row.names(accuracy_table), "0"), 
                                        str_detect(colnames(accuracy_table), "0")])/nrow(SR_ro_cc)
sum_table$n <- nrow(SR_ro_cc)
auc_ci <-roc_model[["ci"]]
sum_table$ROC_AUC <- paste0(round(roc_model$auc,3),
                            " (", round(auc_ci[1],3), "; ",
                            round(auc_ci[3],3), ")")
sum_table$ROC <- roc_model$auc

`tables/Supp_Table9_cont` <- full_join(`tables/Supp_Table9_cont`, sum_table)
write_xlsx(`tables/Supp_Table9_cont`, "tables/Supp_Table9_cont.xlsx")
