############################################################################################
#
#Identifying features at presentation that can differentiate between T1D & T2D
#beyond Age at diagnosis & BMI

#This does this in StartRight 18-50s
#Primary Outcome

#Table 2



#############################################################################################
#load libraries
library(tidyverse)
library(writexl)

#load functions
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")

#load data-------------------------------------------------------------------------
#load dataset from 01_11
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")

#Add model vars ----------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
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
SR_SRout_ccc <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars))




#A) StartRight -----------------------------------------------------------------------
##1) For SRoutcome in SR_SRout - Table 1 ----------------------------------------------------
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
###i) Univariate on continuous numeric features -----------------------------------------
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
                 dataset = SR_SRout_ccc,
                 outcome = "SRoutcome",
                 #thresholds = tbl1_thresholds,
                 saving_name = "tables/table2_cont_thresholds",
                 complete_case = TRUE,
                 plots = FALSE)
model_continuous(varlist,
                 dataset = SR_SRout_ccc,
                 outcome = "SRoutcome",
                 thresholds = tbl1_thresholds,
                 saving_name = "tables/Table2_cont",
                 complete_case = TRUE,
                 plots = FALSE)

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
SR_SRout_ccc$cut_threshold <- ifelse(SR_SRout_ccc$wh_ratio_v1 < 0.90,
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
  
  `tables/Table2_cont` <- full_join(`tables/Table2_cont`, sum_table)
  write_xlsx(`tables/Table2_cont`, "tables/Table2_cont.xlsx")
###ii) Univariate on categorical features --------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
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
table(SR_SRout_ccc$Gender_v1, useNA = "ifany")
table(SR_SRout_ccc$DKA, useNA = "ifany")
table(SR_SRout_ccc$Unintentional_weight_loss_v1, useNA = "ifany")
table(SR_SRout_ccc$autoimmune, useNA = "ifany")
table(SR_SRout_ccc$osmotic, useNA = "ifany")
table(SR_SRout_ccc$famhisdiab, useNA = "ifany")
table(SR_SRout_ccc$famhisnoninsdiab, useNA = "ifany")
table(SR_SRout_ccc$famhisauto, useNA = "ifany")
cat_contingency(varlist_cat, 
                dataset = SR_SRout_ccc, 
                outcome = "SRoutcome", 
                saving_name = "tables/Table2_cat",
                complete_case = TRUE, 
                plots = FALSE, 
                decimals = 3)

# top left (specificity) and bottom right (sensitivity)
#prop.table(table(SR_SRout_ccc$Acanthosis_Nigricans_v1, SR_SRout_ccc$SRoutcome), margin = 2)
# top left (NPV) and bottom right (PPV)
#prop.table(table(SR_SRout_ccc$Acanthosis_Nigricans_v1, SR_SRout_ccc$SRoutcome), margin = 1)


