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
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")

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
  "Eth_5cat",
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  #"famhisdiab",
  "famhisnoninsdiab",
  "famhisauto"
)

#Produce complete case datasets ----------------------------------------------------------
all_vars <- c(varlist, varlist_cat)
##StartRight
SR_SRout_ccc <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars))

SR_SRout_ccc %>%
  filter(is.na(bmi_diag)) %>%
  count()


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
                 saving_name = "tables/Supp_Table6_cont_thresholds",
                 complete_case = TRUE,
                 plots = FALSE,
                 decimals = 1)
model_continuous(varlist,
                 dataset = SR_SRout_ccc,
                 outcome = "SRoutcome",
                 thresholds = tbl1_thresholds,
                 saving_name = "tables/Supp_Table6_cont",
                 complete_case = TRUE,
                 plots = FALSE,
                 decimals = 1)

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
threshold <- 0.90
sum_table$Threshold <- as.character(model_pr$threshold)
decimals <- 3
SR_SRout_ccc$cut_threshold <- ifelse(SR_SRout_ccc$wh_ratio_v1 < 0.90,
                                   "1",
                                   "0")

SR_SRout_ccc$SRoutcome_rev <- ifelse(SR_SRout_ccc$SRoutcome == "1", 0, 1)
  #Assign model summary information to summary data.frame
conf_matrix <- confusion_matrix(SR_SRout_ccc$SRoutcome_rev,
                                SR_SRout_ccc$wh_ratio_v1,
                                thresholds = threshold)
specif <- conf_matrix$cm_stats["sensitivity"]*100
specif_lci <- conf_matrix$cm_stats["sensitivity_lcl"]*100
specif_uci <- conf_matrix$cm_stats["sensitivity_ucl"]*100
sum_table$Specificity <- paste0(round(specif[2,1],decimals),
                                " (", round(specif_lci[2,1],decimals), "; ",
                                round(specif_uci[2,1],decimals), ")")

sens <- conf_matrix$cm_stats["specificity"]*100
sens_lci <- conf_matrix$cm_stats["specificity_lcl"]*100
sens_uci <- conf_matrix$cm_stats["specificity_ucl"]*100
sum_table$Sensitivity <- paste0(round(sens[2,1],decimals),
                                " (", round(sens_lci[2,1],decimals), "; ",
                                round(sens_uci[2,1],decimals), ")")
npv <- conf_matrix$cm_stats["ppv"]*100
npv_lci <- conf_matrix$cm_stats["ppv_lcl"]*100
npv_uci <- conf_matrix$cm_stats["ppv_ucl"]*100
sum_table$NPV <- paste0(round(npv[2,1],decimals),
                        " (", round(npv_lci[2,1],decimals), "; ",
                        round(npv_uci[2,1],decimals), ")")
ppv <- conf_matrix$cm_stats["npv"]*100
ppv_lci <- conf_matrix$cm_stats["npv_lcl"]*100
ppv_uci <- conf_matrix$cm_stats["npv_ucl"]*100
sum_table$PPV <- paste0(round(ppv[2,1],decimals),
                        " (", round(ppv_lci[2,1],decimals), "; ",
                        round(ppv_uci[2,1],decimals), ")")
accur <- conf_matrix$cm_stats["accuracy"]*100
accur_lci <- conf_matrix$cm_stats["accuracy_lcl"]*100
accur_uci <- conf_matrix$cm_stats["accuracy_ucl"]*100
sum_table$Accuracy <- paste0(round(accur[2,1],decimals),
                             " (", round(accur_lci[2,1],decimals), "; ",
                             round(accur_uci[2,1],decimals), ")")
auc_ci <-roc_model[["ci"]]
sum_table$ROC_AUC <- paste0(round(roc_model$auc,decimals),
                            " (", round(auc_ci[1],decimals), "; ",
                            round(auc_ci[3],decimals), ")")

  `tables/Supp_Table6_cont` <- full_join(`tables/Supp_Table6_cont`, sum_table)
  write_xlsx(`tables/Supp_Table6_cont`, "tables/Supp_Table6_cont.xlsx")
  # SR_SRout_ccc <- SR_SRout_ccc %>%
  #   mutate(SRoutcome_rev = ifelse(SRoutcome == "1", 0, 1))
  # conf_matrix <- confusion_matrix(SR_SRout_ccc$SRoutcome_rev,
  #                                 SR_SRout_ccc$wh_ratio_v1,
  #                                 thresholds = 0.90)
  # sens <- conf_matrix$cm_stats["sensitivity"]
  # sens_lci <- conf_matrix$cm_stats["sensitivity_lcl"]
  # sens_uci <- conf_matrix$cm_stats["sensitivity_ucl"]
  # sum_table$Sensitivity <- paste0(round(sens[2,1],decimals),
  #                                 " (", round(sens_lci[2,1],decimals), "; ",
  #                                 round(sens_uci[2,1],decimals), ")")
  conf_matrix <- confusion_matrix(SR_SRout_ccc$SRoutcome,
                                  SR_SRout_ccc$HbA1c_at_diagnosis_v1,
                                  thresholds = 80)
  specif <- conf_matrix$cm_stats["sensitivity"]*100
  specif_lci <- conf_matrix$cm_stats["sensitivity_lcl"]*100
  specif_uci <- conf_matrix$cm_stats["sensitivity_ucl"]*100
  sum_table$Specificity <- paste0(round(specif[2,1],decimals),
                                  " (", round(specif_lci[2,1],decimals), "; ",
                                  round(specif_uci[2,1],decimals), ")")
###ii) Univariate on categorical features --------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(
    Gender_v1 = ifelse(Gender_v1 == "Female", 1, 0),
    DKA = as.numeric(DKA),
    Unintentional_weight_loss_v1 = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0),
    autoimmune = as.numeric(autoimmune),
    osmotic = as.numeric(osmotic),
    #famhisdiab = ifelse(famhisdiab == "No", 1, 0),
    famhisnoninsdiab = ifelse(famhisnoninsdiab == "No", 1, 0),
    famhisauto = as.numeric(famhisauto)
  )
table(SR_SRout_ccc$Gender_v1, useNA = "ifany")
table(SR_SRout_ccc$DKA, useNA = "ifany")
table(SR_SRout_ccc$Unintentional_weight_loss_v1, useNA = "ifany")
table(SR_SRout_ccc$autoimmune, useNA = "ifany")
table(SR_SRout_ccc$osmotic, useNA = "ifany")
#table(SR_SRout_ccc$famhisdiab, useNA = "ifany")
table(SR_SRout_ccc$famhisnoninsdiab, useNA = "ifany")
table(SR_SRout_ccc$famhisauto, useNA = "ifany")
SR_SRout_ccc$Eth_5cat <- factor(SR_SRout_ccc$Eth_5cat, levels = c("White", "Black", "South Asian", "Mixed", "Other"))
cat_contingency(varlist_cat,
                dataset = SR_SRout_ccc,
                outcome = "SRoutcome",
                saving_name = "tables/Supp_Table6_cat",
                complete_case = TRUE,
                plots = FALSE,
                decimals = 3)

# top left (specificity) and bottom right (sensitivity)
#prop.table(table(SR_SRout_ccc$Acanthosis_Nigricans_v1, SR_SRout_ccc$SRoutcome), margin = 2)
# top left (NPV) and bottom right (PPV)
#prop.table(table(SR_SRout_ccc$Acanthosis_Nigricans_v1, SR_SRout_ccc$SRoutcome), margin = 1)

model <- glm(SRoutcome ~relevel(factor(Eth_5cat), ref = "White"),
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
                 SR_SRout_ccc$model_pp,
                 plot = TRUE,
                 print.thres = "best",
                 print.auc = TRUE,
                 ci = TRUE)
