############################################################################################
#
#Identifying features at presentation that can differentiate between T1D & T2D
#beyond Age at diagnosis & BMI

#This does this in StartRight 18-50s

#Supplementary Table 7

#Contents:
##) Addition to age & BMI model comparison


#############################################################################################
#load libraries
library(tidyverse)
#library(writexl)

#load functions
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
source("functions/model_comparison2.R")
#load data-------------------------------------------------------------------------
#load dataset from 01_11
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
#load("~/PhD/StartRight_paper/data/SR_ro_cc_1_2025.RData")

#Prep variables ---------------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(Eth_5cat = ifelse(ethnic_origin %in% c("White British",
                                                "White Irish",
                                                "Other White"),
                           "White",
                           ifelse(ethnic_origin %in% c("Pakistani",
                                                       "Indian",
                                                       "Other Asian"),
                                  "South Asian",
                                  ifelse(ethnic_origin %in% c("Black African",
                                                              "Black Caribbean",
                                                              "Other Black"),
                                         "Black",
                                         ifelse(ethnic_origin %in% c("Other Mixed",
                                                                     "White & Black African",
                                                                     "White & Black Caribbean",
                                                                     "White Asian"),
                                                "Mixed",
                                                ifelse(ethnic_origin %in% c("Any Other Group",
                                                                            "Not Known",
                                                                            "Not Stated",
                                                                            "Chinese"),
                                                       "Other",
                                                       NA
                                                )
                                         )
                                  )
                           )
  ),
  bmi_model = ifelse(is.na(bmi_diag),
                     bmi,
                     bmi_diag),
  famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
  ) 
# SR_SRout_ccc <- SR_SRout_ccc %>%
#   mutate(ketosis_v1 = ifelse((!is.na(Blood_ketone_result_at_diag_v1) & Blood_ketone_result_at_diag_v1 >= 0.7) 
#                              | ((!is.na(urine_ketones_at_diagnosis_v1) & urine_ketones_at_diagnosis_v1 %in% c("++", "+++", "++++") )
#                                 | (!is.na(urine_ketones_at_diagnosis2_v1) & (urine_ketones_at_diagnosis2_v1 >= 17 | (urine_ketones_at_diagnosis2_v1 >= 3 & urine_ketones_at_diagnosis2_v1 <= 6)))
#                              ),
#                              "1", 
#                              ifelse(is.na(Blood_ketone_result_at_diag_v1) & (is.na(urine_ketones_at_diagnosis_v1) | urine_ketones_at_diagnosis_v1 %in% c("negative", "trace", "unknown")), NA, "0")))
# SR_SRout_ccc <- SR_SRout_ccc %>%
#   mutate(ketosis_no_acidosis_v1 = ifelse(ketosis_v1 == "1" & DKA == "0", "1",
#                                          ifelse(ketosis_v1 == "1" & DKA == "1", NA, "0")))
table(SR_SRout_ccc$ketosis_no_acidosis_v1, useNA = "ifany")
table(SR_SRout_ccc$ketosis_v1, useNA = "ifany")
table(SR_SRout_ccc$DKA, useNA = "ifany")
#A) For SRoutcome in SR_SRout ----------------------------------------------------
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)

table(SR_SRout_ccc$famhisnoninsdiab, useNA = "ifany")

varlist = c("AgeatDiagnosis", 
            "bmi_model",
            "Waist_v1", 
            "wh_ratio_v1", 
            "Glucose_at_diagnosis_v1",
            "HbA1c_at_diagnosis_v1", 
            "c_peptide_v1"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat = c(
  #"Eth_5cat",
  "Gender_v1",
  "DKA", 
  "ketosis_no_acidosis_v1",
  "osmotic",
  "Unintentional_weight_loss_v1", 
  "autoimmune", 
  "famhisdiab",
  "famhisinsdiab", 
  "famhisnoninsdiab", 
  "famhisauto",
  "Acanthosis_Nigricans_v1", 
  "Hypertension_on_meds_v1")



##3) Addition to age & BMI model comparison ---------------------------------------------------------
#Define needed variables ------------------------------------------------------------------
base_model_vars = c("AgeatDiagnosis", 
                    "bmi_model")
#SR_SRout_ccc$SRoutcome <- ifelse(SR_SRout_ccc$SRoutcome == "Type 1", 1, 0)
model_comparisons(varlist = varlist, 
                  varlist_cat = varlist_cat,
                  base_model_vars = base_model_vars, 
                  dataset = SR_SRout_ccc,
                  outcome = "SRoutcome",
                  saving_name = "Supp_Table7",
                  complete_case = FALSE
)



####a) for Eth_cat5 ----------------------------------------------------------------------------
model_var <- c(base_model_vars, "Eth_5cat")
dataset_cc <- SR_SRout_ccc %>%
  drop_na(all_of(model_var))

#make dummy data.frame for each variable
sum_table <- data.frame(model = NA, 
                        n = NA,
                        coef_estimate = NA,
                        coef_pvalue = NA,
                        accuracy = NA, 
                        sensitivity = NA, 
                        specificity = NA, 
                        PPV = NA, 
                        NPV = NA,
                        threshold = NA, 
                        ROCAUC = NA, 
                        ROC_CI = NA, 
                        ROCAUC_CI = NA
                        #lrtest = NA
)
#assign numeric variable name to row
sum_table$model <- "Age at diagnosis + BMI + Eth_5cat"

#make base model
model <- glm(SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + factor(Eth_5cat), 
             data = dataset_cc, 
             family = binomial)

dataset_cc <- dataset_cc %>%
  mutate(
    #make predicted probability of model in dataset column 
    model_pp = predict(model, dataset_cc, type = "response"),
    #make log-odds of model in dataset column
    model_log_odds = predict(model, dataset_cc)
  )
#Run ROC analysis
roc_model <- roc(dataset_cc$SRoutcome,
                 dataset_cc$model_pp,
                 plot = TRUE, 
                 print.thres = "best",
                 print.auc = TRUE, 
                 ci = TRUE)
#Extract valuable summary information from ROC object
model_pr <- coords(roc_model, 
                   x = "best", 
                   ret=c("threshold",
                         "specificity",
                         "sensitivity",
                         "accuracy", 
                         "precision",
                         "recall", 
                         "ppv",
                         "npv"),
                   transpose = FALSE)

#likely_test <- lrtest(base_model, model)

#Assign model summary information to dummary data.frame
#model_comp_table$model <- variable_order
#model_comp_table$model <- "R"
sum_table$n <- nrow(dataset_cc)
#sum_table$coef_estimate <- coef(summary(model))[str_detect(row.names(coef(summary(model))), "Eth_5cat"), 1]
#sum_table$coef_pvalue <- coef(summary(model))[str_detect(row.names(coef(summary(model))), "Eth_5cat"), 4]
sum_table$accuracy <- model_pr$accuracy[1]
sum_table$sensitivity <- model_pr$sensitivity[1]
sum_table$specificity <- model_pr$specificity[1]
sum_table$PPV <- model_pr$ppv[1]
sum_table$NPV <- model_pr$npv[1]
#sum_table$coefficient <- summary(base_model)$coefficients[2,1]
sum_table$threshold <- model_pr$threshold[1]
sum_table$ROCAUC <- roc_model$auc
sum_table$ROC_CI <- paste0("(", round(roc_model$ci[1], 3), "; ", round(roc_model$ci[3],3), ")")
sum_table$ROCAUC_CI <- paste(sum_table$ROCAUC, sum_table$ROC_CI)
#sum_table$AIC <- model$aic
#sum_table$lrtest <- likely_test$p.value
summary(model)
confint(model)
