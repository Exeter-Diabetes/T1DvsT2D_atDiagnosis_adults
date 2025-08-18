#####################################################################################

#Paper figures

#Table 5
#Coefficients and ORs for Scoring table
#Unstandardised

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)

#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/model_continuous1.R")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/data/SR_50_SRout_ccc_20_3_2025.RData")
load("m1.RData")

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
##StartRight Prime --------------------------------------------------------------------
SR_50_SRout_ccc<- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
  )
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
#Model12
SR_50_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12))

##Scoring Table -----------------------------------------------------------------------
#StartRight ----------------------------------------------------------------------------------
SR_mD_data <- SR_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model, 
         HbA1c_at_diagnosis_v1, Gender_v1, 
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA, 
         famhisnoninsdiab, num_anti) %>%
  mutate(
    m1_pp = predict(m1, SR_m12_data, type = "response"),
    agedx = case_when(
      AgeatDiagnosis <37 ~ 2,
      AgeatDiagnosis <40 ~ 1,
      AgeatDiagnosis <=50 ~ 0
    ), 
    bmi = case_when(
      bmi_model < 25 ~ 4,
      bmi_model <= 31 ~ 2,
      bmi_model > 31 ~ 0
    ), 
    hba1c = ifelse(
      HbA1c_at_diagnosis_v1 < 58, 0, 1
    ), 
    sex = ifelse(Gender_v1 == "Female", 1, 0), 
    osmotic = ifelse(osmotic == "1", 1, 0), 
    autoimmune = ifelse(autoimmune == "1", 1, 0), 
    weightloss = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0), 
    dka = ifelse(DKA == "1", 1, 0),
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    pos_anti = ifelse(num_anti >= 1, 1, 0)
  ) 

SR_mD_data <- SR_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex, 
         osmotic, autoimmune, weightloss, dka, pardm, pos_anti) %>%
  mutate(ScoreD = select(., agedx:pardm) %>% rowSums(na.rm = TRUE))
varlist = c("ScoreD")

mD_sum <- SR_mD_data %>%
  summarise(
    n = n(),
    totalover = sum(ScoreD >= 7),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(ScoreD >= 7 & pos_anti == 1),
    PPV1 = (sum(ScoreD >= 7 & pos_anti == 1)/sum(ScoreD >= 7))*100,
    # nmissed_anti_pos = sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1),
    # Missedcases = ((sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1))/sum(pos_anti == 1))*100,
    all_three = sum(ScoreD >= 7 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((ScoreD < 7 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(ScoreD < 7 | pos_anti == 0),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    #n_total_antineg = sum(pos_anti == 0),
    Spec = (all_three_under/n_T2D)*100)

mD_sum <- mD_sum %>%
  mutate(
    Score = "Score D",
    Threshold = ">=7",
    `% Meet criteria for antibody testing` = paste0(
      round(perc_testing,2), " (", totalover, "/", n, ")"),
    `Proportion of those eligible for testing, antibody positive` = paste0(
      round(PPV1,2), " (", n_anti_pos_pickedup, "/", totalover, ")"),
    PPV = paste0(
      round(PPV2,2), " (",all_three, "/", n_anti_pos_pickedup, ")"),
    NPV = paste0(
      round(NPV2,2), " (", all_three_under, "/", nmissed_antineg, ")"),
    Sensitivity = paste0(
      round(Sens,2), " (", all_three, "/", n_T1D,")"), 
    Specificity = paste0(
      round(Spec,2), " (", all_three_under, "/", n_T2D, ")")
  ) %>%
  select(
    Score, 
    Threshold,
    `% Meet criteria for antibody testing`,
    `Proportion of those eligible for testing, antibody positive`,
    PPV,
    NPV,
    Sensitivity, 
    Specificity
  )


#StartRight Prime----------------------------------------------------------------------------------
SR_50_mD_data <- SR_50_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model, 
         HbA1c_at_diagnosis_v1, Gender_v1, 
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA, 
         famhisnoninsdiab, num_anti) %>%
  mutate(
    m1_pp = predict(m1, SR_50_m12_data, type = "response"),
    agedx = case_when(
      AgeatDiagnosis <37 ~ 2,
      AgeatDiagnosis <40 ~ 1,
      AgeatDiagnosis >=40 ~ 0
    ), 
    bmi = case_when(
      bmi_model < 25 ~ 4,
      bmi_model <= 31 ~ 2,
      bmi_model > 31 ~ 0
    ), 
    hba1c = ifelse(
      HbA1c_at_diagnosis_v1 < 58, 0, 1
    ), 
    sex = ifelse(Gender_v1 == "Female", 1, 0), 
    osmotic = ifelse(osmotic == "1", 1, 0), 
    autoimmune = ifelse(autoimmune == "1", 1, 0), 
    weightloss = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0), 
    dka = ifelse(DKA == "1", 1, 0),
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    pos_anti = ifelse(num_anti >= 1, 1, 0)
  ) 

SR_50_mD_data <- SR_50_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex, 
         osmotic, autoimmune, weightloss, dka, pardm, pos_anti) %>%
  mutate(ScoreD = select(., agedx:pardm) %>% rowSums(na.rm = TRUE))
varlist = c("ScoreD")

mD_50_sum <- SR_50_mD_data %>%
  summarise(
    n = n(),
    totalover = sum(ScoreD >= 7),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(ScoreD >= 7 & pos_anti == 1),
    PPV1 = (sum(ScoreD >= 7 & pos_anti == 1)/sum(ScoreD >= 7))*100,
    # nmissed_anti_pos = sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1),
    # Missedcases = ((sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1))/sum(pos_anti == 1))*100,
    all_three = sum(ScoreD >= 7 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((ScoreD < 7 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(ScoreD < 7 | pos_anti == 0),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    #n_total_antineg = sum(pos_anti == 0),
    Spec = (all_three_under/n_T2D)*100)
mD_50_sum <- SR_50_mD_data %>%
  summarise(
    n = n(),
    totalover = sum(ScoreD >= 6),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(ScoreD >= 6 & pos_anti == 1),
    PPV1 = (sum(ScoreD >= 6 & pos_anti == 1)/sum(ScoreD >= 6))*100,
    # nmissed_anti_pos = sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1),
    # Missedcases = ((sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1))/sum(pos_anti == 1))*100,
    all_three = sum(ScoreD >= 6 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((ScoreD < 6 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(ScoreD < 6 | pos_anti == 0),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    #n_total_antineg = sum(pos_anti == 0),
    Spec = (all_three_under/n_T2D)*100)

mD_50_sum <- mD_50_sum %>%
  mutate(
    Score = "Prime",
    Threshold = ">=7",
    `% Meet criteria for antibody testing` = paste0(
      round(perc_testing,2), " (", totalover, "/", n, ")"),
    `Proportion of those eligible for testing, antibody positive` = paste0(
      round(PPV1,2), " (", n_anti_pos_pickedup, "/", totalover, ")"),
    PPV = paste0(
      round(PPV2,2), " (",all_three, "/", n_anti_pos_pickedup, ")"),
    NPV = paste0(
      round(NPV2,2), " (", all_three_under, "/", nmissed_antineg, ")"),
    Sensitivity = paste0(
      round(Sens,2), " (", all_three, "/", n_T1D,")"), 
    Specificity = paste0(
      round(Spec,2), " (", all_three_under, "/", n_T2D, ")")
  ) %>%
  select(
    Score, 
    Threshold,
    `% Meet criteria for antibody testing`,
    `Proportion of those eligible for testing, antibody positive`,
    PPV,
    NPV,
    Sensitivity, 
    Specificity
  )

Scoring_table <- rbind(
  mD_sum, 
  mD_50_sum)
write_xlsx(Scoring_table,"tables/Supp_Table16.xlsx")