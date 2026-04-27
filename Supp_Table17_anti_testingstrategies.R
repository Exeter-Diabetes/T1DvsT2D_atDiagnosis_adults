##########################################################################################
#CPRD code
##########################################################################################
#setwd("C:/Users/ky279/OneDrive - University of Exeter/CPRD/2025/Julieanne progression/Final/") ##########################################################################################

# Get data from MySQL
library(tidyverse)
library(aurum)

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")
analysis <- cprd$analysis("jk_at_diag")
data <- data %>% analysis$cached("cohort_v4") %>% collect()

# Format and rename variables
data <- data %>%
  mutate(Gender_v1=ifelse(gender==1, "Male", "Female"),
         DKA=dka_at_diag,
         Unintentional_weight_loss_v1=ifelse(weight_loss_at_diag==1, "Yes", "No"),
         osmotic=osmotic_symptoms_at_diag,
         autoimmune=ifelse(coeliac==1 | vitiligo==1 | addisons==1 | graves==1 | hashimotos==1 | pernicious_anaemia==1 | rheumatoidarthritis==1, 1, 0),
         famhisdiab=ifelse(is.na(fh_diabetes) | fh_diabetes==0, "No", "Yes"),
         Eth_5cat = ethnicity_5cat) %>%
  rename(AgeatDiagnosis=dm_diag_age,
         bmi_model=prebmi,
         HbA1c_at_diagnosis_v1=prehba1c) %>%
  select(Gender_v1, DKA, Unintentional_weight_loss_v1, osmotic, autoimmune,
         famhisdiab, ins_3yrs, follow_up_3yrs, AgeatDiagnosis, bmi_model, HbA1c_at_diagnosis_v1,
         dka, t1_code, basal_bolus, prerandomglucose, Eth_5cat)


# Check no missingness except for ethnicity and IMD

data %>%
  filter(is.na(Gender_v1)
         | is.na(DKA)
         | is.na(Unintentional_weight_loss_v1)
         | is.na(osmotic)
         | is.na(autoimmune)
         | is.na(famhisdiab)
         #| is.na(SRoutcome)
         | is.na(AgeatDiagnosis)
         | is.na(bmi_model)
         | is.na(HbA1c_at_diagnosis_v1)) %>%
  count()
#0


##############################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
#library(rms)
#library(naniar)
library(writexl)
library(pROC)
library(patchwork)
library(ggtext)
library(ggpattern)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
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
  "Eth_5cat",
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  "famhisdiab",
  #"famhisauto",
  "num_anti"
)
all_vars_12 <- c(varlist_12, varlist_cat_12)

##Make complete case datasets ----------------------------------------------------------
#StartRight -----------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto),
         data = "StartRight",
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)

SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
#StartRight Prime-----------------------------------------------------------------------
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto),
         data = "StartRight Prime",
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
SR_50_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
#Joint SR ----------------------------------------------------------------------
SR_joint_cc <- full_join(SR_m12_data, SR_50_m12_data)
#CPRD -------------------------------------------------------------------------------
data <- data %>%
  mutate(
    famhisnoninsdiab = ifelse(is.na(famhisdiab), "No", famhisdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("3", "4"), "Other/Mixed",
                      ifelse(Eth_5cat == "0", "White",
                             ifelse(Eth_5cat == "2", "Black",
                                    ifelse(Eth_5cat == "1", "South Asian", Eth_5cat))))
  )
data$DKA <- as.character(data$DKA)
data$osmotic <- as.character(data$osmotic)
data$autoimmune <- as.character(data$autoimmune)

varlist_cat_12 = c(
  "Eth_5cat",
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  "famhisdiab"#,
  #"famhisauto",
  #"num_anti"
)
all_vars_12 <- c(varlist_12, varlist_cat_12)

data <- data %>%
  drop_na(all_of(all_vars_12))

#Define strategies -----------------------------------------------------------------
##Joint SR ----------------------------------------------------------------------
SR_joint_cc <- SR_joint_cc %>%
  mutate(
    NICEfeatures1_noage = ifelse(
      (!is.na(DKA) & DKA == "1")
      | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes")
      | (!is.na(AgeatDiagnosis) & AgeatDiagnosis < 50)
      | (!is.na(bmi_model) & bmi_model < 25)
      | (!is.na(autoimmune) & autoimmune == "1"
         | !is.na(famhisauto) & famhisauto == "1"),
      "1+ NICE features",
      ifelse(is.na(DKA)
             & is.na(Unintentional_weight_loss_v1)
             & is.na(AgeatDiagnosis)
             & is.na(bmi_model)
             & is.na(autoimmune)
             & is.na(famhisauto),
             NA,
             "No NICE features")),
    pos_anti = ifelse(num_anti >= 1, "1+ pos anti", "Negative antibodies"),
    NICEfeatures1 = ifelse((!is.na(DKA) & DKA == "1")
                                 | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes")
                                 | (!is.na(bmi_model) & bmi_model < 25)
                                 | (!is.na(autoimmune) & autoimmune == "1"
                                    | !is.na(famhisauto) & famhisauto == "1"),
                                 "1+ NICE features",
                                 ifelse(is.na(DKA)
                                        & is.na(Unintentional_weight_loss_v1)
                                        & is.na(bmi_model)
                                        & is.na(autoimmune)
                                        & is.na(famhisauto),
                                        NA,
                                        "No NICE features")),
    ADAfeatures = ifelse(
      (!is.na(DKA) & DKA == "1")
      | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes")
      | (!is.na(AgeatDiagnosis) & AgeatDiagnosis < 35)
      | (!is.na(bmi_model) & bmi_model < 25)
      #Glucose at diagnosis
      | (!is.na(Glucose_at_diagnosis_v1) & Glucose_at_diagnosis_v1 > 20),
      "1+ ADA features",
      ifelse(is.na(DKA)
             & is.na(Unintentional_weight_loss_v1)
             & is.na(AgeatDiagnosis)
             & is.na(bmi_model)
             & is.na(Glucose_at_diagnosis_v1),
             NA,
             "No ADA features")
    ))
SR_joint_cc <- SR_joint_cc %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, num_anti, NICEfeatures1,
         NICEfeatures1_noage,
         ADAfeatures,
         pos_anti, data, ethnicity, Eth_4cat) %>%
  mutate(
    m1_pp = predict(m1, SR_joint_cc, type = "response"),
    m2_pp = predict(m2, SR_joint_cc, type = "response"),
    agedx = case_when(
      AgeatDiagnosis <=24 ~ 3,
      AgeatDiagnosis <=34 ~ 2,
      AgeatDiagnosis <=44 ~ 1,
      AgeatDiagnosis >44 ~ 0
    ),
    bmi = case_when(
      bmi_model < 25 ~ 5,
      bmi_model < 35 ~ 3,
      bmi_model >= 35 ~ 0
    ),
    hba1c = case_when(
      HbA1c_at_diagnosis_v1 < 58 ~ 0,
      HbA1c_at_diagnosis_v1 >= 58 ~ 1
    ),
    sex = ifelse(Gender_v1 == "Female", 1, 0),
    osmo = ifelse(osmotic == "1", 1, 0),
    auto = ifelse(autoimmune == "1", 1, 0),
    weightloss = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0),
    dka = ifelse(DKA == "1", 1, 0),
    eth = ifelse(ethnicity == "White", 0, -1),
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    agedx1 = case_when(
      AgeatDiagnosis <=24 ~ 2,
      AgeatDiagnosis <=34 ~ 2,
      AgeatDiagnosis <=44 ~ 1,
      AgeatDiagnosis >44 ~ 0
    ),
    bmi1 = case_when(
      bmi_model < 25 ~ 4,
      bmi_model < 35 ~ 2,
      bmi_model >= 35 ~ 0
    ),
    anti = case_when(
      num_anti == 0 ~ 0,
      num_anti == 1 ~ 2,
      num_anti == 2 ~ 3,
      num_anti == 3 ~ 4
    )
  )

SR_joint_cc <- SR_joint_cc %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, m2_pp, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, dka, eth, pardm, agedx1, bmi1, anti, pos_anti, NICEfeatures1,
         NICEfeatures1_noage,
         ADAfeatures,
         data) %>%
  mutate(Score1 = select(., agedx:pardm) %>% rowSums(na.rm = TRUE),
         Score2 = select(., hba1c:anti) %>% rowSums(na.rm = TRUE))

SR_joint_cc <- SR_joint_cc %>%
  mutate(
  #   SR_score_cat = case_when(
  #   Score1 < 6 ~ "0-5",
  #   Score1 <= 11 ~ "6-11",
  #   Score1 > 11 ~ "12-14"
  # ),
  SR_score_cat1 = case_when(
    Score1 < 7 ~ "0-6",
    Score1 <= 11 ~ "7-11",
    Score1 > 11 ~ "12-15"
  ),
  SR_score_cat2 = case_when(
    Score2 < 9 ~ "0-8",
    Score2 >= 9 ~ "9-17"
  ),
  model1_cat = case_when(
    m1_pp < 0.1 ~"<10%",
    m1_pp <= 0.9 ~"10-90%",
    m1_pp > 0.9 ~ ">90%"
  ),
  model2_cat = case_when(
    m2_pp < 0.5 ~"<50%",
    m2_pp >= 0.5 ~ ">=50%"
  ))
varlist_cat <- c(
  "pos_anti",
  "NICE1_pos",
  "NICE1_pos_noage",
  "ADAfeatures",
  "model10_pos",
  "#score5_pos",
  "model_antis",
  "score6_pos",
  "score_antis")

NICE_m12 <- SR_joint_cc %>%
  dplyr::select(SRoutcome,
                NICEfeatures1,
                NICEfeatures1_noage, ADAfeatures,
                pos_anti, Score1,Score2, m1_pp, m2_pp,
                SR_score_cat1, SR_score_cat2, model1_cat, model2_cat, data) %>%
  filter(!is.na(pos_anti)) %>%
  mutate(
    NICEfeatures1 = ifelse(NICEfeatures1 == "1+ NICE features", 1, 0),
    NICEfeatures1_noage = ifelse(NICEfeatures1_noage == "1+ NICE features", 1, 0),
    pos_anti = ifelse(pos_anti == "1+ pos anti", 1, 0),
    model10 = ifelse(m1_pp >=0.1, 1, 0),
    #model10_90 = ifelse(m1_pp >=0.1 & m1_pp <=0.9, 1, 0),
    model_antis = ifelse(m1_pp >=0.1 & m2_pp >= 0.5, 1, 0),
    #score6_11 = ifelse(SR_score_cat == "6-11", 1, 0),
    #score5 = ifelse(Score1 > 5, 1, 0),
    #score7_11 = ifelse(SR_score_cat1 == "7-11", 1, 0),
    score6 = ifelse(Score1 > 6, 1, 0),
    #Combinations with antibodies
    NICE1_pos = ifelse(NICEfeatures1 == 1 & pos_anti ==1, 1, 0),
    NICE1_pos_noage = ifelse(NICEfeatures1_noage == 1 & pos_anti == 1, 1, 0),
    #model10_90_pos = ifelse(model10_90 ==1 & pos_anti ==1, 1, 0),
    #score6_11_pos = ifelse(score6_11 ==1 & pos_anti ==1, 1, 0),
    model10_pos = ifelse(model10 ==1 & pos_anti ==1, 1, 0),
    #score5_pos = ifelse(score5 ==1 & pos_anti ==1, 1, 0),
    score6_pos = ifelse(score6 ==1 & pos_anti ==1, 1, 0),
    score_antis = ifelse(Score1 > 6 & Score2 > 8, 1, 0),
    #model_pos = ifelse((model10_90 ==1 & pos_anti ==1) | m1_pp > 0.9, 1, 0),
    #score_pos = ifelse((score6_11 ==1 & pos_anti ==1) | SR_score_cat == "11-13", 1, 0),
    ADAfeatures = ifelse(ADAfeatures == "1+ ADA features", 1, 0),
    ADAfeatures_pos = ifelse(ADAfeatures == 1 & pos_anti ==1, 1, 0)
  )

table(NICE_m12$score_antis, NICE_m12$SR_score_cat2)
table(NICE_m12$score_antis, NICE_m12$SR_score_cat1)
NICE_m12 %>%
  filter(score_antis == 0 & SR_score_cat2 == "9-17") %>%
  select(score_antis, SR_score_cat2, SR_score_cat1)
NICE_m12 %>%
  filter(score_antis == 0 & SR_score_cat2 == "0-8" & SR_score_cat1 %in% c("7-11", "12-15")) %>%
  count()
table(NICE_m12$model_antis, NICE_m12$model2_cat)
NICE_m12 %>%
  filter(model_antis == 0 & model2_cat == ">=50%") %>%
  select(model_antis, model2_cat, model1_cat)
## CPRD -------------------------------------------------------------------------
data <- data %>%
  mutate(
    NICEfeatures1_noage = ifelse(
      (!is.na(DKA) & DKA == "1")
      | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes")
      | (!is.na(AgeatDiagnosis) & AgeatDiagnosis < 50)
      | (!is.na(bmi_model) & bmi_model < 25)
      | (!is.na(autoimmune) & autoimmune == "1"
         #| !is.na(famhisauto) & famhisauto == "1"
      ),
      "1+ NICE features",
      ifelse(is.na(DKA)
             & is.na(Unintentional_weight_loss_v1)
             & is.na(AgeatDiagnosis)
             & is.na(bmi_model)
             & is.na(autoimmune),
             #& is.na(famhisauto),
             NA,
             "No NICE features")),
    ADAfeatures = ifelse(
      (!is.na(DKA) & DKA == "1")
      | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes")
      | (!is.na(AgeatDiagnosis) & AgeatDiagnosis < 35)
      | (!is.na(bmi_model) & bmi_model < 25)
      #Glucose at diagnosis
      | (!is.na(prerandomglucose) & prerandomglucose > 20),
      "1+ ADA features",
      ifelse(is.na(DKA)
             & is.na(Unintentional_weight_loss_v1)
             & is.na(AgeatDiagnosis)
             & is.na(bmi_model)
             & is.na(prerandomglucose),
             NA,
             "No ADA features")
    ),
    famhisnoninsdiab = famhisdiab)

SR_mD_data_CPRD <- data %>%
  select(ins_3yrs, follow_up_3yrs, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,famhisdiab,
         #famhisnoninsdiab,
         NICEfeatures1_noage,
         ADAfeatures,dka, t1_code, basal_bolus, Eth_4cat, Eth_5cat) %>%
  mutate(
    m1_pp = predict(m1, data, type = "response"),
    #m2_pp = predict(m2, data, type = "response"),
    agedx = case_when(
      AgeatDiagnosis <=24 ~ 3,
      AgeatDiagnosis <=34 ~ 2,
      AgeatDiagnosis <=44 ~ 1,
      AgeatDiagnosis >44 ~ 0
    ),
    bmi = case_when(
      bmi_model < 25 ~ 5,
      bmi_model < 35 ~ 3,
      bmi_model >= 35 ~ 0
    ),
    hba1c = case_when(
      HbA1c_at_diagnosis_v1 < 58 ~ 0,
      HbA1c_at_diagnosis_v1 >= 58 ~ 1
    ),
    sex = ifelse(Gender_v1 == "Female", 1, 0),
    osmo = ifelse(osmotic == "1", 1, 0),
    auto = ifelse(autoimmune == "1", 1, 0),
    weightloss = ifelse(Unintentional_weight_loss_v1 == "Yes", 1, 0),
    DKA = ifelse(DKA == "1", 1, 0),
    eth = ifelse(Eth_5cat == "0", 0, -1),
    pardm = ifelse(famhisdiab == "No", 1, 0)
  )

SR_mD_data_CPRD <- SR_mD_data_CPRD %>%
  select(ins_3yrs, follow_up_3yrs, m1_pp, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, DKA, eth, pardm,
         NICEfeatures1_noage,
         ADAfeatures,dka, t1_code, basal_bolus, AgeatDiagnosis) %>%
  mutate(Score1 = select(., agedx:pardm) %>% rowSums(na.rm = TRUE),
         SRoutcome = ins_3yrs)

SR_mD_data_CPRD <- SR_mD_data_CPRD %>%
  mutate(
  #   SR_score_cat = case_when(
  #   Score1 < 6 ~ "0-5",
  #   Score1 <= 11 ~ "6-11",
  #   Score1 > 11 ~ "12-14"
  # ),
  SR_score_cat1 = case_when(
    Score1 < 7 ~ "0-6",
    Score1 <= 11 ~ "7-11",
    Score1 > 11 ~ "12-14"
  ),
  model1_cat = case_when(
    m1_pp < 0.1 ~"<10%",
    m1_pp <= 0.9 ~"10-90%",
    m1_pp > 0.9 ~ ">90%"
  ))

NICE_m12_CPRD <- SR_mD_data_CPRD %>%
  dplyr::select(SRoutcome, ins_3yrs, follow_up_3yrs,
                NICEfeatures1_noage, ADAfeatures,Score1, m1_pp,
                SR_score_cat1, model1_cat, dka, t1_code, basal_bolus, AgeatDiagnosis) %>%
  mutate(
    NICEfeatures1_noage = ifelse(NICEfeatures1_noage == "1+ NICE features", 1, 0),
    model10 = ifelse(m1_pp >=0.1, 1, 0),
    #model10_90 = ifelse(m1_pp >=0.1 & m1_pp <=0.9, 1, 0),
    #score6_11 = ifelse(SR_score_cat == "6-11", 1, 0),
    #score5 = ifelse(Score1 > 5, 1, 0),
    score6 = ifelse(Score1 > 6, 1, 0),
    ADAfeatures = ifelse(ADAfeatures == "1+ ADA features", 1, 0)
  )

SR_mD_data_CPRD %>%
  filter(Score1 > 5) %>%
  count()
SR_mD_data_CPRD %>%
  filter(Score1 > 6) %>%
  count()
SR_mD_data_CPRD %>%
  filter(Score1 > 4) %>%
  count()


#Run functions ---------------------------------------------------------------------
varlist_cat <- c(
  "pos_anti",
  #"NICE1_pos",
  "NICE1_pos_noage",
  #"ADAfeatures",
  "ADAfeatures_pos",
  "model10_pos",
  "model_antis",
  #"score5_pos",
  "score6_pos",
  "score_antis")
##StartRight -----------------------------------------------------------------------
NICE_m12_SR <-  NICE_m12 %>%
  filter(data == "StartRight")

cat_contingency(varlist_cat,
                dataset = NICE_m12_SR,
                outcome = "SRoutcome",
                saving_name = "tables/SR_MODELS_SCORE_function",
                complete_case = TRUE,
                plots = FALSE,
                decimals = 1)
##StartRight Prime ---------------------------------------------------------------
NICE_m12_PRIME <-  NICE_m12 %>%
  filter(data == "StartRight Prime")

cat_contingency(varlist_cat,
                dataset = NICE_m12_PRIME,
                outcome = "SRoutcome",
                saving_name = "tables/PRIME_MODELS_SCORE_function",
                complete_case = TRUE,
                plots = FALSE,
                decimals = 1)
##Joint ---------------------------------------------------------------------------
cat_contingency(varlist_cat,
                dataset = NICE_m12,
                outcome = "SRoutcome",
                saving_name = "tables/Joint_MODELS_SCORE_function",
                complete_case = TRUE,
                plots = FALSE,
                decimals = 1)
##CPRD -----------------------------------------------------------------------------
# cat_contingency(varlist_cat,
#                 dataset = NICE_m12_CPRD,
#                 outcome = "SRoutcome",
#                 saving_name = "tables/CPRD_MODELS_SCORE_function",
#                 complete_case = TRUE,
#                 plots = FALSE,
#                 decimals = 1)

#Testing strategies ----------------------------------------------------------------
##StartRight -----------------------------------------------------------------------
#1) antibodies in SR ----------------------------------------------------------
ANTI1_SR <- NICE_m12 %>%
  filter(data == "StartRight") %>%
  summarise(
    variable = "pos_anti",
    n = n(),
    totalover = n(),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(pos_anti == 1),
    PPV1 = (sum(pos_anti == 1)/totalover)*100,
    all_three = sum(pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum(pos_anti == 0 & SRoutcome == 0),
    nmissed_antineg = sum(pos_anti == 0),
    test_true = n_anti_pos_pickedup,
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#2) NICE1 without age-----------------------------------------------------------------
NICE1_noage_SR <- NICE_m12 %>%
  filter(data == "StartRight") %>%
  summarise(
    variable = "NICE1_pos_noage",
    n = n(),
    totalover = sum(NICEfeatures1_noage == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(NICEfeatures1_noage == 1 & pos_anti == 1),
    PPV1 = (sum(NICEfeatures1_noage == 1 & pos_anti == 1)/sum(NICEfeatures1_noage == 1))*100,
    all_three = sum(NICEfeatures1_noage == 1 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((NICEfeatures1_noage == 0 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(NICEfeatures1_noage == 0 | pos_anti == 0),
    test_true = n_anti_pos_pickedup,
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#3) ADA features - StartRight -------------------------------------------------------
ADA_SR <- NICE_m12 %>%
  filter(data == "StartRight") %>%
  summarise(
    variable = "ADAfeatures_pos",
    n = n(),
    totalover = sum(ADAfeatures == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(ADAfeatures == 1 & pos_anti == 1),
    PPV1 = (sum(ADAfeatures == 1 & pos_anti == 1)/sum(ADAfeatures == 1))*100,
    all_three = sum(ADAfeatures == 1 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((ADAfeatures == 0 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(ADAfeatures == 0 | pos_anti == 0),
    test_true = sum(ADAfeatures == 1 & pos_anti == 1),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#4a) score 5  ------------------------------------------------------------------
# SCORE5_SR <- NICE_m12 %>%
#   filter(data == "StartRight") %>%
#   summarise(
#     variable = "score5_pos",
#     n = n(),
#     totalover = sum(score5 == 1),
#     perc_testing = (totalover/n)*100,
#     n_anti_pos_pickedup = sum(score5_pos == 1),
#     PPV1 = (n_anti_pos_pickedup/totalover)*100,
#     all_three = sum(score5_pos == 1 & SRoutcome == 1),
#     all_three_under = sum(score5_pos == 0 & SRoutcome == 0),
#     nmissed_antineg = sum(score5_pos == 0),
#     test_true = sum(score5_pos == 1),
#     PPV2 = (all_three/test_true)*100,
#     NPV2 = (all_three_under/sum(nmissed_antineg))*100,
#     n_T1D = sum(SRoutcome == 1),
#     n_T2D = sum(SRoutcome == 0),
#     Sens = (all_three/n_T1D)*100,
#     Spec = (all_three_under/n_T2D)*100,
#     True = all_three + all_three_under,
#     Accuracy = (True/n)*100
#   )
#4b) score 6  ------------------------------------------------------------------
SCORE6_SR <- NICE_m12 %>%
  filter(data == "StartRight") %>%
  summarise(
    variable = "score6_pos",
    n = n(),
    totalover = sum(score6 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(score6_pos == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(score6_pos == 1 & SRoutcome == 1),
    all_three_under = sum(score6_pos == 0 & SRoutcome == 0),
    nmissed_antineg = sum(score6_pos == 0),
    test_true = sum(score6_pos == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

#4c) score 6 antis  ------------------------------------------------------------------
SCORE_ANTIS_SR <- NICE_m12 %>%
  filter(data == "StartRight") %>%
  summarise(
    variable = "score_antis",
    n = n(),
    totalover = sum(score6 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(score_antis == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(score_antis == 1 & SRoutcome == 1),
    all_three_under = sum(score_antis == 0 & SRoutcome == 0),
    nmissed_antineg = sum(score_antis == 0),
    test_true = sum(score_antis == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

#5a) MODEL 10 ------------------------------------------------------------------
MODEL10_SR <- NICE_m12 %>%
  filter(data == "StartRight") %>%
  summarise(
    variable = "model10_pos",
    n = n(),
    totalover = sum(model10 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(model10_pos == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(model10_pos == 1 & SRoutcome == 1),
    all_three_under = sum(model10_pos == 0 & SRoutcome == 0),
    nmissed_antineg = sum(model10_pos == 0),
    test_true = sum(model10_pos == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#5b) MODEL antis ------------------------------------------------------------------
MODEL_ANTIS_SR <- NICE_m12 %>%
  filter(data == "StartRight") %>%
  summarise(
    variable = "model_antis",
    n = n(),
    totalover = sum(model10 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(model_antis == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(model_antis == 1 & SRoutcome == 1),
    all_three_under = sum(model_antis == 0 & SRoutcome == 0),
    nmissed_antineg = sum(model_antis == 0),
    test_true = sum(model_antis == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
##StartRight Prime ----------------------------------------------------------------
#1) antibodies in SR PRIME----------------------------------------------------------
ANTI1_PRIME <- NICE_m12 %>%
  filter(data == "StartRight Prime") %>%
  summarise(
    variable = "pos_anti",
    n = n(),
    totalover = n(),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(pos_anti == 1),
    PPV1 = (sum(pos_anti == 1)/totalover)*100,
    all_three = sum(pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum(pos_anti == 0 & SRoutcome == 0),
    nmissed_antineg = sum(pos_anti == 0),
    test_true = n_anti_pos_pickedup,
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#2) NICE1 without age-----------------------------------------------------------------
NICE1_noage_PRIME <- NICE_m12 %>%
  filter(data == "StartRight Prime") %>%
  summarise(
    variable = "NICE1_pos_noage",
    n = n(),
    totalover = sum(NICEfeatures1_noage == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(NICEfeatures1_noage == 1 & pos_anti == 1),
    PPV1 = (sum(NICEfeatures1_noage == 1 & pos_anti == 1)/sum(NICEfeatures1_noage == 1))*100,
    all_three = sum(NICEfeatures1_noage == 1 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((NICEfeatures1_noage == 0 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(NICEfeatures1_noage == 0 | pos_anti == 0),
    test_true = n_anti_pos_pickedup,
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#3) ADA features ----------------------------------------------------------------
ADA_PRIME <- NICE_m12 %>%
  filter(data == "StartRight Prime") %>%
  summarise(
    variable = "ADAfeatures_pos",
    n = n(),
    totalover = sum(ADAfeatures == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(ADAfeatures == 1 & pos_anti == 1),
    PPV1 = (sum(ADAfeatures == 1 & pos_anti == 1)/sum(ADAfeatures == 1))*100,
    all_three = sum(ADAfeatures == 1 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((ADAfeatures == 0 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(ADAfeatures == 0 | pos_anti == 0),
    test_true = sum(ADAfeatures == 1 & pos_anti == 1),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    #n_total_antineg = sum(pos_anti == 0),
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#4a) score 5  ------------------------------------------------------------------
# SCORE5_PRIME <- NICE_m12 %>%
#   filter(data == "StartRight Prime") %>%
#   summarise(
#     variable = "score5_pos",
#     n = n(),
#     totalover = sum(score5 == 1),
#     perc_testing = (totalover/n)*100,
#     n_anti_pos_pickedup = sum(score5_pos == 1),
#     PPV1 = (n_anti_pos_pickedup/totalover)*100,
#     all_three = sum(score5_pos == 1 & SRoutcome == 1),
#     all_three_under = sum(score5_pos == 0 & SRoutcome == 0),
#     nmissed_antineg = sum(score5_pos == 0),
#     test_true = sum(score5_pos == 1),
#     PPV2 = (all_three/test_true)*100,
#     NPV2 = (all_three_under/sum(nmissed_antineg))*100,
#     n_T1D = sum(SRoutcome == 1),
#     n_T2D = sum(SRoutcome == 0),
#     Sens = (all_three/n_T1D)*100,
#     Spec = (all_three_under/n_T2D)*100,
#     True = all_three + all_three_under,
#     Accuracy = (True/n)*100
#   )

#4b) score 6  ------------------------------------------------------------------
SCORE6_PRIME <- NICE_m12 %>%
  filter(data == "StartRight Prime") %>%
  summarise(
    variable = "score6_pos",
    n = n(),
    totalover = sum(score6 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(score6_pos == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(score6_pos == 1 & SRoutcome == 1),
    all_three_under = sum(score6_pos == 0 & SRoutcome == 0),
    nmissed_antineg = sum(score6_pos == 0),
    test_true = sum(score6_pos == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

#4c) score antis  ------------------------------------------------------------------
SCORE_ANTIS_PRIME <- NICE_m12 %>%
  filter(data == "StartRight Prime") %>%
  summarise(
    variable = "score_antis",
    n = n(),
    totalover = sum(score6 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(score_antis == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(score_antis == 1 & SRoutcome == 1),
    all_three_under = sum(score_antis == 0 & SRoutcome == 0),
    nmissed_antineg = sum(score_antis == 0),
    test_true = sum(score_antis == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

#5) MODEL 10 ------------------------------------------------------------------
MODEL10_PRIME <- NICE_m12 %>%
  filter(data == "StartRight Prime") %>%
  summarise(
    variable = "model10_pos",
    n = n(),
    totalover = sum(model10 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(model10_pos == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(model10_pos == 1 & SRoutcome == 1),
    all_three_under = sum(model10_pos == 0 & SRoutcome == 0),
    nmissed_antineg = sum(model10_pos == 0),
    test_true = sum(model10_pos == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

#5B) MODEL ANTIS ------------------------------------------------------------------
MODEL_ANTIS_PRIME <- NICE_m12 %>%
  filter(data == "StartRight Prime") %>%
  summarise(
    variable = "model_antis",
    n = n(),
    totalover = sum(model10 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(model_antis == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(model_antis == 1 & SRoutcome == 1),
    all_three_under = sum(model_antis == 0 & SRoutcome == 0),
    nmissed_antineg = sum(model_antis == 0),
    test_true = sum(model_antis == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

##Joint SR -------------------------------------------------------------------------
#1) antibodies in joint ----------------------------------------------------------
ANTI1 <- NICE_m12 %>%
  summarise(
    variable = "pos_anti",
    n = n(),
    totalover = n(),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(pos_anti == 1),
    PPV1 = (sum(pos_anti == 1)/totalover)*100,
    all_three = sum(pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum(pos_anti == 0 & SRoutcome == 0),
    nmissed_antineg = sum(pos_anti == 0),
    test_true = n_anti_pos_pickedup,
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#2) NICE1 without age-----------------------------------------------------------------
NICE1_noage <- NICE_m12 %>%
  summarise(
    variable = "NICE1_pos_noage",
    n = n(),
    totalover = sum(NICEfeatures1_noage == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(NICEfeatures1_noage == 1 & pos_anti == 1),
    PPV1 = (sum(NICEfeatures1_noage == 1 & pos_anti == 1)/sum(NICEfeatures1_noage == 1))*100,
    all_three = sum(NICEfeatures1_noage == 1 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((NICEfeatures1_noage == 0 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(NICEfeatures1_noage == 0 | pos_anti == 0),
    test_true = n_anti_pos_pickedup,
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#3) ADA features  -------------------------------------------------------
ADA <- NICE_m12 %>%
  summarise(
    variable = "ADAfeatures_pos",
    n = n(),
    totalover = sum(ADAfeatures == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(ADAfeatures == 1 & pos_anti == 1),
    PPV1 = (sum(ADAfeatures == 1 & pos_anti == 1)/sum(ADAfeatures == 1))*100,
    all_three = sum(ADAfeatures == 1 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((ADAfeatures == 0 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(ADAfeatures == 0 | pos_anti == 0),
    test_true = sum(ADAfeatures == 1 & pos_anti == 1),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    #n_total_antineg = sum(pos_anti == 0),
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#4a) score 5  ------------------------------------------------------------------
# SCORE5 <- NICE_m12 %>%
#   summarise(
#     variable = "score5_pos",
#     n = n(),
#     totalover = sum(score5 == 1),
#     perc_testing = (totalover/n)*100,
#     n_anti_pos_pickedup = sum(score5_pos == 1),
#     PPV1 = (n_anti_pos_pickedup/totalover)*100,
#     all_three = sum(score5_pos == 1 & SRoutcome == 1),
#     all_three_under = sum(score5_pos == 0 & SRoutcome == 0),
#     nmissed_antineg = sum(score5_pos == 0),
#     test_true = sum(score5_pos == 1),
#     PPV2 = (all_three/test_true)*100,
#     NPV2 = (all_three_under/sum(nmissed_antineg))*100,
#     n_T1D = sum(SRoutcome == 1),
#     n_T2D = sum(SRoutcome == 0),
#     Sens = (all_three/n_T1D)*100,
#     Spec = (all_three_under/n_T2D)*100,
#     True = all_three + all_three_under,
#     Accuracy = (True/n)*100
#   )

#4b) score 6  ------------------------------------------------------------------
SCORE6 <- NICE_m12 %>%
  summarise(
    variable = "score6_pos",
    n = n(),
    totalover = sum(score6 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(score6_pos == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(score6_pos == 1 & SRoutcome == 1),
    all_three_under = sum(score6_pos == 0 & SRoutcome == 0),
    nmissed_antineg = sum(score6_pos == 0),
    test_true = sum(score6_pos == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

#4b) score ANTIS  ------------------------------------------------------------------
SCORE_ANTIS <- NICE_m12 %>%
  summarise(
    variable = "score_antis",
    n = n(),
    totalover = sum(score6 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(score_antis == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(score_antis == 1 & SRoutcome == 1),
    all_three_under = sum(score_antis == 0 & SRoutcome == 0),
    nmissed_antineg = sum(score_antis == 0),
    test_true = sum(score_antis == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

#5a) MODEL 10 ------------------------------------------------------------------
MODEL10 <- NICE_m12 %>%
  summarise(
    variable = "model10_pos",
    n = n(),
    totalover = sum(model10 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(model10_pos == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(model10_pos == 1 & SRoutcome == 1),
    all_three_under = sum(model10_pos == 0 & SRoutcome == 0),
    nmissed_antineg = sum(model10_pos == 0),
    test_true = sum(model10_pos == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )
#5b) MODEL ANTIS ------------------------------------------------------------------
MODEL_ANTIS <- NICE_m12 %>%
  summarise(
    variable = "model_antis",
    n = n(),
    totalover = sum(model10 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(model_antis == 1),
    PPV1 = (n_anti_pos_pickedup/totalover)*100,
    all_three = sum(model_antis == 1 & SRoutcome == 1),
    all_three_under = sum(model_antis == 0 & SRoutcome == 0),
    nmissed_antineg = sum(model_antis == 0),
    test_true = sum(model_antis == 1),
    PPV2 = (all_three/test_true)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    Spec = (all_three_under/n_T2D)*100,
    True = all_three + all_three_under,
    Accuracy = (True/n)*100
  )

##CPRD ---------------------------------------------------------------------------
#1) antibodies -----------------------------------------------------------------------
ANTI1_CPRD <- NICE_m12_CPRD %>%
  summarise(
    variable = "pos_anti",
    n = n(),
    totalover = n(),
    perc_testing = (totalover/n)*100,
    nins = sum(SRoutcome == 1 ),
    pins = nins/totalover*100,
    nDKA = sum(dka == 1 ),
    pDKA = nDKA/totalover*100,
    nT1D = sum(t1_code == 1 ),
    pT1D = nT1D/totalover*100,
    nbasal = sum(basal_bolus == 1 ),
    pbasal = nbasal/totalover*100
  )
#2) NICE1 without age-----------------------------------------------------------------
NICE1_noage_CPRD <- NICE_m12_CPRD %>%
  summarise(
    variable = "NICE1_pos_noage",
    n = n(),
    totalover = sum(NICEfeatures1_noage == 1),
    perc_testing = (totalover/n)*100,
    nins = sum(SRoutcome == 1 & NICEfeatures1_noage == 1),
    pins = nins/totalover*100,
    nDKA = sum(dka == 1 & NICEfeatures1_noage == 1),
    pDKA = nDKA/totalover*100,
    nT1D = sum(t1_code == 1 & NICEfeatures1_noage == 1),
    pT1D = nT1D/totalover*100,
    nbasal = sum(basal_bolus == 1 & NICEfeatures1_noage == 1),
    pbasal = nbasal/totalover*100
  )

#3) ADA features ------------------------------------------------------------------
ADA_CPRD <- NICE_m12_CPRD %>%
  summarise(
    variable = "ADAfeatures_pos",
    n = n(),
    totalover = sum(ADAfeatures == 1),
    perc_testing = (totalover/n)*100,
    nins = sum(SRoutcome == 1 & ADAfeatures == 1),
    pins = nins/totalover*100,
    nDKA = sum(dka == 1 & ADAfeatures == 1),
    pDKA = nDKA/totalover*100,
    nT1D = sum(t1_code == 1 & ADAfeatures == 1),
    pT1D = nT1D/totalover*100,
    nbasal = sum(basal_bolus == 1 & ADAfeatures == 1),
    pbasal = nbasal/totalover*100
  )

#4a) score 5  ------------------------------------------------------------------
# SCORE5_CPRD <- NICE_m12_CPRD %>%
#   summarise(
#     variable = "score5_pos",
#     n = n(),
#     totalover = sum(score5 == 1),
#     perc_testing = (totalover/n)*100,
#     nins = sum(SRoutcome == 1 & score5 == 1),
#     pins = nins/totalover*100,
#     nDKA = sum(dka == 1 & score5 == 1),
#     pDKA = nDKA/totalover*100,
#     nT1D = sum(t1_code == 1 & score5 == 1),
#     pT1D = nT1D/totalover*100,
#     nbasal = sum(basal_bolus == 1 & score5 == 1),
#     pbasal = nbasal/totalover*100
#   )

#4b) score 6  ------------------------------------------------------------------
SCORE6_CPRD <- NICE_m12_CPRD %>%
  summarise(
    variable = "score6_pos",
    n = n(),
    totalover = sum(score6 == 1),
    perc_testing = (totalover/n)*100,
    nins = sum(SRoutcome == 1 & score6 == 1),
    pins = nins/totalover*100,
    nDKA = sum(dka == 1 & score6 == 1),
    pDKA = nDKA/totalover*100,
    nT1D = sum(t1_code == 1 & score6 == 1),
    pT1D = nT1D/totalover*100,
    nbasal = sum(basal_bolus == 1 & score6 == 1),
    pbasal = nbasal/totalover*100
  )

#5) MODEL 10 ------------------------------------------------------------------
MODEL10_CPRD <- NICE_m12_CPRD %>%
  summarise(
    variable = "model10_pos",
    n = n(),
    totalover = sum(model10 == 1),
    perc_testing = (totalover/n)*100,
    nins = sum(SRoutcome == 1 & model10 == 1),
    pins = nins/totalover*100,
    nDKA = sum(dka == 1 & model10 == 1),
    pDKA = nDKA/totalover*100,
    nT1D = sum(t1_code == 1 & model10 == 1),
    pT1D = nT1D/totalover*100,
    nbasal = sum(basal_bolus == 1 & model10 == 1),
    pbasal = nbasal/totalover*100
  )




#Tables ----------------------------------------------------------------------------
#A) Separate ---------------------------------------------------------------------------
####StartRight  --------------------------------------------------------------------
SR_criteria_table <- rbind(ANTI1_SR, NICE1_noage_SR, ADA_SR, MODEL10_SR, MODEL_ANTIS_SR,
                           #SCORE5_SR,
                           SCORE6_SR, SCORE_ANTIS_SR)
SR_criteria_table <- SR_criteria_table %>%
  mutate(
    `SR: % Meet criteria for antibody testing` = paste0(
      round(perc_testing,2), " [", totalover, "/", n, "]"),
    `SR: Proportion of those eligible for testing, antibody positive` = paste0(
      round(PPV1,2), " [", n_anti_pos_pickedup, "/", totalover, "]"),
    Sensitivity_N = paste0(
      "[", all_three, "/", n_T1D,"]"),
    Specificity_N = paste0(
      "[", all_three_under, "/", n_T2D, "]")
  ) %>%
  select(
    variable,
    `SR: % Meet criteria for antibody testing`,
    `SR: Proportion of those eligible for testing, antibody positive`,
    Sensitivity_N,
    Specificity_N
  )
`tables/SR_MODELS_SCORE_function` <- `tables/SR_MODELS_SCORE_function` %>%
  select(variable, `1 n`, n, ROC_AUC, Sensitivity, Specificity)

SR_criteria <- left_join(SR_criteria_table, `tables/SR_MODELS_SCORE_function`) %>%
  mutate(
    `SR: Sensitivity` = paste0(Sensitivity, " ", Sensitivity_N),
    `SR: Specificity` = paste0(Specificity, " ", Specificity_N)
  ) %>%
  select(
    variable,
    `SR: % Meet criteria for antibody testing`,
    `SR: Proportion of those eligible for testing, antibody positive`,
    `SR: Sensitivity`,
    `SR: Specificity`
  )

#write_xlsx(NICE_criteria,"tables/Supp_Table22_SR_criteria.xlsx")
##Prime -------------------------------------------------------------------------------
PRIME_criteria_table <- rbind(ANTI1_PRIME, NICE1_noage_PRIME, ADA_PRIME, MODEL10_PRIME, MODEL_ANTIS_PRIME,
                              #SCORE5_PRIME,
                              SCORE6_PRIME, SCORE_ANTIS_PRIME)

PRIME_criteria_table <- PRIME_criteria_table %>%
  mutate(
    `PRIME: % Meet criteria for antibody testing` = paste0(
      round(perc_testing,2), " [", totalover, "/", n, "]"),
    `PRIME: Proportion of those eligible for testing, antibody positive` = paste0(
      round(PPV1,2), " [", n_anti_pos_pickedup, "/", totalover, "]"),
    Sensitivity_N = paste0(
      "[", all_three, "/", n_T1D,"]"),
    Specificity_N = paste0(
      "[", all_three_under, "/", n_T2D, "]")
  ) %>%
  select(
    variable,
    `PRIME: % Meet criteria for antibody testing`,
    `PRIME: Proportion of those eligible for testing, antibody positive`,
    Sensitivity_N,
    Specificity_N
  )

`tables/PRIME_MODELS_SCORE_function` <- `tables/PRIME_MODELS_SCORE_function` %>%
  select(variable, `1 n`, n, ROC_AUC, Sensitivity, Specificity)

criteria_PRIME <- left_join(PRIME_criteria_table, `tables/PRIME_MODELS_SCORE_function`) %>%
  mutate(
    `PRIME: Sensitivity` = paste0(Sensitivity, " ", Sensitivity_N),
    `PRIME: Specificity` = paste0(Specificity, " ", Specificity_N)
  ) %>%
  select(
    variable,
    `PRIME: % Meet criteria for antibody testing`,
    `PRIME: Proportion of those eligible for testing, antibody positive`,
    `PRIME: Sensitivity`,
    `PRIME: Specificity`
  )

#write_xlsx(NICE_criteria_PRIME,"tables/Supp_Table22_PRIME_criteria.xlsx")

#B) Joint ------------------------------------------------------------------------------
Joint_criteria_table <- rbind(ANTI1, NICE1_noage, ADA, MODEL10, MODEL_ANTIS,
                              #SCORE5,
                              SCORE6, SCORE_ANTIS)
Joint_criteria_table <- Joint_criteria_table %>%
  mutate(
    `JOINT: % Meet criteria for antibody testing` = paste0(
      round(perc_testing,2), " [", totalover, "/", n, "]"),
    `JOINT: Proportion of those eligible for testing, antibody positive` = paste0(
      round(PPV1,2), " [", n_anti_pos_pickedup, "/", totalover, "]"),
    Sensitivity_N = paste0(
      "[", all_three, "/", n_T1D,"]"),
    Specificity_N = paste0(
      "[", all_three_under, "/", n_T2D, "]")
  ) %>%
  select(
    variable,
    `JOINT: % Meet criteria for antibody testing`,
    `JOINT: Proportion of those eligible for testing, antibody positive`,
    Sensitivity_N,
    Specificity_N
  )
`tables/Joint_MODELS_SCORE_function` <- `tables/Joint_MODELS_SCORE_function` %>%
  select(variable, `1 n`, n, ROC_AUC, Sensitivity, Specificity) %>%
  filter(!is.na(`1 n`))

Joint_criteria <- left_join(Joint_criteria_table, `tables/Joint_MODELS_SCORE_function`) %>%
  mutate(
    `JOINT: Sensitivity` = paste0(Sensitivity, " ", Sensitivity_N),
    `JOINT: Specificity` = paste0(Specificity, " ", Specificity_N)
  ) %>%
  select(
    variable,
    `JOINT: % Meet criteria for antibody testing`,
    `JOINT: Proportion of those eligible for testing, antibody positive`,
    `JOINT: Sensitivity`,
    `JOINT: Specificity`
  )

#write_xlsx(NICE_criteria,"tables/Supp_Table22_SR_criteria.xlsx")
#NICE_criteria_table1 <- rbind(ADA_SR, ADA_SR1)
#CPRD ------------------------------------------------------------------------------
CPRD_criteria_table <- rbind(ANTI1_CPRD, NICE1_noage_CPRD, ADA_CPRD, MODEL10_CPRD,
                             #SCORE5_CPRD,
                             SCORE6_CPRD)
CPRD_criteria_table <- CPRD_criteria_table %>%
  mutate(
    `% CPRD` = paste0(
      round(perc_testing,2), " [", totalover, "/", n, "]")) %>%
  select(variable, `% CPRD`)
write_xlsx(CPRD_criteria_table,"tables/Supp_Table17_CPRD_only.xlsx")

#Make table ----------------------------------------------------------------------
#A) Supp_Table17A (separate)
# Tbl17A <- left_join(CPRD_criteria_table, SR_criteria)
# Tbl17A <- left_join(Tbl17A, criteria_PRIME)
# write_xlsx(Tbl17A,"tables/Supp_Table17A.xlsx")
#B) Supp_Table17B (joint)
#Tbl17B <- left_join(CPRD_criteria_table, Joint_criteria)
Tbl17A <- full_join(CPRD_criteria_table, Joint_criteria)
write_xlsx(Tbl17A,"tables/Supp_Table17.xlsx")
################################################################################################
