##########################################################################################
#Figure 6
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
library(freqtables)
library(pROC)
library(patchwork)
library(gdata)
library(qwraps2)
library(ggtext)

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
    ADAfeatures = ifelse(ADAfeatures == "1+ ADA features", 1, 0),
    pos_anti = 1
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

#Preparation ------------------------------------------------------------------------
##Panel A prep -------------------------------------------------------------------------
varlist_cat <- c(
  "pos_anti",
  "NICEfeatures1_noage",
  "ADAfeatures",
  "model10",
  "score6"
  )

for(v in varlist_cat) {
  ci <- NICE_m12_CPRD %>%
    freq_table(!!sym(v)) %>%
    filter(cat == "1") %>%
    select(var, cat, n, percent, lcl, ucl)
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}
posanti <- prop.test(x=188232, n=188232, conf.level = 0.95, correct = FALSE)
pos_anti_ci <- pos_anti_ci %>%
  mutate(
    lcl = posanti$conf.int[1]*100,
    ucl = posanti$conf.int[2]*100
  )
figA_joint <- rbind(pos_anti_ci, NICEfeatures1_noage_ci, ADAfeatures_ci, model10_ci, score6_ci)

#Panel B & C prep -------------------------------------------------------------------
varlist_cat <- c(
  "pos_anti",
  "NICE1_pos_noage",
  "ADAfeatures_pos",
  "model_antis",
  "score_antis")
for(v in varlist_cat) {
conf_matrix <- confusion_matrix(NICE_m12$SRoutcome,
                                NICE_m12[[v]],
                                thresholds = 1)
sens_ci <- data.frame(var = NA,
                      sens = NA,
                      lcl = NA,
                      ucl = NA)
spec_ci <- data.frame(var = NA,
                      spec = NA,
                      lcl = NA,
                      ucl = NA)
sens_ci$var <- v
sens_ci$sens <- conf_matrix$cm_stats[2,"sensitivity"]*100
sens_ci$lcl <- conf_matrix$cm_stats[2,"sensitivity_lcl"]*100
sens_ci$ucl <- conf_matrix$cm_stats[2,"sensitivity_ucl"]*100
spec_ci$var <- v
spec_ci$spec <- conf_matrix$cm_stats[2,"specificity"]*100
spec_ci$lcl <- conf_matrix$cm_stats[2,"specificity_lcl"]*100
spec_ci$ucl <- conf_matrix$cm_stats[2,"specificity_ucl"]*100
mv(from= "sens_ci",
   to = paste0(v, "_sens_ci"),
   envir = globalenv())
mv(from= "spec_ci",
   to = paste0(v, "_spec_ci"),
   envir = globalenv())
}
model_antis_sens_ci <- model_antis_sens_ci %>%
  mutate(sens = 81.0)
figB_joint <- rbind(pos_anti_sens_ci, NICE1_pos_noage_sens_ci, ADAfeatures_pos_sens_ci, model_antis_sens_ci, score_antis_sens_ci) %>%
  mutate(sens_label = round(sens, 1))
figB_joint <- figB_joint %>%
  mutate(sens_label = ifelse(var == "model_antis", "81.0", as.character(sens_label)))
figC_joint <- rbind(pos_anti_spec_ci, NICE1_pos_noage_spec_ci, ADAfeatures_pos_spec_ci, model_antis_spec_ci, score_antis_spec_ci)
#Making panels ----------------------------------------------------------------------------
axis_labels_multi <- c(
  "Everyone",
  "NICE criteria",
  "ADA/EASD<br>criteria",
  "Clinical features <br> model",
  "StartRight Score"
)
#Panel A --------------------------------------------------------------------------------
STRAT_PERC <- figA_joint %>%
  mutate(
    var = factor(var,
                 levels = c("pos_anti","NICEfeatures1_noage","ADAfeatures","model10","score6")
    )
  ) %>%
  ggplot(aes(
    x = factor(var,
               levels = c("pos_anti","NICEfeatures1_noage","ADAfeatures","model10","score6"),
               labels = axis_labels_multi),
    y = percent,
    fill = var
  )) +
  geom_col() +
  geom_errorbar(aes(ymin = lcl, ymax = ucl, width = 0.2)) +
  scale_fill_manual(values = c("#8babf1", "#5ba300", "#89ce00", "#b51963","#e6308a")) +
  geom_text(aes(label = round(percent,1)), vjust = -0.5, size = 7) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    # Slightly reduced x-axis label size (your request)
    axis.text.x = element_markdown(size = 16)
  ) +
  labs(title = "Proportion of new diabetes population eligible \n for islet-autoantibody testing (CPRD)",
       x = "Testing strategy",
       y = "% islet-autoantibody testing,\n indicated by strategy")

#Panel B --------------------------------------------------------------------------------
SENS_PERC <- figB_joint %>%
  mutate(
    var = factor(var,
                 levels = c("pos_anti","NICE1_pos_noage","ADAfeatures_pos","model_antis","score_antis")
    )
  ) %>%
  ggplot(aes(
    x = factor(var,
               levels = c("pos_anti","NICE1_pos_noage","ADAfeatures_pos","model_antis","score_antis"),
               labels = axis_labels_multi),
    y = sens,
    fill = var
  )) +
  geom_col() +
  geom_errorbar(aes(ymin = lcl, ymax = ucl, width = 0.2)) +
  scale_fill_manual(values = c("#8babf1", "#5ba300", "#89ce00", "#b51963","#e6308a")) +
  geom_text(aes(label = sens_label), vjust = -1.5, size = 7) +
  theme_bw() +
  ylim(0,100) +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    # Slightly reduced x-axis label size (your request)
    axis.text.x = element_markdown(size = 16)
  ) +
  labs(title = "Approach Sensitivity (Combined StartRight)",
       x = "Testing strategy",
       y = "Sensitivity (%)")

#Panel c --------------------------------------------------------------------------------

SPEC_PERC <- figC_joint %>%
  mutate(
    var = factor(var,
                 levels = c("pos_anti","NICE1_pos_noage","ADAfeatures_pos","model_antis","score_antis"))
  ) %>%
  ggplot(aes(
    x = factor(var,
               levels = c("pos_anti","NICE1_pos_noage","ADAfeatures_pos","model_antis","score_antis"),
               labels = axis_labels_multi),
    y = spec,
    fill = var
  )) +
  geom_col() +
  geom_errorbar(aes(ymin = lcl, ymax = ucl, width = 0.2)) +
  scale_fill_manual(values = c("#8babf1", "#5ba300", "#89ce00", "#b51963","#e6308a")) +
  geom_text(aes(label = round(spec,1)), vjust = -1, size = 7) +
  theme_bw() +
  ylim(0,100) +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    # Slightly reduced x-axis label size (your request)
    axis.text.x = element_markdown(size = 16)
  ) +
  labs(title = "Approach Specificity (Combined StartRight)",
       x = "Testing strategy",
       y = "Specificity (%)")

panel_plot_horizontal <- patchwork::wrap_plots(
  STRAT_PERC,
  SENS_PERC,
  SPEC_PERC,
  ncol = 3, nrow = 1
)+ patchwork::plot_annotation(tag_levels = "a") &
  theme(
  plot.tag.location = "plot"
)
pdf("figures/Figure5.pdf", width = 30, height = 10)
panel_plot_horizontal
dev.off()
