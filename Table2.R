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
load("m1.RData")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
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
###StartRight ----------------------------------------------------------------------------
SR_SRout_ccc<- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)

#Model12
SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12))


#Test
SR_m12_data <- SR_m12_data %>%
  mutate(
    agediag = factor(case_when(
      AgeatDiagnosis <25 ~ "18-24",
      AgeatDiagnosis <35 ~ "25-34",
      AgeatDiagnosis <45 ~ "35-44",
      AgeatDiagnosis >=45 ~ ">=45",
    )),
    bmi_cat = factor(case_when(
      bmi_model < 25 ~ "<25",
      bmi_model <35 ~ "25-35",
      bmi_model >=35 ~ ">=35",
    )),
    hba1c_diag = factor(ifelse(
      HbA1c_at_diagnosis_v1 < 58,"<58",
      " >=58")))
SR_m12_data %>%
  group_by(agediag) %>%
  summarise(
    min = min(AgeatDiagnosis),
    max = max(AgeatDiagnosis),
    n = n()
  )
SR_m12_data %>%
  group_by(bmi_cat) %>%
  summarise(
    min = min(bmi_model),
    max = max(bmi_model),
    n = n()
  )
SR_m12_data %>%
  group_by(hba1c_diag) %>%
  summarise(
    min = min(HbA1c_at_diagnosis_v1),
    max = max(HbA1c_at_diagnosis_v1),
    n = n()
  )
quantile(SR_m12_data$HbA1c_at_diagnosis_v1, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
###Model 1  --------------------------------------------------------------------------
SR_mD_data <- SR_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, num_anti) %>%
  mutate(
    m1_pp = predict(m1, SR_m12_data, type = "response"),
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
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    pos_anti = ifelse(num_anti >= 1, 1, 0),
    anti_cat = ifelse(num_anti == 0, "0", ifelse(num_anti == 1, "1", "2+"))
  )

SR_mD_data <- SR_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, dka, pardm, pos_anti, anti_cat) %>%
  mutate(ScoreD = select(., agedx:pardm) %>% rowSums(na.rm = TRUE))

SR_mD_data <- SR_mD_data %>%
  mutate(
    SR_score_cat1 = case_when(
      ScoreD <= 5 ~ "0-5",
      ScoreD <= 11 ~ "6-11",
      ScoreD > 11 ~ "11-14"
    ),
    SR_score_cat2 = case_when(
      ScoreD < 6 ~ "0-6",
      ScoreD <= 11 ~ "7-11",
      ScoreD > 11 ~ "12-14"
    ))
varlist = c("ScoreD")


m1 <- glm(SRoutcome ~ relevel(agediag, ref = ">=45") +
            relevel(bmi_cat, ref = ">=35") +
            relevel(hba1c_diag, ref = "<58") +
            #scale(c_peptide_v1) +
            relevel(factor(Gender_v1), ref = "Male") +
            osmotic +
            autoimmune +
            Unintentional_weight_loss_v1 +
            DKA +
            #relevel(factor(Eth_4cat), ref = "White") +
            relevel(factor(ethnicity), ref = "White") +
            relevel(factor(famhisnoninsdiab),ref = "Yes"),
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
# model_info(model = m2,
#            test_data = SR_m12_data,
#            outcome = "SRoutcome",
#            saving_name = "03_04_sm2",
#            manual_plotting = TRUE,
#            manual_plot_name = "m2",
#            decimals = 5)
model_info(model = m1,
           test_data = SR_m12_data,
           outcome = "SRoutcome",
           saving_name = "03_04_sm1",
           manual_plotting = TRUE,
           manual_plot_name = "m1",
           decimals = 5)
coef_table_m1 <- coefs_m1 %>%
  mutate(coef = estimate,
         SCORE = round(coef)) %>%
  dplyr::select(variable, coef, OR, p_value, SCORE) %>%
  rename(`Model Feature` = variable,
         `Model 1 B coefficient` = coef,
         `Model 1 OR` = OR,
         `Model 1 p value` = p_value,
         `Model 1 Score` = SCORE
  )
# coef_table_m2 <- coefs_m2 %>%
#   mutate(coef = estimate,
#          SCORE = round(coef)) %>%
#   dplyr::select(variable, coef, OR, p_value, SCORE) %>%
#   rename(`Model Feature` = variable,
#          `Model 2 B coefficient` = coef,
#          `Model 2 OR` = OR,
#          `Model 2 p value` = p_value,
#          `Model 2 Score` = SCORE
#   )
#coef_table <- left_join(coef_table_m1, coef_table_m1)
write_xlsx(coef_table_m1,"tables/Table2.xlsx")
