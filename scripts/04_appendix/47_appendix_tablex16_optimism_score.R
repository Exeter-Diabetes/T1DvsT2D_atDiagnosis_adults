#just auc
#####################################################################################
#Scoring table performance
#Figures
#########################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(pROC)


#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
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


##Scoring Table -----------------------------------------------------------------------
SR_mD_data <- SR_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, ethnicity, num_anti) %>%
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
    eth = ifelse(ethnicity == "White", 0, -1),
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    pos_anti = ifelse(num_anti >= 1, 1, 0)
  )
table(SR_mD_data$eth, useNA = "ifany")
table(SR_m12_data$ethnicity, useNA = "ifany")

SR_mD_data <- SR_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, dka, eth,
         pardm, pos_anti) %>%
  mutate(ScoreD = select(., agedx:pardm) %>% rowSums(na.rm = TRUE)) %>%
  mutate(id = 1:n())

set.seed(123)
n_boot <- 1000
bootstrapped_ROC <- data.frame(training = NA)
dataset_bootstrap <- modelr::bootstrap(SR_mD_data, n_boot)
#Apparent ROC of original model to original data
full_roc_model <- roc(SR_mD_data$SRoutcome,
                      SR_mD_data$ScoreD,
                      plot = FALSE,
                      ci = TRUE)


for (i in 1:1000) {
  print(i)
  #sample new dataset based on bootstrap iteration
  new_dataset <- SR_mD_data[dataset_bootstrap$strap[[i]]$idx,]
  #new_dataset <- SR_m12_data[dataset_bootstrap$strap[[1]]$idx,]



  ##Optimism ------------------------------------------------------------------------------
  ###AUCROC
  #This is the bootstrap sample
  ROC <- data.frame(training = NA)
  training_roc_model <- roc(new_dataset$SRoutcome,
                            new_dataset$ScoreD,
                              plot = FALSE,
                              ci = TRUE)
  ROC$training <- training_roc_model$auc
  bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
}
  m1_optimism_rocs <- data.frame(
    `Performance metric` = "ROCAUC",
    Apparent = full_roc_model$auc,
    Bootstrap = mean(bootstrapped_ROC$training, na.rm = TRUE)
  ) %>%
    mutate(
      Optimism = Bootstrap - Apparent
    )
  #browser()

#Score with antibodies -------------------------------------------------------------
  #Model12
  SR_m12_data <- SR_SRout_ccc %>%
    drop_na(all_of(all_vars_12)) %>%
    mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
  #StartRight----------------------------------------------------------------------------------
  SR_mD_data <- SR_m12_data %>%
    select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
           HbA1c_at_diagnosis_v1, Gender_v1,
           osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
           famhisnoninsdiab, ethnicity, num_anti) %>%
    mutate(
      agedx = case_when(
        AgeatDiagnosis <=24 ~ 2,
        AgeatDiagnosis <=34 ~ 2,
        AgeatDiagnosis <=44 ~ 1,
        AgeatDiagnosis >44 ~ 0
      ),
      bmi = case_when(
        bmi_model < 25 ~ 4,
        bmi_model < 35 ~ 2,
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
      eth = ifelse(ethnicity == "White", 0, -1),
      num_antis = case_when(
        num_anti == 0 ~ 0,
        num_anti == 1 ~ 2,
        num_anti == 2 ~ 3,
        num_anti == 3 ~ 4
      ),
      pos_anti = ifelse(num_anti >= 1, 1, 0))

  SR_mD_data <- SR_mD_data %>%
    select(Study_ID, SRoutcome, robust_outcome, agedx, bmi, hba1c, sex,
           osmo, auto, weightloss, dka, pardm, eth, num_antis, pos_anti) %>%
    mutate(ScoreD = select(., agedx:num_antis) %>% rowSums(na.rm = TRUE))%>%
    mutate(id = 1:n())

  set.seed(123)
  n_boot <- 1000
  bootstrapped_ROC <- data.frame(training = NA)
  dataset_bootstrap <- modelr::bootstrap(SR_mD_data, n_boot)
  #Apparent ROC of original model to original data
  full_roc_model <- roc(SR_mD_data$SRoutcome,
                        SR_mD_data$ScoreD,
                        plot = FALSE,
                        ci = TRUE)


  for (i in 1:1000) {
    print(i)
    #sample new dataset based on bootstrap iteration
    new_dataset <- SR_mD_data[dataset_bootstrap$strap[[i]]$idx,]
    #new_dataset <- SR_m12_data[dataset_bootstrap$strap[[1]]$idx,]



    ##Optimism ------------------------------------------------------------------------------
    ###AUCROC
    #This is the bootstrap sample
    ROC <- data.frame(training = NA)
    training_roc_model <- roc(new_dataset$SRoutcome,
                              new_dataset$ScoreD,
                              plot = FALSE,
                              ci = TRUE)
    ROC$training <- training_roc_model$auc
    bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
  }
  m2_optimism_rocs <- data.frame(
    `Performance metric` = "ROCAUC",
    Apparent = full_roc_model$auc,
    Bootstrap = mean(bootstrapped_ROC$training, na.rm = TRUE)
  ) %>%
    mutate(
      Optimism = Bootstrap - Apparent
    )

###construct optimism table across bootstraps --------------------------------------
writexl::write_xlsx(
    list(Score = m1_optimism_rocs,
         Score_antis  = m2_optimism_rocs),
    "tables/Supp_Tablex16_Score_optimism_tables.xlsx"
  )
