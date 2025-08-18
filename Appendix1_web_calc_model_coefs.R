#####################################################################################

#Paper figures

#Coefficients for web prototype app
#Unstandardised

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)

#load functions ------------------------------------------------------------------
source("functions/model_info1.R")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")

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
### Models3 ------------------------------------------------------------------------
#Continuous variables 
varlist_3 = c("AgeatDiagnosis", 
              "bmi_model", 
              "HbA1c_at_diagnosis_v1",
              "T1DGRS2"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat_3 = c(
  "Gender_v1",
  "DKA", 
  "Unintentional_weight_loss_v1", 
  "autoimmune", 
  "osmotic", 
  "famhisnoninsdiab",
  #"famhisauto",
  "num_anti"
)
all_vars_3 <- c(varlist_3, varlist_cat_3)
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
#Model3
SR_m3_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3))
##Models -------------------------------------------------------------------------------
###Model 1
m1 <- glm(SRoutcome ~ AgeatDiagnosis +
            bmi_model +
            HbA1c_at_diagnosis_v1 +
            #scale(c_peptide_v1) +
            Gender_v1 +
            osmotic +
            autoimmune +
            Unintentional_weight_loss_v1 +
            DKA +
            famhisnoninsdiab,
          #famhisauto,
          data = SR_m12_data,
          family = binomial)

m1_alt <- glm(SRoutcome ~ AgeatDiagnosis +
                bmi_model +
                HbA1c_at_diagnosis_v1 +
                #scale(c_peptide_v1) +
                Gender_v1 +
                osmotic +
                autoimmune +
                Unintentional_weight_loss_v1 +
                DKA +
                famhisdiab,
              #famhisauto,
              data = SR_m12_data,
              family = binomial)



###Model 2
m2 <- glm(SRoutcome ~ AgeatDiagnosis +
            bmi_model + 
            HbA1c_at_diagnosis_v1 +
            #scale(c_peptide_v1) +
            Gender_v1 + 
            osmotic + 
            autoimmune + 
            Unintentional_weight_loss_v1 + 
            DKA + 
            famhisnoninsdiab +
            #famhisauto +
            num_anti, 
          data = SR_m12_data, 
          family = binomial)





###Model 3
m3 <- glm(SRoutcome ~ AgeatDiagnosis +
            bmi_model + 
            HbA1c_at_diagnosis_v1 +
            #scale(c_peptide_v1) +
            Gender_v1 + 
            osmotic + 
            autoimmune + 
            Unintentional_weight_loss_v1 + 
            DKA + 
            famhisnoninsdiab +
            #famhisauto +
            num_anti +
            T1DGRS2, 
          data = SR_m3_data, 
          family = binomial)



###Model 3
m4 <- glm(SRoutcome ~ AgeatDiagnosis +
            bmi_model + 
            HbA1c_at_diagnosis_v1 +
            #scale(c_peptide_v1) +
            Gender_v1 + 
            osmotic + 
            autoimmune + 
            Unintentional_weight_loss_v1 + 
            DKA + 
            famhisnoninsdiab +
            #famhisauto +
            T1DGRS2, 
          data = SR_m3_data, 
          family = binomial)
#save(m4, file = "m4.RData")



##Display item---------------------------------------------------------------------------
###Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m1, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm1", 
           manual_plotting = TRUE, 
           manual_plot_name = "m1", 
           decimals = 5)
model_info(model = m1_alt, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm1_alt", 
           manual_plotting = TRUE, 
           manual_plot_name = "m1_alt",
           decimals = 5)


###Model 2 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m2, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm2", 
           manual_plotting = TRUE, 
           manual_plot_name = "m2",
           decimals = 5)

###Model 3 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m3, 
           test_data = SR_m3_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm3", 
           manual_plotting = TRUE, 
           manual_plot_name = "m3",
           decimals = 5)

###Model 4 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m4, 
           test_data = SR_m3_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm4", 
           manual_plotting = TRUE, 
           manual_plot_name = "m4",
           decimals = 5)

##Table of coefficients---------------------------------------------------------------
#Need to transpose remerge coefs_ordered_m with neatened names and column bind
coef_table_m1 <- coefs_m1 %>%
  mutate(coef = estimate) %>%
  dplyr::select(variable, coef) %>%
  rename(`Model Feature` = variable, 
         `Model 1 Beta coefficient (95% CI)` = coef
         )

coef_table_m2 <- coefs_m2 %>%
  mutate(coef = estimate) %>%
  dplyr::select(variable, coef) %>%
  rename(`Model Feature` = variable, 
         `Model 2 Beta coefficient (95% CI)` = coef)

coef_table_m3 <- coefs_m3 %>%
  mutate(coef = estimate) %>%
  dplyr::select(variable, coef) %>%
  rename(`Model Feature` = variable, 
         `Model 3 Beta coefficient (95% CI)` = coef)
coef_table_m4 <- coefs_m4 %>%
  mutate(coef = estimate) %>%
  dplyr::select(variable, coef) %>%
  rename(`Model Feature` = variable, 
         `Model 4 Beta coefficient (95% CI)` = coef)

model_coef_table <- left_join(coef_table_m3, coef_table_m2)
model_coef_table <- left_join(model_coef_table, coef_table_m1)
model_coef_table <- left_join(model_coef_table, coef_table_m4)

write_xlsx(model_coef_table,"tables/web_app_model_coefs.xlsx")
