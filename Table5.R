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

##Make complete case datasets ----------------------------------------------------------
###StartRight ----------------------------------------------------------------------------
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


#Test
SR_m12_data <- SR_m12_data %>%
  mutate(
    agediag = factor(case_when(
      AgeatDiagnosis <37 ~ "<37",
      AgeatDiagnosis <40 ~ "37-40",
      AgeatDiagnosis <=50 ~ "40-50"
    )),
    bmi_cat = factor(case_when(
      bmi_model < 25 ~ "<25",
      bmi_model <= 31 ~ "25-31",
      bmi_model > 31 ~ ">31"
    )),
    hba1c_diag = factor(ifelse(
      HbA1c_at_diagnosis_v1 < 58,"<58",
      " >=58"
    ))
  )
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
m1 <- glm(SRoutcome ~ relevel(agediag, ref = "40-50") +
            relevel(bmi_cat, ref = ">31") +
            relevel(hba1c_diag, ref = "<58") +
            #scale(c_peptide_v1) +
            relevel(factor(Gender_v1), ref = "Male") +
            osmotic +
            autoimmune +
            Unintentional_weight_loss_v1 +
            DKA +
            relevel(factor(famhisnoninsdiab),ref = "Yes"),
          #famhisauto,
          data = SR_m12_data,
          family = binomial)
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
         `B coefficient` = coef,
         `OR` = OR, 
         `p value` = p_value
  )
write_xlsx(coef_table_m1,"tables/Table5.xlsx")
