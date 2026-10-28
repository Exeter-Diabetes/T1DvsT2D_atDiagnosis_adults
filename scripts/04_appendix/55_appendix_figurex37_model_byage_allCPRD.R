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
         Eth_5cat = ethnicity_5cat
         #SRoutcome=ifelse(diabetes_type=="type 1", 1, 0)
  ) %>%
  rename(AgeatDiagnosis=dm_diag_age,
         bmi_model=prebmi,
         HbA1c_at_diagnosis_v1=prehba1c) %>%
  dplyr::select(Gender_v1, DKA, Unintentional_weight_loss_v1, osmotic, autoimmune,
                famhisdiab, ins_3yrs, follow_up_3yrs, AgeatDiagnosis, bmi_model, HbA1c_at_diagnosis_v1,
                dka, t1_code, basal_bolus, prerandomglucose,Eth_5cat)


# Check no missingness except for ethnicity and IMD

data %>%
  filter(is.na(Gender_v1)
         | is.na(DKA)
         | is.na(Unintentional_weight_loss_v1)
         | is.na(osmotic)
         | is.na(autoimmune)
         | is.na(famhisdiab)
         | is.na(AgeatDiagnosis)
         | is.na(bmi_model)
         | is.na(HbA1c_at_diagnosis_v1)) %>%
  count()
#0


##############################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(freqtables)
library(gdata)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
#Load data -------------------------------------------------------------------------
#load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
#load("m1.RData")

# Model parameters and formulas -----------------------------------------------------------
## Model 1 -----------------------------------------------------------------------------------
m1_beta <- c(
  "(Intercept)" = 8.64624460772943,
  "AgeatDiagnosis" = -0.0964441683502511,
  "bmi_model" = -0.243555720621392,
  "HbA1c_at_diagnosis_v1" = 0.00341120554236777,
  "Gender_v1Male" = -0.863116972338412,
  "osmotic1" = 0.961134759411358,
  "autoimmune1" = 0.823010855901484,
  "Unintentional_weight_loss_v1Yes" = 1.35018265494132,
  "DKA1" = 0.734312980329503,
  'Eth_4catBlack' = -1.32530674465559,
  'Eth_4catOther/Mixed' = -1.34567865383264,
  'Eth_4catSouth Asian' = -0.793896878252353,
  "famhisnoninsdiabYes" = -1.01442801038726
)
m1_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab
# User-defined function -------------------------------------------------------------
prepare_prediction_data <- function(dat){

  dat %>%
    mutate(
      Eth_4cat = factor(
        Eth_4cat,
        levels = c(
          "White",
          "Black",
          "Other/Mixed",
          "South Asian"
        )
      ),
      Gender_v1 = factor(
        Gender_v1,
        levels = c("Female", "Male")
      )
    )
}

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
  "famhisdiab"#,
  #"famhisauto",
  #"num_anti"
)
all_vars_12 <- c(varlist_12, varlist_cat_12)

##Make complete case datasets ----------------------------------------------------------
data <- data %>%
  drop_na(all_of(all_vars_12)) %>%
  mutate(
    famhisnoninsdiab = ifelse(is.na(famhisdiab), "No", famhisdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("3", "4"), "Other/Mixed",
                      ifelse(Eth_5cat == "0", "White",
                             ifelse(Eth_5cat == "2", "Black",
                                    ifelse(Eth_5cat == "1", "South Asian", Eth_5cat)))))
#data$SRoutcome <- as.numeric(data$SRoutcome)
data$DKA <- as.character(data$DKA)
data$osmotic <- as.character(data$osmotic)
data$autoimmune <- as.character(data$autoimmune)

###Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
# model_info(model = m1,
#            test_data = data,
#            outcome = "SRoutcome",
#            saving_name = "model",
#            manual_plotting = TRUE,
#            manual_plot_name = "m1")

#pred_prob_m1 <- predict(m1, data, type = "response")
# Get model information for plotting ---------------------------------------------
#Predicted probabilities -----------------------------------------------------------------
##StartRight --------------------------------------------------------------------------
###Model 1
data <- prepare_prediction_data(data)
X <- model.matrix(
  m1_formula,
  data = data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

data <- data %>%
  mutate(pred_prob_m1 = plogis(linpred))
##StartRight
data <- data %>%
  mutate(prob = case_when(
    pred_prob_m1 < 0.1 ~ "<0.1",
    pred_prob_m1 < 0.2 ~ "0.1-0.2",
    pred_prob_m1 < 0.3 ~ "0.2-0.3",
    pred_prob_m1 < 0.4 ~ "0.3-0.4",
    pred_prob_m1 < 0.5 ~ "0.4-0.5",
    pred_prob_m1 < 0.6 ~ "0.5-0.6",
    pred_prob_m1 < 0.7 ~ "0.6-0.7",
    pred_prob_m1 < 0.8 ~ "0.7-0.8",
    pred_prob_m1 < 0.9 ~ "0.8-0.9",
    pred_prob_m1 >= 0.9 ~ ">=0.9"),
    prob3 = case_when(
      pred_prob_m1 < 0.1 ~ "<0.1",
      pred_prob_m1 < 0.2 ~ "0.1-0.2",
      pred_prob_m1 < 0.3 ~ "0.2-0.3",
      pred_prob_m1 < 0.4 ~ "0.3-0.4",
      pred_prob_m1 < 0.5 ~ "0.4-0.5",
      pred_prob_m1 >= 0.5 ~ ">=0.5"),
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
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0)
  )

data <- data %>%
  dplyr::select(prob, prob3, AgeatDiagnosis, agedx, bmi, hba1c, sex,
                osmo, auto, weightloss, DKA, eth, pardm,dka, ins_3yrs, t1_code,basal_bolus, Eth_4cat) %>%
  mutate(ScoreD = dplyr::select(., agedx:pardm) %>% rowSums(na.rm = TRUE),
         SRoutcome = ins_3yrs)

#Split by age
data18_50 <- data %>%
  filter(AgeatDiagnosis <51)

data50plus <- data %>%
  filter(AgeatDiagnosis >=51)

varlist_cat <- c(
  "SRoutcome",
  "dka",
  "t1_code",
  "basal_bolus"
)

#18-50s ----------------------------------------------------------------------------------
for(v in varlist_cat) {
  ci <- data18_50 %>%
    freq_table(prob, !!sym(v)) %>%
    filter(col_cat == "1") %>%
    dplyr::select(row_cat, n_row, col_cat, percent_row, lcl_row, ucl_row) %>%
    rename(
      prob = row_cat
    ) %>%
    mutate(
      prob_label = paste0(prob, "\n (n=", n_row,")"))
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}

basal_bolus_ci %>%
  dplyr::select(prob_label)
CPRD_decile_checks <- data18_50 %>%
  group_by(prob) %>%
  #group_by(prob3) %>%
  summarise(
    n=n(),
    nT1_outcome = sum(SRoutcome == 1),
    pT1_outcome = nT1_outcome/n*100,
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  )
CPRD_decile_checks
#Make plots
##With error bars ---------------------------------------------------------------------
#A) prog insulin
plotA1 <- ggplot(SRoutcome_ci,
                aes(x = factor(prob_label,
                               levels = c("<0.1\n (n=48454)",
                                          "0.1-0.2\n (n=2018)",
                                          "0.2-0.3\n (n=677)",
                                          "0.3-0.4\n (n=394)",
                                          "0.4-0.5\n (n=254)",
                                          "0.5-0.6\n (n=174)",
                                          "0.6-0.7\n (n=149)",
                                          "0.7-0.8\n (n=154)",
                                          "0.8-0.9\n (n=149)",
                                          ">=0.9\n (n=105)"),
                               labels = c("0-9\n (n=48,454)",
                                          "10-19\n (n=2,018)",
                                          "20-29\n (n=677)",
                                          "30-39\n (n=394)",
                                          "40-49\n (n=254)",
                                          "50-59\n (n=174)",
                                          "60-69\n (n=149)",
                                          "70-79\n (n=154)",
                                          "80-89\n (n=149)",
                                          "90-100\n (n=105)")),
                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Insulin treatment within three years",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#B) DKA ever
plotB1 <- ggplot(dka_ci, aes(x = factor(prob_label,
                                        levels = c("<0.1\n (n=48454)",
                                                   "0.1-0.2\n (n=2018)",
                                                   "0.2-0.3\n (n=677)",
                                                   "0.3-0.4\n (n=394)",
                                                   "0.4-0.5\n (n=254)",
                                                   "0.5-0.6\n (n=174)",
                                                   "0.6-0.7\n (n=149)",
                                                   "0.7-0.8\n (n=154)",
                                                   "0.8-0.9\n (n=149)",
                                                   ">=0.9\n (n=105)"),
                                        labels = c("0-9\n (n=48,454)",
                                                   "10-19\n (n=2,018)",
                                                   "20-29\n (n=677)",
                                                   "30-39\n (n=394)",
                                                   "40-49\n (n=254)",
                                                   "50-59\n (n=174)",
                                                   "60-69\n (n=149)",
                                                   "70-79\n (n=154)",
                                                   "80-89\n (n=149)",
                                                   "90-100\n (n=105)")),
                            y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_bw() +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Diabetes ketoacidosis ever",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#C) T1D code
plotC1 <- ggplot(t1_code_ci, aes(x = factor(prob_label,
                                            levels = c("<0.1\n (n=48454)",
                                                       "0.1-0.2\n (n=2018)",
                                                       "0.2-0.3\n (n=677)",
                                                       "0.3-0.4\n (n=394)",
                                                       "0.4-0.5\n (n=254)",
                                                       "0.5-0.6\n (n=174)",
                                                       "0.6-0.7\n (n=149)",
                                                       "0.7-0.8\n (n=154)",
                                                       "0.8-0.9\n (n=149)",
                                                       ">=0.9\n (n=105)"),
                                            labels = c("0-9\n (n=48,454)",
                                                       "10-19\n (n=2,018)",
                                                       "20-29\n (n=677)",
                                                       "30-39\n (n=394)",
                                                       "40-49\n (n=254)",
                                                       "50-59\n (n=174)",
                                                       "60-69\n (n=149)",
                                                       "70-79\n (n=154)",
                                                       "80-89\n (n=149)",
                                                       "90-100\n (n=105)")),
                                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(0,100) +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Type 1 diabetes code ever",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#D) Basal bolus
plotD1 <- ggplot(basal_bolus_ci, aes(x = factor(prob_label,
                                                levels = c("<0.1\n (n=48454)",
                                                           "0.1-0.2\n (n=2018)",
                                                           "0.2-0.3\n (n=677)",
                                                           "0.3-0.4\n (n=394)",
                                                           "0.4-0.5\n (n=254)",
                                                           "0.5-0.6\n (n=174)",
                                                           "0.6-0.7\n (n=149)",
                                                           "0.7-0.8\n (n=154)",
                                                           "0.8-0.9\n (n=149)",
                                                           ">=0.9\n (n=105)"),
                                                labels = c("0-9\n (n=48,454)",
                                                           "10-19\n (n=2,018)",
                                                           "20-29\n (n=677)",
                                                           "30-39\n (n=394)",
                                                           "40-49\n (n=254)",
                                                           "50-59\n (n=174)",
                                                           "60-69\n (n=149)",
                                                           "70-79\n (n=154)",
                                                           "80-89\n (n=149)",
                                                           "90-100\n (n=105)")),
                                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw() +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Basal bolus insulin at latest follow-up",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))

#>50s ----------------------------------------------------------------------------------
for(v in varlist_cat) {
  ci <- data50plus %>%
    freq_table(prob3, !!sym(v)) %>%
    #freq_table(prob, !!sym(v)) %>%
    filter(col_cat == "1") %>%
    dplyr::select(row_cat, n_row, col_cat, percent_row, lcl_row, ucl_row) %>%
    rename(
      #prob = row_cat
      prob3 = row_cat
    ) %>%
    mutate(
      prob_label = paste0(prob3, "\n (n=", n_row,")")
      #prob_label = paste0(prob, "\n (n=", n_row,")")
    )
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}
summary(factor(SRoutcome_ci$prob_label))

CPRD_decile_checks <- data50plus %>%
  #group_by(prob) %>%
  group_by(prob3) %>%
  summarise(
    n=n(),
    nT1_outcome = sum(SRoutcome == 1),
    pT1_outcome = nT1_outcome/n*100,
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  )
CPRD_decile_checks
#Make plots
##With error bars ---------------------------------------------------------------------
#A) prog insulin
plotA2 <- ggplot(SRoutcome_ci,
                 aes(x = factor(prob_label,
                                levels = c("<0.1\n (n=134752)",
                                           "0.1-0.2\n (n=628)",
                                           "0.2-0.3\n (n=196)",
                                           "0.3-0.4\n (n=63)",
                                           "0.4-0.5\n (n=29)",
                                           # "0.5-0.6\n (n=1388)",
                                           # "0.6-0.7\n (n=821)",
                                           # "0.7-0.8\n (n=523)",
                                           # "0.8-0.9\n (n=328)",
                                           ">=0.5\n (n=36)"),
                                labels = c("0-9\n (n=134,752)",
                                           "10-19\n (n=628)",
                                           "20-29\n (n=196)",
                                           "30-39\n (n=63)",
                                           "40-49\n (n=29)",
                                           # "50-59\n (n=1,388)",
                                           # "60-69\n (n=821)",
                                           # "70-79\n (n=523)",
                                           # "80-89\n (n=328)",
                                           "50-100\n (n=36)")),
                     y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(0,100) +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Insulin treatment within three years",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#B) DKA ever
b_3to40 <- prop.test(x=0, n=63, conf.level = 0.95, correct = FALSE)
`dka_ci` <- `dka_ci` %>%
  add_row(prob3 = "0.3-0.4",
          prob_label = "0.3-0.4\n (n=63)",
          percent_row = 0,
          lcl_row = b_3to40$conf.int[1]*100,
          ucl_row = b_3to40$conf.int[2]*100)
plotB2 <- ggplot(dka_ci, aes(x = factor(prob_label,
                                        levels = c("<0.1\n (n=134752)",
                                                   "0.1-0.2\n (n=628)",
                                                   "0.2-0.3\n (n=196)",
                                                   "0.3-0.4\n (n=63)",
                                                   "0.4-0.5\n (n=29)",
                                                   # "0.5-0.6\n (n=1388)",
                                                   # "0.6-0.7\n (n=821)",
                                                   # "0.7-0.8\n (n=523)",
                                                   # "0.8-0.9\n (n=328)",
                                                   ">=0.5\n (n=36)"),
                                        labels = c("0-9\n (n=134,752)",
                                                   "10-19\n (n=628)",
                                                   "20-29\n (n=196)",
                                                   "30-39\n (n=63)",
                                                   "40-49\n (n=29)",
                                                   # "50-59\n (n=1,388)",
                                                   # "60-69\n (n=821)",
                                                   # "70-79\n (n=523)",
                                                   # "80-89\n (n=328)",
                                                   "50-100\n (n=36)")),
                             y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_bw() +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Diabetes ketoacidosis ever",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#C) T1D code
plotC2 <- ggplot(t1_code_ci, aes(x = factor(prob_label,
                                            levels = c("<0.1\n (n=134752)",
                                                       "0.1-0.2\n (n=628)",
                                                       "0.2-0.3\n (n=196)",
                                                       "0.3-0.4\n (n=63)",
                                                       "0.4-0.5\n (n=29)",
                                                       # "0.5-0.6\n (n=1388)",
                                                       # "0.6-0.7\n (n=821)",
                                                       # "0.7-0.8\n (n=523)",
                                                       # "0.8-0.9\n (n=328)",
                                                       ">=0.5\n (n=36)"),
                                            labels = c("0-9\n (n=134,752)",
                                                       "10-19\n (n=628)",
                                                       "20-29\n (n=196)",
                                                       "30-39\n (n=63)",
                                                       "40-49\n (n=29)",
                                                       # "50-59\n (n=1,388)",
                                                       # "60-69\n (n=821)",
                                                       # "70-79\n (n=523)",
                                                       # "80-89\n (n=328)",
                                                       "50-100\n (n=36)")),
                                 y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(0,100) +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Type 1 diabetes code ever",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#D) Basal bolus
plotD2 <- ggplot(basal_bolus_ci, aes(x = factor(prob_label,
                                                levels = c("<0.1\n (n=134752)",
                                                           "0.1-0.2\n (n=628)",
                                                           "0.2-0.3\n (n=196)",
                                                           "0.3-0.4\n (n=63)",
                                                           "0.4-0.5\n (n=29)",
                                                           # "0.5-0.6\n (n=1388)",
                                                           # "0.6-0.7\n (n=821)",
                                                           # "0.7-0.8\n (n=523)",
                                                           # "0.8-0.9\n (n=328)",
                                                           ">=0.5\n (n=36)"),
                                                labels = c("0-9\n (n=134,752)",
                                                           "10-19\n (n=628)",
                                                           "20-29\n (n=196)",
                                                           "30-39\n (n=63)",
                                                           "40-49\n (n=29)",
                                                           # "50-59\n (n=1,388)",
                                                           # "60-69\n (n=821)",
                                                           # "70-79\n (n=523)",
                                                           # "80-89\n (n=328)",
                                                           "50-100\n (n=36)")),
                                     y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw() +
  ylim(0,100) +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Basal bolus insulin at latest follow-up",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))


pdf("figures/Supp_Figurex35.pdf", height = 16, width = 24)
joint_plot <- patchwork::wrap_plots(
  plotA1,
  plotB1,
  plotA2,
  plotB2,
  plotC1,
  plotD1,
  plotC2,
  plotD2,
  ncol = 2, nrow = 4
)+ patchwork::plot_annotation(tag_levels = list(c("A1", "B1", "A2", "B2","C1", "D1", "C2", "D2")))
print(joint_plot)
dev.off()
ggsave("figures/Supp_Figurex35.jpeg", joint_plot, height = 16, width = 26)


