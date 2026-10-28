##########################################################################################
#CPRD code
##########################################################################################
#setwd("C:/Users/ky279/OneDrive - University of Exeter/CPRD/2025/Julieanne progression/Final/") ##########################################################################################

# Get data from MySQL
library(tidyverse)
library(aurum)

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")
analysis <- cprd$analysis("jk_at_diag")
data <- data %>% analysis$cached("cohort_v2") %>% collect()

# Format and rename variables
data <- data %>%
  mutate(Gender_v1=ifelse(gender==1, "Male", "Female"),
         DKA=dka_at_diag,
         Unintentional_weight_loss_v1=ifelse(weight_loss_at_diag==1, "Yes", "No"),
         osmotic=osmotic_symptoms_at_diag,
         autoimmune=ifelse(coeliac==1 | vitiligo==1 | addisons==1 | graves==1 | hashimotos==1 | pernicious_anaemia==1 | rheumatoidarthritis==1, 1, 0),
         famhisdiab=ifelse(is.na(fh_diabetes) | fh_diabetes==0, "No", "Yes"),
         SRoutcome=ifelse(diabetes_type=="type 1", 1, 0),
         Eth_5cat = ethnicity_5cat) %>%
  rename(AgeatDiagnosis=dm_diag_age,
         bmi_model=prebmi,
         HbA1c_at_diagnosis_v1=prehba1c) %>%
  dplyr::select(Gender_v1, DKA, Unintentional_weight_loss_v1, osmotic, autoimmune,
         famhisdiab, SRoutcome, AgeatDiagnosis, bmi_model, HbA1c_at_diagnosis_v1,
         dka, t1_code, basal_bolus,Eth_5cat)


# Check no missingness except for ethnicity and IMD

data %>%
  filter(is.na(Gender_v1)
         | is.na(DKA)
         | is.na(Unintentional_weight_loss_v1)
         | is.na(osmotic)
         | is.na(autoimmune)
         | is.na(famhisdiab)
         | is.na(SRoutcome)
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
#source("functions/model_info1.R")
#source("functions/var_characteristics_1.R")
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
## Model 2 -----------------------------------------------------------------------------------
m2_beta <- c(
  "(Intercept)" = 5.65739369171834,
  "AgeatDiagnosis" = -0.0761310235937549,
  "bmi_model" = -0.205897904461199,
  "HbA1c_at_diagnosis_v1" = 0.00311485068438993,
  "Gender_v1Male" = -0.770769399543392,
  "osmotic1" = 0.897232400482147,
  "autoimmune1" = 0.670223920517065,
  "Unintentional_weight_loss_v1Yes" = 1.20670197330313,
  "DKA1" = 0.645909312453152,
  'Eth_4catBlack' = -0.786936569308031,
  'Eth_4catOther/Mixed' = -0.60231161939665,
  'Eth_4catSouth Asian' = -0.362368506943113,
  "famhisnoninsdiabYes" = -0.640773160200783,
  "num_anti1" = 1.96631270605778,
  "num_anti2" = 2.54455916410863,
  "num_anti3" = 3.46277643855415
)
m2_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  num_anti
## Model 3 -----------------------------------------------------------------------------------
m3_beta <- c(
  "(Intercept)" = 5.59111707576748,
  "AgeatDiagnosis" = -0.0658265444495303,
  "bmi_model" = -0.215588994357022,
  "HbA1c_at_diagnosis_v1" = 0.0010205665084689,
  "Gender_v1Male" = -0.769413515416535,
  "osmotic1" = 0.961696294455552,
  "autoimmune1" = 0.718466592548281,
  "Unintentional_weight_loss_v1Yes" = 0.973882805795106,
  "DKA1" = 0.450769465099517,
  'Eth_4catBlack' = -0.0448130235508751,
  'Eth_4catOther/Mixed' = -1.19595487818422,
  'Eth_4catSouth Asian' = -0.508601030699211,
  "famhisnoninsdiabYes" = -0.649989577718691,
  "num_anti1" = 1.69269869149969,
  "num_anti2" = 2.1106207424025,
  "num_anti3" = 2.9710916181995,
  "T1DGRS2_z" = 0.588101930916182
)
m3_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  num_anti +
  T1DGRS2_z

## Model 4 -----------------------------------------------------------------------------------
m4_beta <- c(
  "(Intercept)" = 7.68913552392495,
  "AgeatDiagnosis" = -0.0792922627586783,
  "bmi_model" = -0.245795596269301,
  "HbA1c_at_diagnosis_v1" = 0.00135050843483768,
  "Gender_v1Male" = -0.745200775272355,
  "osmotic1" = 1.01679460706228,
  "autoimmune1" = 0.94971768216283,
  "Unintentional_weight_loss_v1Yes" = 1.05073780432584,
  "DKA1" = 0.626511986416986,
  'Eth_4catBlack' = -0.13926992184133,
  'Eth_4catOther/Mixed' = -1.67150151029812,
  'Eth_4catSouth Asian' = -0.76869417190735,
  "famhisnoninsdiabYes" = -1.07704095684858,
  "T1DGRS2_z" = 0.922151368226217
)
m4_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  T1DGRS2_z

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
  mutate(
    famhisnoninsdiab = ifelse(is.na(famhisdiab), "No", famhisdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("3", "4"), "Other/Mixed",
                      ifelse(Eth_5cat == "0", "White",
                             ifelse(Eth_5cat == "2", "Black",
                                    ifelse(Eth_5cat == "1", "South Asian", Eth_5cat))))
  )
data$SRoutcome <- as.numeric(data$SRoutcome)
data$DKA <- as.character(data$DKA)
data$osmotic <- as.character(data$osmotic)
data$autoimmune <- as.character(data$autoimmune)


##StartRight
data <- data %>%
  drop_na(all_of(all_vars_12))
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

data <- data %>%
  mutate(
    #pred_prob_m1 = predict(m1, data, type = "response"),
    prob = case_when(
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
    prob2 = case_when(
      #pred_prob_m1 < 0.1 ~ "<0.1",
      pred_prob_m1 < 0.2 ~ "0.0-0.2",
      #pred_prob_m1 < 0.3 ~ "0.2-0.3",
      pred_prob_m1 < 0.4 ~ "0.2-0.4",
      #pred_prob_m1 < 0.5 ~ "0.4-0.5",
      pred_prob_m1 < 0.6 ~ "0.4-0.6",
      #pred_prob_m1 < 0.7 ~ "0.6-0.7",
      pred_prob_m1 < 0.8 ~ "0.6-0.8",
      #pred_prob_m1 < 0.9 ~ "0.8-0.9",
      pred_prob_m1 >= 0.9 ~ ">=0.8"),
    prob3 = case_when(
      pred_prob_m1 < 0.1 ~ "<0.1",
      pred_prob_m1 < 0.2 ~ "0.1-0.2",
      pred_prob_m1 < 0.3 ~ "0.2-0.3",
      pred_prob_m1 < 0.4 ~ "0.3-0.4",
      pred_prob_m1 < 0.5 ~ "0.4-0.5",
      #pred_prob_m1 < 0.6 ~ "0.5-0.6",
      #pred_prob_m1 < 0.7 ~ "0.6-0.7",
      #pred_prob_m1 < 0.8 ~ "0.7-0.8",
      #pred_prob_m1 < 0.9 ~ "0.8-0.9",
      pred_prob_m1 >= 0.5 ~ ">=0.5"),
    prob4 = case_when(
      pred_prob_m1 < 0.1 ~ "<0.1",
      pred_prob_m1 < 0.2 ~ "0.1-0.2",
      pred_prob_m1 < 0.3 ~ "0.2-0.3",
      pred_prob_m1 < 0.4 ~ "0.3-0.4",
      #pred_prob_m1 < 0.5 ~ "0.4-0.5",
      #pred_prob_m1 < 0.6 ~ "0.5-0.6",
      #pred_prob_m1 < 0.7 ~ "0.6-0.7",
      #pred_prob_m1 < 0.8 ~ "0.7-0.8",
      #pred_prob_m1 < 0.9 ~ "0.8-0.9",
      pred_prob_m1 >= 0.4 ~ ">=0.4"),
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
  dplyr::select(SRoutcome,  prob, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, DKA, pardm,dka, t1_code,basal_bolus) %>%
  mutate(ScoreD = dplyr::select(., agedx:pardm) %>% rowSums(na.rm = TRUE))

varlist_cat <- c(
  "SRoutcome",
  "dka",
  "t1_code",
  "basal_bolus"
)
for(v in varlist_cat) {
  ci <- data %>%
    freq_table(prob, !!sym(v)) %>%
    filter(col_cat == "1") %>%
    dplyr::select(row_cat, n_row, col_cat, percent_row, lcl_row, ucl_row) %>%
    rename(
      prob = row_cat
    ) %>%
    mutate(
      prob_label = paste0(prob, "\n (n=", n_row,")")
    )
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}
summary(factor(SRoutcome_ci$prob_label))

CPRD_decile_checks <- data %>%
  group_by(prob) %>%
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
b_7to80 <- prop.test(x=21, n=21, conf.level = 0.95, correct = FALSE)
SRoutcome_ci <- SRoutcome_ci %>%
  mutate(lcl_row = ifelse(prob == ">=0.9",
          b_7to80$conf.int[1]*100,
          lcl_row),
         ucl_row = ifelse(prob == ">=0.9",
                          b_7to80$conf.int[2]*100,
                          ucl_row))
plotA <- ggplot(SRoutcome_ci,
                aes(x = factor(prob_label,
                               levels = c("<0.1\n (n=82203)",
                                          "0.1-0.2\n (n=1115)",
                                          "0.2-0.3\n (n=377)",
                                          "0.3-0.4\n (n=160)",
                                          "0.4-0.5\n (n=97)",
                                          "0.5-0.6\n (n=77)",
                                          "0.6-0.7\n (n=58)",
                                          "0.7-0.8\n (n=45)",
                                          "0.8-0.9\n (n=41)",
                                          ">=0.9\n (n=21)"),
                               labels = c("0-9\n (n=82,203)",
                                          "10-19\n (n=1,115)",
                                          "20-29\n (n=377)",
                                          "30-39\n (n=160)",
                                          "40-49\n (n=97)",
                                          "50-59\n (n=77)",
                                          "60-69\n (n=58)",
                                          "70-79\n (n=45)",
                                          "80-89\n (n=41)",
                                          "90-100\n (n=21)")
                               ),
                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  #geom_text(aes(y = 0, label = n_row), vjust = 1) +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Progression to insulin within three years") +
  theme(text = element_text(size = 20))
#B) DKA ever
plotB <- ggplot(dka_ci, aes(x = factor(prob_label,
                                       levels = c("<0.1\n (n=82203)",
                                                  "0.1-0.2\n (n=1115)",
                                                  "0.2-0.3\n (n=377)",
                                                  "0.3-0.4\n (n=160)",
                                                  "0.4-0.5\n (n=97)",
                                                  "0.5-0.6\n (n=77)",
                                                  "0.6-0.7\n (n=58)",
                                                  "0.7-0.8\n (n=45)",
                                                  "0.8-0.9\n (n=41)",
                                                  ">=0.9\n (n=21)"),
                                       labels = c("0-9\n (n=82,203)",
                                                  "10-19\n (n=1,115)",
                                                  "20-29\n (n=377)",
                                                  "30-39\n (n=160)",
                                                  "40-49\n (n=97)",
                                                  "50-59\n (n=77)",
                                                  "60-69\n (n=58)",
                                                  "70-79\n (n=45)",
                                                  "80-89\n (n=41)",
                                                  "90-100\n (n=21)")),
                                        y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_bw() +
  #geom_text(aes(y = 0, label = n_row), vjust = 1) +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Diabetes ketoacidosis") +
  theme(text = element_text(size = 20))
#C) T1D code
plotC <- ggplot(t1_code_ci, aes(x = factor(prob_label,
                                           levels = c("<0.1\n (n=82203)",
                                                      "0.1-0.2\n (n=1115)",
                                                      "0.2-0.3\n (n=377)",
                                                      "0.3-0.4\n (n=160)",
                                                      "0.4-0.5\n (n=97)",
                                                      "0.5-0.6\n (n=77)",
                                                      "0.6-0.7\n (n=58)",
                                                      "0.7-0.8\n (n=45)",
                                                      "0.8-0.9\n (n=41)",
                                                      ">=0.9\n (n=21)"),
                                           labels = c("0-9\n (n=82,203)",
                                                      "10-19\n (n=1,115)",
                                                      "20-29\n (n=377)",
                                                      "30-39\n (n=160)",
                                                      "40-49\n (n=97)",
                                                      "50-59\n (n=77)",
                                                      "60-69\n (n=58)",
                                                      "70-79\n (n=45)",
                                                      "80-89\n (n=41)",
                                                      "90-100\n (n=21)")),
                                        y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  #geom_text(aes(label = n_row),  vjust = -1) +
  ylim(0,100) +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Type 1 diabetes code") +
  theme(text = element_text(size = 20))
#D) Basal bolus
plotD <- ggplot(basal_bolus_ci, aes(x = factor(prob_label,
                                               levels = c("<0.1\n (n=82203)",
                                                          "0.1-0.2\n (n=1115)",
                                                          "0.2-0.3\n (n=377)",
                                                          "0.3-0.4\n (n=160)",
                                                          "0.4-0.5\n (n=97)",
                                                          "0.5-0.6\n (n=77)",
                                                          "0.6-0.7\n (n=58)",
                                                          "0.7-0.8\n (n=45)",
                                                          "0.8-0.9\n (n=41)",
                                                          ">=0.9\n (n=21)"),
                                               labels = c("0-9\n (n=82,203)",
                                                          "10-19\n (n=1,115)",
                                                          "20-29\n (n=377)",
                                                          "30-39\n (n=160)",
                                                          "40-49\n (n=97)",
                                                          "50-59\n (n=77)",
                                                          "60-69\n (n=58)",
                                                          "70-79\n (n=45)",
                                                          "80-89\n (n=41)",
                                                          "90-100\n (n=21)")),
                                        y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw() +
  #geom_text(aes(y = 0, label = n_row), vjust = 1) +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "% Basal bolus insulin at latest follow-up") +
  theme(text = element_text(size = 20))

pdf("figures/Figure4.pdf", height = 16, width = 26)
joint_plot <- patchwork::wrap_plots(
  plotA,
  plotB,
  plotC,
  plotD,
  ncol = 2, nrow = 2
)+ patchwork::plot_annotation(tag_levels = list(c("A", "B", "C", "D")))
print(joint_plot)
dev.off()
#ggsave("figures/Figure4.jpeg", joint_plot, height = 16, width = 26)


pdf("figures/Figure4A.pdf", width = 13, height = 8)
plotA
dev.off()
pdf("figures/Figure4B.pdf", width = 13, height = 8)
plotB
dev.off()
pdf("figures/Figure4C.pdf", width = 13, height = 8)
plotC
dev.off()
pdf("figures/Figure4D.pdf", width = 13, height = 8)
plotD
dev.off()
