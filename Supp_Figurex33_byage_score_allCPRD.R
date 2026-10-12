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

pred_prob_m1 <- predict(m1, data, type = "response")

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
  dplyr::select(prob, AgeatDiagnosis, agedx, bmi, hba1c, sex,
                osmo, auto, weightloss, DKA, eth, pardm,dka, ins_3yrs, t1_code,basal_bolus, Eth_4cat) %>%
  mutate(ScoreD = dplyr::select(., agedx:pardm) %>% rowSums(na.rm = TRUE),
         SRoutcome = ins_3yrs)

data <- data %>%
  mutate(
    score_cat = ifelse(ScoreD >= 12, "12-15", ScoreD),
    score_cat2 = ifelse(ScoreD >=10, "10-15", ScoreD)
  )

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
#18-50-------------------------------------------------------------------------------------
for(v in varlist_cat) {
  ci <- data18_50 %>%
    freq_table(score_cat, !!sym(v)) %>%
    filter(col_cat == "1") %>%
    dplyr::select(row_cat, n_row, col_cat, percent_row, lcl_row, ucl_row) %>%
    rename(
      score_cat = row_cat
    ) %>%
    mutate(
      score_label = paste0(score_cat, "\n (n=", n_row,")"))
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}

basal_bolus_ci %>%
  dplyr::select(score_label)
#A) prog insulin
plotA1 <- ggplot(SRoutcome_ci,
                aes(x = factor(score_label,
                               levels = c("-1\n (n=248)",
                                          "0\n (n=1908)",
                                          "1\n (n=5527)",
                                          "2\n (n=7965)",
                                          "3\n (n=9431)",
                                          "4\n (n=9823)",
                                          "5\n (n=8409)",
                                          "6\n (n=4919)",
                                          "7\n (n=2337)",
                                          "8\n (n=1038)",
                                          "9\n (n=505)",
                                          "10\n (n=243)",
                                          "11\n (n=121)",
                                          "12-15\n (n=54)"),
                               labels = c("-1\n (n=248)",
                                          "0\n (n=1,908)",
                                          "1\n (n=5,527)",
                                          "2\n (n=7,965)",
                                          "3\n (n=9,431)",
                                          "4\n (n=9,823)",
                                          "5\n (n=8,409)",
                                          "6\n (n=4,919)",
                                          "7\n (n=2,337)",
                                          "8\n (n=1,038)",
                                          "9\n (n=505)",
                                          "10\n (n=243)",
                                          "11\n (n=121)",
                                          "12-15\n (n=54)")),
                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Insulin treatment within three years",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#B) DKA ever
plotB1 <- ggplot(dka_ci, aes(x = factor(score_label,
                                        levels = c("-1\n (n=248)",
                                                   "0\n (n=1908)",
                                                   "1\n (n=5527)",
                                                   "2\n (n=7965)",
                                                   "3\n (n=9431)",
                                                   "4\n (n=9823)",
                                                   "5\n (n=8409)",
                                                   "6\n (n=4919)",
                                                   "7\n (n=2337)",
                                                   "8\n (n=1038)",
                                                   "9\n (n=505)",
                                                   "10\n (n=243)",
                                                   "11\n (n=121)",
                                                   "12-15\n (n=54)"),
                                        labels = c("-1\n (n=248)",
                                                   "0\n (n=1,908)",
                                                   "1\n (n=5,527)",
                                                   "2\n (n=7,965)",
                                                   "3\n (n=9,431)",
                                                   "4\n (n=9,823)",
                                                   "5\n (n=8,409)",
                                                   "6\n (n=4,919)",
                                                   "7\n (n=2,337)",
                                                   "8\n (n=1,038)",
                                                   "9\n (n=505)",
                                                   "10\n (n=243)",
                                                   "11\n (n=121)",
                                                   "12-15\n (n=54)")),
                            y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_bw() +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Diabetes ketoacidosis ever",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#C) T1D code
plotC1 <- ggplot(t1_code_ci, aes(x = factor(score_label,
                                            levels = c("-1\n (n=248)",
                                                       "0\n (n=1908)",
                                                       "1\n (n=5527)",
                                                       "2\n (n=7965)",
                                                       "3\n (n=9431)",
                                                       "4\n (n=9823)",
                                                       "5\n (n=8409)",
                                                       "6\n (n=4919)",
                                                       "7\n (n=2337)",
                                                       "8\n (n=1038)",
                                                       "9\n (n=505)",
                                                       "10\n (n=243)",
                                                       "11\n (n=121)",
                                                       "12-15\n (n=54)"),
                                            labels = c("-1\n (n=248)",
                                                       "0\n (n=1,908)",
                                                       "1\n (n=5,527)",
                                                       "2\n (n=7,965)",
                                                       "3\n (n=9,431)",
                                                       "4\n (n=9,823)",
                                                       "5\n (n=8,409)",
                                                       "6\n (n=4,919)",
                                                       "7\n (n=2,337)",
                                                       "8\n (n=1,038)",
                                                       "9\n (n=505)",
                                                       "10\n (n=243)",
                                                       "11\n (n=121)",
                                                       "12-15\n (n=54)")),
                                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(0,100) +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Type 1 diabetes code ever",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#D) Basal bolus
plotD1 <- ggplot(basal_bolus_ci, aes(x = factor(score_label,
                                                levels = c("-1\n (n=248)",
                                                           "0\n (n=1908)",
                                                           "1\n (n=5527)",
                                                           "2\n (n=7965)",
                                                           "3\n (n=9431)",
                                                           "4\n (n=9823)",
                                                           "5\n (n=8409)",
                                                           "6\n (n=4919)",
                                                           "7\n (n=2337)",
                                                           "8\n (n=1038)",
                                                           "9\n (n=505)",
                                                           "10\n (n=243)",
                                                           "11\n (n=121)",
                                                           "12-15\n (n=54)"),
                                                labels = c("-1\n (n=248)",
                                                           "0\n (n=1,908)",
                                                           "1\n (n=5,527)",
                                                           "2\n (n=7,965)",
                                                           "3\n (n=9,431)",
                                                           "4\n (n=9,823)",
                                                           "5\n (n=8,409)",
                                                           "6\n (n=4,919)",
                                                           "7\n (n=2,337)",
                                                           "8\n (n=1,038)",
                                                           "9\n (n=505)",
                                                           "10\n (n=243)",
                                                           "11\n (n=121)",
                                                           "12-15\n (n=54)")),
                                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw() +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Basal bolus insulin at latest follow-up",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))

#>50-------------------------------------------------------------------------------------
for(v in varlist_cat) {
  ci <- data50plus %>%
    freq_table(score_cat2, !!sym(v)) %>%
    filter(col_cat == "1") %>%
    dplyr::select(row_cat, n_row, col_cat, percent_row, lcl_row, ucl_row) %>%
    rename(
      score_cat2 = row_cat
    ) %>%
    mutate(
      score_label = paste0(score_cat2, "\n (n=", n_row,")"))
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}

basal_bolus_ci %>%
  dplyr::select(score_label)
SRoutcome_ci %>%
  dplyr::select(score_label)
#A) prog insulin
plotA2 <- ggplot(SRoutcome_ci,
                aes(x = factor(score_label,
                               levels = c("-1\n (n=346)",
                                          "0\n (n=4289)",
                                          "1\n (n=16081)",
                                          "2\n (n=16716)",
                                          "3\n (n=17127)",
                                          "4\n (n=34208)",
                                          "5\n (n=27051)",
                                          "6\n (n=12385)",
                                          "7\n (n=5625)",
                                          "8\n (n=1601)",
                                          "9\n (n=249)",
                                          #"10\n (n=267)",
                                          #"11\n (n=123)",
                                          "10-15\n (n=26)"),
                               labels = c("-1\n (n=346)",
                                          "0\n (n=4,289)",
                                          "1\n (n=16,081)",
                                          "2\n (n=16,716)",
                                          "3\n (n=17,127)",
                                          "4\n (n=34,208)",
                                          "5\n (n=27,051)",
                                          "6\n (n=12,385)",
                                          "7\n (n=5,625)",
                                          "8\n (n=1,601)",
                                          "9\n (n=249)",
                                          #"10\n (n=267)",
                                          #"11\n (n=123)",
                                          "10-15\n (n=26)")),
                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Insulin treatment within three years",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#B) DKA ever
plotB2 <- ggplot(dka_ci, aes(x = factor(score_label,
                                        levels = c("-1\n (n=346)",
                                                   "0\n (n=4289)",
                                                   "1\n (n=16081)",
                                                   "2\n (n=16716)",
                                                   "3\n (n=17127)",
                                                   "4\n (n=34208)",
                                                   "5\n (n=27051)",
                                                   "6\n (n=12385)",
                                                   "7\n (n=5625)",
                                                   "8\n (n=1601)",
                                                   "9\n (n=249)",
                                                   #"10\n (n=267)",
                                                   #"11\n (n=123)",
                                                   "10-15\n (n=26)"),
                                        labels = c("-1\n (n=346)",
                                                   "0\n (n=4,289)",
                                                   "1\n (n=16,081)",
                                                   "2\n (n=16,716)",
                                                   "3\n (n=17,127)",
                                                   "4\n (n=34,208)",
                                                   "5\n (n=27,051)",
                                                   "6\n (n=12,385)",
                                                   "7\n (n=5,625)",
                                                   "8\n (n=1,601)",
                                                   "9\n (n=249)",
                                                   #"10\n (n=267)",
                                                   #"11\n (n=123)",
                                                   "10-15\n (n=26)")),
                            y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_bw() +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Diabetes ketoacidosis ever",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#C) T1D code
plotC2 <- ggplot(t1_code_ci, aes(x = factor(score_label,
                                            levels = c("-1\n (n=346)",
                                                       "0\n (n=4289)",
                                                       "1\n (n=16081)",
                                                       "2\n (n=16716)",
                                                       "3\n (n=17127)",
                                                       "4\n (n=34208)",
                                                       "5\n (n=27051)",
                                                       "6\n (n=12385)",
                                                       "7\n (n=5625)",
                                                       "8\n (n=1601)",
                                                       "9\n (n=249)",
                                                       #"10\n (n=267)",
                                                       #"11\n (n=123)",
                                                       "10-15\n (n=26)"),
                                            labels = c("-1\n (n=346)",
                                                       "0\n (n=4,289)",
                                                       "1\n (n=16,081)",
                                                       "2\n (n=16,716)",
                                                       "3\n (n=17,127)",
                                                       "4\n (n=34,208)",
                                                       "5\n (n=27,051)",
                                                       "6\n (n=12,385)",
                                                       "7\n (n=5,625)",
                                                       "8\n (n=1,601)",
                                                       "9\n (n=249)",
                                                       #"10\n (n=267)",
                                                       #"11\n (n=123)",
                                                       "10-15\n (n=26)")),
                                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(0,100) +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Type 1 diabetes code ever",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#D) Basal bolus
plotD2 <- ggplot(basal_bolus_ci, aes(x = factor(score_label,
                                                levels = c("-1\n (n=346)",
                                                           "0\n (n=4289)",
                                                           "1\n (n=16081)",
                                                           "2\n (n=16716)",
                                                           "3\n (n=17127)",
                                                           "4\n (n=34208)",
                                                           "5\n (n=27051)",
                                                           "6\n (n=12385)",
                                                           "7\n (n=5625)",
                                                           "8\n (n=1601)",
                                                           "9\n (n=249)",
                                                           #"10\n (n=267)",
                                                           #"11\n (n=123)",
                                                           "10-15\n (n=26)"),
                                                labels = c("-1\n (n=346)",
                                                           "0\n (n=4,289)",
                                                           "1\n (n=16,081)",
                                                           "2\n (n=16,716)",
                                                           "3\n (n=17,127)",
                                                           "4\n (n=34,208)",
                                                           "5\n (n=27,051)",
                                                           "6\n (n=12,385)",
                                                           "7\n (n=5,625)",
                                                           "8\n (n=1,601)",
                                                           "9\n (n=249)",
                                                           #"10\n (n=267)",
                                                           #"11\n (n=123)",
                                                           "10-15\n (n=26)")),
                                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw() +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Basal bolus insulin at latest follow-up",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))

pdf("figures/Supp_FigureX33.pdf", height = 16, width = 36)
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
#ggsave("figures/Figure4_Score_ALL_errorbars.png", joint_plot, height = 16, width = 24)
ggsave("figures/Supp_FigureX33.jpeg", joint_plot, height = 16, width = 36)
