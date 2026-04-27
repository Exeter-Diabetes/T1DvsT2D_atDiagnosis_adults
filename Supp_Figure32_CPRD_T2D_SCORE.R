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
         dka, t1_code,Eth_5cat, ethnicity_5cat, basal_bolus)


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
data <- data %>%
  mutate(
    pred_prob_m1 = predict(m1, data, type = "response"),
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
  dplyr::select(SRoutcome, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, DKA, eth, pardm,dka, t1_code,basal_bolus) %>%
  mutate(ScoreD = dplyr::select(., agedx:pardm) %>% rowSums(na.rm = TRUE))

varlist_cat <- c(
  "SRoutcome",
  "dka",
  "t1_code",
  "basal_bolus"
)

data <- data %>%
  mutate(
    score_cat = ifelse(ScoreD >= 11, "11-15", ScoreD)
  )
#Make plots
##Score categories With error bars ---------------------------------------------------------------------
for(v in varlist_cat) {
  ci <- data %>%
    freq_table(score_cat, !!sym(v)) %>%
    filter(col_cat == "1") %>%
    dplyr::select(row_cat, col_cat, n_row, percent_row, lcl_row, ucl_row) %>%
    rename(
      score_cat = row_cat
    )%>%
    mutate(
      ScoreD_label = paste0(score_cat, "\n (n=", n_row,")")
    )
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}
summary(factor(SRoutcome_ci$ScoreD_label))
#A) prog insulin
plotA <- ggplot(SRoutcome_ci,
                aes(x = factor(ScoreD_label,
                               levels = c(
                                 "-1\n (n=262)",
                                 "0\n (n=2773)",
                                "1\n (n=9858)",
                                "2\n (n=10966)",
                                "3\n (n=11972)",
                                "4\n (n=20389)",
                                "5\n (n=15818)",
                                "6\n (n=7442)",
                                "7\n (n=3237)",
                                "8\n (n=1107)",
                                "9\n (n=267)",
                                "10\n (n=66)",
                                "11-15\n (n=37)"),
                               labels = c(
                                 "-1\n (n=262)",
                                 "0\n (n=2,773)",
                                "1\n (n=9,858)",
                                "2\n (n=10,966)",
                                "3\n (n=11,972)",
                                "4\n (n=20,389)",
                                "5\n (n=15,818)",
                                "6\n (n=7,442)",
                                "7\n (n=3,237)",
                                "8\n (n=1,107)",
                                "9\n (n=267)",
                                "10\n (n=66)",
                                "11-15\n (n=37)")),
                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Progression to insulin within three years") +
  theme(text = element_text(size = 20))
#B) DKA ever
plotB <- ggplot(dka_ci, aes(x = factor(ScoreD_label,
                                       levels = c(
                                         "-1\n (n=262)",
                                         "0\n (n=2773)",
                                         "1\n (n=9858)",
                                         "2\n (n=10966)",
                                         "3\n (n=11972)",
                                         "4\n (n=20389)",
                                         "5\n (n=15818)",
                                         "6\n (n=7442)",
                                         "7\n (n=3237)",
                                         "8\n (n=1107)",
                                         "9\n (n=267)",
                                         "10\n (n=66)",
                                         "11-15\n (n=37)"),
                                       labels = c(
                                         "-1\n (n=262)",
                                         "0\n (n=2,773)",
                                         "1\n (n=9,858)",
                                         "2\n (n=10,966)",
                                         "3\n (n=11,972)",
                                         "4\n (n=20,389)",
                                         "5\n (n=15,818)",
                                         "6\n (n=7,442)",
                                         "7\n (n=3,237)",
                                         "8\n (n=1,107)",
                                         "9\n (n=267)",
                                         "10\n (n=66)",
                                         "11-15\n (n=37)")),
                            y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_bw() +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Diabetes ketoacidosis ever") +
  theme(text = element_text(size = 20))
#C) T1D code
plotC <- ggplot(t1_code_ci, aes(x = factor(ScoreD_label,
                                           levels = c(
                                             "-1\n (n=262)",
                                             "0\n (n=2773)",
                                             "1\n (n=9858)",
                                             "2\n (n=10966)",
                                             "3\n (n=11972)",
                                             "4\n (n=20389)",
                                             "5\n (n=15818)",
                                             "6\n (n=7442)",
                                             "7\n (n=3237)",
                                             "8\n (n=1107)",
                                             "9\n (n=267)",
                                             "10\n (n=66)",
                                             "11-15\n (n=37)"),
                                           labels = c(
                                             "-1\n (n=262)",
                                             "0\n (n=2,773)",
                                             "1\n (n=9,858)",
                                             "2\n (n=10,966)",
                                             "3\n (n=11,972)",
                                             "4\n (n=20,389)",
                                             "5\n (n=15,818)",
                                             "6\n (n=7,442)",
                                             "7\n (n=3,237)",
                                             "8\n (n=1,107)",
                                             "9\n (n=267)",
                                             "10\n (n=66)",
                                             "11-15\n (n=37)")),
                                y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(0,100) +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Type 1 diabetes code ever") +
  theme(text = element_text(size = 20))
#D) Basal bolus
plotD <- ggplot(basal_bolus_ci, aes(x = factor(ScoreD_label,
                                               levels = c(
                                                 "-1\n (n=262)",
                                                 "0\n (n=2773)",
                                                 "1\n (n=9858)",
                                                 "2\n (n=10966)",
                                                 "3\n (n=11972)",
                                                 "4\n (n=20389)",
                                                 "5\n (n=15818)",
                                                 "6\n (n=7442)",
                                                 "7\n (n=3237)",
                                                 "8\n (n=1107)",
                                                 "9\n (n=267)",
                                                 "10\n (n=66)",
                                                 "11-15\n (n=37)"),
                                               labels = c(
                                                 "-1\n (n=262)",
                                                 "0\n (n=2,773)",
                                                 "1\n (n=9,858)",
                                                 "2\n (n=10,966)",
                                                 "3\n (n=11,972)",
                                                 "4\n (n=20,389)",
                                                 "5\n (n=15,818)",
                                                 "6\n (n=7,442)",
                                                 "7\n (n=3,237)",
                                                 "8\n (n=1,107)",
                                                 "9\n (n=267)",
                                                 "10\n (n=66)",
                                                 "11-15\n (n=37)")),
                                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw() +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Basal bolus insulin at latest follow-up") +
  theme(text = element_text(size = 20))

pdf("figures/Figure5.pdf", height = 16, width = 34)
joint_plot <- patchwork::wrap_plots(
  plotA,
  plotB,
  plotC,
  plotD,
  ncol = 2, nrow = 2
)+ patchwork::plot_annotation(tag_levels = list(c("A", "B", "C", "D")))
print(joint_plot)
dev.off()
ggsave("figures/Figure5.jpeg", joint_plot, height = 16, width = 34)
