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
#load("m1.RData")
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
    # pred_prob_m1 = predict(m1, data, type = "response"),
    # prob = case_when(
    #   pred_prob_m1 < 0.1 ~ "<0.1",
    #   pred_prob_m1 < 0.2 ~ "0.1-0.2",
    #   pred_prob_m1 < 0.3 ~ "0.2-0.3",
    #   pred_prob_m1 < 0.4 ~ "0.3-0.4",
    #   pred_prob_m1 < 0.5 ~ "0.4-0.5",
    #   pred_prob_m1 < 0.6 ~ "0.5-0.6",
    #   pred_prob_m1 < 0.7 ~ "0.6-0.7",
    #   pred_prob_m1 < 0.8 ~ "0.7-0.8",
    #   pred_prob_m1 < 0.9 ~ "0.8-0.9",
    #   pred_prob_m1 >= 0.9 ~ ">=0.9"),
    # prob2 = case_when(
    #   #pred_prob_m1 < 0.1 ~ "<0.1",
    #   pred_prob_m1 < 0.2 ~ "0.0-0.2",
    #   #pred_prob_m1 < 0.3 ~ "0.2-0.3",
    #   pred_prob_m1 < 0.4 ~ "0.2-0.4",
    #   #pred_prob_m1 < 0.5 ~ "0.4-0.5",
    #   pred_prob_m1 < 0.6 ~ "0.4-0.6",
    #   #pred_prob_m1 < 0.7 ~ "0.6-0.7",
    #   pred_prob_m1 < 0.8 ~ "0.6-0.8",
    #   #pred_prob_m1 < 0.9 ~ "0.8-0.9",
    #   pred_prob_m1 >= 0.9 ~ ">=0.8"),
    # prob3 = case_when(
    #   pred_prob_m1 < 0.1 ~ "<0.1",
    #   pred_prob_m1 < 0.2 ~ "0.1-0.2",
    #   pred_prob_m1 < 0.3 ~ "0.2-0.3",
    #   pred_prob_m1 < 0.4 ~ "0.3-0.4",
    #   pred_prob_m1 < 0.5 ~ "0.4-0.5",
    #   #pred_prob_m1 < 0.6 ~ "0.5-0.6",
    #   #pred_prob_m1 < 0.7 ~ "0.6-0.7",
    #   #pred_prob_m1 < 0.8 ~ "0.7-0.8",
    #   #pred_prob_m1 < 0.9 ~ "0.8-0.9",
    #   pred_prob_m1 >= 0.5 ~ ">=0.5"),
    # prob4 = case_when(
    #   pred_prob_m1 < 0.1 ~ "<0.1",
    #   pred_prob_m1 < 0.2 ~ "0.1-0.2",
    #   pred_prob_m1 < 0.3 ~ "0.2-0.3",
    #   pred_prob_m1 < 0.4 ~ "0.3-0.4",
    #   #pred_prob_m1 < 0.5 ~ "0.4-0.5",
    #   #pred_prob_m1 < 0.6 ~ "0.5-0.6",
    #   #pred_prob_m1 < 0.7 ~ "0.6-0.7",
    #   #pred_prob_m1 < 0.8 ~ "0.7-0.8",
    #   #pred_prob_m1 < 0.9 ~ "0.8-0.9",
    #   pred_prob_m1 >= 0.4 ~ ">=0.4"),
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
  dplyr::select(SRoutcome, AgeatDiagnosis, agedx, bmi, hba1c, sex,
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
    score_cat = ifelse(ScoreD >= 11, "11-15", ScoreD),
    score_cat2 = ifelse(ScoreD >=9, "9-15", ScoreD)
  )

#Split by age
data18_50 <- data %>%
  filter(AgeatDiagnosis <51)

data50plus <- data %>%
  filter(AgeatDiagnosis >=51)
#Make plots
#18-50---------------------------------------------------------------------------------------------------
##Score categories With error bars ---------------------------------------------------------------------
for(v in varlist_cat) {
  ci <- data18_50 %>%
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
plotA1 <- ggplot(SRoutcome_ci,
                aes(x = factor(ScoreD_label,
                               levels = c(
                                 "-1\n (n=113)",
                                 "0\n (n=870)",
                                 "1\n (n=2569)",
                                 "2\n (n=3585)",
                                 "3\n (n=4130)",
                                 "4\n (n=4408)",
                                 "5\n (n=3670)",
                                 "6\n (n=2102)",
                                 "7\n (n=952)",
                                 "8\n (n=432)",
                                 "9\n (n=171)",
                                 "10\n (n=60)",
                                 "11-15\n (n=35)"),
                               labels = c(
                                 "-1\n (n=113)",
                                 "0\n (n=870)",
                                 "1\n (n=2,569)",
                                 "2\n (n=3,585)",
                                 "3\n (n=4,130)",
                                 "4\n (n=4,408)",
                                 "5\n (n=3,670)",
                                 "6\n (n=2,102)",
                                 "7\n (n=952)",
                                 "8\n (n=432)",
                                 "9\n (n=171)",
                                 "10\n (n=60)",
                                 "11-15\n (n=35)")),
                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Progression to insulin within three years",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#B) DKA ever
plotB1 <- ggplot(dka_ci, aes(x = factor(ScoreD_label,
                                       levels = c(
                                         "-1\n (n=113)",
                                         "0\n (n=870)",
                                         "1\n (n=2569)",
                                         "2\n (n=3585)",
                                         "3\n (n=4130)",
                                         "4\n (n=4408)",
                                         "5\n (n=3670)",
                                         "6\n (n=2102)",
                                         "7\n (n=952)",
                                         "8\n (n=432)",
                                         "9\n (n=171)",
                                         "10\n (n=60)",
                                         "11-15\n (n=35)"),
                                       labels = c(
                                         "-1\n (n=113)",
                                         "0\n (n=870)",
                                         "1\n (n=2,569)",
                                         "2\n (n=3,585)",
                                         "3\n (n=4,130)",
                                         "4\n (n=4,408)",
                                         "5\n (n=3,670)",
                                         "6\n (n=2,102)",
                                         "7\n (n=952)",
                                         "8\n (n=432)",
                                         "9\n (n=171)",
                                         "10\n (n=60)",
                                         "11-15\n (n=35)")),
                            y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_bw() +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Diabetes ketoacidosis ever",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#C) T1D code
b_4to50 <- prop.test(x=0, n=113, conf.level = 0.95, correct = FALSE)
`t1_code_ci` <- `t1_code_ci` %>%
  add_row(score_cat = "-1",
          ScoreD_label = "-1\n (n=113)",
          percent_row = 0,
          lcl_row = b_4to50$conf.int[1]*100,
          ucl_row = b_4to50$conf.int[2]*100)
plotC1 <- ggplot(t1_code_ci, aes(x = factor(ScoreD_label,
                                           levels = c(
                                             "-1\n (n=113)",
                                             "0\n (n=870)",
                                             "1\n (n=2569)",
                                             "2\n (n=3585)",
                                             "3\n (n=4130)",
                                             "4\n (n=4408)",
                                             "5\n (n=3670)",
                                             "6\n (n=2102)",
                                             "7\n (n=952)",
                                             "8\n (n=432)",
                                             "9\n (n=171)",
                                             "10\n (n=60)",
                                             "11-15\n (n=35)"),
                                           labels = c(
                                             "-1\n (n=113)",
                                             "0\n (n=870)",
                                             "1\n (n=2,569)",
                                             "2\n (n=3,585)",
                                             "3\n (n=4,130)",
                                             "4\n (n=4,408)",
                                             "5\n (n=3,670)",
                                             "6\n (n=2,102)",
                                             "7\n (n=952)",
                                             "8\n (n=432)",
                                             "9\n (n=171)",
                                             "10\n (n=60)",
                                             "11-15\n (n=35)")),
                                y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(0,100) +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Type 1 diabetes code ever",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))
#D) Basal bolus
plotD1 <- ggplot(basal_bolus_ci, aes(x = factor(ScoreD_label,
                                               levels = c(
                                                 "-1\n (n=113)",
                                                 "0\n (n=870)",
                                                 "1\n (n=2569)",
                                                 "2\n (n=3585)",
                                                 "3\n (n=4130)",
                                                 "4\n (n=4408)",
                                                 "5\n (n=3670)",
                                                 "6\n (n=2102)",
                                                 "7\n (n=952)",
                                                 "8\n (n=432)",
                                                 "9\n (n=171)",
                                                 "10\n (n=60)",
                                                 "11-15\n (n=35)"),
                                               labels = c(
                                                 "-1\n (n=113)",
                                                 "0\n (n=870)",
                                                 "1\n (n=2,569)",
                                                 "2\n (n=3,585)",
                                                 "3\n (n=4,130)",
                                                 "4\n (n=4,408)",
                                                 "5\n (n=3,670)",
                                                 "6\n (n=2,102)",
                                                 "7\n (n=952)",
                                                 "8\n (n=432)",
                                                 "9\n (n=171)",
                                                 "10\n (n=60)",
                                                 "11-15\n (n=35)")),
                                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw() +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Basal bolus insulin at latest follow-up",
       subtitle = "Adults diagnosed 18-50 years") +
  theme(text = element_text(size = 20))

#>50---------------------------------------------------------------------------------------------------
##Score categories With error bars ---------------------------------------------------------------------
for(v in varlist_cat) {
  ci <- data50plus %>%
    freq_table(score_cat2, !!sym(v)) %>%
    filter(col_cat == "1") %>%
    dplyr::select(row_cat, col_cat, n_row, percent_row, lcl_row, ucl_row) %>%
    rename(
      score_cat2 = row_cat
    )%>%
    mutate(
      ScoreD_label = paste0(score_cat2, "\n (n=", n_row,")")
    )
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}
summary(factor(SRoutcome_ci$ScoreD_label))
#A) prog insulin
plotA2 <- ggplot(SRoutcome_ci,
                aes(x = factor(ScoreD_label,
                               levels = c(
                                 "-1\n (n=149)",
                                 "0\n (n=1903)",
                                 "1\n (n=7289)",
                                 "2\n (n=7381)",
                                 "3\n (n=7842)",
                                 "4\n (n=15981)",
                                 "5\n (n=12148)",
                                 "6\n (n=5340)",
                                 "7\n (n=2285)",
                                 "8\n (n=675)",
                                 #"9\n (n=267)",
                                 #"10\n (n=66)",
                                 "9-15\n (n=104)"),
                               labels = c(
                                 "-1\n (n=149)",
                                 "0\n (n=1,903)",
                                 "1\n (n=7,289)",
                                 "2\n (n=7,381)",
                                 "3\n (n=7,842)",
                                 "4\n (n=15,981)",
                                 "5\n (n=12,148)",
                                 "6\n (n=5,340)",
                                 "7\n (n=2,285)",
                                 "8\n (n=675)",
                                 #"9\n (n=267)",
                                 #"10\n (n=66)",
                                 "9-15\n (n=104)")),
                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Progression to insulin within three years",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#B) DKA ever
b_4to50 <- prop.test(x=0, n=149, conf.level = 0.95, correct = FALSE)
`dka_ci` <- `dka_ci` %>%
  add_row(score_cat2 = "-1",
          ScoreD_label = "-1\n (n=149)",
          percent_row = 0,
          lcl_row = b_4to50$conf.int[1]*100,
          ucl_row = b_4to50$conf.int[2]*100)
b_4to50 <- prop.test(x=0, n=104, conf.level = 0.95, correct = FALSE)
`dka_ci` <- `dka_ci` %>%
  add_row(score_cat2 = "9-15",
          ScoreD_label = "9-15\n (n=104)",
          percent_row = 0,
          lcl_row = b_4to50$conf.int[1]*100,
          ucl_row = b_4to50$conf.int[2]*100)
plotB2 <- ggplot(dka_ci, aes(x = factor(ScoreD_label,
                                        levels = c(
                                          "-1\n (n=149)",
                                          "0\n (n=1903)",
                                          "1\n (n=7289)",
                                          "2\n (n=7381)",
                                          "3\n (n=7842)",
                                          "4\n (n=15981)",
                                          "5\n (n=12148)",
                                          "6\n (n=5340)",
                                          "7\n (n=2285)",
                                          "8\n (n=675)",
                                          #"9\n (n=267)",
                                          #"10\n (n=66)",
                                          "9-15\n (n=104)"),
                                        labels = c(
                                          "-1\n (n=149)",
                                          "0\n (n=1,903)",
                                          "1\n (n=7,289)",
                                          "2\n (n=7,381)",
                                          "3\n (n=7,842)",
                                          "4\n (n=15,981)",
                                          "5\n (n=12,148)",
                                          "6\n (n=5,340)",
                                          "7\n (n=2,285)",
                                          "8\n (n=675)",
                                          #"9\n (n=267)",
                                          #"10\n (n=66)",
                                          "9-15\n (n=104)")),
                            y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_bw() +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Diabetes ketoacidosis ever",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#C) T1D code
plotC2 <- ggplot(t1_code_ci, aes(x = factor(ScoreD_label,
                                            levels = c(
                                              "-1\n (n=149)",
                                              "0\n (n=1903)",
                                              "1\n (n=7289)",
                                              "2\n (n=7381)",
                                              "3\n (n=7842)",
                                              "4\n (n=15981)",
                                              "5\n (n=12148)",
                                              "6\n (n=5340)",
                                              "7\n (n=2285)",
                                              "8\n (n=675)",
                                              #"9\n (n=267)",
                                              #"10\n (n=66)",
                                              "9-15\n (n=104)"),
                                            labels = c(
                                              "-1\n (n=149)",
                                              "0\n (n=1,903)",
                                              "1\n (n=7,289)",
                                              "2\n (n=7,381)",
                                              "3\n (n=7,842)",
                                              "4\n (n=15,981)",
                                              "5\n (n=12,148)",
                                              "6\n (n=5,340)",
                                              "7\n (n=2,285)",
                                              "8\n (n=675)",
                                              #"9\n (n=267)",
                                              #"10\n (n=66)",
                                              "9-15\n (n=104)")),
                                y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(0,100) +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Type 1 diabetes code ever",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))
#D) Basal bolus
b_4to50 <- prop.test(x=0, n=149, conf.level = 0.95, correct = FALSE)
`basal_bolus_ci` <- `basal_bolus_ci` %>%
  add_row(score_cat2 = "-1",
          ScoreD_label = "-1\n (n=149)",
          percent_row = 0,
          lcl_row = b_4to50$conf.int[1]*100,
          ucl_row = b_4to50$conf.int[2]*100)
plotD2 <- ggplot(basal_bolus_ci, aes(x = factor(ScoreD_label,
                                                levels = c(
                                                  "-1\n (n=149)",
                                                  "0\n (n=1903)",
                                                  "1\n (n=7289)",
                                                  "2\n (n=7381)",
                                                  "3\n (n=7842)",
                                                  "4\n (n=15981)",
                                                  "5\n (n=12148)",
                                                  "6\n (n=5340)",
                                                  "7\n (n=2285)",
                                                  "8\n (n=675)",
                                                  #"9\n (n=267)",
                                                  #"10\n (n=66)",
                                                  "9-15\n (n=104)"),
                                                labels = c(
                                                  "-1\n (n=149)",
                                                  "0\n (n=1,903)",
                                                  "1\n (n=7,289)",
                                                  "2\n (n=7,381)",
                                                  "3\n (n=7,842)",
                                                  "4\n (n=15,981)",
                                                  "5\n (n=12,148)",
                                                  "6\n (n=5,340)",
                                                  "7\n (n=2,285)",
                                                  "8\n (n=675)",
                                                  #"9\n (n=267)",
                                                  #"10\n (n=66)",
                                                  "9-15\n (n=104)")),
                                    y = percent_row)) +
  geom_bar(stat="identity", fill = "#ffc49f") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw() +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "StartRight Score",
       y = "Percentage",
       title = "% Basal bolus insulin at latest follow-up",
       subtitle = "Adults diagnosed >50 years") +
  theme(text = element_text(size = 20))

pdf("figures/Supp_Figurex41.pdf", height = 16, width = 34)
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
ggsave("figures/Supp_Figurex41.jpeg", joint_plot, height = 16, width = 34)
