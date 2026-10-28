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
  ) %>%
  rename(AgeatDiagnosis=dm_diag_age,
         bmi_model=prebmi,
         HbA1c_at_diagnosis_v1=prehba1c) %>%
  dplyr::select(Gender_v1, DKA, Unintentional_weight_loss_v1, osmotic, autoimmune,
         famhisdiab, ins_3yrs, follow_up_3yrs, AgeatDiagnosis, bmi_model, HbA1c_at_diagnosis_v1,
         dka, t1_code, basal_bolus, prerandomglucose, Eth_5cat, ethnicity_5cat, ethnicity_16cat)


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
#source("functions/model_info1.R")
#source("functions/var_characteristics_1.R")
#Load data -------------------------------------------------------------------------
#load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
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
  mutate(
    famhisnoninsdiab = ifelse(is.na(famhisdiab), "No", famhisdiab),
    # eth = ifelse(ethnicity_5cat == "0", "White",
    #              ifelse(ethnicity_5cat == "1", "South Asian",
    #                     ifelse(ethnicity_5cat == "2", "Black",
    #                            ifelse(!is.na(ethnicity_16cat)
    #                                   & (ethnicity_16cat == "11" | ethnicity_16cat == "15"),
    #                                   "East Asian", NA)))),
    # eth2 = ifelse(ethnicity_16cat %in% c("1", "2", "3"), "White",
    #               ifelse(ethnicity_16cat %in% c("9", "10", "11"), "South Asian",
    #                      ifelse(ethnicity_16cat %in% c("11", "15"), "East Asian",
    #                             ifelse(ethnicity_16cat %in% c("12", "13", "14"), "Black", NA))))
    Eth_4cat = ifelse(Eth_5cat %in% c("3", "4"), "Other/Mixed",
                      ifelse(Eth_5cat == "0", "White",
                             ifelse(Eth_5cat == "2", "Black",
                                    ifelse(Eth_5cat == "1", "South Asian", Eth_5cat))))
  )
#data$SRoutcome <- as.numeric(data$SRoutcome)
data$DKA <- as.character(data$DKA)
data$osmotic <- as.character(data$osmotic)
data$autoimmune <- as.character(data$autoimmune)

#table(data$eth, data$prob3, useNA = "ifany")

###Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
# model_info(model = m1,
#            test_data = data,
#            outcome = "SRoutcome",
#            saving_name = "model",
#            manual_plotting = TRUE,
#            manual_plot_name = "m1")

#pred_prob_m1 <- predict(m1, data, type = "response")

##StartRight
data <- data %>%
  drop_na(all_of(all_vars_12)) #%>%
# mutate(
#   pred_prob_m1 <- predict(m1, data, type = "response"))

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
    prob5 = case_when(
      pred_prob_m1 < 0.1 ~ "<0.1",
      pred_prob_m1 < 0.2 ~ "0.1-0.2",
      pred_prob_m1 < 0.3 ~ "0.2-0.3",
      pred_prob_m1 < 0.4 ~ "0.3-0.4",
      pred_prob_m1 < 0.5 ~ "0.4-0.5",
      pred_prob_m1 < 0.6 ~ "0.5-0.6",
      pred_prob_m1 < 0.7 ~ "0.6-0.7",
      pred_prob_m1 < 0.8 ~ "0.7-0.8",
      #pred_prob_m1 < 0.9 ~ "0.8-0.9",
      pred_prob_m1 >= 0.8 ~ ">=0.8"),
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
  ) #%>%


varlist_cat <- c(
  "1",
  "2",
  "0"
)

for(v in varlist_cat) {
  ci <- data %>%
    filter(ethnicity_5cat == v) %>%
    freq_table(prob5, t1_code) %>%
    filter(col_cat == "1") %>%
    dplyr::select(row_cat, n_row, col_cat, percent_row, lcl_row, ucl_row) %>%
    rename(
      prob5 = row_cat
    ) %>%
    mutate(
      prob_label = paste0(prob5, "\n (n=", n_row,")")
    )
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}
# summary(factor(`South Asian_ci`$percent_row))
# summary(factor(`East Asian_ci`$percent_row))
# summary(factor(`Black_ci`$percent_row))
# summary(factor(`White_ci`$percent_row))

# `South Asian_ci` %>% select(prob_label, percent_row)
# `East Asian_ci` %>% select(prob_label, percent_row)
# `Black_ci` %>% select(prob_label, percent_row)
# `White_ci` %>% select(prob_label, percent_row)


`0_ci` %>% dplyr::select(prob_label, percent_row)
`1_ci` %>% dplyr::select(prob_label, percent_row)
`2_ci` %>% dplyr::select(prob_label, percent_row)
`0_ci` <- `0_ci` %>%
  left_join(data %>%
              filter(ethnicity_5cat == "0") %>%
              group_by(prob5) %>%
              summarise(
                mean = round(mean(pred_prob_m1)*100,1))
  )
`1_ci` <- `1_ci` %>%
  left_join(data %>%
              filter(ethnicity_5cat == "1") %>%
              group_by(prob5) %>%
              summarise(
                mean = round(mean(pred_prob_m1)*100,1))
  )
`2_ci` <- `2_ci` %>%
  left_join(data %>%
              filter(ethnicity_5cat == "2") %>%
              group_by(prob5) %>%
              summarise(
                mean = round(mean(pred_prob_m1)*100,1))
  )

`2_ci`$prob_label
`1_ci`$prob_label
`0_ci`$prob_label

 # `2_ci` <- `2_ci` %>%
 #   add_row(prob5 = "0.6-0.7", prob_label = "0.6-0.7\n (n=1)", percent_row = 0)
 b_6to70 <- prop.test(x=0, n=1, conf.level = 0.95, correct = FALSE)

 b_7to80 <- prop.test(x=3, n=3, conf.level = 0.95, correct = FALSE)
 b_80plus <- prop.test(x=2, n=2, conf.level = 0.95, correct = FALSE)
 `2_ci` <- `2_ci` %>%
   add_row(prob5 = "0.6-0.7", prob_label = "0.6-0.7\n (n=1)", percent_row = 0,
           lcl_row = b_6to70$conf.int[1]*100,
           ucl_row = b_6to70$conf.int[2]*100) %>%
   mutate(
     lcl_row = ifelse(prob5 == "0.7-0.8", b_7to80$conf.int[1]*100,
                      ifelse(prob5 == ">=0.8", b_80plus$conf.int[1]*100,lcl_row)),
     ucl_row = ifelse(prob5 == "0.7-0.8", b_7to80$conf.int[2]*100,
                      ifelse(prob5 == ">=0.8", b_80plus$conf.int[2]*100,ucl_row))
   )
#Make plots (prog insulin)
##With error bars ---------------------------------------------------------------------
#A) black
plotA <- ggplot(`2_ci`,
                aes(x = factor(prob_label,
                               levels = c("<0.1\n (n=10940)",
                                          "0.1-0.2\n (n=102)",
                                          "0.2-0.3\n (n=29)",
                                          "0.3-0.4\n (n=15)",
                                          "0.4-0.5\n (n=8)",
                                          "0.5-0.6\n (n=6)",
                                          "0.6-0.7\n (n=1)",
                                          "0.7-0.8\n (n=3)",
                                          ">=0.8\n (n=2)"),
                               labels = c("0-9 \n (n=10,940)",
                                          "10-19 \n (n=102)",
                                          "20-29 \n (n=29)",
                                          "30-39 \n (n=15)",
                                          "40-49 \n (n=8)",
                                          "50-59 \n (n=6)",
                                          "60-69 \n (n=1)",
                                          "70-79 \n (n=3)",
                                          "80-100 \n (n=2)")
                ),
                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(c(-2, 100)) +
  geom_text(aes(y = -2, label = mean), vjust = 1, size = 6) +
  annotate("text", x = 0.3, y = -2, label = "Mean \n prob.", size = 6, vjust = 1, hjust = 1, lineheight = 0.5) +
  coord_cartesian(xlim = c(1, 9), clip = "off") +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "Black ethnicity") +
  theme(text = element_text(size = 20))


#c) south asian
plotB <- ggplot(`1_ci`,
                aes(x = factor(prob_label,
                               levels = c("<0.1\n (n=22524)",
                                          "0.1-0.2\n (n=627)",
                                          "0.2-0.3\n (n=152)",
                                          "0.3-0.4\n (n=69)",
                                          "0.4-0.5\n (n=37)",
                                          "0.5-0.6\n (n=21)",
                                          "0.6-0.7\n (n=16)",
                                          "0.7-0.8\n (n=9)",
                                          ">=0.8\n (n=6)"),
                               labels = c("0-9 \n (n=22,524)",
                                          "10-19 \n (n=627)",
                                          "20-29 \n (n=152)",
                                          "30-39 \n (n=69)",
                                          "40-49 \n (n=37)",
                                          "50-59 \n (n=21)",
                                          "60-69 \n (n=16)",
                                          "70-79 \n (n=9)",
                                          "80-100 \n (n=6)")
                ),
                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(c(-2, 100)) +
  geom_text(aes(y = -2, label = mean), vjust = 1, size = 6) +
  annotate("text", x = 0.3, y = -2, label = "Mean \n prob.", size = 6, vjust = 1, hjust = 1, lineheight = 0.5) +
  coord_cartesian(xlim = c(1, 9), clip = "off") +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "South Asian ethnicity") +
  theme(text = element_text(size = 20))

#d) white
plotC <- ggplot(`0_ci`,
                aes(x = factor(prob_label,
                               levels = c("<0.1\n (n=143364)",
                                          "0.1-0.2\n (n=1864)",
                                          "0.2-0.3\n (n=678)",
                                          "0.3-0.4\n (n=362)",
                                          "0.4-0.5\n (n=231)",
                                          "0.5-0.6\n (n=159)",
                                          "0.6-0.7\n (n=142)",
                                          "0.7-0.8\n (n=143)",
                                          ">=0.8\n (n=247)"),
                               labels = c("0-9 \n (n=143,364)",
                                          "10-19 \n (n=1,864)",
                                          "20-29 \n (n=678)",
                                          "30-39 \n (n=362)",
                                          "40-49 \n (n=231)",
                                          "50-59 \n (n=159)",
                                          "60-69 \n (n=142)",
                                          "70-79 \n (n=143)",
                                          "80-100 \n (n=247)")
                ),
                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  ylim(c(-2, 100)) +
  geom_text(aes(y = -2, label = mean), vjust = 1, size = 6) +
  annotate("text", x = 0.3, y = -2, label = "Mean \n prob.", size = 6, vjust = 1, hjust = 1, lineheight = 0.5) +
  coord_cartesian(xlim = c(1, 9), clip = "off") +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "White ethnicity",
       tag = "Mean") +
  theme(text = element_text(size = 20)
  )



pdf("figures/Supp_Figurex44.pdf", height = 16, width = 24)
joint_plot <- patchwork::wrap_plots(
  plotA,
  plotB,
  plotC,
  #plotD,
  ncol = 2, nrow = 2
)+ patchwork::plot_annotation(tag_levels = list(c("A", "B", "C"#,
                                                  #"D"
)))
print(joint_plot)
dev.off()
#ggsave("figures/Figure5_T1Dcode.png", joint_plot, height = 16, width = 24)
ggsave("figures/Supp_Figurex44.jpeg", joint_plot, height = 16, width = 24)
