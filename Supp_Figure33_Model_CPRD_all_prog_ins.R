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
#filter(ethnicity_5cat == "0")

# table(data$eth, data$prob3, useNA = "ifany")
# table(data$eth2, data$prob3, useNA = "ifany")
# table(data$ethnicity_5cat, data$prob3, useNA = "ifany")
# table(data$eth, data$prob4, useNA = "ifany")
# table(data$eth2, data$prob4, useNA = "ifany")
# table(data$ethnicity_5cat, data$prob4, useNA = "ifany")
# table(data$ethnicity_5cat, data$prob, useNA = "ifany")
# table(data$ethnicity_5cat, data$prob5, useNA = "ifany")

# data %>%
#   filter(ethnicity_5cat %in% c("0", "1", "2")) %>%
#   group_by(prob, ethnicity_5cat) %>%
#   summarise(
#     n=n())
# data %>%
#   filter(ethnicity_5cat %in% c("0", "1", "2")) %>%
#   group_by(prob2, ethnicity_5cat) %>%
#   summarise(
#     n=n())
# data %>%
#   filter(ethnicity_5cat %in% c("0", "1", "2")) %>%
#   group_by(prob3, ethnicity_5cat) %>%
#   summarise(
#     n=n())
#
# data %>%
#   group_by(prob3, eth) %>%
#   summarise(
#     n=n())

# data <- data %>%
#   select(SRoutcome,  prob3, agedx, bmi, hba1c, sex,
#          osmo, auto, weightloss, DKA, pardm,dka, t1_code,basal_bolus, ethnicity_5cat, eth, ethnicity_16cat) %>%
#   mutate(ScoreD = select(., agedx:pardm) %>% rowSums(na.rm = TRUE))

varlist_cat <- c(
  "1",
  "2",
  "0"
)
# varlist_cat <- c(
#   "Black",
#   "East Asian",
#   "South Asian",
#   "White"
# )
for(v in varlist_cat) {
  ci <- data %>%
    filter(ethnicity_5cat == v) %>%
    freq_table(prob5, ins_3yrs) %>%
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
`0_ci` %>%
  dplyr::select(prob_label, percent_row)
`1_ci` %>%
  dplyr::select(prob_label, percent_row)
`2_ci` %>%
  dplyr::select(prob_label, percent_row)
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
#test <- prop.test(x=0, n=3, conf.level = 0.95, correct = FALSE)
b_7to80 <- prop.test(x=2, n=2, conf.level = 0.95, correct = FALSE)
b_80plus <- prop.test(x=3, n=3, conf.level = 0.95, correct = FALSE)
`2_ci` <- `2_ci` %>%
  add_row(prob5 = "0.6-0.7", prob_label = "0.6-0.7\n (n=0)", percent_row = 0) %>%
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
                               levels = c("<0.1\n (n=10959)",
                                          "0.1-0.2\n (n=89)",
                                          "0.2-0.3\n (n=24)",
                                          "0.3-0.4\n (n=16)",
                                          "0.4-0.5\n (n=6)",
                                          "0.5-0.6\n (n=6)",
                                          "0.6-0.7\n (n=0)",
                                          "0.7-0.8\n (n=2)",
                                          ">=0.8\n (n=3)"),
                               labels = c("0-9 \n (n=10,959)",
                                          "10-19 \n (n=89)",
                                          "20-29 \n (n=24)",
                                          "30-39 \n (n=16)",
                                          "40-49 \n (n=6)",
                                          "50-59 \n (n=6)",
                                          "60-69 \n (n=0)",
                                          "70-79 \n (n=2)",
                                          "80-100 \n (n=3)")
                ),
                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
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
                               levels = c("<0.1\n (n=22672)",
                                          "0.1-0.2\n (n=513)",
                                          "0.2-0.3\n (n=126)",
                                          "0.3-0.4\n (n=62)",
                                          "0.4-0.5\n (n=35)",
                                          "0.5-0.6\n (n=21)",
                                          "0.6-0.7\n (n=15)",
                                          "0.7-0.8\n (n=11)",
                                          ">=0.8\n (n=6)"),
                               labels = c("0-9 \n (n=22,672)",
                                          "10-19 \n (n=513)",
                                          "20-29 \n (n=126)",
                                          "30-39 \n (n=62)",
                                          "40-49 \n (n=35)",
                                          "50-59 \n (n=21)",
                                          "60-69 \n (n=15)",
                                          "70-79 \n (n=11)",
                                          "80-100 \n (n=6)")
                ),
                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
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
                               levels = c("<0.1\n (n=143733)",
                                          "0.1-0.2\n (n=1611)",
                                          "0.2-0.3\n (n=601)",
                                          "0.3-0.4\n (n=330)",
                                          "0.4-0.5\n (n=221)",
                                          "0.5-0.6\n (n=154)",
                                          "0.6-0.7\n (n=130)",
                                          "0.7-0.8\n (n=139)",
                                          ">=0.8\n (n=271)"),
                               labels = c("0-9 \n (n=143,733)",
                                          "10-19 \n (n=1,611)",
                                          "20-29 \n (n=601)",
                                          "30-39 \n (n=330)",
                                          "40-49 \n (n=221)",
                                          "50-59 \n (n=154)",
                                          "60-69 \n (n=130)",
                                          "70-79 \n (n=139)",
                                          "80-100 \n (n=271)")
                ),
                y = percent_row)) +
  geom_bar(stat="identity", fill = "#e2a9f1") +
  geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
  theme_bw()  +
  geom_text(aes(y = -2, label = mean), vjust = 1, size = 6) +
  annotate("text", x = 0.3, y = -2, label = "Mean \n prob.", size = 6, vjust = 1, hjust = 1, lineheight = 0.5) +
  coord_cartesian(xlim = c(1, 9), clip = "off") +
  labs(x = "Clinical features only model probability (%)",
       y = "Percentage",
       title = "White ethnicity",
       tag = "Mean") +
  theme(text = element_text(size = 20)
  )



pdf("figures/Supp_Figure32.pdf", height = 16, width = 24)
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
#ggsave("figures/Figure5_prog_ins.png", joint_plot, height = 16, width = 24)
ggsave("figures/Supp_Figure32.jpeg", joint_plot, height = 16, width = 24)
