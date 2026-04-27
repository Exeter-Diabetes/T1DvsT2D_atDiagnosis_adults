#####################################################################################
#Scoring table performance
#Figures
#########################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)

#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/model_continuous1.R")

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
  "Eth_5cat",
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
#StartRight Prime --------------------------------------------------------------------
SR_50_SRout_ccc<- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
#Model12
SR_50_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12))

##Scoring Table -----------------------------------------------------------------------
#StartRight----------------------------------------------------------------------------------
SR_mD_data <- SR_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, ethnicity,num_anti) %>%
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
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    eth = ifelse(ethnicity == "White", 0, -1),
    pos_anti = ifelse(num_anti >= 1, 1, 0)
  )

SR_mD_data <- SR_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, dka, eth,pardm, pos_anti, num_anti) %>%
  mutate(ScoreD = select(., agedx:pardm) %>% rowSums(na.rm = TRUE)) #%>%
  #mutate(ScoreD = rowSums(across(c(agedx:pardm)), na.rm = TRUE))

SR_mD_data <- SR_mD_data %>%
  mutate(
    SR_score_cat1 = case_when(
    ScoreD <= 5 ~ "0-5",
    ScoreD <= 10 ~ "6-10",
    ScoreD > 10 ~ "11-14"
  ),
  SR_score_cat2 = case_when(
    ScoreD < 7 ~ "0-6",
    ScoreD <= 10 ~ "7-10",
    ScoreD > 10 ~ "11-14"
  ),
  SR_score_cat3 = case_when(
    ScoreD <= 5 ~ "0-5",
    ScoreD <= 11 ~ "6-11",
    ScoreD > 11 ~ "12-14"
  ),
  SR_score_cat4 = case_when(
    ScoreD < 7 ~ "0-6",
    ScoreD <= 11 ~ "7-11",
    ScoreD > 11 ~ "12-14"
  ),
  model_cat = case_when(
    m1_pp < 0.1 ~"<10%",
    m1_pp <= 0.9 ~"10-90%",
    m1_pp > 0.9 ~ ">90%"
  ),
  anti_pos = ifelse(num_anti == "0", 0, 1),
  score_cat = ifelse(ScoreD >= 11, ">=11", ifelse(ScoreD <= 4, "<=4", as.character(ScoreD))))
# SR_m12_data <- SR_SRout_ccc %>%
#   #drop_na(all_of(all_vars_12)) %>%
#   mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
#StartRight Prime----------------------------------------------------------------------------------
SR_50_mD_data <- SR_50_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, ethnicity,num_anti) %>%
  mutate(
    m1_pp = predict(m1, SR_50_m12_data, type = "response"),
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
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    eth = ifelse(ethnicity == "White", 0, -1),
    pos_anti = ifelse(num_anti >= 1, 1, 0)
  )

SR_50_mD_data <- SR_50_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, dka, eth,pardm, pos_anti, num_anti) %>%
  mutate(ScoreD = select(., agedx:pardm) %>% rowSums(na.rm = TRUE))

SR_50_mD_data <- SR_50_mD_data %>%
  mutate(
    SR_score_cat1 = case_when(
      ScoreD <= 5 ~ "0-5",
      ScoreD <= 10 ~ "6-10",
      ScoreD > 10 ~ "11-14"
    ),
    SR_score_cat2 = case_when(
      ScoreD < 7 ~ "0-6",
      ScoreD <= 10 ~ "7-10",
      ScoreD > 10 ~ "11-14"
    ),
    SR_score_cat3 = case_when(
      ScoreD <= 5 ~ "0-5",
      ScoreD <= 11 ~ "6-11",
      ScoreD > 11 ~ "12-14"
    ),
    SR_score_cat4 = case_when(
      ScoreD < 7 ~ "0-6",
      ScoreD <= 11 ~ "7-11",
      ScoreD > 11 ~ "12-14"
    ),
    model_cat = case_when(
      m1_pp < 0.1 ~"<10%",
      m1_pp <= 0.9 ~"10-90%",
      m1_pp > 0.9 ~ ">90%"
    ),
    anti_pos = ifelse(num_anti == "0", 0, 1),
    score_cat = ifelse(ScoreD >= 11, ">=11", ifelse(ScoreD <= 4, "<=4", as.character(ScoreD))))

# SR_50_m12_data <- SR_50_SRout_ccc %>%
#   #drop_na(all_of(all_vars_12)) %>%
#   mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
###Model performance plot -------------------------------------------------------------
####Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
# model_info(model = m1,
#            test_data = SR_mD_data,
#            outcome = "anti_pos",
#            saving_name = "03_05_sm1_antipos",
#            manual_plotting = TRUE,
#            manual_plot_name = "m1")
# ####Model 1 ROW --------------------------------------------------------------------------
# ####Get large amount of model info automated
# model_info(model = m1,
#            test_data = SR_50_m12_data,
#            outcome = "anti_pos",
#            saving_name = "03_05_sm1_50_antipos",
#            manual_plotting = TRUE,
#            manual_plot_name = "m1_50")



#Supp_Figure 27 -----------------------------------------------------------------
DESIGN_abc <- "
11111#
  222333
  222333
  222333
  222333
  222333
"
m12_n_start <- nrow(SR_mD_data)
n12_T1D_start <- SR_mD_data %>%
  filter(SRoutcome == 1) %>%
  nrow()
m12_n_prime <- nrow(SR_50_mD_data)
n12_T1D_prime <- SR_50_mD_data %>%
  filter(SRoutcome == 1) %>%
  nrow()



## --- Title grobs (right-aligned) -----------------------------------------
model1_text_start <- paste0("StartRight: n=", m12_n_start, " (Type 1=", n12_T1D_start, ")")
model1_text_prime <- paste0("StartRight Prime: n=", m12_n_prime, " (Type 1=", n12_T1D_prime, ")")

title_m1_start <- patchwork::wrap_elements(ggpubr::text_grob(model1_text_start, face = "bold", size = 20, hjust = 1.1))
title_m1_prime <- patchwork::wrap_elements(ggpubr::text_grob(model1_text_prime, face = "bold", size = 20, hjust = 0.95))



#Using 6 cut off -------------------------------------------------------------------------
##StartRight ---------------------------------------------------------------------------
cal_prep_m1 <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
                               pred = SR_mD_data$m1_pp,
                               pos_anti = SR_mD_data$pos_anti,
                               ScoreD = SR_mD_data$score_cat) %>%
  group_by(ScoreD) %>%
  mutate(prob_obs = sum(y) / n(),
         obs = sum(y),
         n_group = n(),
         mnpred = mean(pred),
         lower = lapply(sum(y), prop.test, n = n()),
         upper = sapply(lower, function(x) x$conf.int[2]),
         lower = sapply(lower, function(x) x$conf.int[1]))
#A)
cal_outcome <- ggplot(cal_prep_m1, aes(y = prob_obs,
                                      x = factor(
                                        ScoreD,
                                        #levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
                                        levels = c("<=4", "5", "6", "7", "8", "9", "10", ">=11"),
                                        labels = c("0-4", "5", "6", "7", "8", "9", "10", "11-15")
                                      ))) +
  geom_point(size = 3) +
  xlab("StartRight Score") +
  ylab("Observed proportion \n of positive islet- \n autoantibody") +
  ylim(c(0, 1)) +
  #xlim(c(0, 1)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = 0.1,
                size = 0.7) +
  #scale_x_discrete(labels = c("<=1", "2", "3", "4", "5", "6", "7", "8", "9", ">=10")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none")


#B
dec_SR_nm1_antis <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
                               pred = SR_mD_data$m1_pp,
                               pos_anti = SR_mD_data$pos_anti,
                               ScoreD = SR_mD_data$ScoreD) %>%
  group_by(pos_anti, ScoreD) %>%
  mutate(prob_obs = sum(y) / n(),
         obs = sum(y),
         n_group = n(),
         mnpred = mean(pred),
         lower = lapply(sum(y), prop.test, n = n()),
         upper = sapply(lower, function(x) x$conf.int[2]),
         lower = sapply(lower, function(x) x$conf.int[1]))
cal_antis <- ggplot(dec_SR_nm1_antis , aes(y = prob_obs,
                                           x = ScoreD,
                                           color = factor(pos_anti))) +
  geom_point(aes(alpha = 0.5), size = 3) +
  xlab("StartRight score") +
  ylab("Observed proportion \n of T1D") +
  #labs(color = "Antibody Status") +
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylim(c(0, 1)) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 11.5) +
  #xlim(c(0, 1)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper,
                    color = factor(pos_anti),
                    alpha = 0.5),
                width = 0.1,
                size = 0.7) +
  scale_x_continuous(breaks=seq(0,20,1)) +
  # geom_hline(yintercept = 0.1) +
  # geom_hline(yintercept = 0.9) +
  # geom_vline(xintercept = 4.5) +
  # geom_vline(xintercept = 10.5) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none")
##StartRight Prime---------------------------------------------------------------------------
#C
cal_prep_m1_50 <- data.frame(y = as.numeric(SR_50_mD_data$SRoutcome),
                                  pred = SR_50_mD_data$m1_pp,
                                  pos_anti = SR_50_mD_data$pos_anti,
                                  ScoreD = SR_50_mD_data$score_cat) %>%
  group_by(ScoreD) %>%
  mutate(prob_obs = sum(y) / n(),
         obs = sum(y),
         n_group = n(),
         mnpred = mean(pred),
         lower = lapply(sum(y), prop.test, n = n()),
         upper = sapply(lower, function(x) x$conf.int[2]),
         lower = sapply(lower, function(x) x$conf.int[1]))

cal_outcome_50 <- ggplot(cal_prep_m1_50, aes(y = prob_obs,
                                          x = factor(
                                            ScoreD,
                                            #levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
                                            levels = c("<=4", "5", "6", "7", "8", "9", "10", ">=11"),
                                            labels = c("0-4", "5", "6", "7", "8", "9", "10", "11-15")
                                          ))) +
  geom_point(size = 3) +
  xlab("StartRight Score") +
  ylab("Observed proportion \n of positive islet- \n autoantibody") +
  ylim(c(0, 1)) +
  #xlim(c(0, 1)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = 0.1,
                size = 0.7) +
  #scale_x_discrete(labels = c("<=1", "2", "3", "4", "5", "6", "7", "8", "9", ">=10")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none")

#D
dec_SR_50_nm1_antis <- data.frame(y = as.numeric(SR_50_mD_data$SRoutcome),
                                  pred = SR_50_mD_data$m1_pp,
                                  pos_anti = SR_50_mD_data$pos_anti,
                                  ScoreD = SR_50_mD_data$ScoreD) %>%
  group_by(pos_anti, ScoreD) %>%
  mutate(prob_obs = sum(y) / n(),
         obs = sum(y),
         n_group = n(),
         mnpred = mean(pred),
         lower = lapply(sum(y), prop.test, n = n()),
         upper = sapply(lower, function(x) x$conf.int[2]),
         lower = sapply(lower, function(x) x$conf.int[1]))

cal_antis_50 <- ggplot(dec_SR_50_nm1_antis, aes(y = prob_obs,
                                                x = ScoreD,
                                                color = factor(pos_anti))) +
  geom_point(aes(alpha = 0.5), size = 3) +
  xlab("StartRight score") +
  ylab("Observed proportion \n of T1D") +
  #labs(color = "Antibody Status") +
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylim(c(0, 1)) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 11.5) +
  #xlim(c(0, 1)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper,
                    color = factor(pos_anti),
                    alpha = 0.5),
                width = 0.1,
                size = 0.7) +
  scale_x_continuous(breaks=seq(0,20,1)) +
  # geom_hline(yintercept = 0.1) +
  # geom_hline(yintercept = 0.9) +
  # geom_vline(xintercept = 4.5) +
  # geom_vline(xintercept = 10.5) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none")
##merge plots -----------------------------------------------------------------------

model_display_item1 <- patchwork::wrap_plots(
  title_m1_start,
  cal_outcome,
  cal_antis
) + patchwork::plot_layout(design = DESIGN_abc)
model_display_item2 <- patchwork::wrap_plots(
  title_m1_prime,
  cal_outcome_50,
  cal_antis_50
) + patchwork::plot_layout(design = DESIGN_abc)

pdf("figures/Supp_Figure27_props_no_outcome6.pdf", height = 10, width = 13)
model_display_item <- patchwork::wrap_plots(
  model_display_item1,
  model_display_item2,
  ncol = 1, nrow = 2
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "", "C", "D")))

print(model_display_item)
dev.off()
ggsave("figures/Supp_Figure27_props_no_outcome6.jpeg", model_display_item, height = 10, width = 13)
#Thresholds -----------------------------------------------------------------------
##StartRight --------------------------------------------------------------------------
SR_mD_data %>%
  filter(m1_pp < 0.05) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_mD_data %>%
  filter(m1_pp > 0.95) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  filter(m1_pp < 0.1) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_mD_data %>%
  filter(m1_pp > 0.9) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  filter(ScoreD < 7) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  filter(ScoreD < 5) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    max_prob = max(m1_pp)
  )

SR_mD_data %>%
  filter(ScoreD < 7) %>%
  #group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    median_prob = median(m1_pp),
    max_prob = max(m1_pp)
  )

SR_mD_data %>%
  filter(ScoreD < 6) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    max_prob = max(m1_pp)
  )

SR_mD_data %>%
  filter(ScoreD < 6) %>%
  #group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    median_prob = median(m1_pp),
    max_prob = max(m1_pp)
  )

SR_mD_data %>%
  filter(ScoreD < 7) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    max_prob = max(m1_pp)
  )

SR_mD_data %>%
  filter(ScoreD < 7) %>%
  #group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    median_prob = median(m1_pp),
    max_prob = max(m1_pp)
  )

SR_mD_data %>%
  filter(ScoreD >= 7 & ScoreD <= 11) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  filter(ScoreD >= 7 & ScoreD <= 11) %>%
  mutate(pos_anti = ifelse(num_anti == 1, "1 anti", ifelse(num_anti > 1, "2+ antis", "no antis"))) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  filter(ScoreD > 10) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  filter(ScoreD > 5) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
##StartRight Prime ----------------------------------------------------------------------
SR_50_mD_data %>%
  filter(m1_pp < 0.05) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_50_mD_data %>%
  filter(m1_pp > 0.95) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  filter(m1_pp < 0.1) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_50_mD_data %>%
  filter(m1_pp > 0.9) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  filter(ScoreD < 7) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  filter(ScoreD < 5) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    max_prob = max(m1_pp)
  )

SR_50_mD_data %>%
  filter(ScoreD < 5) %>%
  #group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    median_prob = median(m1_pp),
    max_prob = max(m1_pp)
  )

SR_50_mD_data %>%
  filter(ScoreD < 6) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    max_prob = max(m1_pp)
  )

SR_50_mD_data %>%
  filter(ScoreD < 6) %>%
  #group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    median_prob = median(m1_pp),
    max_prob = max(m1_pp)
  )

SR_50_mD_data %>%
  filter(ScoreD < 7) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    max_prob = max(m1_pp)
  )

SR_50_mD_data %>%
  filter(ScoreD < 7) %>%
  #group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100,
    mean_prob = mean(m1_pp),
    median_prob = median(m1_pp),
    max_prob = max(m1_pp)
  )

SR_50_mD_data %>%
  filter(ScoreD >= 5 & ScoreD <= 10) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  filter(ScoreD >= 5 & ScoreD <= 10) %>%
  mutate(pos_anti = ifelse(num_anti == 1, "1 anti", ifelse(num_anti > 1, "2+ antis", "no antis"))) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  filter(ScoreD > 10) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  filter(ScoreD > 5) %>%
  group_by(pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

#Score categories ---------------------------------------------------------------------
SR_mD_data %>%
  group_by(SR_score_cat1, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_mD_data %>%
  group_by(SR_score_cat2, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_mD_data %>%
  group_by(SR_score_cat3, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_mD_data %>%
  group_by(SR_score_cat4, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_50_mD_data %>%
  group_by(SR_score_cat1, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_50_mD_data %>%
  group_by(SR_score_cat2, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_50_mD_data %>%
  group_by(SR_score_cat3, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_50_mD_data %>%
  group_by(SR_score_cat4, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  mutate(SR_score_cat = ifelse(ScoreD <= 5, "<=5", ">5")) %>%
  group_by(SR_score_cat, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  mutate(SR_score_cat = ifelse(ScoreD <= 5, "<=5", ">5")) %>%
  group_by(SR_score_cat, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  mutate(SR_score_cat = ifelse(ScoreD <= 6, "<=6", ">6")) %>%
  group_by(SR_score_cat, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  mutate(SR_score_cat = ifelse(ScoreD <= 6, "<=6", ">6")) %>%
  group_by(SR_score_cat, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  group_by(model_cat, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  group_by(model_cat, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_mD_data %>%
  mutate(model_cat1 = ifelse(m1_pp < 0.1, "<10%", ">=10%")) %>%
  group_by(model_cat1, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_mD_data %>%
  mutate(model_cat1 = ifelse(m1_pp < 0.1, "<10%", ">=10%")) %>%
  group_by(model_cat1, pos_anti) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

#Using 5 cut off -------------------------------------------------------------------------
##StartRight ---------------------------------------------------------------------------
# dec_SR_nm1 <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
#                          pred = SR_mD_data$m1_pp,
#                          pos_anti = SR_mD_data$pos_anti,
#                          ScoreD = SR_mD_data$ScoreD) %>%
#   group_by(ScoreD) %>%
#   mutate(mnpred = mean(pred),
#          obs = sum(y),
#          n_group = n(),
#          #score = mean(pred),
#          lower = quantile(pred, prob = 0.025),
#          upper = quantile(pred, prob = 0.975))
# #A
# cal_outcome <- ggplot(dec_SR_nm1, aes(y = mnpred,
#                                       x = ScoreD,
#                                       # color = factor(y,
#                                       #                levels = c(1, 0),
# )) +
#   geom_point(aes(alpha = 0.5),
#              size = 3) +
#   xlab("Clinical features score") +
#   ylab("Mean predicted probability \n of clinical features model") +
#   labs(color = "Study outcome") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   #scale_color_manual(values = c("#5a8be2", "#f1b955")) +
#   ylim(c(0, 1)) +
#   geom_hline(yintercept = 0.1) +
#   geom_hline(yintercept = 0.9) +
#   geom_vline(xintercept = 5.5) +
#   geom_vline(xintercept = 11.5) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     #color = factor(y),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,20,1)) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")
#
# dec_SR_nm1_antis <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
#                          pred = SR_mD_data$m1_pp,
#                          pos_anti = SR_mD_data$pos_anti,
#                          ScoreD = SR_mD_data$ScoreD) %>%
#   group_by(pos_anti, ScoreD) %>%
#   mutate(prob_obs = sum(y) / n(),
#          obs = sum(y),
#          n_group = n(),
#          mnpred = mean(pred),
#          lower = lapply(sum(y), prop.test, n = n()),
#          upper = sapply(lower, function(x) x$conf.int[2]),
#          lower = sapply(lower, function(x) x$conf.int[1]))
# #B
# cal_antis <- ggplot(dec_SR_nm1_antis , aes(y = prob_obs,
#                                     x = ScoreD,
#                                     color = factor(pos_anti))) +
#   geom_point(aes(alpha = 0.5), size = 3) +
#   xlab("Clinical features score") +
#   ylab("Observed proportion \n of T1D") +
#   #labs(color = "Antibody Status") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   ylim(c(0, 1)) +
#   geom_vline(xintercept = 5.5) +
#   geom_vline(xintercept = 11.5) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     color = factor(pos_anti),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,20,1)) +
#   # geom_hline(yintercept = 0.1) +
#   # geom_hline(yintercept = 0.9) +
#   # geom_vline(xintercept = 4.5) +
#   # geom_vline(xintercept = 10.5) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")
# ##StartRight Prime---------------------------------------------------------------------------
# dec_50_SR_nm1 <- data.frame(y = as.numeric(SR_50_mD_data$SRoutcome),
#                          pred = SR_50_mD_data$m1_pp,
#                          pos_anti = SR_50_mD_data$pos_anti,
#                          ScoreD = SR_50_mD_data$ScoreD) %>%
#   group_by(ScoreD) %>%
#   mutate(mnpred = mean(pred),
#          obs = sum(y),
#          n_group = n(),
#          #score = mean(pred),
#          lower = quantile(pred, prob = 0.025),
#          upper = quantile(pred, prob = 0.975))
# #C
# cal_outcome_50 <- ggplot(dec_50_SR_nm1, aes(y = mnpred,
#                                       x = ScoreD,
#                                       # color = factor(y,
#                                       #                levels = c(1, 0),
# )) +
#   geom_point(aes(alpha = 0.5),
#              size = 3) +
#   xlab("Clinical features score") +
#   ylab("Mean predicted probability \n of clinical features model") +
#   labs(color = "Study outcome") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   #scale_color_manual(values = c("#5a8be2", "#f1b955")) +
#   ylim(c(0, 1)) +
#   geom_hline(yintercept = 0.1) +
#   geom_hline(yintercept = 0.9) +
#   geom_vline(xintercept = 5.5) +
#   geom_vline(xintercept = 11.5) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     #color = factor(y),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,20,1)) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")
# #D
# dec_SR_50_nm1_antis <- data.frame(y = as.numeric(SR_50_mD_data$SRoutcome),
#                                pred = SR_50_mD_data$m1_pp,
#                                pos_anti = SR_50_mD_data$pos_anti,
#                                ScoreD = SR_50_mD_data$ScoreD) %>%
#   group_by(pos_anti, ScoreD) %>%
#   mutate(prob_obs = sum(y) / n(),
#          obs = sum(y),
#          n_group = n(),
#          mnpred = mean(pred),
#          lower = lapply(sum(y), prop.test, n = n()),
#          upper = sapply(lower, function(x) x$conf.int[2]),
#          lower = sapply(lower, function(x) x$conf.int[1]))
# cal_antis_50 <- ggplot(dec_SR_50_nm1_antis, aes(y = prob_obs,
#                                     x = ScoreD,
#                                     color = factor(pos_anti))) +
#   geom_point(aes(alpha = 0.5), size = 3) +
#   xlab("Clinical features score") +
#   ylab("Observed proportion \n of T1D") +
#   #labs(color = "Antibody Status") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   ylim(c(0, 1)) +
#   geom_vline(xintercept = 5.5) +
#   geom_vline(xintercept = 11.5) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     color = factor(pos_anti),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,20,1)) +
#   # geom_hline(yintercept = 0.1) +
#   # geom_hline(yintercept = 0.9) +
#   # geom_vline(xintercept = 4.5) +
#   # geom_vline(xintercept = 10.5) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")
# ##merge plots -----------------------------------------------------------------------
#
# model_display_item1 <- patchwork::wrap_plots(
#   title_m1_start,
#   cal_outcome,
#   cal_antis
# ) + patchwork::plot_layout(design = DESIGN_abc)
# model_display_item2 <- patchwork::wrap_plots(
#   title_m1_prime,
#   cal_outcome_50,
#   cal_antis_50
# ) + patchwork::plot_layout(design = DESIGN_abc)
#
# pdf("figures/Supp_Figure27_props_no_outcome5.pdf", height = 10, width = 12)
# model_display_item <- patchwork::wrap_plots(
#   model_display_item1,
#   model_display_item2,
#   ncol = 1, nrow = 2
# ) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "", "C", "D")))
#
# print(model_display_item)
# dev.off()
# ggsave("figures/Supp_Figure27_props_no_outcome5.png", model_display_item, height = 10, width = 12)
#A
# cal_outcome <- ggplot(dec_SR_nm1, aes(y = mnpred,
#                                       x = ScoreD,
#                                       # color = factor(y,
#                                       #                levels = c(1, 0),
# )) +
#   geom_point(aes(alpha = 0.5),
#              size = 3) +
#
#   ylab("Mean predicted probability \n of clinical features model") +
#   labs(color = "Study outcome") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   #scale_color_manual(values = c("#5a8be2", "#f1b955")) +
#   ylim(c(0, 1)) +
#   geom_hline(yintercept = 0.1) +
#   geom_hline(yintercept = 0.9) +
#   geom_vline(xintercept = 6.5) +
#   geom_vline(xintercept = 11.5) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     #color = factor(y),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,20,1)) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")
#By SRoutcome ------------------------------------------------------------------------
# dec_SR_nm1 <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
#                          pred = SR_mD_data$m1_pp,
#                          pos_anti = SR_mD_data$pos_anti,
#                          ScoreD = SR_mD_data$ScoreD) %>%
#   group_by(
#     #y,
#     ScoreD) %>%
#   mutate(prob_obs = sum(y) / n(),
#          obs = sum(y),
#          n_group = n(),
#          mnpred = mean(pred),
#          lower = lapply(sum(y), prop.test, n = n()),
#          upper = sapply(lower, function(x) x$conf.int[2]),
#          lower = sapply(lower, function(x) x$conf.int[1]))
# cal_outcome <- ggplot(dec_SR_nm1, aes(y = prob_obs,
#                                       x = ScoreD,
#                                       color = factor(y))) +
#   geom_point(aes(alpha = 0.5),
#              size = 3) +
#   xlab("Clinical features score") +
#   ylab("Observed proportion of T1D") +
#   ylim(c(0, 1)) +
#   #xlim(c(0, 1)) +
#   labs(color = "Study outcome") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   scale_color_manual(values = c("#5a8be2", "#f1b955")) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     color = factor(y),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,13,1)) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")

# ##PDF ---------------------------------------------------------------------------------
# pdf("figures/Supp_Figure27_props.pdf", height = 7, width = 15)
# model_display_item <- patchwork::wrap_plots(
#   cal_outcome,
#   cal_antis,
#   ncol = 2, nrow = 1
# ) + patchwork::plot_annotation(tag_levels = list(c("A")))
# print(model_display_item)
# dev.off()
#Against predicted probability ---------------------------------------------------------
##By antistatus ------------------------------------------------------------------------
# dec_SR_nm1 <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
#                          pred = SR_mD_data$m1_pp,
#                          pos_anti = SR_mD_data$pos_anti,
#                          ScoreD = SR_mD_data$ScoreD) %>%
#   group_by(pos_anti, ScoreD) %>%
#   mutate(mnpred = mean(pred),
#          obs = sum(y),
#          n_group = n(),
#          #score = mean(pred),
#          lower = quantile(pred, prob = 0.025),
#          upper = quantile(pred, prob = 0.975))
# cal_antis <- ggplot(dec_SR_nm1, aes(y = mnpred,
#                                     x = ScoreD,
#                                     color = factor(pos_anti))) +
#   geom_point(aes(alpha = 0.5), size = 3) +
#   xlab("Clinical features score") +
#   ylab("Mean predicted probability \n of clinical features model") +
#   labs(color = "Antibody Status") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   ylim(c(0, 1)) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     color = factor(pos_anti),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,20,1)) +
#   geom_hline(yintercept = 0.1) +
#   geom_hline(yintercept = 0.9) +
#   geom_vline(xintercept = 4.5) +
#   geom_vline(xintercept = 10.5) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")
#
# ##By SRoutcome ------------------------------------------------------------------------
# dec_SR_nm1 <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
#                          pred = SR_mD_data$m1_pp,
#                          pos_anti = SR_mD_data$pos_anti,
#                          ScoreD = SR_mD_data$ScoreD) %>%
#   group_by(y, ScoreD) %>%
#   mutate(mnpred = mean(pred),
#          obs = sum(y),
#          n_group = n(),
#          #score = mean(pred),
#          lower = quantile(pred, prob = 0.025),
#          upper = quantile(pred, prob = 0.975))
# cal_outcome <- ggplot(dec_SR_nm1, aes(y = mnpred,
#                                       x = ScoreD,
#                                       color = factor(y,
#                                                      levels = c(1, 0),
#                                       ))) +
#   geom_point(aes(alpha = 0.5),
#              size = 3) +
#   xlab("Clinical features score") +
#   ylab("Mean predicted probability \n of clinical features model") +
#   labs(color = "Study outcome") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   scale_color_manual(values = c("#5a8be2", "#f1b955")) +
#   ylim(c(0, 1)) +
#   geom_hline(yintercept = 0.1) +
#   geom_hline(yintercept = 0.9) +
#   geom_vline(xintercept = 4.5) +
#   geom_vline(xintercept = 10.5) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     color = factor(y),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,20,1)) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")
#
# ##PDF ---------------------------------------------------------------------------------
# # pdf("figures/Supp_Figure27_probs.pdf", height = 7, width = 15)
# # model_display_item <- patchwork::wrap_plots(
# #   cal_outcome,
# #   cal_antis,
# #   ncol = 2, nrow = 1
# # ) + patchwork::plot_annotation(tag_levels = list(c("A", "B")))
# # print(model_display_item)
# # dev.off()
#
# #Against propotion of type 1 ------------------------------------------------------
# #By antistatus ------------------------------------------------------------------------
# dec_SR_nm1 <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
#                          pred = SR_mD_data$m1_pp,
#                          pos_anti = SR_mD_data$pos_anti,
#                          ScoreD = SR_mD_data$ScoreD) %>%
#   group_by(pos_anti, ScoreD) %>%
#   mutate(prob_obs = sum(y) / n(),
#          obs = sum(y),
#          n_group = n(),
#          mnpred = mean(pred),
#          lower = lapply(sum(y), prop.test, n = n()),
#          upper = sapply(lower, function(x) x$conf.int[2]),
#          lower = sapply(lower, function(x) x$conf.int[1]))
# cal_antis <- ggplot(dec_SR_nm1, aes(y = prob_obs,
#                                     x = ScoreD,
#                                     color = factor(pos_anti))) +
#   geom_point(aes(alpha = 0.5), size = 3) +
#   xlab("Clinical features score") +
#   ylab("Observed proportion of T1D") +
#   #labs(color = "Antibody Status") +
#   #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   ylim(c(0, 1)) +
#   geom_vline(xintercept = 4.5) +
#   geom_vline(xintercept = 10.5) +
#   #xlim(c(0, 1)) +
#   geom_errorbar(aes(ymin = lower,
#                     ymax = upper,
#                     color = factor(pos_anti),
#                     alpha = 0.5),
#                 width = 0.1,
#                 size = 0.7) +
#   scale_x_continuous(breaks=seq(0,20,1)) +
#   # geom_hline(yintercept = 0.1) +
#   # geom_hline(yintercept = 0.9) +
#   # geom_vline(xintercept = 4.5) +
#   # geom_vline(xintercept = 10.5) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none")
