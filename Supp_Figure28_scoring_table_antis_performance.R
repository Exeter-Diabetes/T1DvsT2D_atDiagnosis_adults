#####################################################################################
#Scoring table + antibodies performance
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
load("m2.RData")

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
##StartRight Prime --------------------------------------------------------------------
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
         famhisnoninsdiab, ethnicity, num_anti) %>%
  mutate(
    m1_pp = predict(m1, SR_m12_data, type = "response"),
    m2_pp = predict(m2, SR_m12_data, type = "response"),
    agedx = case_when(
      AgeatDiagnosis <=24 ~ 2,
      AgeatDiagnosis <=34 ~ 2,
      AgeatDiagnosis <=44 ~ 1,
      AgeatDiagnosis >44 ~ 0
    ),
    bmi = case_when(
      bmi_model < 25 ~ 4,
      bmi_model < 35 ~ 2,
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
    num_antis = case_when(
      num_anti == 0 ~ 0,
      num_anti == 1 ~ 2,
      num_anti == 2 ~ 3,
      num_anti == 3 ~ 4
    ),
    pos_anti = ifelse(num_anti >= 1, 1, 0))

SR_mD_data <- SR_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, m2_pp,agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, dka, pardm, eth, num_antis, pos_anti) %>%
  mutate(ScoreD = select(., agedx:num_antis) %>% rowSums(na.rm = TRUE))

varlist = c("ScoreD")
model_continuous(varlist,
                 dataset = SR_mD_data,
                 outcome = "SRoutcome",
                 saving_name = "tables/Supp_Figure28",
                 complete_case = TRUE,
                 plots = TRUE,
                 manual_plotting = TRUE,
                 manual_plot_name = "score_SR")

SR_mD_data <- SR_mD_data %>%
  mutate(
    score_cat = ifelse(ScoreD >= 11, ">=11", ifelse(ScoreD <= 4, "<=4", as.character(ScoreD)))
  )
#StartRight Prime----------------------------------------------------------------------------------
SR_50_mD_data <- SR_50_m12_data %>%
  select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, ethnicity, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, num_anti) %>%
  mutate(
    m1_pp = predict(m1, SR_50_m12_data, type = "response"),
    agedx = case_when(
      AgeatDiagnosis <=24 ~ 2,
      AgeatDiagnosis <=34 ~ 2,
      AgeatDiagnosis <=44 ~ 1,
      AgeatDiagnosis >44 ~ 0
    ),
    bmi = case_when(
      bmi_model < 25 ~ 4,
      bmi_model < 35 ~ 2,
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
    eth = ifelse(ethnicity == "White", 0, -1),
    dka = ifelse(DKA == "1", 1, 0),
    pardm = ifelse(famhisnoninsdiab == "No", 1, 0),
    num_antis = case_when(
      num_anti == 0 ~ 0,
      num_anti == 1 ~ 2,
      num_anti == 2 ~ 3,
      num_anti == 3 ~ 4
    ),
    pos_anti = ifelse(num_anti >= 1, 1, 0))

SR_50_mD_data <- SR_50_mD_data %>%
  select(Study_ID, SRoutcome, robust_outcome, m1_pp, agedx, bmi, hba1c, sex,
         osmo, auto, weightloss, dka, pardm, eth, num_antis, pos_anti) %>%
  mutate(ScoreD = select(., agedx:num_antis) %>% rowSums(na.rm = TRUE))

varlist = c("ScoreD")
model_continuous(varlist,
                 dataset = SR_50_mD_data,
                 outcome = "SRoutcome",
                 saving_name = "tables/Supp_Figure28_Prime",
                 complete_case = TRUE,
                 plots = TRUE,
                 manual_plotting = TRUE,
                 manual_plot_name = "score_prime")

SR_50_mD_data <- SR_50_mD_data %>%
  mutate(
    score_cat = ifelse(ScoreD >= 11, ">=11", ifelse(ScoreD <= 4, "<=4", as.character(ScoreD)))
  )
#Plot ----------------------------------------------------------------------------
DESIGN_abc <- "
11111#
  223344
  223344
  223344
  223344
  223344
"
##Calibration prep
dec_SR_nm1 <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
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


m12_n <- nrow(SR_mD_data)
n12_T1D <- SR_mD_data %>%
  filter(SRoutcome == 1) %>%
  nrow()
model1_text <- paste0("StartRight (n=",m12_n,"; Type 1=", n12_T1D, ")")
model2_text <- paste0("Model 2: Clinical features + number of positive antibodies (n=",m12_n,"; T1D=", n12_T1D, ")")

dat_text_score_SR <- dat_text_score_SR %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_score_prime <- dat_text_score_prime %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))

score_SR_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 1.5)
  ),
  #roc_plot
  roc_curves_score_SR %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_score_SR,
      mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
      size = 6,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    ),
  #boxplot
  ggplot(SR_mD_data, aes(x=factor(SRoutcome,
                                  levels = c("1","0"),
                                  labels = c("Type 1", "Type 2")),
                         y = ScoreD)) +
    geom_violin(aes(fill = factor(SRoutcome,
                                  levels = c("1","0"),
                                  labels = c("Type 1", "Type 2")), alpha = 0.2)) +
    geom_boxplot(aes(fill = factor(SRoutcome,
                                   levels = c("1","0"),
                                   labels = c("Type 1", "Type 2"))),width = .15,
                 outlier.size = 1) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    xlab("Type of diabetes")+
    ylab("StartRight Score") +
    scale_y_continuous(breaks=seq(0,20,1)) +
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size = 20)),
  #calibration plot
  ggplot(dec_SR_nm1, aes(y = prob_obs,
                         x = factor(ScoreD,
                                    levels = c("<=4", "5", "6", "7", "8", "9", "10", ">=11"),
                                    labels = c("0-4", "5", "6", "7", "8", "9", "10", "11-17")))) +
    geom_point(size = 3) +
    xlab("StartRight Score") +
    ylab("Observed proportion \n of T1D") +
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
) + patchwork::plot_layout(design = DESIGN_abc)

#Plot ------------------------------------------------------------------------------
m3_n <- nrow(SR_50_mD_data)
n3_T1D <- SR_50_mD_data %>%
  filter(SRoutcome == 1) %>%
  nrow()
model2_text <- paste0("StartRight Prime (n=",m3_n,"; Type 1=", n3_T1D, ")")

##Calibration prep
dec_PRIME_nm1 <- data.frame(y = as.numeric(SR_50_mD_data$SRoutcome),
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

score_prime_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model2_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 1.27)
  ),
  #roc_plot
  roc_curves_score_prime %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_score_prime,
      mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
      size = 6,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    ),
  #boxplot
  ggplot(SR_50_mD_data, aes(x=factor(SRoutcome,
                                     levels = c("1","0"),
                                     labels = c("Type 1", "Type 2")),
                            y = ScoreD)) +
    geom_violin(aes(fill = factor(SRoutcome,
                                  levels = c("1","0"),
                                  labels = c("Type 1", "Type 2")), alpha = 0.2)) +
    geom_boxplot(aes(fill = factor(SRoutcome,
                                   levels = c("1","0"),
                                   labels = c("Type 1", "Type 2"))),width = .15,
                 outlier.size = 1) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    scale_y_continuous(breaks=seq(0,20,1)) +
    xlab("Type of diabetes")+
    ylab("StartRight Score") +
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size = 20)),
  #calibration plot
  ggplot(dec_PRIME_nm1, aes(y = prob_obs,
                            x = factor(ScoreD,
                                       levels = c("<=4", "5", "6", "7", "8", "9", "10", ">=11"),
                                       labels = c("0-4", "5", "6", "7", "8", "9", "10", "11-17")))) +
    geom_point(size = 3) +
    xlab("StartRight Score") +
    ylab("Observed proportion \n of T1D") +
    ylim(c(0, 1)) +
    #xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper
    ),
    width = 0.1,
    size = 0.7) +
    #scale_x_discrete() +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none")
) + patchwork::plot_layout(design = DESIGN_abc)

#Both plots as PDF ---------------------------------------------------------------
pdf("figures/Supp_Figure28_score_plusantis_plots.pdf", height = 10, width = 16)
model_display_item <- patchwork::wrap_plots(
  score_SR_plots_abc,
  score_prime_plots_abc,
  #m3_plots_abc,
  ncol = 1, nrow = 2
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
                                                   "", "D", "E", "F")))
print(model_display_item)
dev.off()
ggsave("figures/Supp_Figure28_score_antis_plots.jpeg", model_display_item, height = 10, width = 16)


#####################################################################################
#Exploring comparison to clinical features + antibodies model
#By antistatus ------------------------------------------------------------------------
dec_SR_nm1 <- data.frame(y = as.numeric(SR_mD_data$SRoutcome),
                         pred = SR_mD_data$m2_pp,
                         pos_anti = SR_mD_data$pos_anti,
                         ScoreD = SR_mD_data$ScoreD) %>%
  group_by(ScoreD) %>%
  mutate(mnpred = mean(pred),
         obs = sum(y),
         n_group = n(),
         #score = mean(pred),
         lower = quantile(pred, prob = 0.025),
         upper = quantile(pred, prob = 0.975))
cal_antis <- ggplot(dec_SR_nm1, aes(y = mnpred,
                                    x = ScoreD#,
                                    #color = factor(pos_anti)
                                    )) +
  geom_point(size = 3) +
  xlab("Clinical features score") +
  ylab("Mean predicted probability \n of clinical features model") +
  #labs(color = "Antibody Status") +
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylim(c(0, 1)) +
  #xlim(c(0, 1)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper#,
                    #color = factor(pos_anti)
                    ),
                width = 0.1,
                size = 0.7) +
  scale_x_continuous(breaks=seq(0,17,1)) +
  geom_hline(yintercept = 0.5) +
  #geom_hline(yintercept = 0.9) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none")

##PDF ---------------------------------------------------------------------------------
pdf("figures/Supp_score2_bymodel2.pdf", height = 10, width = 15)
print(cal_antis)
dev.off()
