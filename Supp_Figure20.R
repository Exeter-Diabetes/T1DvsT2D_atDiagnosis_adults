#####################################################################################

#Paper figures: Subgroup analysis

#To get the two final paper complete case model datasets
#And do subgrouo analysis 

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
#library(naniar)
library(writexl)
#library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/data/SR_ro_cc_1_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
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
### Models3 ------------------------------------------------------------------------
#Continuous variables 
varlist_3 = c("AgeatDiagnosis", 
              "bmi_model", 
              "HbA1c_at_diagnosis_v1", 
              "T1DGRS2_z"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat_3 = c(
  "Gender_v1",
  "DKA", 
  "Unintentional_weight_loss_v1", 
  "autoimmune", 
  "osmotic", 
  "famhisnoninsdiab",
  #"famhisauto",
  "num_anti"
)
all_vars_3 <- c(varlist_3, varlist_cat_3)
##Make complete case datasets ----------------------------------------------------------
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
  drop_na(all_of(all_vars_12)) %>%
  mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
#Model3


###Model performance plot -------------------------------------------------------------
####Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m1, 
           test_data = SR_m12_data, 
           outcome = "anti_pos",
           saving_name = "03_05_sm1_antipos", 
           manual_plotting = TRUE, 
           manual_plot_name = "m1")
###Calibration prep for model 1 by anti_status ----------------------------------------
SR_m12_data <- SR_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0, 
                              "0", 
                              "1"),
         m1_pp = predict(m1, SR_m12_data, type = "response"))
brks_SR_nm1 <- quantile(SR_m12_data$m1_pp, 
                        probs = seq(0, 1, by = 0.1), 
                        na.rm = TRUE)
brks_SR_nm1[1] <- 0.99 * brks_SR_nm1[1]
brks_SR_nm1[length(brks_SR_nm1)] <- 1.1 * brks_SR_nm1[length(brks_SR_nm1)]
dec_SR_nm1 <- cut(
  SR_m12_data$m1_pp, 
  breaks = brks_SR_nm1,
  include_lowest = TRUE
)

dec_SR_nm1 <- data.frame(y = SR_m12_data$SRoutcome, 
                         pred = SR_m12_data$m1_pp, 
                         anti_status = SR_m12_data$anti_status, 
                         dec = dec_SR_nm1) %>%
  group_by(anti_status, dec) %>%
  mutate(prob_obs = sum(y) / n(), 
         obs = sum(y),
         n_group = n(),
         mnpred = mean(pred),
         lower = lapply(sum(y), prop.test, n = n()), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1]))


##PDF ---------------------------------------------------------------------------------
pdf("figures/Supp_Figure20.pdf", height = 10, width = 22)
model_display_item <- patchwork::wrap_plots(
  #calibration plot for model 1 predict antipos
  ggplot(cal_prep_m1, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability in each decile") +
    ylab("Proportion of Antibody \n positive in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw(),
  #Calibration plot for model 1 split by antibody positivity
  ggplot(dec_SR_nm1, aes(x = mnpred, 
                         y = prob_obs, 
                         color = factor(anti_status))) +
    geom_point() +
    xlab("Mean predicted probability of clinical features model") +
    ylab("Proportion of T1D in each decile") +
    labs(color = "Antibody Status") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, 
                      ymax = upper, 
                      color = factor(anti_status))) + 
    theme_bw() + 
    theme(
      legend.position = "bottom"
    ),
  ncol = 2, nrow = 1
) + patchwork::plot_annotation(tag_levels = list(c("A", "B")))
print(model_display_item)
dev.off()

## Threshold analysis -------------------------------------------------------------------
SR_m12_data <- SR_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0, 
                              "0", 
                              "1"),
         m1_pp = predict(m1, SR_m12_data, type = "response"))

SR_m12_data %>%
  filter(m1_pp < 0.05) %>%
  group_by(anti_status) %>%
  summarise(
    n = n(), 
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
SR_m12_data %>%
  filter(m1_pp > 0.95) %>%
  group_by(anti_status) %>%
  summarise(
    n = n(), 
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_m12_data %>%
  filter(m1_pp < 0.1) %>%
  group_by(anti_status) %>%
  summarise(
    n = n(), 
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_m12_data %>%
  filter(m1_pp > 0.9) %>%
  group_by(anti_status) %>%
  summarise(
    n = n(), 
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )