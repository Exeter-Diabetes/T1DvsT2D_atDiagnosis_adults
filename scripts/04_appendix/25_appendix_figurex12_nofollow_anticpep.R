#####################################################################################
# Supplementary Figure 11
# StartRight and StartRight Prime dataset: lost to follow up data vs SR and SR prime
# Primary Outcome: T1DGRS
#####################################################################################

# Libraries -------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(writexl)

# Load functions --------------------------------------------------------------------
source("functions/model_info2.R")
# Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_noSRout_15_10_2026.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_noSRout_15_10_2026.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("m1.RData")

# User-defined function -------------------------------------------------------------
build_row <- function(title_grob, roc, dat_text, violin_data, pred_prob_name, cal_prep) {
  p_roc <- roc %>%
    ggplot(aes(x = 1 - specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    geom_label(
      data = dat_text,
      aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
      size = 6,
      label.r = unit(0, "pt"),
      label.padding = unit(0.4, "lines")
    ) +
    theme(panel.spacing.x = unit(1.5, "lines"), text = element_text(size = 20))

  p_violin <- ggplot(
    violin_data,
    aes(
      x = factor(outcome, levels = c("1", "0"), labels = c("Type 1", "Type 2")),
      y = !!rlang::sym(pred_prob_name)
    )
  ) +
    geom_violin(aes(fill = outcome), alpha = 0.2) +
    geom_boxplot(aes(fill = outcome), width = .15, outlier.size = 1) +
    scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
    xlab("Type of diabetes") +
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw() +
    theme(legend.position = "none", text = element_text(size = 20))

  p_cal <- ggplot(cal_prep, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) +
    xlim(c(0, 1)) +
    geom_smooth(method = "loess", span = 1, se = FALSE, colour = "blue", linewidth = 0.75) +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    theme_bw() +
    theme(text = element_text(size = 20))

  content <- patchwork::wrap_plots(p_roc, p_violin, p_cal, ncol = 3)
  patchwork::wrap_plots(title_grob, content, ncol = 1, heights = c(0.12, 1))
}
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
  "Eth_4cat",
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
  "Eth_4cat",
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
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat),
         outcome = ifelse(num_anti >= 2 | c_peptide_v1 < 200, 1, ifelse(num_anti == 0 & c_peptide_v1 >=600, 0, NA))
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)
#Model12
SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3), outcome) %>%
  mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
#Model3

#StartRight Prime -------------------------------------------------------------------
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat),
         outcome = ifelse(num_anti >= 2 | c_peptide_v1 < 200, 1, ifelse(num_anti == 0 & c_peptide_v1 >=600, 0, NA))
  )

SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$num_anti <- as.character(SR_50_SRout_ccc$num_anti)
#Model12
# SR_50_m12_data <- SR_50_SRout_ccc %>%
#   drop_na(all_of(all_vars_12))
#Model12
SR_50_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12), outcome) %>%
  mutate(anti_pos = ifelse(num_anti == "0", 0, 1))

# Prepare complete case datasets ----------------------------------------------------
#StartRight
SR_noSRout <- SR_noSRout %>%
  mutate(
    bmi_model = ifelse(is.na(bmi_diag), bmi_calc_v1, bmi_diag),
    famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat),
    anti_pos = ifelse(num_anti == "0", 0, 1),
    outcome = ifelse(num_anti >= 2 | c_peptide_v1 < 200, 1, ifelse(num_anti == 0 & c_peptide_v1 >=600, 0, NA))
  )

#SR_noSRout$SRoutcome <- as.numeric(SR_noSRout$SRoutcome)
SR_noSRout$DKA <- as.character(SR_noSRout$DKA)
SR_noSRout$osmotic <- as.character(SR_noSRout$osmotic)
SR_noSRout$autoimmune <- as.character(SR_noSRout$autoimmune)

SR_no_m12_data <- SR_noSRout %>%
  drop_na(all_of(all_vars_12), outcome)
#StartRight Prime
SR_50_noSRout <- SR_50_noSRout %>%
  mutate(
    bmi_model = ifelse(is.na(bmi_diag), bmi_calc_v1, bmi_diag),
    famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat),
    anti_pos = ifelse(num_anti == "0", 0, 1),
    outcome = ifelse(num_anti >= 2 | c_peptide_v1 < 200, 1, ifelse(num_anti == 0 & c_peptide_v1 >=600, 0, NA))
  )

#SR_50_noSRout$SRoutcome <- as.numeric(SR_50_noSRout$SRoutcome)
SR_50_noSRout$DKA <- as.character(SR_50_noSRout$DKA)
SR_50_noSRout$osmotic <- as.character(SR_50_noSRout$osmotic)
SR_50_noSRout$autoimmune <- as.character(SR_50_noSRout$autoimmune)

SR_no_50_m12_data <- SR_50_noSRout %>%
  drop_na(all_of(all_vars_12), outcome)
#--------------------------------------------------------------------------------------------------------
#Get predicted probabilities ---------------------------------------------------------
#StartRight
###Included
SR_m12_data <- prepare_prediction_data(SR_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_m12_data<- SR_m12_data %>%
  mutate(m1_pp = plogis(linpred))
###Excluded
SR_no_m12_data <- prepare_prediction_data(SR_no_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_no_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_no_m12_data<- SR_no_m12_data %>%
  mutate(m1_pp = plogis(linpred))

#StartRight Prime
###Included
SR_50_m12_data <- prepare_prediction_data(SR_50_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_50_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_50_m12_data<- SR_50_m12_data %>%
  mutate(m1_pp = plogis(linpred))
###Excluded
SR_no_50_m12_data <- prepare_prediction_data(SR_no_50_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_no_50_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_no_50_m12_data<- SR_no_50_m12_data %>%
  mutate(m1_pp = plogis(linpred))

# Get model information for plotting ---------------------------------------------
##StartRight -------------------------------------------------------------------------
##Included
model_info(test_data = SR_m12_data,
           outcome = "outcome",
           pred_prob_var = "m1_pp",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m1_SR_with")



##Excluded
model_info(test_data = SR_no_m12_data,
           outcome = "outcome",
           pred_prob_var = "m1_pp",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m1_SR_without")



#StartRight Prime -------------------------------------------------------------------
##Included
model_info(test_data = SR_50_m12_data,
           outcome = "outcome",
           pred_prob_var = "m1_pp",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m1_SRP_with")
##Excluded
model_info(test_data = SR_no_50_m12_data,
           outcome = "outcome",
           pred_prob_var = "m1_pp",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m1_SRP_without")

## --- Compute dataset counts for model headings ----------------------------
m12_n_start <- nrow(SR_m12_data)
n12_T1D_start <- SR_m12_data %>%
  filter(outcome == 1) %>%
  nrow()
m12_n_prime <- nrow(SR_50_m12_data)
n12_T1D_prime <- SR_50_m12_data %>%
  filter(outcome == 1) %>%
  nrow()
m12_n_start_no <- nrow(SR_no_m12_data)
m12_n_prime_no <- nrow(SR_no_50_m12_data)

n12_T1D_start_no <- SR_no_m12_data %>%
  filter(outcome == 1) %>%
  nrow()
n12_T1D_prime_no <- SR_no_50_m12_data %>%
  filter(outcome == 1) %>%
  nrow()


SR_m12_data <- SR_m12_data %>%
  mutate(outcome = as.character(outcome))
SR_no_m12_data <- SR_no_m12_data %>%
  mutate(outcome = as.character(outcome))
SR_50_m12_data <- SR_50_m12_data %>%
  mutate(outcome = as.character(outcome))
SR_no_50_m12_data <- SR_no_50_m12_data %>%
  mutate(outcome = as.character(outcome))

dat_text_m1_SR_with <- dat_text_m1_SR_with %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_m1_SRP_with <- dat_text_m1_SRP_with %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))

dat_text_m1_SR_without <- dat_text_m1_SR_without %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_m1_SRP_without <- dat_text_m1_SRP_without %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
## --- Title grobs (right-aligned) -----------------------------------------
start_text <- paste0("StartRight: Clinical features only model")
prime_text <- paste0("StartRight Prime: Clinical features only model")

text_m1_SR_with <- paste0("Included: With 3-year C-peptide (n=", m12_n_start, "; Type 1=", n12_T1D_start, ")")
text_m1_SRP_with <- paste0("Included: With 3-year C-peptide (n=", m12_n_prime, "; Type 1=", n12_T1D_prime, ")")
text_m1_SR_without <- paste0("Excluded: Without 3-year C-peptide (n=", m12_n_start_no, "; Type 1=", n12_T1D_start_no,")")
text_m1_SRP_without <- paste0("Excluded: Without 3-year C-peptide (n=", m12_n_prime_no, "; Type 1=", n12_T1D_prime_no,")")

start_heading <- patchwork::wrap_elements(ggpubr::text_grob(start_text, face = "bold", size = 20, hjust = 1.38))
prime_heading <- patchwork::wrap_elements(ggpubr::text_grob(prime_text, face = "bold", size = 20, hjust = 1.2))

title_m1_SR_with <- patchwork::wrap_elements(ggpubr::text_grob(text_m1_SR_with, face = "bold", size = 20, hjust = 1.05))
title_m1_SRP_with <- patchwork::wrap_elements(ggpubr::text_grob(text_m1_SRP_with, face = "bold", size = 20, hjust = 1.07))
title_m1_SR_without <- patchwork::wrap_elements(ggpubr::text_grob(text_m1_SR_without, face = "bold", size = 20, hjust = 0.97))
title_m1_SRP_without <- patchwork::wrap_elements(ggpubr::text_grob(text_m1_SRP_without, face = "bold", size = 20, hjust = 1.025))


#Plot prep --------------------------------------------------------------------------
# Build plot row objects -----------------------------------------------------------
## StartRight
###Included
row_m1_SR_with <- build_row(
  title_m1_SR_with,
  roc_curves_m1_SR_with,
  dat_text_m1_SR_with,
  SR_m12_data,
  "pred_prob_m1_SR_with",
  cal_prep_m1_SR_with
)

###Excluded
row_m1_SR_without <- build_row(
  title_m1_SR_without,
  roc_curves_m1_SR_without,
  dat_text_m1_SR_without,
  SR_no_m12_data,
  "pred_prob_m1_SR_without",
  cal_prep_m1_SR_without
)

## StartRight Prime
### Included
row_m1_SRP_with <- build_row(
  title_m1_SRP_with,
  roc_curves_m1_SRP_with,
  dat_text_m1_SRP_with,
  SR_50_m12_data,
  "pred_prob_m1_SRP_with",
  cal_prep_m1_SRP_with
)

### Excluded
row_m1_SRP_without <- build_row(
  title_m1_SRP_without,
  roc_curves_m1_SRP_without,
  dat_text_m1_SRP_without,
  SR_no_50_m12_data,
  "pred_prob_m1_SRP_without",
  cal_prep_m1_SRP_without
)
# Combine rows into a single figure -------------------------------------------------
model_display_item <- patchwork::wrap_plots(
  start_heading,
  row_m1_SR_with,
  row_m1_SR_without,
  prime_heading,
  row_m1_SRP_with,
  row_m1_SRP_without,
  ncol = 1,
  heights = c(0.08, 1, 1, 0.08, 1, 1)
) + patchwork::plot_annotation(tag_levels = list(c("", "", "A", "B", "C",
                                                   "", "D", "E", "F",
                                                   "", "", "G", "H", "I",
                                                   "", "J", "K", "L")))
# Save outputs ----------------------------------------------------------------------
pdf("figures/Supp_Figurex12.pdf", height = 24, width = 16)
print(model_display_item)
dev.off()
ggsave("figures/Supp_Figurex12.jpeg", model_display_item, height = 24, width = 16)
