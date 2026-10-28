#####################################################################################
# Supplementary Figure 10
# StartRight dataset (18-50s)
# Clinically diagnosed T2D
# Primary Outcome: SRoutcome
#####################################################################################

# Libraries -------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(patchwork)
library(ggpubr)
library(pROC)

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
      x = factor(SRoutcome, levels = c("1", "0"), labels = c("Type 1", "Type 2")),
      y = !!rlang::sym(pred_prob_name)
    )
  ) +
    geom_violin(aes(fill = SRoutcome), alpha = 0.2) +
    geom_boxplot(aes(fill = SRoutcome), width = .15, outlier.size = 1) +
    scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
    xlab("Type of diabetes") +
    ylab("Predicted probability \n of T1D vs T2D") +
    ylim(c(0, 1)) +
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

# Load functions --------------------------------------------------------------------
source("functions/model_info2.R")

# Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
# load("m1.RData")
# load("m2.RData")
# load("m3.RData")
# load("m4.RData")

# Define variables for models -------------------------------------------------------
# Models 1 & 2
varlist_12 <- c("AgeatDiagnosis", "bmi_model", "HbA1c_at_diagnosis_v1")
varlist_cat_12 <- c("Eth_5cat", "Gender_v1", "DKA", "Unintentional_weight_loss_v1", "autoimmune",
                    "osmotic", "famhisnoninsdiab", "num_anti")
all_vars_12 <- c(varlist_12, varlist_cat_12)

# Models 3 & 4
varlist_3 <- c("AgeatDiagnosis", "bmi_model", "HbA1c_at_diagnosis_v1", "T1DGRS2_z")
varlist_cat_3 <- c("Eth_5cat", "Gender_v1", "DKA", "Unintentional_weight_loss_v1", "autoimmune",
                   "osmotic", "famhisnoninsdiab", "num_anti")
all_vars_3 <- c(varlist_3, varlist_cat_3)

# Prepare complete case datasets ----------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(
    bmi_model = ifelse(is.na(bmi_diag), bmi, bmi_diag),
    famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)

SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12))

SR_m3_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3))

# Subset data -----------------------------------------------------------------------
SR_ss_12 <- SR_m12_data %>%
  filter(clinical_diagnosis_v1 == "Type 2")

SR_ss_3 <- SR_m3_data %>%
  filter(clinical_diagnosis_v1 == "Type 2")

# Extract dataset statistics --------------------------------------------------------
m12_n <- nrow(SR_ss_12)
m3_n <- nrow(SR_ss_3)
n12_T2D <- SR_ss_12 %>%
  filter(SRoutcome == 1) %>%
  nrow()
n3_T2D <- SR_ss_3 %>%
  filter(SRoutcome == 1) %>%
  nrow()



# Get model information for plotting ---------------------------------------------
#Predicted probabilities -----------------------------------------------------------------
##StartRight --------------------------------------------------------------------------
###Model 1
SR_ss_12 <- prepare_prediction_data(SR_ss_12)
X <- model.matrix(
  m1_formula,
  data = SR_ss_12
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_ss_12<- SR_ss_12 %>%
  mutate(m1_pp = plogis(linpred))

###Model 2
SR_ss_12 <- prepare_prediction_data(SR_ss_12)
X <- model.matrix(
  m2_formula,
  data = SR_ss_12
)
colnames(X)
missing_cols <- setdiff(names(m2_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m2_beta), drop = FALSE]
linpred <- as.vector(X %*% m2_beta)

SR_ss_12<- SR_ss_12 %>%
  mutate(m2_pp = plogis(linpred))
###Model 3
SR_ss_3 <- prepare_prediction_data(SR_ss_3)
X <- model.matrix(
  m3_formula,
  data = SR_ss_3
)
colnames(X)
missing_cols <- setdiff(names(m3_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m3_beta), drop = FALSE]
linpred <- as.vector(X %*% m3_beta)

SR_ss_3<- SR_ss_3 %>%
  mutate(m3_pp = plogis(linpred))

###Model 4
SR_ss_3 <- prepare_prediction_data(SR_ss_3)
X <- model.matrix(
  m4_formula,
  data = SR_ss_3
)
colnames(X)
missing_cols <- setdiff(names(m4_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m4_beta), drop = FALSE]
linpred <- as.vector(X %*% m4_beta)

SR_ss_3<- SR_ss_3 %>%
  mutate(m4_pp = plogis(linpred))


###Model 1 ROW --------------------------------------------------------------------------
model_info(pred_prob_var = "m1_pp",
           test_data = SR_ss_12,
           outcome = "SRoutcome",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m1")

###Model 2 ROW --------------------------------------------------------------------------
model_info(pred_prob_var = "m2_pp",
           test_data = SR_ss_12,
           outcome = "SRoutcome",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m2")

###Model 3 ROW --------------------------------------------------------------------------
model_info(pred_prob_var = "m3_pp",
           test_data = SR_ss_3,
           outcome = "SRoutcome",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m3")
###Model 4 ROW --------------------------------------------------------------------------
model_info(pred_prob_var = "m4_pp",
           test_data = SR_ss_3,
           outcome = "SRoutcome",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m4")
# Make sure outcome variable is a character for plotting ----------------------------
SR_ss_12 <- SR_ss_12 %>%
  mutate(SRoutcome = as.character(SRoutcome))

SR_ss_3 <- SR_ss_3 %>%
  mutate(SRoutcome = as.character(SRoutcome))
# Prepare model titles --------------------------------------------------------------
model1_text <- paste0("Clinical features only model (n=", m12_n, "; Type 1=", n12_T2D, ")")
model2_text <- paste0("Clinical features + number of positive antibodies model (n=", m12_n, "; Type 1=", n12_T2D, ")")
model3_text <- paste0("Clinical features + number of positive antibodies + T1DGRS model (n=", m3_n, "; Type 1=", n3_T2D, ")")
model4_text <- paste0("Clinical features + T1DGRS model (n=", m3_n, "; Type 1=", n3_T2D, ")")

# Tweak AUCROC labels --------------------------------------------------------------
# Ensure all AUCs are to 2 decimal places
# Manual overwrite for model 1 AUC to ensure the 2 decimal "0" in "0.80"
dat_text_m1 <- dat_text_m1 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
# Default rounding for other models
dat_text_m2 <- dat_text_m2 %>%
  mutate(auc_full = "AUCROC: 0.96 (0.91;1.00)")
dat_text_m3 <- dat_text_m3 %>%
  mutate(auc_full = "AUCROC: 0.95 (0.85;1.00)")
dat_text_m4 <- dat_text_m4 %>%
  mutate(auc_full = "AUCROC: 0.93 (0.83;1.00)")
# dat_text_m2 <- dat_text_m2 %>%
#   mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
# dat_text_m3 <- dat_text_m3 %>%
#   mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
# dat_text_m4 <- dat_text_m4 %>%
#   mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))

# Build row (model) heading objects ------------------------------------------------
## For model 1
title_m1 <- patchwork::wrap_elements(
  ggpubr::text_grob(model1_text,
                    face = "bold",
                    size = 20,
                    color = "black",
                    hjust = 1.12)
)
## For model 2
title_m2 <- patchwork::wrap_elements(
  ggpubr::text_grob(model2_text,
                    face = "bold",
                    size = 20,
                    color = "black",
                    hjust = 0.72)
)
## For model 3
title_m3 <- patchwork::wrap_elements(
  ggpubr::text_grob(model3_text,
                    face = "bold",
                    size = 20,
                    color = "black",
                    hjust = 0.62)
)
## For model 4
title_m4 <- patchwork::wrap_elements(
  ggpubr::text_grob(model4_text,
                    face = "bold",
                    size = 20,
                    color = "black",
                    hjust = 1)
)

# Build plot row objects -----------------------------------------------------------
## For model 1
row_m1 <- build_row(
  title_m1,
  roc_curves_m1,
  dat_text_m1,
  SR_ss_12,
  "pred_prob_m1",
  cal_prep_m1
)
## For model 2
row_m2 <- build_row(
  title_m2,
  roc_curves_m2,
  dat_text_m2,
  SR_ss_12,
  "pred_prob_m2",
  cal_prep_m2
)
## For model 3
row_m3 <- build_row(
  title_m3,
  roc_curves_m3,
  dat_text_m3,
  SR_ss_3,
  "pred_prob_m3",
  cal_prep_m3
)
## For model 4
row_m4 <- build_row(
  title_m4,
  roc_curves_m4,
  dat_text_m4,
  SR_ss_3,
  "pred_prob_m4",
  cal_prep_m4
)

# Combine rows into a single figure -------------------------------------------------
model_display_item <- patchwork::wrap_plots(
  row_m1, row_m2, row_m3, row_m4,
  ncol = 1, nrow = 4
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
                                                   "", "D", "E", "F",
                                                   "", "G", "H", "I",
                                                   "", "J", "K", "L")))

# Save outputs ----------------------------------------------------------------------
pdf("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/figures/Supp_Figurex14.pdf", height = 20, width = 16)
print(model_display_item)
dev.off()

ggsave("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/figures/Supp_Figurex14.jpeg", model_display_item, height = 20, width = 16)
