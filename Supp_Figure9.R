#####################################################################################
# Supplementary Figure 9_1
# StartRight dataset (18-50s)
# Clinically diagnosed T1D
# Primary Outcome: SRoutcome
#####################################################################################

# Libraries -------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(patchwork)
library(ggpubr)
library(pROC)

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
      x = factor(SRoutcome, levels = c("1", "0"), labels = c("Type 1", "Type 2")),
      y = !!rlang::sym(pred_prob_name)
    )
  ) +
    geom_violin(aes(fill = SRoutcome), alpha = 0.2) +
    geom_boxplot(aes(fill = SRoutcome), width = .15, outlier.size = 1) +
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
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    theme_bw() +
    theme(text = element_text(size = 20))

  content <- patchwork::wrap_plots(p_roc, p_violin, p_cal, ncol = 3)
  patchwork::wrap_plots(title_grob, content, ncol = 1, heights = c(0.12, 1))
}

# Load functions --------------------------------------------------------------------
source("functions/model_info1.R")

# Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
load("m4.RData")

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
  filter(clinical_diagnosis_v1 == "Type 1")

SR_ss_3 <- SR_m3_data %>%
  filter(clinical_diagnosis_v1 == "Type 1")

# Extract dataset statistics --------------------------------------------------------
m12_n <- nrow(SR_ss_12)
m3_n <- nrow(SR_ss_3)
n12_T1D <- SR_ss_12 %>%
  filter(SRoutcome == 1) %>%
  nrow()
n3_T1D <- SR_ss_3 %>%
  filter(SRoutcome == 1) %>%
  nrow()

# Get model information for plotting ---------------------------------------------
###Model 1 ROW --------------------------------------------------------------------------
model_info(model = m1,
           test_data = SR_ss_12,
           outcome = "SRoutcome",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m1")

###Model 2 ROW --------------------------------------------------------------------------
model_info(model = m2,
           test_data = SR_ss_12,
           outcome = "SRoutcome",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m2")

###Model 3 ROW --------------------------------------------------------------------------
model_info(model = m3,
           test_data = SR_ss_3,
           outcome = "SRoutcome",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m3")
###Model 4 ROW --------------------------------------------------------------------------
model_info(model = m4,
           test_data = SR_ss_3,
           outcome = "SRoutcome",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m4")
#Make sure outcome variable is a character for plotting------------------------------
SR_ss_12 <- SR_ss_12 %>%
  mutate(SRoutcome = as.character(SRoutcome))
SR_ss_3 <- SR_ss_3 %>%
  mutate(SRoutcome = as.character(SRoutcome))
# Prepare model titles --------------------------------------------------------------
model1_text <- paste0("Clinical features only model (n=", m12_n, "; Type 1=", n12_T1D, ")")
model2_text <- paste0("Clinical features + number of positive antibodies model (n=", m12_n, "; Type 1=", n12_T1D, ")")
model3_text <- paste0("Clinical features + number of positive antibodies + T1DGRS model (n=", m3_n, "; Type 1=", n3_T1D, ")")
model4_text <- paste0("Clinical features + T1DGRS model (n=", m3_n, "; Type 1=", n3_T1D, ")")
# Tweak AUCROC labels --------------------------------------------------------------
# Ensure all AUCs are to 2 decimal places
# Manual overwrite for model 1 AUC to ensure the 2 decimal "0" in "0.80"
dat_text_m1 <- dat_text_m1 %>%
  mutate(auc_full = "AUCROC: 0.84 (0.80;0.88)")
# Default rounding for other models
dat_text_m2 <- dat_text_m2 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_m3 <- dat_text_m3 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_m4 <- dat_text_m4 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))

# Build row (model) heading objects ------------------------------------------------
## For model 1
title_m1 <- patchwork::wrap_elements(
  ggpubr::text_grob(model1_text,
                    face = "bold",
                    size = 20,
                    color = "black",
                    hjust = 0.95)
)
## For model 2
title_m2 <- patchwork::wrap_elements(
  ggpubr::text_grob(model2_text,
                    face = "bold",
                    size = 20,
                    color = "black",
                    hjust = 0.61)
)
## For model 3
title_m3 <- patchwork::wrap_elements(
  ggpubr::text_grob(model3_text,
                    face = "bold",
                    size = 20,
                    color = "black",
                    hjust = 0.53)
)
## For model 4
title_m4 <- patchwork::wrap_elements(
  ggpubr::text_grob(model4_text,
                    face = "bold",
                    size = 20,
                    color = "black",
                    hjust = 0.85)
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
pdf("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/neat/AI_neat/figures/Supp_Figure9_1.pdf", height = 20, width = 16)
print(model_display_item)
dev.off()

ggsave("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/neat/AI_neat/figures/Supp_Figure9_1.jpeg", model_display_item, height = 20, width = 16)
