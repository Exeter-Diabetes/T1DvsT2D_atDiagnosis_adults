## Supp_Figure23.R
## Build Supplementary Figure 23 (StartRight and StartRight Prime; anti-pos_anti outcome)

## --- Libraries -------------------------------------------------------------
library(tidyverse)
library(pROC)
library(patchwork)
library(ggpubr)

## --- Helpers / functions ---------------------------------------------------
source("functions/model_info1.R")
source("functions/model_comparison2.R")

## --- Output ----------------------------------------------------------------
out_pdf <- "figures/Supp_Figure23.pdf"

## --- Load data -------------------------------------------------------------
load("data/SR_SRout_ccc_20_3_2025.RData")
load("data/SR_50_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
load("m4.RData")

## --- Preplotting: tidy and complete-case datasets -------------------------
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

SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(
    bmi_model = ifelse(is.na(bmi_diag), bmi, bmi_diag),
    famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)

varlist_12 <- c("AgeatDiagnosis", "bmi_model", "HbA1c_at_diagnosis_v1")
varlist_cat_12 <- c("Gender_v1", "DKA", "Unintentional_weight_loss_v1", "autoimmune", "osmotic", "famhisnoninsdiab", "num_anti")
all_vars_12 <- c(varlist_12, varlist_cat_12)

start_SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12)) %>%
  mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
prime_SR_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12)) %>%
  mutate(anti_pos = ifelse(num_anti == "0", 0, 1))

## --- Run model_info for Model 1 on StartRight and PRIME using outcome 'anti_pos' --
model_info(
  model = m1,
  test_data = start_SR_m12_data,
  outcome = "anti_pos",
  saving_name = "03_05_sm1_antipos_start",
  manual_plotting = TRUE,
  manual_plot_name = "start_m1"
)

model_info(
  model = m1,
  test_data = prime_SR_m12_data,
  outcome = "anti_pos",
  saving_name = "03_05_sm1_antipos_prime",
  manual_plotting = TRUE,
  manual_plot_name = "prime_m1"
)

## --- Compute counts and title grobs --------------------------------------
# StartRight counts for model1 dataset
m12_start <- nrow(start_SR_m12_data)
n12_T1D_start <- (start_SR_m12_data %>% filter(anti_pos == 1) %>% nrow())
m12_prime <- nrow(prime_SR_m12_data)
n12_T1D_prime <- (prime_SR_m12_data %>% filter(anti_pos == 1) %>% nrow())

title_m1_start <- patchwork::wrap_elements(
  ggpubr::text_grob(paste0("Clinical features only model (n=", m12_start, "; Type 1=", n12_T1D_start, ")"),
                    face = "bold", size = 20, hjust = 1.13))
title_m1_prime <- patchwork::wrap_elements(
  ggpubr::text_grob(paste0("Clinical features only model (n=", m12_prime, "; Type 1=", n12_T1D_prime, ")"),
                    face = "bold", size = 20, hjust = 1.13))
#Tweak AUCROC label
dat_text_start_m1 <- dat_text_start_m1 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_prime_m1 <- dat_text_prime_m1 %>%
  mutate(auc_full = "AUCROC: 0.86 (0.82;0.90)")
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
      x = factor(anti_pos, levels = c("1", "0"), labels = c("Positive", "Negative")),
      y = !!rlang::sym(pred_prob_name)
    )
  ) +
    geom_violin(aes(fill = anti_pos), alpha = 0.2) +
    geom_boxplot(aes(fill = anti_pos), width = .15, outlier.size = 1) +
    scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
    xlab("Islet-autoantibody \n status") +
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw() +
    theme(legend.position = "none", text = element_text(size = 20))

  p_cal <- ggplot(cal_prep, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of positive \n islet-autoantibody \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) +
    xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    theme_bw() +
    theme(text = element_text(size = 20))

  content <- patchwork::wrap_plots(p_roc, p_violin, p_cal, ncol = 3)
  patchwork::wrap_plots(title_grob, content, ncol = 1, heights = c(0.12, 1))
}

start_SR_m12_data$anti_pos <- as.character(start_SR_m12_data$anti_pos)
prime_SR_m12_data$anti_pos <- as.character(prime_SR_m12_data$anti_pos)

start_row <- build_row(
  title_m1_start,
  roc_curves_start_m1,
  dat_text_start_m1,
  start_SR_m12_data,
  "pred_prob_start_m1",
  cal_prep_start_m1
)

prime_row <- build_row(
  title_m1_prime,
  roc_curves_prime_m1,
  dat_text_prime_m1,
  prime_SR_m12_data,
  "pred_prob_prime_m1",
  cal_prep_prime_m1
)


start_heading <- patchwork::wrap_elements(
  ggpubr::text_grob("StartRight", face = "bold", size = 20, hjust = 5.1))
prime_heading <- patchwork::wrap_elements(
  ggpubr::text_grob("StartRight Prime", face = "bold", size = 20, hjust = 3.25))

## --- Save combined page --------------------------------------------------
dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)
pdf(out_pdf, width = 16, height = 10)
model_display_item <- patchwork::wrap_plots(
  start_heading,
  start_row,
  prime_heading,
  prime_row,
  ncol = 1,
  heights = c(0.08, 1, 0.08, 1)
) + patchwork::plot_annotation(tag_levels = list(c("", "", "A", "B", "C",
                                                   "", "", "D", "E", "F")))
print(model_display_item)
dev.off()
ggsave("figures/Supp_Figure23.jpeg", model_display_item, height = 10, width = 16)
message("Wrote ", out_pdf)
