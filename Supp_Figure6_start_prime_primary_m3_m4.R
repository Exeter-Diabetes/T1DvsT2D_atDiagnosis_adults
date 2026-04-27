#####################################################################################
# merged_SuppFigures_start_prime.R
# Combined script: StartRight (Supp_Figure8_model_4.R) + PRIME (Supp_Figure_PRIME_m3_m4.R)
# This file keeps original code structure but prefixes objects to avoid name collisions.
#####################################################################################

# Libraries (single place to avoid duplication)
library(tidyverse)
library(writexl)
library(pROC)
library(patchwork)
library(ggpubr)

# Source helper functions used by both original scripts
source("functions/model_info1.R")
source("functions/model_comparison2.R")

# -----------------------------
# StartRight section (from Supp_Figure8_model_4.R)
# Prefix all large objects with start_ to avoid collisions
# -----------------------------

# Load data (adjust path if necessary)
load("data/SR_SRout_ccc_20_3_2025.RData")
load("data/SR_50_SRout_ccc_20_3_2025.RData") # provides SR_50_SRout_ccc
load("m3.RData")
load("m4.RData")
#StartRight -------------------------------------------------------------------------
# Prepare dataset (kept original logic)
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag), bmi, bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)

# Variable lists (kept)
varlist_3 <- c("AgeatDiagnosis",
               "bmi_model",
               "HbA1c_at_diagnosis_v1",
               "T1DGRS2_z")
varlist_cat_3 <- c(
  "Eth_5cat",
  "Gender_v1",
                   "DKA",
                   "Unintentional_weight_loss_v1",
                   "autoimmune",
                   "osmotic",
                   "famhisnoninsdiab",
                   "num_anti")
all_vars_3 <- c(varlist_3, varlist_cat_3)

# StartRight dataset for model building / testing
start_SR_m3_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3))

#StartRight Prime -------------------------------------------------------------------------
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag), bmi, bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat))

SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)


prime_SR_m3_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_3))
#Get model info ---------------------------------------------------------------------------
##StartRight ---------------------------------------------------------------------------------
# Run model_info for StartRight models - use manual_plot_name prefixes
model_info(model = m3,
           test_data = start_SR_m3_data,
           outcome = "SRoutcome",
           saving_name = "03_04_start_m3",
           manual_plotting = TRUE,
           manual_plot_name = "start_m3")

model_info(model = m4,
           test_data = start_SR_m3_data,
           outcome = "SRoutcome",
           saving_name = "03_04_start_m4",
           manual_plotting = TRUE,
           manual_plot_name = "start_m4")

##StartRight Prime -------------------------------------------------------------------------
model_info(model = m4,
           test_data = prime_SR_m3_data,
           outcome = "SRoutcome",
           saving_name = "03_04_prime_m4",
           manual_plotting = TRUE,
           manual_plot_name = "prime_m4")
model_info(model = m3,
           test_data = prime_SR_m3_data,
           outcome = "SRoutcome",
           saving_name = "03_04_prime_m3",
           manual_plotting = TRUE,
           manual_plot_name = "prime_m3")
#Build display items -----------------------------------------------------------------------
##Generic-------------------------------------------------------------------------------------
# Build StartRight model display rows (adapted variable names)
DESIGN_abc <- "\n11111#\n  223344\n  223344\n  223344\n  223344\n  223344\n"

##Labels and titles-----------------------------------------------------------------------
###StartRight
start_SR_m3_data <- start_SR_m3_data %>%
  mutate(SRoutcome = as.character(SRoutcome))

n3_T1D_start <- start_SR_m3_data %>%
  filter(SRoutcome == "1") %>%
  nrow()
start_m3_n <- nrow(start_SR_m3_data)

model4_text_start <- "Clinical features + T1DGRS model"
model3_text_start <- "Clinical features + islet autoantibodies + T1DGRS model"

# Build m3 row only if start_m3 exists: create left-aligned title grob and three content plots with tags A/B/C
title_grob_start_m3 <- patchwork::wrap_elements(
  ggpubr::text_grob(
    model3_text_start,
    face = "bold",
    size = 20,
    color = "black",
    hjust = 0.93))
dat_text_start_m3 <- dat_text_start_m3 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_start_m4 <- dat_text_start_m4 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
###StartRight Prime---------------------------------------------------------------------
prime_SR_m3_data <- prime_SR_m3_data %>%
  mutate(SRoutcome = as.character(SRoutcome))
prime_n3_T1D <- prime_SR_m3_data %>%
  filter(SRoutcome == "1") %>% nrow()
prime_m3_n <- nrow(prime_SR_m3_data)

prime_model4_text <- "Clinical features + T1DGRS model"
prime_model3_text <- "Clinical features + islet autoantibodies + T1DGRS model"

# Build prime m3 row with tags G/H/I and left-aligned title
title_grob_prime_m3 <- patchwork::wrap_elements(
  ggpubr::text_grob(
    prime_model3_text,
    face = "bold",
    size = 20,
    color = "black",
    hjust = 0.95))
title_grob_prime_m4 <- patchwork::wrap_elements(
  ggpubr::text_grob(
    prime_model4_text,
    face = "bold",
    size = 20,
    color = "black",
    hjust = 1.6))
dat_text_prime_m3 <- dat_text_prime_m3 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_prime_m4 <- dat_text_prime_m4 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
##Plot objects ---------------------------------------------------------------------------
###StartRight --------------------------------------------------------------------------
####Model 3 -----------------------------------------------------------------------------
p_roc_start_m3 <- roc_curves_start_m3 %>%
  ggplot(aes(x = 1 - specificities, y = sensitivities)) +
  geom_path() +
  theme_bw() +
  scale_y_continuous("Sensitivity", labels = scales::percent) +
  scale_x_continuous("1- Specificity", labels = scales::percent) +
  geom_label(
    data = dat_text_start_m3,
    mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
    size = 6,
    label.r = unit(0, "pt"),
    label.padding = unit(0.4, "lines")
) +
theme(panel.spacing.x = unit(1.5, "lines"), text = element_text(size = 20)) +
theme(plot.tag.position = c(0.01, 0.98), plot.tag = element_text(size = 20, face = "bold"))

p_violin_start_m3 <- ggplot(
  start_SR_m3_data,
  aes(
    x = factor(SRoutcome, levels = c("1","0"), labels = c("Type 1", "Type 2")),
    y = pred_prob_start_m3
  )
) +
  geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
  geom_boxplot(aes(fill = SRoutcome), width = .15, outlier.size = 1) +
  scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
  xlab("Type of diabetes") +
  ylab("Predicted probability \n of T1D vs T2D") +
theme_bw() +
theme(legend.position = "none", text = element_text(size = 20)) +
theme(plot.tag.position = c(0.01, 0.98), plot.tag = element_text(size = 20, face = "bold"))

p_cal_start_m3 <- ggplot(cal_prep_start_m3, aes(x = mnpred, y = prob_obs)) +
  geom_point() +
  xlab("Mean predicted probability \n in each decile") +
  ylab("Proportion of T1D \n in each decile") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylim(c(0,1)) +
  xlim(c(0,1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
theme_bw() +
theme(text = element_text(size = 20)) +
theme(plot.tag.position = c(0.01, 0.98), plot.tag = element_text(size = 20, face = "bold"))

# assemble title above the three panels (title stacked on top of the three-panel row)
content_row_start_m3 <- patchwork::wrap_plots(
  p_roc_start_m3,
  p_violin_start_m3,
  p_cal_start_m3,
  ncol = 3
) +
  patchwork::plot_annotation(tag_levels = list(c("A", "B", "C")))

m3_plots_abc_start <- patchwork::wrap_plots(title_grob_start_m3,
                                            content_row_start_m3,
                                            ncol = 1,
                                            heights = c(0.15, 1))

####Model 4 -----------------------------------------------------------------------------
# Build m4 row (StartRight) with tags D/E/F and left-aligned title
title_grob_start_m4 <- patchwork::wrap_elements(
  ggpubr::text_grob(
    model4_text_start,
    face = "bold",
    size = 20,
    color = "black",
    hjust = 1.6))

p_roc_start_m4 <- roc_curves_start_m4 %>%
  ggplot(aes(x = 1 - specificities, y = sensitivities)) +
  geom_path() +
  theme_bw() +
  scale_y_continuous("Sensitivity", labels = scales::percent) +
  scale_x_continuous("1- Specificity", labels = scales::percent) +
  geom_label(
    data = dat_text_start_m4,
    mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
    size = 6,
    label.r = unit(0, "pt"),
    label.padding = unit(0.4, "lines")
  ) +
  theme(panel.spacing.x = unit(1.5, "lines"),
        text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 20, face = "bold"))

p_violin_start_m4 <- ggplot(
  start_SR_m3_data,
  aes(
    x = factor(SRoutcome, levels = c("1","0"), labels = c("Type 1", "Type 2")),
    y = pred_prob_start_m4
  )
) +
  geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
  geom_boxplot(aes(fill = SRoutcome), width = .15, outlier.size = 1) +
  scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
  xlab("Type of diabetes") +
  ylab("Predicted probability \n of T1D vs T2D") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 20, face = "bold"))

p_cal_start_m4 <- ggplot(cal_prep_start_m4,
                         aes(x = mnpred, y = prob_obs)) +
  geom_point() +
  xlab("Mean predicted probability \n in each decile") +
  ylab("Proportion of T1D \n in each decile") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylim(c(0,1)) +
  xlim(c(0,1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 20, face = "bold"))

content_row_start_m4 <- patchwork::wrap_plots(
  p_roc_start_m4,
  p_violin_start_m4,
  p_cal_start_m4,
  ncol = 3
) +
  patchwork::plot_annotation(tag_levels = list(c("D", "E", "F")))

m4_plots_abc_start <- patchwork::wrap_plots(
  title_grob_start_m4,
  content_row_start_m4,
  ncol = 1,
  heights = c(0.15, 1))

# Optionally write StartRight PDF (commented)
# pdf("figures/Supp_Figure8_withm3_start.pdf", height = 10, width = 16)
# print(m3_plots_abc_start)
# print(m4_plots_abc_start)
# dev.off()

###StartRight Prime ------------------------------------------------------------------------
####Model 3 ------------------------------------------------------------------------
p_roc_prime_m3 <- roc_curves_prime_m3 %>%
  ggplot(aes(x = 1 - specificities, y = sensitivities)) +
  geom_path() +
  theme_bw() +
  scale_y_continuous("Sensitivity", labels = scales::percent) +
  scale_x_continuous("1- Specificity", labels = scales::percent) +
  geom_label(
    data = dat_text_prime_m3,
    mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
    size = 6,
    label.r = unit(0, "pt"),
    label.padding = unit(0.4, "lines")
  ) +
  theme(panel.spacing.x = unit(1.5, "lines"),
        text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 20, face = "bold"))

p_violin_prime_m3 <- ggplot(
  prime_SR_m3_data,
  aes(
    x = factor(SRoutcome, levels = c("1","0"), labels = c("Type 1", "Type 2")),
    y = pred_prob_prime_m3
  )
) +
  geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
  geom_boxplot(aes(fill = SRoutcome), width = .15, outlier.size = 1) +
  scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
  xlab("Type of diabetes") +
  ylab("Predicted probability \n of T1D vs T2D") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 20, face = "bold"))

p_cal_prime_m3 <- ggplot(cal_prep_prime_m3, aes(x = mnpred, y = prob_obs)) +
  geom_point() +
  xlab("Mean predicted probability \n in each decile") +
  ylab("Proportion of T1D \n in each decile") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylim(c(0,1)) +
  xlim(c(0,1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 20, face = "bold"))

content_row_prime_m3 <- patchwork::wrap_plots(
  p_roc_prime_m3,
  p_violin_prime_m3,
  p_cal_prime_m3,
  ncol = 3
) +
  patchwork::plot_annotation(tag_levels = list(c("G", "H", "I")))

m3_plots_abc_prime <- patchwork::wrap_plots(
  title_grob_prime_m3,
  content_row_prime_m3,
  ncol = 1,
  heights = c(0.15, 1))



####Model 4 ----------------------------------------------------------------------
p_roc_prime_m4 <- roc_curves_prime_m4 %>%
  ggplot(aes(x = 1 - specificities, y = sensitivities)) +
  geom_path() +
  theme_bw() +
  scale_y_continuous("Sensitivity", labels = scales::percent) +
  scale_x_continuous("1- Specificity", labels = scales::percent) +
  geom_label(
    data = dat_text_prime_m4,
    mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
    size = 6,
    label.r = unit(0, "pt"),
    label.padding = unit(0.4, "lines")
  ) +
  theme(panel.spacing.x = unit(1.5, "lines"),
        text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 20, face = "bold"))

p_violin_prime_m4 <- ggplot(
  prime_SR_m3_data,
  aes(
    x = factor(SRoutcome, levels = c("1","0"), labels = c("Type 1", "Type 2")),
    y = pred_prob_prime_m4
  )
) +
  geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
  geom_boxplot(aes(fill = SRoutcome), width = .15, outlier.size = 1) +
  scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
  xlab("Type of diabetes") +
  ylab("Predicted probability \n of T1D vs T2D") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 20, face = "bold"))

p_cal_prime_m4 <- ggplot(
  cal_prep_prime_m4,
  aes(x = mnpred, y = prob_obs)) +
  geom_point() +
  xlab("Mean predicted probability \n in each decile") +
  ylab("Proportion of T1D \n in each decile") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylim(c(0,1)) +
  xlim(c(0,1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        plot.tag.position = c(0.01, 0.98),
        plot.tag = element_text(size = 14, face = "bold"))

content_row_prime_m4 <- patchwork::wrap_plots(
  p_roc_prime_m4,
  p_violin_prime_m4,
  p_cal_prime_m4,
  ncol = 3
) +
  patchwork::plot_annotation(tag_levels = list(c("J", "K", "L")))

m4_plots_abc_prime <- patchwork::wrap_plots(
  title_grob_prime_m4,
  content_row_prime_m4,
  ncol = 1,
  heights = c(0.15, 1))

# Create column headers above left and right columns
start_col_header <- patchwork::wrap_elements(
  ggpubr::text_grob(
    paste0("StartRight (n=", start_m3_n, "; Type 1= ", n3_T1D_start, ")"),
    face = "bold",
    size = 20,
    hjust = 1.7
  )
)

prime_col_header <- patchwork::wrap_elements(
  ggpubr::text_grob(
    paste0("StartRight Prime (n=", prime_m3_n, "; Type 1 = ", prime_n3_T1D, ")"),
    face = "bold",
    size = 20,
    hjust = 1.5
  )
)


# Combine into PDF -----------------------------------------------
pdf(
  "figures/Supp_Figure6.pdf",
  height = 20, width = 16
)

# Build ordered single-column content: header, m3, m4 for StartRight; header, m3, m4 for StartRight Prime
left_top <- m3_plots_abc_start
left_bottom <- m4_plots_abc_start
right_top <- m3_plots_abc_prime
right_bottom <- m4_plots_abc_prime

combined <- patchwork::wrap_plots(
  start_col_header,
  left_top,
  left_bottom,
  prime_col_header,
  right_top,
  right_bottom,
  ncol = 1,
  heights = c(0.05, 0.225, 0.225, 0.05, 0.225, 0.225)
) + patchwork::plot_annotation(tag_levels = list(c("", "", "A", "B", "C",
                                                   "", "D", "E", "F",
                                                   "", "", "G", "H", "I",
                                                   "", "J", "K", "L")))

print(combined)
dev.off()
#ggsave("figures/Supp_Figure6.png", combined, height = 20, width = 16)
ggsave("figures/Supp_Figure6.jpeg", combined, height = 20, width = 16)
message("Wrote figures/combined_Start_Prime_models_m3_m4.pdf")
#####################################################################################
