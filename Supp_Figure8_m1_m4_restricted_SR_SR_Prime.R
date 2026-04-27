# Supp_Figure8.R
# Build Supplementary Figure 8 (StartRight model displays)

# --- Libraries -------------------------------------------------------------
library(tidyverse)
library(pROC)
library(patchwork)
library(ggpubr)

# --- Helpers / functions ---------------------------------------------------
# project helper functions
source("functions/model_info1.R")
source("functions/model_comparison2.R")

# --- Output ----------------------------------------------------------------
out_pdf <- "figures/Supp_Figure8.pdf"

# --- Load data -------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_ro_cc_1_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_ro_cc_2_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
load("m4.RData")

# --- Preplotting: tidy and complete-case datasets -------------------------
#Make complete case datasets
# model variable lists
varlist_12 <- c("AgeatDiagnosis",
                "bmi_model",
                "HbA1c_at_diagnosis_v1")
varlist_3 <- c("AgeatDiagnosis",
               "bmi_model",
               "HbA1c_at_diagnosis_v1",
               "T1DGRS2_z")
varlist_cat_12 <- c("Gender_v1",
                    "DKA",
                    "Unintentional_weight_loss_v1",
                    "autoimmune",
                    "osmotic",
                    "famhisnoninsdiab",
                    "num_anti")

all_vars_12 <- c(varlist_12, varlist_cat_12)
all_vars_3 <- c(varlist_3, varlist_cat_12)
##StartRight ---------------------------------------------------------------------------
SR_ro_cc<- SR_ro_cc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_ro_cc$DKA <- as.character(SR_ro_cc$DKA)
SR_ro_cc$osmotic <- as.character(SR_ro_cc$osmotic)
SR_ro_cc$autoimmune <- as.character(SR_ro_cc$autoimmune)
SR_ro_cc$robust_outcome <- ifelse(SR_ro_cc$robust_outcome == "T1D", 1, 0)
SR_ro_cc$num_anti <- as.character(SR_ro_cc$num_anti)
#Model12
SR_m12_data <- SR_ro_cc %>%
  drop_na(all_of(all_vars_12))
#Model3
SR_m3_data <- SR_ro_cc %>%
  drop_na(all_of(all_vars_3))
##StartRight Prime -------------------------------------------------------------------
SR_50_ro_cc<- SR_50_ro_cc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )
SR_50_ro_cc$DKA <- as.character(SR_50_ro_cc$DKA)
SR_50_ro_cc$osmotic <- as.character(SR_50_ro_cc$osmotic)
SR_50_ro_cc$autoimmune <- as.character(SR_50_ro_cc$autoimmune)
SR_50_ro_cc$robust_outcome <- ifelse(SR_50_ro_cc$robust_outcome == "T1D", 1, 0)
SR_50_ro_cc$num_anti <- as.character(SR_50_ro_cc$num_anti)

#Model12
SR_50_m12_data <- SR_50_ro_cc %>%
  drop_na(all_of(all_vars_12))
#Model3
SR_50_m3_data <- SR_50_ro_cc %>%
  drop_na(all_of(all_vars_3))


# --- Model dataset counts for headings -----------------------------------
## StartRight counts
m12_n_start <- nrow(SR_m12_data)
n12_T1D_start <- SR_m12_data %>%
  filter(robust_outcome == 1) %>%
  nrow()
m3_n_start <- nrow(SR_m3_data)
n3_T1D_start <- SR_m3_data %>%
  filter(robust_outcome == 1) %>%
  nrow()

# PRIME counts
m12_n_prime <- nrow(SR_50_m12_data)
n12_T1D_prime <- SR_50_m12_data %>%
  filter(robust_outcome == 1) %>%
  nrow()
m3_n_prime <- nrow(SR_50_m3_data)
n3_T1D_prime <- SR_50_m3_data %>%
  filter(robust_outcome == 1) %>%
  nrow()

# --- Preplotting: plotting objects (load or create) ----------------------------
#  Run model_info for StartRight and PRIME (robust outcome, Models 1 & 4)
##StartRight -----------------------------------------------------------------------
model_info(
  model = m1,
  test_data = SR_m12_data,
  outcome = "robust_outcome",
  saving_name = "03_05_sm1_sr_restrictive",
  manual_plotting = TRUE,
  manual_plot_name = "start_m1"
)

model_info(
  model = m4,
  test_data = SR_m3_data,
  outcome = "robust_outcome",
  saving_name = "03_05_sm4_sr_restrictive",
  manual_plotting = TRUE,
  manual_plot_name = "start_m4"
)

##StartRight Prime --------------------------------------------------------------------
model_info(
  model = m1,
  test_data = SR_50_m12_data,
  outcome = "robust_outcome",
  saving_name = "03_05_sm1_prime_restrictive",
  manual_plotting = TRUE,
  manual_plot_name = "prime_m1"
)
model_info(
  model = m4,
  test_data = SR_50_m3_data,
  outcome = "robust_outcome",
  saving_name = "03_05_sm4_prime_restrictive",
  manual_plotting = TRUE,
  manual_plot_name = "prime_m4"
)


# --- Plotting helpers -----------------------------------------------------
build_row <- function(title_grob,
                      roc,
                      dat_text,
                      violin_data,
                      pred_prob_name,
                      cal_prep) {
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
      x = factor(robust_outcome, levels = c("1", "0"), labels = c("Type 1", "Type 2")),
      y = !!rlang::sym(pred_prob_name)
    )
  ) +
    geom_violin(aes(fill = robust_outcome), alpha = 0.2) +
    geom_boxplot(aes(fill = robust_outcome), width = .15, outlier.size = 1) +
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

# --- Titles and rows -----------------------------------------------------
model1_text_start <- paste0("Clinical features only model (n=", m12_n_start, "; Type 1=", n12_T1D_start, ")")
model1_text_prime <- paste0("Clinical features only model (n=", m12_n_prime, "; Type 1=", n12_T1D_prime, ")")

model4_text_start <- paste0("Clinical features + T1DGRS model (n=", m3_n_start, "; Type 1=", n3_T1D_start, ")")
model4_text_prime <- paste0("Clinical features + T1DGRS model (n=", m3_n_prime, "; Type 1=", n3_T1D_prime, ")")

title_m1_start <- patchwork::wrap_elements(
  ggpubr::text_grob(
    model1_text_start,
    face = "bold",
    size = 20,
    hjust = 1.12
  )
)

title_m1_prime <- patchwork::wrap_elements(
  ggpubr::text_grob(
    model1_text_prime,
    face = "bold",
    size = 20,
    hjust = 1.14
  )
)

title_m4_start <- patchwork::wrap_elements(
  ggpubr::text_grob(
    model4_text_start,
    face = "bold",
    size = 20,
    hjust = 1.0
  )
)

title_m4_prime <- patchwork::wrap_elements(
  ggpubr::text_grob(
    model4_text_prime,
    face = "bold",
    size = 20,
    hjust = 1.03
  )
)

SR_m12_data$robust_outcome <- as.character(SR_m12_data$robust_outcome)
SR_50_m12_data$robust_outcome <- as.character(SR_50_m12_data$robust_outcome)
SR_m3_data$robust_outcome <- as.character(SR_m3_data$robust_outcome)
SR_50_m3_data$robust_outcome <- as.character(SR_50_m3_data$robust_outcome)

start_heading <- patchwork::wrap_elements(
  ggpubr::text_grob("StartRight",
                    face = "bold",
                    size = 20,
                    hjust = 5.1)
)

prime_heading <- patchwork::wrap_elements(
  ggpubr::text_grob("StartRight Prime",
                    face = "bold",
                    size = 20,
                    hjust = 3.2)
)
#Tweak AUCROC label
dat_text_start_m1 <- dat_text_start_m1 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_start_m4 <- dat_text_start_m4 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_prime_m1 <- dat_text_prime_m1 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
dat_text_prime_m4 <- dat_text_prime_m4 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
#Build model objects -------------------------------------------------------------------------
##StartRight ----------------------------------------------------------------------------------
row_m1 <- build_row(
  title_m1_start,
  roc_curves_start_m1,
  dat_text_start_m1,
  SR_m12_data,
  "pred_prob_start_m1",
  cal_prep_start_m1
)

row_m4 <- build_row(
  title_m4_start,
  roc_curves_start_m4,
  dat_text_start_m4,
  SR_m3_data,
  "pred_prob_start_m4",
  cal_prep_start_m4
)


##StartRight Prime ---------------------------------------------------------------------
row_prime_m1 <- build_row(
  title_m1_prime,
  roc_curves_prime_m1,
  dat_text_prime_m1,
  SR_50_m12_data,
  "pred_prob_prime_m1",
  cal_prep_prime_m1
)

row_prime_m4 <- build_row(
  title_m4_prime,
  roc_curves_prime_m4,
  dat_text_prime_m4,
  SR_50_m3_data,
  "pred_prob_prime_m4",
  cal_prep_prime_m4
)


# --- Write output: single page with four model rows ----------------------
dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)
pdf(out_pdf, width = 16, height = 20)
combined_page <- patchwork::wrap_plots(
  start_heading,
  row_m1,
  row_m4,
  prime_heading,
  row_prime_m1,
  row_prime_m4,
  ncol = 1,
  heights = c(0.08, 1, 1, 0.08, 1, 1)
) + patchwork::plot_annotation(tag_levels = list(c("", "", "A", "B", "C",
                                                   "", "D", "E", "F",
                                                   "", "", "G", "H", "I",
                                                   "", "J", "K", "L")))
print(combined_page)
dev.off()
ggsave("figures/Supp_Figure8.jpeg", combined_page, height = 20, width = 16)
message("Wrote ", out_pdf)
