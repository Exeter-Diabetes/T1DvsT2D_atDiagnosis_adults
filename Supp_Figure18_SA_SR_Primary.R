#####################################################################################

#Paper figures: Subgroup analysis

#To get the two final paper complete case model datasets
#And do subgrouo analysis

#Supplementary Figure 10
#StartRight 18-50s
#SA ethnicity
#Primary Outcome

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
load("m4.RData")
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
#Model3
SR_m3_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3))

generate_model_objects <- function(model, full_data, outcome, ethnicity_filter) {
  #browser()
  # Apply predict() to the full dataset
  full_data <- full_data %>%
    mutate(pred_prob = predict(model, full_data, type = "response"))

  # Filter the dataset based on the specified ethnicity
  filtered_data <- full_data %>%
    filter(Eth_4cat == ethnicity_filter)

  # Generate ROC curve data
  roc_curves <- data.frame(prob = filtered_data$pred_prob) %>%
    cbind(Outcome = filtered_data[[outcome]]) %>%
    pROC::roc(response = Outcome, predictor = prob) %>%
    magrittr::extract(2:4) %>%
    as.data.frame() %>%
    mutate(
      auc =  unname(data.frame(prob = filtered_data$pred_prob) %>%
                      cbind(Outcome = filtered_data[[outcome]]) %>%
                      pROC::roc(response = Outcome, predictor = prob) %>%
                      magrittr::extract(c(9)) %>%
                      unlist()),
      sum_sens_spec = sensitivities + specificities,
      max_ss_sum = max(sum_sens_spec),
      best_threshold = ifelse(sum_sens_spec == max_ss_sum, thresholds, NA),
      best_sens = ifelse(sum_sens_spec == max_ss_sum, sensitivities, NA),
      best_spec = ifelse(sum_sens_spec == max_ss_sum, specificities, NA),
      mean = mean(filtered_data$pred_prob, na.rm = TRUE),
      auc_low = unname(data.frame(prob = filtered_data$pred_prob) %>%
                         cbind(Outcome = filtered_data[[outcome]]) %>%
                         pROC::roc(response = Outcome,
                                   predictor = prob,
                                   ci = TRUE) %>%
                         magrittr::extract2(16) %>%
                         magrittr::extract(1) %>%
                         unlist()),
      auc_high = unname(data.frame(prob = filtered_data$pred_prob) %>%
                          cbind(Outcome = filtered_data[[outcome]]) %>%
                          pROC::roc(response = Outcome,
                                    predictor = prob,
                                    ci = TRUE) %>%
                          magrittr::extract2(16) %>%
                          magrittr::extract(3) %>%
                          unlist())
    )

  dat_text <- roc_curves %>%
    dplyr::select(-sensitivities, -specificities) %>%
    distinct() %>%
    mutate(
      auc_full = paste0("AUC: ", signif(auc, 3)),
      mean = paste0("Mean prob:", signif(mean, 3)*100, "%")
    )

  # Prepare calibration data
  cal_prep <- filtered_data %>%
    mutate(pred_decile = ntile(pred_prob, 10)) %>%
    group_by(pred_decile) %>%
    summarise(mnpred = mean(pred_prob),
              prob_obs = mean(as.numeric(.data[[outcome]])),
              lower = binom.test(sum(as.numeric(.data[[outcome]])), n())$conf.int[1],
              upper = binom.test(sum(as.numeric(.data[[outcome]])), n())$conf.int[2]) %>%
    ungroup()


  # Return the objects
  list(roc_curves = roc_curves,
       pred_prob = filtered_data$pred_prob,
       cal_prep = cal_prep,
       dat_text = dat_text)
}

# Generate objects for Model 1
model1_objects <- generate_model_objects(
  model = m1,
  full_data = SR_m12_data,
  outcome = "SRoutcome",
  ethnicity_filter = "South Asian")

roc_curves_m1 <- model1_objects$roc_curves
pred_prob_m1 <- model1_objects$pred_prob
cal_prep_m1 <- model1_objects$cal_prep
dat_text_m1 <- model1_objects$dat_text
# Generate objects for Model 2
model2_objects <- generate_model_objects(
  model = m2,
  full_data = SR_m12_data,
  outcome = "SRoutcome",
  ethnicity_filter = "South Asian")

roc_curves_m2 <- model2_objects$roc_curves
pred_prob_m2 <- model2_objects$pred_prob
cal_prep_m2 <- model2_objects$cal_prep
dat_text_m2 <- model2_objects$dat_text
# Generate objects for Model 3
model3_objects <- generate_model_objects(
  model = m3,
  full_data = SR_m3_data,
  outcome = "SRoutcome",
  ethnicity_filter = "South Asian")

roc_curves_m3 <- model3_objects$roc_curves
pred_prob_m3 <- model3_objects$pred_prob
cal_prep_m3 <- model3_objects$cal_prep
dat_text_m3 <- model3_objects$dat_text
# Generate objects for Model 4
model4_objects <- generate_model_objects(
  model = m4,
  full_data = SR_m3_data,
  outcome = "SRoutcome",
  ethnicity_filter = "South Asian")

roc_curves_m4 <- model4_objects$roc_curves
pred_prob_m4 <- model4_objects$pred_prob
cal_prep_m4 <- model4_objects$cal_prep
dat_text_m4 <- model4_objects$dat_text
###Black --------------------------------------------------------------------
SR_SA_12 <- SR_m12_data %>%
  mutate(m1_pp = predict(m1, SR_m12_data, type = "response"),
         m2_pp = predict(m2, SR_m12_data, type = "response")) %>%
  filter(Eth_5cat == "South Asian")

SR_SA_3 <- SR_m3_data %>%
  mutate(m1_pp = predict(m1, SR_m3_data, type = "response"),
         m2_pp = predict(m2, SR_m3_data, type = "response"),
         m3_pp = predict(m3, SR_m3_data, type = "response"),
         m4_pp = predict(m4, SR_m3_data, type = "response")) %>%
  filter(Eth_5cat == "South Asian")


####Compiled pdf -----------------------------------------------------------------------
DESIGN_abc <- "
11111#
  223344
  223344
  223344
  223344
  223344
"
m12_n <- nrow(SR_SA_12)
m3_n <- nrow(SR_SA_3)
n12_T1D <- SR_SA_12 %>%
  filter(SRoutcome == 1) %>%
  nrow()
n3_T1D <- SR_SA_3 %>%
  filter(SRoutcome == 1) %>%
  nrow()
SR_SA_12 <- SR_SA_12 %>%
  mutate(SRoutcome = as.character(SRoutcome))
SR_SA_3 <- SR_SA_3 %>%
  mutate(SRoutcome = as.character(SRoutcome))
model1_text <- paste0("Clinical features only model (n=",m12_n,"; Type 1=",n12_T1D,")")
model2_text <- paste0("Clinical features + number of positive antibodies model (n=",m12_n,"; Type 1=",n12_T1D,")")
model3_text <- paste0("Clinical features + number of positive antibodies + T1DGRS model (n=",m3_n,"; Type 1=",n3_T1D,")")
model4_text <- paste0("Clinical features + T1DGRS model (n=",m3_n,"; Type 1=",n3_T1D,")")
#Tweak AUCROC label
dat_text_m1 <- dat_text_m1 %>%
  mutate(auc_full = "AUCROC: 0.95 (0.87;1.00)")
dat_text_m2 <- dat_text_m2 %>%
  mutate(auc_full = "AUCROC: 0.97 (0.93;1.00)")
dat_text_m3 <- dat_text_m3 %>%
  mutate(auc_full = "AUCROC: 0.98 (0.93;1.00)")
dat_text_m4 <- dat_text_m4 %>%
  mutate(auc_full = "AUCROC: 0.94 (0.86;1.00)")
#### Model1 row output
m1_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.95)
  ),
  #roc_plot
  roc_curves_m1 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m1,
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
  ggplot(SR_SA_12, aes(x=factor(SRoutcome,
                                   levels = c("1","0"),
                                   labels = c("Type 1", "Type 2")),
                          y = pred_prob_m1)) +
    geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = SRoutcome),width = .15,
                 outlier.size = 1) +
    scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size = 20)),
  #calibration plot
  ggplot(cal_prep_m1, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw() +
    theme(text = element_text(size = 20))
) + patchwork::plot_layout(design = DESIGN_abc)


#### Model2
m2_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model2_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.61)
  ),
  #roc_plot
  roc_curves_m2 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m2,
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
  ggplot(SR_SA_12, aes(x=factor(SRoutcome,
                                   levels = c("1","0"),
                                   labels = c("Type 1", "Type 2")),
                          y = pred_prob_m2)) +
    geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = SRoutcome),width = .15,
                 outlier.size = 1) +
    scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size = 20)),
  #calibration plot
  ggplot(cal_prep_m2, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw() +
    theme(text = element_text(size = 20))
)  + patchwork::plot_layout(design = DESIGN_abc)

####Model 3
m3_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model3_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.53)
  ),
  #roc_plot
  roc_curves_m3 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m3,
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
  ggplot(SR_SA_3, aes(x=factor(SRoutcome,
                                  levels = c("1","0"),
                                  labels = c("Type 1", "Type 2")),
                         y = pred_prob_m3)) +
    geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = SRoutcome),width = .15,
                 outlier.size = 1) +
    scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size = 20)),
  #calibration plot
  ggplot(cal_prep_m3, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw() +
    theme(text = element_text(size = 20))
) + patchwork::plot_layout(design = DESIGN_abc)

####Model 4
m4_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model4_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.85)
  ),
  #roc_plot
  roc_curves_m4 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m4,
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
  ggplot(SR_SA_3, aes(x=factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")),
                      y = pred_prob_m4)) +
    geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = SRoutcome),width = .15,
                 outlier.size = 1) +
    scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size = 20)),
  #calibration plot
  ggplot(cal_prep_m4, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw() +
    theme(text = element_text(size = 20))
) + patchwork::plot_layout(design = DESIGN_abc)


####PDF
pdf("figures/Supp_Figure18.pdf", height = 20, width = 16)
model_display_item <- patchwork::wrap_plots(
  m1_plots_abc,
  m2_plots_abc,
  m3_plots_abc,
  m4_plots_abc,
  ncol = 1, nrow = 4
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
                                                   "", "D", "E", "F",
                                                   "", "G", "H", "I",
                                                   "", "J", "K", "L")))
print(model_display_item)
dev.off()
ggsave("figures/Supp_Figure18.jpeg", model_display_item, height = 20, width = 16)
