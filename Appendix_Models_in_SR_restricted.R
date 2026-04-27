#####################################################################################

#Paper figures: Subgroup analysis

#To get the two final paper complete case model datasets
#And do subgrouo analysis

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_ro_cc_1_2025.RData")
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
SR_ro_cc <- SR_ro_cc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
  )


SR_ro_cc$SRoutcome <- as.numeric(SR_ro_cc$SRoutcome)
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

####Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m1,
           test_data = SR_m12_data,
           outcome = "robust_outcome",
           saving_name = "SR_restrictive_m1",
           manual_plotting = TRUE,
           manual_plot_name = "m1")

####Model 2 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m2,
           test_data = SR_m12_data,
           outcome = "robust_outcome",
           saving_name = "SR_restrictive_m2",
           manual_plotting = TRUE,
           manual_plot_name = "m2")
n1_2 <- nrow(SR_m12_data)
n1_T1D <- SR_m12_data %>%
  filter(robust_outcome == 1) %>%
  nrow()

####Model 3 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m3,
           test_data = SR_m3_data,
           outcome = "robust_outcome",
           saving_name = "SR_restrictive_m3",
           manual_plotting = TRUE,
           manual_plot_name = "m3")
n3 <- nrow(SR_m3_data)
n3_T1D <- SR_m3_data %>%
  filter(robust_outcome == 1) %>%
  nrow()
####Model 4 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m4,
           test_data = SR_m3_data,
           outcome = "robust_outcome",
           saving_name = "SR_restrictive_m4",
           manual_plotting = TRUE,
           manual_plot_name = "m4")

####Compiled pdf -----------------------------------------------------------------------
SR_m3_data <- SR_m3_data %>%
  mutate(robust_outcome = as.character(robust_outcome))
SR_m12_data <- SR_m12_data %>%
  mutate(robust_outcome = as.character(robust_outcome))
model1_text <- paste0("Model 1: Clinical features (n=",n1_2,"; T1D=",n1_T1D,")")
model2_text <- paste0("Model 2: Clinical features + number of positive antibodies (n=",n1_2, "; T1D=",n1_T1D,")")
model3_text <- paste0("Model 3: Clinical features + number of positive antibodies + T1DGRS (n=",n3,"; T1D=",n3_T1D,")")
model4_text <- paste0("Model 4: Clinical features + T1DGRS (n=",n3,"; T1D=",n3_T1D,")")

DESIGN_abc <- "
11111#
  223344
  223344
  223344
  223344
  223344
"
#### Model1 row output
m1_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 1.4)
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
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    ),
  #boxplot
  ggplot(SR_m12_data, aes(x=factor(robust_outcome,
                                   levels = c("1","0"),
                                   labels = c("Type 1", "Type 2")),
                          y = pred_prob_m1)) +
    geom_violin(aes(fill = robust_outcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = robust_outcome),width = .15,
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
    theme_bw()+
    theme(text = element_text(size = 20))
) + patchwork::plot_layout(design = DESIGN_abc)


#### Model2
m2_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model2_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.71)
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
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    ),
  #boxplot
  ggplot(SR_m12_data, aes(x=factor(robust_outcome,
                                   levels = c("1","0"),
                                   labels = c("Type 1", "Type 2")),
                          y = pred_prob_m2)) +
    geom_violin(aes(fill = robust_outcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = robust_outcome),width = .15,
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
    theme_bw()+
    theme(text = element_text(size = 20))
)  + patchwork::plot_layout(design = DESIGN_abc)

####Model 3 ------------------------------------------------------------------------------
m3_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model3_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.61)
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
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    ),
  #boxplot
  ggplot(SR_m3_data, aes(x=factor(robust_outcome,
                                  levels = c("1","0"),
                                  labels = c("Type 1", "Type 2")),
                         y = pred_prob_m3)) +
    geom_violin(aes(fill = robust_outcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = robust_outcome),width = .15,
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
####Model 4 ------------------------------------------------------------------------------
m4_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model4_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 1.05)
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
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    ),
  #boxplot
  ggplot(SR_m3_data, aes(x=factor(robust_outcome,
                                  levels = c("1","0"),
                                  labels = c("Type 1", "Type 2")),
                         y = pred_prob_m4)) +
    geom_violin(aes(fill = robust_outcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = robust_outcome),width = .15,
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
    theme_bw()+
    theme(text = element_text(size = 20))
) + patchwork::plot_layout(design = DESIGN_abc)


####PDF
# pdf("figures/SR_restrictive.pdf", height = 20, width = 16)
# model_display_item <- patchwork::wrap_plots(
#   m1_plots_abc,
#   m2_plots_abc,
#   m3_plots_abc,
#   m4_plots_abc,
#   ncol = 1, nrow = 4
# ) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
#                                                    "", "A", "B", "C",
#                                                    "", "A", "B", "C",
#                                                    "", "A", "B", "C")))
# print(model_display_item)
# dev.off()


# pdf("figures/SR_restrictive.pdf", height = 15, width = 16)
# model_display_item <- patchwork::wrap_plots(
#   m1_plots_abc,
#   m2_plots_abc,
#   m3_plots_abc,
#   ncol = 1, nrow = 3
# ) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
#                                                    "", "A", "B", "C",
#                                                    "", "A", "B", "C")))
# print(model_display_item)
# dev.off()

# pdf("figures/SR_restrictive_nogrs.pdf", height = 10, width = 16)
# model_display_item <- patchwork::wrap_plots(
#   m1_plots_abc,
#   m2_plots_abc,
#   #m3_plots_abc,
#   ncol = 1, nrow = 2
# ) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
#                                                    "", "A", "B", "C")))
# print(model_display_item)
# dev.off()

pdf("figures/SR_restrictive_noantis.pdf", height = 10, width = 16)
model_display_item <- patchwork::wrap_plots(
  m1_plots_abc,
  #m2_plots_abc,
  #m3_plots_abc,
  m4_plots_abc,
  ncol = 1, nrow = 2
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
                                                   "", "A", "B", "C")))
print(model_display_item)
dev.off()


