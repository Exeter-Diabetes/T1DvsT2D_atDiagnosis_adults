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
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
# load("m3.RData")
# load("m4.RData")
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
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
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
SR_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
#Model3
SR_m3_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_3))

####Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m1,
           test_data = SR_m12_data,
           outcome = "SRoutcome",
           saving_name = "03_15_sm1_Prime",
           manual_plotting = TRUE,
           manual_plot_name = "m1")

####Model 2 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m2,
           test_data = SR_m12_data,
           outcome = "SRoutcome",
           saving_name = "03_15_sm2_Prime",
           manual_plotting = TRUE,
           manual_plot_name = "m2")
n1_2 <- nrow(SR_m12_data)

# ####Model 3 ROW --------------------------------------------------------------------------
# ####Get large amount of model info automated
# model_info(model = m3,
#            test_data = SR_m3_data,
#            outcome = "SRoutcome",
#            saving_name = "03_15_sm3_Prime",
#            manual_plotting = TRUE,
#            manual_plot_name = "m3")
# n3 <- nrow(SR_m3_data)
# ####Model 4 ROW --------------------------------------------------------------------------
# ####Get large amount of model info automated
# model_info(model = m4,
#            test_data = SR_m3_data,
#            outcome = "SRoutcome",
#            saving_name = "03_15_sm34_Prime",
#            manual_plotting = TRUE,
#            manual_plot_name = "m4")

####Compiled pdf -----------------------------------------------------------------------
m12_n <- nrow(SR_m12_data)
n12_T1D <- SR_m12_data %>%
  filter(SRoutcome == 1) %>%
  nrow()
model1_text <- paste0("Clinical features only model (n=",m12_n,"; Type 1=",n12_T1D,")")
model2_text <- paste0("Clinical features + number of positive antibodies model (n=",m12_n,"; Type 1=",n12_T1D,")")
dat_text_m1 <- dat_text_m1 %>%
  mutate(auc_full = "AUCROC: 0.93 (0.90;0.96)")
dat_text_m2 <- dat_text_m2 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))

DESIGN_abc <- "
11111#
  223344
  223344
  223344
  223344
  223344
"
SR_m12_data <- SR_m12_data %>%
  mutate(SRoutcome = as.character(SRoutcome))
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
  ggplot(SR_m12_data, aes(x=factor(SRoutcome,
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
  ggplot(SR_m12_data, aes(x=factor(SRoutcome,
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
    theme_bw()+
    theme(text = element_text(size = 20))
)  + patchwork::plot_layout(design = DESIGN_abc)



pdf("figures/Figure3.pdf", height = 10, width = 16)
model_display_item <- patchwork::wrap_plots(
  m1_plots_abc,
  m2_plots_abc,
  ncol = 1, nrow = 2
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
                                                   "", "D", "E", "F")))
print(model_display_item)
dev.off()



