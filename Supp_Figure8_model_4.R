#####################################################################################

#Paper figures

#Supplementary Figure 8:
#StartRight 18-50
#Primary outcome

#Model 4: Clinical features + GRS
###################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/model_comparison2.R")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
#Complete case model data -------------------------------------------------------------
##Define variables in models12 & models3------------------------------------------------
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

#Model3
SR_m3_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3))
#Develop model 4
m4 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) +
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 +
            osmotic +
            autoimmune +
            Unintentional_weight_loss_v1 +
            DKA +
            famhisnoninsdiab +
            scale(T1DGRS2_z),
          #famhisauto,
          data = SR_m3_data,
          family = binomial)
save(m4, file = "m4.RData")

##Display item---------------------------------------------------------------------------
###Model 4 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m4, 
           test_data = SR_m3_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm1", 
           manual_plotting = TRUE, 
           manual_plot_name = "m4")
###Model 4 plot ------------------------------------------------------------------------
DESIGN_abc <- "
11111#
  223344
  223344
  223344
  223344
  223344
"

m3_n <- nrow(SR_m3_data)
model4_text <- paste0("Model 4: Clinical features + T1DGRS (n=",m3_n,")")
#### Model1 row output
m4_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model4_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 1)
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
      panel.spacing.x = unit(1.5, "lines")
    ),
  #boxplot
  ggplot(SR_m3_data, aes(x=factor(SRoutcome, 
                                   levels = c("1","0"), 
                                   labels = c("Type 1", "Type 2")), 
                          y = pred_prob_m4)) +
    geom_violin() + 
    geom_boxplot(width = .15, 
                 outlier.size = 1) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw(),
  #calibration plot
  ggplot(cal_prep_m4, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw()
) + patchwork::plot_layout(design = DESIGN_abc)

pdf("figures/Supp_Figure8.pdf", height = 5, width = 16)
model_display_item <- patchwork::wrap_plots(
  m4_plots_abc
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C")))
print(model_display_item)
dev.off()
