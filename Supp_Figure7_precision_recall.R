########################################################################################

#Supplementary Figure 7: Precision-recall curves

#StartRight 
#Primary outcome

##################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")

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
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab)
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

### Make predictions ------------------------------------------------------------------
#Model1
model_info(model = m1, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm1", 
           manual_plotting = TRUE, 
           manual_plot_name = "m1")
#Model1
model_info(model = m2, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm2", 
           manual_plotting = TRUE, 
           manual_plot_name = "m2")
# #Model3
model_info(model = m3, 
           test_data = SR_m3_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm3", 
           manual_plotting = TRUE, 
           manual_plot_name = "m3")

###Compiled pdf -----------------------------------------------------------------------
DESIGN_abc <- "
111111
222222
222222
222222
222222
222222
222222
222222
"

m12_n <- nrow(SR_m12_data)
m3_n <- nrow(SR_m3_data)
model1_text <- paste0("Model 1: Clinical features (n=",m12_n,")")
model2_text <- paste0("Model 2: Clinical features + number of positive antibodies (n=",m12_n,")")
model3_text <- paste0("Model 3: Clinical features + number of positive antibodies + T1DGRS (n=",m3_n,")")


#### Model1 row output
m1_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_text,
                      face = "bold",
                      size = 12,
                      color = "black",
                      hjust = 1.2
                      )
  ),
  #Precision recall
  prec_recal_curves_m1 %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    theme_bw() +
    # geom_label(
    #   mapping = aes(x = 0.25, 
    #                 y = 0.225, 
    #                 label = unique(dat_text_pr_m1$auc_full), 
    #                 hjust = "centre"),
    #   #size = 7,
    #   #label.r = unit(0, "pt"),
    #   #label.padding=unit(0.4, "lines")
    # ) +
    theme(
      panel.spacing.x = unit(1.5, "lines")
    )
) + patchwork::plot_layout(design = DESIGN_abc) &
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 11)
  )


#### Model2
m2_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model2_text,
                      face = "bold",
                      size = 12,
                      color = "black",
                      hjust = 0.61
                      )
  ),
  #roc_plot
  prec_recal_curves_m2 %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    # geom_label(
    #   data = dat_text_pr_m2,
    #   mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "centre"),
    #   size = 7,
    #   label.r = unit(0, "pt"),
    #   label.padding=unit(0.4, "lines")
    # ) #+
    theme(
      panel.spacing.x = unit(1.5, "lines")
    )
)  + patchwork::plot_layout(design = DESIGN_abc) &
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 11)
  )

####Model 3
m3_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model3_text,
                      face = "bold",
                      size = 12,
                      color = "black",
                      hjust = 0.52
                      )
    #x = 0.125,
    #gp = grid::gpar(col = "black", fontsize = 20),
    #just = "left"
  ),
  #roc_plot
  patchwork::free(
  prec_recal_curves_m3 %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Precision", 
                       labels = scales::percent, 
                       #limits = c(0, 1)
                       ) +
    scale_x_continuous("Recall", 
                       labels = scales::percent, 
                       #limits = c(0, 1)
                       ) +
    # geom_label(
    #   data = dat_text_pr_m3,
    #   mapping = aes(x = 0.55, y = 0.9, label = auc_full, hjust = "centre"),
    #   size = 7,
    #   label.r = unit(0, "pt"),
    #   label.padding=unit(0.4, "lines")
    # ) #+
    theme(
      panel.spacing.x = unit(1.5, "lines")
    )
  )
) + patchwork::plot_layout(design = DESIGN_abc) &
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 11)
  )


####PDF
pdf("figures/Supp_Figure7.pdf", height = 15, width = 8)
model_display_item <- patchwork::wrap_plots(
  m1_plots_abc,
  m2_plots_abc,
  m3_plots_abc,
  ncol = 1, nrow = 3
)
print(model_display_item)
dev.off()

# pdf("figures/Supp_Figure7.pdf", height = 8, width = 8)
# print(m1_plots_abc)
# dev.off()