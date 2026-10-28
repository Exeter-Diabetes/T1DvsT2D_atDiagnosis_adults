##############################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
#library(rms)
#library(naniar)
library(writexl)
library(pROC)
library(patchwork)
library(ggtext)
library(ggpattern)
library(dcurves)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
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
  "Eth_5cat",
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  "famhisdiab",
  #"famhisauto",
  "num_anti"
)
all_vars_12 <- c(varlist_12, varlist_cat_12)

##Make complete case datasets ----------------------------------------------------------
#StartRight -----------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto),
         data = "StartRight",
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)

SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
#StartRight Prime-----------------------------------------------------------------------
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto),
         data = "StartRight Prime",
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
SR_50_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
#Joint SR ----------------------------------------------------------------------
SR_joint_cc <- full_join(SR_m12_data, SR_50_m12_data)



#Define strategies -----------------------------------------------------------------
##Joint SR ----------------------------------------------------------------------
SR_joint_cc <- SR_joint_cc %>%
  mutate(
    NICEfeatures1_noage = ifelse(
      (!is.na(DKA) & DKA == "1")
      | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes")
      | (!is.na(AgeatDiagnosis) & AgeatDiagnosis < 50)
      | (!is.na(bmi_model) & bmi_model < 25)
      | (!is.na(autoimmune) & autoimmune == "1"
         | !is.na(famhisauto) & famhisauto == "1"),
      "1+ NICE features",
      ifelse(is.na(DKA)
             & is.na(Unintentional_weight_loss_v1)
             & is.na(AgeatDiagnosis)
             & is.na(bmi_model)
             & is.na(autoimmune)
             & is.na(famhisauto),
             NA,
             "No NICE features")),
    pos_anti = ifelse(num_anti >= 1, "1+ pos anti", "Negative antibodies"),
    NICEfeatures1 = ifelse((!is.na(DKA) & DKA == "1")
                           | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes")
                           | (!is.na(bmi_model) & bmi_model < 25)
                           | (!is.na(autoimmune) & autoimmune == "1"
                              | !is.na(famhisauto) & famhisauto == "1"),
                           "1+ NICE features",
                           ifelse(is.na(DKA)
                                  & is.na(Unintentional_weight_loss_v1)
                                  & is.na(bmi_model)
                                  & is.na(autoimmune)
                                  & is.na(famhisauto),
                                  NA,
                                  "No NICE features")),
    ADAfeatures = ifelse(
      (!is.na(DKA) & DKA == "1")
      | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes")
      | (!is.na(AgeatDiagnosis) & AgeatDiagnosis < 35)
      | (!is.na(bmi_model) & bmi_model < 25)
      #Glucose at diagnosis
      | (!is.na(Glucose_at_diagnosis_v1) & Glucose_at_diagnosis_v1 > 20),
      "1+ ADA features",
      ifelse(is.na(DKA)
             & is.na(Unintentional_weight_loss_v1)
             & is.na(AgeatDiagnosis)
             & is.na(bmi_model)
             & is.na(Glucose_at_diagnosis_v1),
             NA,
             "No ADA features")
    ))
SR_joint_cc <- SR_joint_cc %>%
  dplyr::select(Study_ID, SRoutcome, robust_outcome, AgeatDiagnosis, bmi_model,
         HbA1c_at_diagnosis_v1, Gender_v1,
         osmotic, autoimmune, Unintentional_weight_loss_v1, DKA,
         famhisnoninsdiab, num_anti, NICEfeatures1,
         NICEfeatures1_noage,
         ADAfeatures,
         pos_anti, data, ethnicity, Eth_4cat) %>%
  mutate(
    m1_pp = predict(m1, SR_joint_cc, type = "response"),
    m2_pp = predict(m2, SR_joint_cc, type = "response"),
    m3_pp = predict(m3, SR_joint_cc, type = "response"),
    m4_pp = predict(m4, SR_joint_cc, type = "response"))





A_M1 <- dca(SRoutcome ~ m1_pp,
         data = SR_joint_cc %>%
           filter(data == "StartRight"),
         thresholds = seq(0, 0.35, by = 0.01))
A_text <- A_M1 %>%
  as_tibble() %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold,
              names_from = variable,
              values_from = net_benefit) %>%
  filter(threshold == 0.1) %>%
  mutate(nb_increase = m1_pp - all)

A <-  plot(A_M1, smooth = TRUE) + ggplot2::scale_color_manual(values = c('black', 'grey', '#f1b955'))

B_M1 <- dca(SRoutcome ~ m2_pp,
            data = SR_joint_cc %>%
              filter(data == "StartRight"),
            thresholds = seq(0, 0.35, by = 0.01))
B_text <- B_M1 %>%
  as_tibble() %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold,
              names_from = variable,
              values_from = net_benefit) %>%
  filter(threshold == 0.1) %>%
  mutate(nb_increase = m2_pp - all)

B <-  plot(B_M1, smooth = TRUE) + ggplot2::scale_color_manual(values = c('black', 'grey', '#f1b955'))

C_M1 <- dca(SRoutcome ~ m3_pp,
            data = SR_joint_cc %>%
              filter(data == "StartRight"),
            thresholds = seq(0, 0.35, by = 0.01))
C_text <- C_M1 %>%
  as_tibble() %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold,
              names_from = variable,
              values_from = net_benefit) %>%
  filter(threshold == 0.1) %>%
  mutate(nb_increase = m3_pp - all)

C <-  plot(C_M1, smooth = TRUE) + ggplot2::scale_color_manual(values = c('black', 'grey', '#f1b955'))

D_M1 <- dca(SRoutcome ~ m4_pp,
            data = SR_joint_cc %>%
              filter(data == "StartRight"),
            thresholds = seq(0, 0.35, by = 0.01))
D_text <- D_M1 %>%
  as_tibble() %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold,
              names_from = variable,
              values_from = net_benefit) %>%
  filter(threshold == 0.1) %>%
  mutate(nb_increase = m4_pp - all)

D <-  plot(D_M1, smooth = TRUE) + ggplot2::scale_color_manual(values = c('black', 'grey', '#f1b955'))

E_M1 <- dca(SRoutcome ~ m1_pp,
            data = SR_joint_cc %>%
              filter(data == "StartRight Prime"),
            thresholds = seq(0, 0.35, by = 0.01))
E_text <- E_M1 %>%
  as_tibble() %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold,
              names_from = variable,
              values_from = net_benefit) %>%
  filter(threshold == 0.1) %>%
  mutate(nb_increase = m1_pp - all)

E <-  plot(E_M1, smooth = TRUE) + ggplot2::scale_color_manual(values = c('black', 'grey', '#f1b955'))

F_M1 <- dca(SRoutcome ~ m2_pp,
            data = SR_joint_cc %>%
              filter(data == "StartRight Prime"),
            thresholds = seq(0, 0.35, by = 0.01))
F_text <- F_M1 %>%
  as_tibble() %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold,
              names_from = variable,
              values_from = net_benefit) %>%
  filter(threshold == 0.1) %>%
  mutate(nb_increase = m2_pp - all)

F1 <-  plot(F_M1, smooth = TRUE) + ggplot2::scale_color_manual(values = c('black', 'grey', '#f1b955'))

G_M1 <- dca(SRoutcome ~ m3_pp,
            data = SR_joint_cc %>%
              filter(data == "StartRight Prime"),
            thresholds = seq(0, 0.35, by = 0.01))
G_text <- G_M1 %>%
  as_tibble() %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold,
              names_from = variable,
              values_from = net_benefit) %>%
  filter(threshold == 0.1) %>%
  mutate(nb_increase = m3_pp - all)

G <-  plot(G_M1, smooth = TRUE) + ggplot2::scale_color_manual(values = c('black', 'grey', '#f1b955'))

H_M1 <- dca(SRoutcome ~ m4_pp,
            data = SR_joint_cc %>%
              filter(data == "StartRight Prime"),
            thresholds = seq(0, 0.35, by = 0.01))
H_text <- H_M1 %>%
  as_tibble() %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold,
              names_from = variable,
              values_from = net_benefit) %>%
  filter(threshold == 0.1) %>%
  mutate(nb_increase = m4_pp - all)

H <-  plot(H_M1, smooth = TRUE) + ggplot2::scale_color_manual(values = c('black', 'grey', '#f1b955'))

DESIGN_abc <- "
11111111
22334455
22334455
22334455
22334455
22334455
22334455
"
model_display_item1 <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob("StartRight",
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 4)
  ),
  A,
  B,
  C,
  D) +  patchwork::plot_layout(design = DESIGN_abc)


model_display_item2 <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob("StartRight Prime",
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 2.5)
  ),
  E,
  F1,
  G,
  H
) +  patchwork::plot_layout(design = DESIGN_abc)

pdf("figures/Supp_Figurex7_Decision_curve.pdf", height = 7, width = 12)
model_display_item <- patchwork::wrap_plots(
  model_display_item1,
  model_display_item2,
  ncol = 1, nrow = 2)+ patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C", "D",
                                                                      "", "E", "F", "G", "H"))
                                                  ) + patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "none")
print(model_display_item)
dev.off()
ggsave("figures/Supp_Figurex7_decisioncurve.jpeg", model_display_item, height = 7, width = 12)
