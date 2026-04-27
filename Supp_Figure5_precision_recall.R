########################################################################################

#Supplementary Figure 7: Precision-recall curves

#StartRight
#Primary outcome

##################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(pROC)
library(patchwork)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
# source("functions/var_characteristics_1.R")
# source("functions/model_continuous1.R")
# source("functions/cat_contingency.R")
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
  "Eth_5cat",
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
#Model 4
model_info(model = m4,
           test_data = SR_m3_data,
           outcome = "SRoutcome",
           saving_name = "03_04_sm4",
           manual_plotting = TRUE,
           manual_plot_name = "m4")

##StartRight Prime ----------------------------------------------------------
##Make complete case datasets ----------------------------------------------------------
SR_50_SRout_ccc<- SR_50_SRout_ccc %>%
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
SR_m12_data_p <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
#Model3
SR_m3_data_p <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_3))
### Make predictions ------------------------------------------------------------------
#Model1
model_info(model = m1,
           test_data = SR_m12_data_p,
           outcome = "SRoutcome",
           saving_name = "03_04_sm1_p",
           manual_plotting = TRUE,
           manual_plot_name = "m1_p")
#Model1
model_info(model = m2,
           test_data = SR_m12_data_p,
           outcome = "SRoutcome",
           saving_name = "03_04_sm2_p",
           manual_plotting = TRUE,
           manual_plot_name = "m2_p")
# #Model3
model_info(model = m3,
           test_data = SR_m3_data_p,
           outcome = "SRoutcome",
           saving_name = "03_04_sm3_p",
           manual_plotting = TRUE,
           manual_plot_name = "m3_p")
#Model 4
model_info(model = m4,
           test_data = SR_m3_data_p,
           outcome = "SRoutcome",
           saving_name = "03_04_sm4_p",
           manual_plotting = TRUE,
           manual_plot_name = "m4_p")

###Compiled pdf -----------------------------------------------------------------------
# DESIGN_abc <- "
# 111111
# 111111
# 222222
# 222222
# 222222
# 222222
# 222222
# 222222
# 222222
# "

m12_n <- nrow(SR_m12_data)
m3_n <- nrow(SR_m3_data)
model1_text <- paste0("Model 1: Clinical features (n=",m12_n,")")
model2_text <- paste0("Model 2: Clinical features + \n number of positive \n antibodies (n=",m12_n,")")
model3_text <- paste0("Model 3: Clinical features + \n number of positive \n antibodies + T1DGRS (n=",m3_n,")")
model4_text <- paste0("Model 4: Clinical features + \n T1DGRS (n=",m3_n,")")


#StartRight ----------------------------------------------------------------------
#### Model1 row output
m1_plots_abc <- prec_recal_curves_m1 %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    theme_bw() +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    )

#### Model2
m2_plots_abc <- prec_recal_curves_m2 %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    )

####Model 3
m3_plots_abc <- prec_recal_curves_m3 %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    )

####Model 4
m4_plots_abc <- prec_recal_curves_m4 %>%
      ggplot(aes(x = recall, y = precision)) +
      geom_path() +
      theme_bw() +
      scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
      scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
      theme(
        panel.spacing.x = unit(1.5, "lines"),
        text = element_text(size = 20)
      )


#StartRight Prime ---------------------------------------------------------------
#### Model1 row output
m1_plots_abc_p <- prec_recal_curves_m1_p %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    theme_bw() +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    )

#### Model2
m2_plots_abc_p <- prec_recal_curves_m2_p %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    )

####Model 3
m3_plots_abc_p <- prec_recal_curves_m3_p %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    )

####Model 4
m4_plots_abc_p <- prec_recal_curves_m4_p %>%
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
    theme(
      panel.spacing.x = unit(1.5, "lines"),
      text = element_text(size = 20)
    )

####PDF
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
                      hjust = 8)
  ),
  m1_plots_abc,
  m2_plots_abc,
  m3_plots_abc,
  m4_plots_abc) +  patchwork::plot_layout(design = DESIGN_abc)


model_display_item2 <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob("StartRight Prime",
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 5)
  ),
  m1_plots_abc_p,
  m2_plots_abc_p,
  m3_plots_abc_p,
  m4_plots_abc_p
) +  patchwork::plot_layout(design = DESIGN_abc)

pdf("figures/Supp_Figure5.pdf", height = 10, width = 24)
model_display_item <- patchwork::wrap_plots(
  model_display_item1,
  model_display_item2,
  ncol = 1, nrow = 2)+ patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C", "D",
                                                   "", "E", "F", "G", "H")))
print(model_display_item)
dev.off()
ggsave("figures/Supp_Figure5.jpeg", model_display_item, height = 10, width = 24)
#model_display_item <- (m1_plots_abc | m2_plots_abc) #/ (m3_plots_abc | m4_plots_abc)
#model_display_item <- (m1_plots_abc | m4_plots_abc)





# m1_plots_abc$patchwork$layout <- NULL
# m2_plots_abc$patchwork$layout <- NULL
# m3_plots_abc$patchwork$layout <- NULL
# m4_plots_abc$patchwork$layout <- NULL


# m1_plots_abc <- m1_plots_abc + labs(tag = "A")
# m2_plots_abc <- m2_plots_abc + labs(tag = "B")
# m3_plots_abc <- m3_plots_abc + labs(tag = "C")
# m4_plots_abc <- m4_plots_abc + labs(tag = "D")


# model_display_item <- patchwork::wrap_plots(
#   m1_plots_abc, m2_plots_abc, m3_plots_abc, m4_plots_abc,
#   ncol = 2, nrow = 2
# )
#
# pdf("figures/Supp_Figure7.pdf", height = 10, width = 12)
# print(model_display_item)
# dev.off()
# pdf("figures/Supp_Figure7.pdf", height = 8, width = 8)
# print(m1_plots_abc)
# dev.off()


# m1_plots_abc <- m1_plots_abc + labs(tag = "A")
# m2_plots_abc <- m2_plots_abc + labs(tag = "B")
# m3_plots_abc <- m3_plots_abc + labs(tag = "C")
# m4_plots_abc <- m4_plots_abc + labs(tag = "D")
#
# # Combine without plot_annotation()
# pdf("figures/Supp_Figure7.pdf", height = 10, width = 12)
# model_display_item <- patchwork::wrap_plots(
#   m1_plots_abc, m2_plots_abc, m3_plots_abc, m4_plots_abc,
#   ncol = 2, nrow = 2
# )
# print(model_display_item)
# dev.off()


#######################################################################################
# Add tags to plots
# m1_plot <- prec_recal_curves_m1 %>%
#   ggplot(aes(x = recall, y = precision)) +
#   geom_path() +
#   scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
#   scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
#   theme_bw() +
#   theme(panel.spacing.x = unit(1.5, "lines"), text = element_text(size = 20)) +
#   labs(tag = "A")
#
# m2_plot <- prec_recal_curves_m2 %>%
#   ggplot(aes(x = recall, y = precision)) +
#   geom_path() +
#   scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
#   scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
#   theme_bw() +
#   theme(panel.spacing.x = unit(1.5, "lines"), text = element_text(size = 20)) +
#   labs(tag = "B")
#
# m3_plot <- prec_recal_curves_m3 %>%
#   ggplot(aes(x = recall, y = precision)) +
#   geom_path() +
#   scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
#   scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
#   theme_bw() +
#   theme(panel.spacing.x = unit(1.5, "lines"), text = element_text(size = 20)) +
#   labs(tag = "C")
#
# m4_plot <- prec_recal_curves_m4 %>%
#   ggplot(aes(x = recall, y = precision)) +
#   geom_path() +
#   scale_y_continuous("Precision", labels = scales::percent, limits = c(0, 1)) +
#   scale_x_continuous("Recall", labels = scales::percent, limits = c(0, 1)) +
#   theme_bw() +
#   theme(panel.spacing.x = unit(1.5, "lines"), text = element_text(size = 20)) +
#   labs(tag = "D")
#
#
# model1_text <- paste0("Model 1: Clinical features (n=",m12_n,")")
# model2_text <- paste0("Model 2: Clinical features + number of \n positive antibodies (n=",m12_n,")")
# model3_text <- paste0("Model 3: Clinical features + number of \n positive antibodies + T1DGRS (n=",m3_n,")")
# model4_text <- paste0("Model 4: Clinical features + T1DGRS (n=",m3_n,")")
#
# # Wrap headers as elements
# header1 <- patchwork::wrap_elements(ggpubr::text_grob(model1_text, face = "bold", size = 12))
# header2 <- patchwork::wrap_elements(ggpubr::text_grob(model2_text, face = "bold", size = 12))
# header3 <- patchwork::wrap_elements(ggpubr::text_grob(model3_text, face = "bold", size = 12))
# header4 <- patchwork::wrap_elements(ggpubr::text_grob(model4_text, face = "bold", size = 12))
#
# # Combine header + plot vertically for each model
# m1_combined <- header1 / m1_plot + patchwork::plot_layout(heights = c(0.1, 1))
# m2_combined <- header2 / m2_plot + patchwork::plot_layout(heights = c(0.1, 1))
# m3_combined <- header3 / m3_plot + patchwork::plot_layout(heights = c(0.1, 1))
# m4_combined <- header4 / m4_plot + patchwork::plot_layout(heights = c(0.1, 1))
#
# # Combine all models in grid
# final_layout <- (m1_combined | m2_combined) / (m3_combined | m4_combined)
#
# # Adjust heights so headers are smaller
# #final_layout <- final_layout #+ patchwork::plot_layout(heights = c(0.1, 1, 0.1, 1))
#
# # Save
# pdf("figures/Supp_Figure7.pdf", height = 12, width = 15)
# print(final_layout)
# dev.off()
