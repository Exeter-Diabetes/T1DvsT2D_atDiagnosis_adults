########################################################################################

#Figure
#Based on information from Table 1 and 2

#Panel plot of univariate features

#StartRight
#Primary outcome

##################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(freqtables)
library(gdata)
#load functions ------------------------------------------------------------------

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
#Add model vars ----------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto)
  )


#list of vars that add -------------------------------------------------------------


#Create variable lists -----------------------------------------------------------------
## Continuous variables
varlist = c("AgeatDiagnosis",
            "bmi_model",
            #"Waist_v1",
            "wh_ratio_v1",
            "HbA1c_at_diagnosis_v1"
)
## create varlist_cat (categorical variables of interest names)
varlist_cat = c(
  "Eth_5cat",
  "Gender_v1",
  "DKA",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "osmotic",
  #"famhisdiab",
  "famhisnoninsdiab",
  "famhisauto"
)

#Produce complete case datasets ----------------------------------------------------------
all_vars <- c(varlist, varlist_cat)
##StartRight
SR_SRout_ccc <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars))

for(v in varlist_cat) {
  ci <- SR_SRout_ccc %>%
    freq_table(SRoutcome, !!sym(v)) %>%
    #filter(col_cat == "1") %>%
    select(row_cat, col_cat, percent_row, lcl_row, ucl_row) %>%
    rename(
      SRoutcome = row_cat
    )
  mv(from= "ci",
     to = paste0(v, "_ci"),
     envir = globalenv())
}

Eth_5cat_ci <- SR_SRout_ccc %>%
  freq_table(Eth_5cat,SRoutcome) %>%
  #filter(col_cat == "1") %>%
  select(row_cat, col_cat, percent_row, lcl_row, ucl_row) %>%
  rename(
    SRoutcome = col_cat,
    Eth_5cat = row_cat
  )


## horizontal ------------------------------------------------------------------------------
panel_plot_horizontal <- patchwork::wrap_plots(
  #a) BMI
  SR_SRout_ccc %>%
    ggplot(aes(fill = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               x = bmi_model)) +
    geom_density(aes(alpha = 0.2)) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    ylim(0,0.12) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          text = element_text(size = 25),
          axis.text.y = element_blank()) +
    labs(x = "BMI (kg/m2) at diagnosis",
         title = "BMI at diagnosis",
         subtitle = "AUCROC: 0.886"),
  #b) weightloss
  Unintentional_weight_loss_v1_ci %>%
    filter(col_cat == "Yes") %>%
    ggplot(aes(x = factor(SRoutcome,
                          levels = c("1","0"),
                          labels = c("Type 1", "Type 2")),
               y = percent_row)) +
    geom_col(aes(fill = factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")))) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
    ylim(0,100) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 25)) +
    labs(y = "% unintentional \n weight loss",
         title = "Unintentional weight loss",
         subtitle = "AUCROC: 0.772"),
  #c) WHR
  SR_SRout_ccc %>%
    ggplot(aes(fill = factor(SRoutcome,
                          levels = c("1","0"),
                          labels = c("Type 1", "Type 2")),
               x = wh_ratio_v1)) +
    geom_density(aes(alpha = 0.2)) +
    ylim(0,5.5) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          text = element_text(size = 25),
          axis.text.y = element_blank()) +
    labs(x = "Waist-hip ratio",
         title = "Waist-hip ratio",
         subtitle = "AUCROC: 0.771"),
  #d) Age at diagnosis
  SR_SRout_ccc %>%
    ggplot(aes(fill = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               x = AgeatDiagnosis)) +
    geom_density(aes(alpha = 0.2)) +
    ylim(0,0.08) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          text = element_text(size = 25),
          axis.text.y = element_blank()) +
    labs(x = "Age at diagnosis (years)",
         title = "Age at diagnosis",
         subtitle = "AUCROC: 0.768"),
  #e) HbA1c
  SR_SRout_ccc %>%
    #filter(robust_outcome == "T1D" & !is.na(HbA1c_at_diagnosis_v1)) %>%
    ggplot(aes(fill = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               x = HbA1c_at_diagnosis_v1)) +
    geom_density(aes(alpha = 0.2)) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    ylim(0,0.025) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          text = element_text(size = 25),
          axis.text.y = element_blank()) +
    labs(x = "HbA1c (mmol/mol) at diagnosis",
         title = "HbA1c at diagnosis",
         subtitle = "AUCROC: 0.715"),
  #f) Parent history of non insulin treated diabetes
  famhisnoninsdiab_ci %>%
    filter(col_cat == "Yes") %>%
    ggplot(aes(x = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               y = percent_row)) +
    geom_col(aes(fill = factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")))) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
    ylim(0,100) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 25)) +
    labs(y = "% Parent with non-insulin \n -treated diabetes",
         title = "Parent with non-insulin-treated \n diabetes",
         subtitle = "AUCROC: 0.609"),
  #g) osmotic
  osmotic_ci %>%
    filter(col_cat == "1") %>%
    ggplot(aes(x = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               y = percent_row)) +
    geom_col(aes(fill = factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")))) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
    ylim(0,100) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 25)) +
    labs(y = "% osmotic symptoms",
         title = "Osmotic symptoms",
         subtitle = "AUCROC: 0.605"),
  #h) DKA
  DKA_ci %>%
    filter(col_cat == "1") %>%
    ggplot(aes(x = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               y = percent_row)) +
    geom_col(aes(fill = factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")))) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
    ylim(0,100) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 25)) +
    labs(y = "% DKA",
         title = "DKA",
         subtitle = "AUCROC: 0.577"),
  #i) Sex
  Gender_v1_ci %>%
    filter(col_cat == "Female") %>%
    ggplot(aes(x = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               y = percent_row)) +
    geom_col(aes(fill = factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")))) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
    ylim(0,100) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 25)) +
    labs(y = "% female",
         title = "Sex",
         subtitle = "AUCROC: 0.550"),
  #J) parent history of autoimmune
  famhisauto_ci %>%
    filter(col_cat == "1") %>%
    ggplot(aes(x = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               y = percent_row)) +
    geom_col(aes(fill = factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")))) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
    ylim(0,100) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 25)) +
    labs(y = "% parent with \n autoimmune disease",
         title = "Parent history of autoimmune \n disease",
         subtitle = "AUCROC: 0.541"),
  #L) ETHNICITY
  #Option 1: stacked
  Eth_5cat_ci %>%
    #filter(col_cat == "1") %>%
    ggplot(aes(x = factor(Eth_5cat,
                          #levels = c(1,0),
                          labels = c("Black","Mixed", "Other", "South Asian", "White")),
               y = percent_row,
               # fill = factor(SRoutcome,
               #               levels = c("1","0"),
               #               labels = c("Type 1", "Type 2"))
               )) +
    geom_col(aes(fill = factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")))) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    #geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
    ylim(0,100) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 25)) +
    labs(y = "%",
         title = "Ethnicity",
         subtitle = "AUCROC: 0.536",
         #fill = "Ethnicity"
         ),
  #Option 2: separate
  # Eth_5cat_ci %>%
  #   #filter(col_cat == "1") %>%
  #   ggplot(aes(x = factor(SRoutcome,
  #                         levels = c(1,0),
  #                         labels = c("Type 1", "Type 2")),
  #              y = percent_row,
  #              fill = col_cat)) +
  #   geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  #   #scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
  #   geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2), position=position_dodge(.9)) +
  #   ylim(0,100) +
  #   theme_light() +
  #   theme(legend.title = element_blank(),
  #         legend.position = "bottom",
  #         axis.title.x = element_blank(),
  #         text = element_text(size = 25)) +
  #   labs(y = "%",
  #        title = "Ethnicity",
  #        subtitle = "AUCROC: 0.536",
  #        fill = "Ethnicity"),
  #L) autoimmune
  autoimmune_ci %>%
    filter(col_cat == "1") %>%
    ggplot(aes(x = factor(SRoutcome,
                          levels = c(1,0),
                          labels = c("Type 1", "Type 2")),
               y = percent_row)) +
    geom_col(aes(fill = factor(SRoutcome,
                               levels = c("1","0"),
                               labels = c("Type 1", "Type 2")))) +
    scale_fill_manual(values = c("#5a8be2", "#f1b955")) +
    geom_errorbar(aes(ymin = lcl_row, ymax = ucl_row, width = 0.2)) +
    ylim(0,100) +
    theme_light() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 25)) +
    labs(y = "% additional autoimmune \n disease",
         title = "Personal history of autoimmune \n disease",
         subtitle = "AUCROC: 0.520"),
  #format
  ncol = 4, nrow = 3
) + patchwork::plot_annotation(tag_levels = "a"
                               #theme = theme(plot.margin = margin(5, 20, 5, 20)) # applies to all plots
                               #theme = theme(plot.margin = margin(5, 5, 5, 5))
) & theme(
  plot.tag.location = "plot"#,
  #plot.background = element_rect(colour = "green"),
  #panel.background = element_rect(colour = "blue"),
  #plot.tag.position = "topleft",
  #plot.margin = margin(5, 5, 5, 5)
  )
#& theme(plot.margin = margin(0,1,0,1))

pdf("figures/Figure1.pdf", width = 30, height = 15)
panel_plot_horizontal
dev.off()
# pdf("figures/Fig_1_HOR_density_presentation.pdf", width = 11.5, height = 4.76)
# panel_plot_horizontal
# dev.off()
