##########################################################################################
#CPRD code
##########################################################################################
#setwd("C:/Users/ky279/OneDrive - University of Exeter/CPRD/2025/Julieanne progression/Final/") ##########################################################################################

# Get data from MySQL
library(tidyverse)
library(aurum)

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")
analysis <- cprd$analysis("jk_at_diag")
data <- data %>% analysis$cached("cohort_v2") %>% collect()

# Format and rename variables
data <- data %>%
  mutate(Gender_v1=ifelse(gender==1, "Male", "Female"),
         DKA=dka_at_diag,
         Unintentional_weight_loss_v1=ifelse(weight_loss_at_diag==1, "Yes", "No"),
         osmotic=osmotic_symptoms_at_diag,
         autoimmune=ifelse(coeliac==1 | vitiligo==1 | addisons==1 | graves==1 | hashimotos==1 | pernicious_anaemia==1 | rheumatoidarthritis==1, 1, 0),
         famhisdiab=ifelse(is.na(fh_diabetes) | fh_diabetes==0, "No", "Yes"),
         SRoutcome=ifelse(diabetes_type=="type 1", 1, 0),
         Eth_5cat = ethnicity_5cat) %>%
  rename(AgeatDiagnosis=dm_diag_age,
         bmi_model=prebmi,
         HbA1c_at_diagnosis_v1=prehba1c) %>%
  select(Gender_v1, DKA, Unintentional_weight_loss_v1, osmotic, autoimmune,
         famhisdiab, SRoutcome, AgeatDiagnosis, bmi_model, HbA1c_at_diagnosis_v1,
         dka, t1_code, basal_bolus, Eth_5cat)


# Check no missingness except for ethnicity and IMD

data %>%
  filter(is.na(Gender_v1)
         | is.na(DKA)
         | is.na(Unintentional_weight_loss_v1)
         | is.na(osmotic)
         | is.na(autoimmune)
         | is.na(famhisdiab)
         | is.na(SRoutcome)
         | is.na(AgeatDiagnosis)
         | is.na(bmi_model)
         | is.na(HbA1c_at_diagnosis_v1)) %>%
  count()
#0


##############################################################################################
#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(naniar)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
#Load data -------------------------------------------------------------------------
#load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
#load("m1_alt.RData")
load("m1.RData")
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
  "famhisdiab"#,
  #"famhisauto",
  #"num_anti"
)
all_vars_12 <- c(varlist_12, varlist_cat_12)

##Make complete case datasets ----------------------------------------------------------
data <- data %>%
  mutate(
    famhisnoninsdiab = ifelse(is.na(famhisdiab), "No", famhisdiab),
    Eth_4cat = ifelse(Eth_5cat %in% c("3", "4"), "Other/Mixed",
                      ifelse(Eth_5cat == "0", "White",
                             ifelse(Eth_5cat == "2", "Black",
                                    ifelse(Eth_5cat == "1", "South Asian", Eth_5cat)))))
data$SRoutcome <- as.numeric(data$SRoutcome)
data$DKA <- as.character(data$DKA)
data$osmotic <- as.character(data$osmotic)
data$autoimmune <- as.character(data$autoimmune)

##StartRight
data <- data %>%
  drop_na(all_of(all_vars_12))
# #Model12
# SR_m12_data <- SR_SRout_ccc %>%
#   drop_na(all_of(all_vars_12))

#Characteristics tables -------------------------------------------------------------
##Whole_SR ----------------------------------------------------------------------------
# var_characteristics(varlist = varlist_3,
#                     varlist_cat = varlist_cat_3,
#                     dataset = SR_SRout_ccc,
#                     numeric_option = "medianIQR",
#                     group = "SRoutcome",
#                     table_name = "tables/table_03_04_SR_medianIQR",
#                     p_value_testing = FALSE)
#
# var_characteristics(varlist = varlist_3,
#                     varlist_cat = varlist_cat_3,
#                     dataset = SR_SRout_ccc,
#                     numeric_option = "medianIQR",
#                     table_name = "tables/table_03_04_SR_medianIQR_all",
#                     p_value_testing = FALSE)

##Model12 ----------------------------------------------------------------------------
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12,
#                     dataset = data,
#                     numeric_option = "medianIQR",
#                     group = "SRoutcome",
#                     table_name = "tables/cprd_table_by_outcome",
#                     p_value_testing = FALSE,
#                     missingness = FALSE)
#
# var_characteristics(varlist = varlist_12,
#                     varlist_cat = varlist_cat_12,
#                     dataset = data,
#                     numeric_option = "medianIQR",
#                     table_name = "tables/cprd_table_overall",
#                     p_value_testing = FALSE,
#                     missingness = FALSE)


#Modelling descriptives------------------------------------------------------------------
##Display item---------------------------------------------------------------------------
###Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m1,
           test_data = data,
           outcome = "SRoutcome",
           saving_name = "model",
           manual_plotting = TRUE,
           manual_plot_name = "m1")

### Compile PDF
DESIGN_abc <- "
11111#
  223344
  223344
  223344
  223344
  223344
"
m12_n <- prettyNum(nrow(data),big.mark=",",scientific=FALSE)
n12_T1D <- data %>%
  filter(SRoutcome == 1) %>%
  nrow()
n12_T1D <- prettyNum(n12_T1D,big.mark=",",scientific=FALSE)
data <- data %>%
  mutate(SRoutcome = as.character(SRoutcome))

model1_text <- paste0("Clinical features only model (n=",m12_n,"; Type 1=",n12_T1D,")")
dat_text_m1 <- dat_text_m1 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))

m1_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 1.3)
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
  ggplot(data, aes(x=factor(SRoutcome,
                                 levels = c("1","0"),
                                 labels = c("Progressed ",
                                            "Did not progress")),
                        y = pred_prob_m1)) +
    geom_violin(aes(fill = SRoutcome, alpha = 0.2)) +
    geom_boxplot(aes(fill = SRoutcome),width = .15,
                 outlier.size = 1) +
    scale_fill_manual(values = c("#f1b955", "#5a8be2")) +
    xlab("Progresion to insulin")+
    ylab("Predicted probability of Progressed  \n vs Not Progressed to insulin") +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 20)),
  #calibration plot
  ggplot(cal_prep_m1, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of Progressors \n to insulin in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw() +
    theme(text = element_text(size = 20))
) + patchwork::plot_layout(design = DESIGN_abc)

pdf("figures/Supp_Figure31.pdf", height = 8, width = 24)
# model_display_item <- patchwork::wrap_plots(
#   m1_plots_abc,
# ) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C")))
model_display_item <- m1_plots_abc + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C")))

print(model_display_item)
dev.off()
ggsave("figures/Supp_Figure31.jpeg", model_display_item, height = 8, width = 24)

#Proportions in >90% probability ----------------------------------------------------------
CPRD_90_checks <- data %>%
  mutate(prob = ifelse(pred_prob_m1 < 0.1, "<0.1", ifelse(pred_prob_m1 > 0.9, ">0.9", "0.1-0.9"))) %>%
  group_by(prob) %>%
  summarise(
    n=n(),
    nT1_outcome = sum(SRoutcome == 1),
    pT1_outcome = nT1_outcome/n*100,
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  ) %>%
  mutate(
    `% progress to insulin within 3 years` = paste0(round(pT1_outcome,1), "(", nT1_outcome, "/", n, ")"),
    `% experienced DKA` = paste0(round(pDKA,1), "(", nDKA, "/", n, ")"),
    `% later T1D code` = paste0(round(pT1D,1), "(", nT1D, "/", n, ")"),
    `% basal bolus regime at latest follow up` = paste0(round(pbasal,1), "(", nbasal, "/", n, ")")
  ) %>%
  select(prob, n, `% progress to insulin within 3 years`,
         `% experienced DKA`, `% later T1D code`, `% basal bolus regime at latest follow up`)
write_xlsx(CPRD_90_checks, "tables/CPRD_90_checks.xlsx")

CPRD_10_checks <- data %>%
  mutate(prob = ifelse(pred_prob_m1 < 0.1, "<0.1", ">=0.1")) %>%
  group_by(prob) %>%
  summarise(
    n=n(),
    nT1_outcome = sum(SRoutcome == 1),
    pT1_outcome = nT1_outcome/n*100,
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  ) %>%
  mutate(
    `% progress to insulin within 3 years` = paste0(round(pT1_outcome,1), "(", nT1_outcome, "/", n, ")"),
    `% experienced DKA` = paste0(round(pDKA,1), "(", nDKA, "/", n, ")"),
    `% later T1D code` = paste0(round(pT1D,1), "(", nT1D, "/", n, ")"),
    `% basal bolus regime at latest follow up` = paste0(round(pbasal,1), "(", nbasal, "/", n, ")")
  ) %>%
  select(prob, n, `% progress to insulin within 3 years`,
         `% experienced DKA`, `% later T1D code`, `% basal bolus regime at latest follow up`)
write_xlsx(CPRD_10_checks, "tables/CPRD_10_checks.xlsx")

CPRD_50_checks <- data %>%
  mutate(prob = ifelse(pred_prob_m1 < 0.5, "<0.5", ">=0.5")) %>%
  group_by(prob) %>%
  summarise(
    n=n(),
    nT1_outcome = sum(SRoutcome == 1),
    pT1_outcome = nT1_outcome/n*100,
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  ) %>%
  mutate(
    `% progress to insulin within 3 years` = paste0(round(pT1_outcome,1), "(", nT1_outcome, "/", n, ")"),
    `% experienced DKA` = paste0(round(pDKA,1), "(", nDKA, "/", n, ")"),
    `% later T1D code` = paste0(round(pT1D,1), "(", nT1D, "/", n, ")"),
    `% basal bolus regime at latest follow up` = paste0(round(pbasal,1), "(", nbasal, "/", n, ")")
  ) %>%
  select(prob, n, `% progress to insulin within 3 years`,
         `% experienced DKA`, `% later T1D code`, `% basal bolus regime at latest follow up`)
write_xlsx(CPRD_50_checks, "tables/CPRD_50_checks.xlsx")

CPRD_decile_checks <- data %>%
  mutate(prob = case_when(
    pred_prob_m1 < 0.1 ~ "<0.1",
    pred_prob_m1 < 0.2 ~ "0.1-0.2",
    pred_prob_m1 < 0.3 ~ "0.2-0.3",
    pred_prob_m1 < 0.4 ~ "0.3-0.4",
    pred_prob_m1 < 0.5 ~ "0.4-0.5",
    pred_prob_m1 < 0.6 ~ "0.5-0.6",
    pred_prob_m1 < 0.7 ~ "0.6-0.7",
    pred_prob_m1 < 0.8 ~ "0.7-0.8",
    pred_prob_m1 < 0.9 ~ "0.8-0.9",
    pred_prob_m1 >= 0.9 ~ ">=0.9")
    ) %>%
  group_by(prob) %>%
  summarise(
    n=n(),
    nT1_outcome = sum(SRoutcome == 1),
    pT1_outcome = nT1_outcome/n*100,
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  ) %>%
  mutate(
    `% progress to insulin within 3 years` = paste0(round(pT1_outcome,1), "(", nT1_outcome, "/", n, ")"),
    `% experienced DKA` = paste0(round(pDKA,1), "(", nDKA, "/", n, ")"),
    `% later T1D code` = paste0(round(pT1D,1), "(", nT1D, "/", n, ")"),
    `% basal bolus regime at latest follow up` = paste0(round(pbasal,1), "(", nbasal, "/", n, ")")
  ) %>%
  select(prob, n, `% progress to insulin within 3 years`,
         `% experienced DKA`, `% later T1D code`, `% basal bolus regime at latest follow up`)
write_xlsx(CPRD_decile_checks, "tables/CPRD_decile_checks.xlsx")

CPRD_qunitile_checks <- data %>%
  mutate(prob = case_when(
    pred_prob_m1 < 0.2 ~ "<0.2",
    pred_prob_m1 < 0.4 ~ "0.2-0.4",
    pred_prob_m1 < 0.6 ~ "0.4-0.6",
    pred_prob_m1 < 0.8 ~ "0.6-0.8",
    pred_prob_m1 >= 0.8 ~ ">=0.8"
  )) %>%
  group_by(prob) %>%
  summarise(
    n=n(),
    nT1_outcome = sum(SRoutcome == 1),
    pT1_outcome = nT1_outcome/n*100,
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  ) %>%
  mutate(
    `% progress to insulin within 3 years` = paste0(round(pT1_outcome,1), "(", nT1_outcome, "/", n, ")"),
    `% experienced DKA` = paste0(round(pDKA,1), "(", nDKA, "/", n, ")"),
    `% later T1D code` = paste0(round(pT1D,1), "(", nT1D, "/", n, ")"),
    `% basal bolus regime at latest follow up` = paste0(round(pbasal,1), "(", nbasal, "/", n, ")")
  ) %>%
  select(prob, n, `% progress to insulin within 3 years`,
         `% experienced DKA`, `% later T1D code`, `% basal bolus regime at latest follow up`)
write_xlsx(CPRD_qunitile_checks, "tables/CPRD_qunitile_checks.xlsx")

data %>%
  filter(pred_prob_m1 < 0.1) %>%
  summarise(
    n=n(),
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  )
data %>%
  filter(pred_prob_m1 < 0.2) %>%
  summarise(
    n=n(),
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  )
data %>%
  filter(pred_prob_m1 > 0.8) %>%
  summarise(
    n=n(),
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  )
data %>%
  filter(pred_prob_m1 >= 0.1 & pred_prob_m1 <= 0.9) %>%
  summarise(
    n=n(),
    nDKA = sum(dka == 1),
    pDKA = nDKA/n*100,
    nT1D = sum(t1_code == 1),
    pT1D = nT1D/n*100,
    nbasal = sum(basal_bolus == 1),
    pbasal = nbasal/n*100
  )
#Clinical features score ----------------------------------------------------------
