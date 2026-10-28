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
  dplyr::select(Gender_v1, DKA, Unintentional_weight_loss_v1, osmotic, autoimmune,
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
source("functions/model_info2.R")
source("functions/var_characteristics_1.R")
#Load data -------------------------------------------------------------------------
#load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
#load("m1_alt.RData")
#load("m1.RData")
# Model parameters and formulas -----------------------------------------------------------
## Model 1 -----------------------------------------------------------------------------------
m1_beta <- c(
  "(Intercept)" = 8.64624460772943,
  "AgeatDiagnosis" = -0.0964441683502511,
  "bmi_model" = -0.243555720621392,
  "HbA1c_at_diagnosis_v1" = 0.00341120554236777,
  "Gender_v1Male" = -0.863116972338412,
  "osmotic1" = 0.961134759411358,
  "autoimmune1" = 0.823010855901484,
  "Unintentional_weight_loss_v1Yes" = 1.35018265494132,
  "DKA1" = 0.734312980329503,
  'Eth_4catBlack' = -1.32530674465559,
  'Eth_4catOther/Mixed' = -1.34567865383264,
  'Eth_4catSouth Asian' = -0.793896878252353,
  "famhisnoninsdiabYes" = -1.01442801038726
)
m1_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab
# User-defined function -------------------------------------------------------------
prepare_prediction_data <- function(dat){

  dat %>%
    mutate(
      Eth_4cat = factor(
        Eth_4cat,
        levels = c(
          "White",
          "Black",
          "Other/Mixed",
          "South Asian"
        )
      ),
      Gender_v1 = factor(
        Gender_v1,
        levels = c("Female", "Male")
      )
    )
}
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

# Get model information for plotting ---------------------------------------------
#Predicted probabilities -----------------------------------------------------------------
###Model 1
data <- prepare_prediction_data(data)
X <- model.matrix(
  m1_formula,
  data = data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

data <- data %>%
  mutate(pred_prob_m1 = plogis(linpred))


#Split by age
data18_50 <- data %>%
  filter(AgeatDiagnosis <51)

data50plus <- data %>%
  filter(AgeatDiagnosis >=51)

#Modelling descriptives------------------------------------------------------------------
##Display item---------------------------------------------------------------------------
###Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(pred_prob_var = "pred_prob_m1",
           test_data = data18_50,
           outcome = "SRoutcome",
           saving_name = "model",
           manual_plotting = TRUE,
           manual_plot_name = "m1")

model_info(pred_prob_var = "pred_prob_m1",
           test_data = data50plus,
           outcome = "SRoutcome",
           saving_name = "model",
           manual_plotting = TRUE,
           manual_plot_name = "m1_50")

### Compile PDF
DESIGN_abc <- "
11111#
  223344
  223344
  223344
  223344
  223344
"

#18-50
m12_18_50_n <- prettyNum(nrow(data18_50),big.mark=",",scientific=FALSE)
n12_18_50_T1D <- data18_50 %>%
  filter(SRoutcome == 1) %>%
  nrow()
n12_18_50_T1D <- prettyNum(n12_18_50_T1D,big.mark=",",scientific=FALSE)
data18_50 <- data18_50 %>%
  mutate(SRoutcome = as.character(SRoutcome))

model1_18_50_text <- paste0("Clinical features only model in adults diagnosed 18-50 years old (n=",m12_18_50_n,"; Type 1=",n12_18_50_T1D,")")
dat_text_m1 <- dat_text_m1 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))

m1_18_50_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_18_50_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.7)
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
  ggplot(data18_50, aes(x=factor(SRoutcome,
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
    geom_smooth(method = "loess", span = 1, se = FALSE, colour = "blue", linewidth = 0.75) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw() +
    theme(text = element_text(size = 20))
) + patchwork::plot_layout(design = DESIGN_abc)


#50plus
m12_50plus_n <- prettyNum(nrow(data50plus),big.mark=",",scientific=FALSE)
n12_50plus_T1D <- data50plus %>%
  filter(SRoutcome == 1) %>%
  nrow()
n12_50plus_T1D <- prettyNum(n12_50plus_T1D,big.mark=",",scientific=FALSE)
data50plus <- data50plus %>%
  mutate(SRoutcome = as.character(SRoutcome))

model1_50plus_text <- paste0("Clinical features only model in adults diagnosed >50 years old (n=",m12_50plus_n,"; Type 1=",n12_50plus_T1D,")")
dat_text_m1_50 <- dat_text_m1_50 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))

m1_50plus_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_50plus_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.7)
  ),
  #roc_plot
  roc_curves_m1_50 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m1_50,
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
  ggplot(data50plus, aes(x=factor(SRoutcome,
                            levels = c("1","0"),
                            labels = c("Progressed ",
                                       "Did not progress")),
                   y = pred_prob_m1_50)) +
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
  ggplot(cal_prep_m1_50, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of Progressors \n to insulin in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_smooth(method = "loess", span = 1, se = FALSE, colour = "blue", linewidth = 0.75) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw() +
    theme(text = element_text(size = 20))
) + patchwork::plot_layout(design = DESIGN_abc)
pdf("figures/Supp_Figurex39.pdf", height = 16, width = 24)
# model_display_item <- patchwork::wrap_plots(
#   m1_plots_abc,
# ) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C")))
model_display_item <- patchwork::wrap_plots(m1_18_50_plots_abc,
                                            m1_50plus_plots_abc,
                                            ncol = 1, nrow = 2
                                            ) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C",
                                                                                               "", "D", "E", "F")))

print(model_display_item)
dev.off()
ggsave("figures/Supp_Figurex39.jpeg", model_display_item, height = 16, width = 24)
