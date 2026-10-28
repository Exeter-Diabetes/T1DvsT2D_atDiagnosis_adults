#####################################################################################

#Paper figures: Subgroup analysis

#To get the two final paper complete case model datasets
#And do subgrouo analysis

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/model_info2.R")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_50_SRout_ccc_20_3_2025.RData")
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
## Model 2 -----------------------------------------------------------------------------------
m2_beta <- c(
  "(Intercept)" = 5.65739369171834,
  "AgeatDiagnosis" = -0.0761310235937549,
  "bmi_model" = -0.205897904461199,
  "HbA1c_at_diagnosis_v1" = 0.00311485068438993,
  "Gender_v1Male" = -0.770769399543392,
  "osmotic1" = 0.897232400482147,
  "autoimmune1" = 0.670223920517065,
  "Unintentional_weight_loss_v1Yes" = 1.20670197330313,
  "DKA1" = 0.645909312453152,
  'Eth_4catBlack' = -0.786936569308031,
  'Eth_4catOther/Mixed' = -0.60231161939665,
  'Eth_4catSouth Asian' = -0.362368506943113,
  "famhisnoninsdiabYes" = -0.640773160200783,
  "num_anti1" = 1.96631270605778,
  "num_anti2" = 2.54455916410863,
  "num_anti3" = 3.46277643855415
)
m2_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  num_anti
## Model 3 -----------------------------------------------------------------------------------
m3_beta <- c(
  "(Intercept)" = 5.59111707576748,
  "AgeatDiagnosis" = -0.0658265444495303,
  "bmi_model" = -0.215588994357022,
  "HbA1c_at_diagnosis_v1" = 0.0010205665084689,
  "Gender_v1Male" = -0.769413515416535,
  "osmotic1" = 0.961696294455552,
  "autoimmune1" = 0.718466592548281,
  "Unintentional_weight_loss_v1Yes" = 0.973882805795106,
  "DKA1" = 0.450769465099517,
  'Eth_4catBlack' = -0.0448130235508751,
  'Eth_4catOther/Mixed' = -1.19595487818422,
  'Eth_4catSouth Asian' = -0.508601030699211,
  "famhisnoninsdiabYes" = -0.649989577718691,
  "num_anti1" = 1.69269869149969,
  "num_anti2" = 2.1106207424025,
  "num_anti3" = 2.9710916181995,
  "T1DGRS2_z" = 0.588101930916182
)
m3_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  num_anti +
  T1DGRS2_z

## Model 4 -----------------------------------------------------------------------------------
m4_beta <- c(
  "(Intercept)" = 7.68913552392495,
  "AgeatDiagnosis" = -0.0792922627586783,
  "bmi_model" = -0.245795596269301,
  "HbA1c_at_diagnosis_v1" = 0.00135050843483768,
  "Gender_v1Male" = -0.745200775272355,
  "osmotic1" = 1.01679460706228,
  "autoimmune1" = 0.94971768216283,
  "Unintentional_weight_loss_v1Yes" = 1.05073780432584,
  "DKA1" = 0.626511986416986,
  'Eth_4catBlack' = -0.13926992184133,
  'Eth_4catOther/Mixed' = -1.67150151029812,
  'Eth_4catSouth Asian' = -0.76869417190735,
  "famhisnoninsdiabYes" = -1.07704095684858,
  "T1DGRS2_z" = 0.922151368226217
)
m4_formula <- ~
  AgeatDiagnosis +
  bmi_model +
  HbA1c_at_diagnosis_v1 +
  Gender_v1 +
  osmotic +
  autoimmune +
  Unintentional_weight_loss_v1 +
  DKA +
  Eth_4cat +
  famhisnoninsdiab +
  T1DGRS2_z

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
#Get predicted probabilities ---------------------------------------------------------
###Model 1
SR_m12_data <- prepare_prediction_data(SR_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_m12_data<- SR_m12_data %>%
  mutate(m1_pp = plogis(linpred))
###Model 2
X <- model.matrix(
  m2_formula,
  data = SR_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m2_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m2_beta), drop = FALSE]
linpred <- as.vector(X %*% m2_beta)

SR_m12_data <- SR_m12_data %>%
  mutate(m2_pp = plogis(linpred))

####Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(pred_prob_var = "m1_pp",
           test_data = SR_m12_data,
           outcome = "SRoutcome",
           saving_name = "03_15_sm1_Prime",
           manual_plotting = TRUE,
           manual_plot_name = "m1")

####Model 2 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(pred_prob_var = "m2_pp",
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
# dat_text_m1 <- dat_text_m1 %>%
#   mutate(auc_full = "AUCROC: 0.93 (0.90;0.96)")
dat_text_m1 <- dat_text_m1 %>%
  mutate(auc_full = paste0("AUCROC: ", round(auc, 2), " (", round(auc_low, 2), ";", round(auc_high, 2), ")"))
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
#roc_plot
A <- roc_curves_m1 %>%
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
  )
#boxplot
B <- ggplot(SR_m12_data, aes(x=factor(SRoutcome,
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
        text = element_text(size = 20))
#calibration plot
C <- ggplot(cal_prep_m1, aes(x = mnpred, y = prob_obs)) +
  geom_point() +
  xlab("Mean predicted probability \n in each decile") +
  ylab("Proportion of T1D \n in each decile") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = "loess", span = 1, se = FALSE, colour = "blue", linewidth = 0.75) +
  ylim(c(0, 1)) + xlim(c(0, 1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  theme_bw()+
  theme(text = element_text(size = 20))
m1_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.95)
  ),
  A,
  B,
  C
) + patchwork::plot_layout(design = DESIGN_abc)


#### Model2
#roc_plot
D <- roc_curves_m2 %>%
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
  )
#boxplot
E <- ggplot(SR_m12_data, aes(x=factor(SRoutcome,
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
        text = element_text(size = 20))
#calibration plot
F_2 <- ggplot(cal_prep_m2, aes(x = mnpred, y = prob_obs)) +
  geom_point() +
  xlab("Mean predicted probability \n in each decile") +
  ylab("Proportion of T1D \n in each decile") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = "loess", span = 1, se = FALSE, colour = "blue", linewidth = 0.75) +
  ylim(c(0, 1)) + xlim(c(0, 1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  theme_bw()+
  theme(text = element_text(size = 20))

m2_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model2_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.61)
  ),
  D,
  E,
  F_2

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


pdf("figures/Figure3A.pdf", width = 7, height = 5)
A
dev.off()
pdf("figures/Figure3B.pdf", width = 7, height = 5)
B
dev.off()
pdf("figures/Figure3C.pdf", width = 7, height = 5)
C
dev.off()
pdf("figures/Figure3D.pdf", width = 7, height = 5)
D
dev.off()
pdf("figures/Figure3E.pdf", width = 7, height = 5)
E
dev.off()
pdf("figures/Figure3F.pdf", width = 7, height = 5)
F_2
dev.off()

