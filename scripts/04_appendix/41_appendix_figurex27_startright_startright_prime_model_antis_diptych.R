#####################################################################################

#Paper figures: Subgroup analysis

#To get the two final paper complete case model datasets
#And do subgrouo analysis

#Dataset: StartRight
#Outcome: Antibody positive (defined by 1+ antibody)

#Diptych

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(writexl)

#load functions ------------------------------------------------------------------
source("functions/model_info2.R")

#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
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
      num_anti = factor(
        num_anti,
        levels = c(0,1,2,3)
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
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)
#Model12
SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12)) %>%
  mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
#Model3

#StartRight Prime -------------------------------------------------------------------
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         Eth_4cat = ifelse(Eth_5cat %in% c("Other", "Mixed"), "Other/Mixed", Eth_5cat)
  )

SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$num_anti <- as.character(SR_50_SRout_ccc$num_anti)
#Model12
# SR_50_m12_data <- SR_50_SRout_ccc %>%
#   drop_na(all_of(all_vars_12))
#Model12
SR_50_m12_data <- SR_50_SRout_ccc %>%
  drop_na(all_of(all_vars_12)) %>%
  mutate(anti_pos = ifelse(num_anti == "0", 0, 1))
###Model performance plot -------------------------------------------------------------
#Predicted probabilities -----------------------------------------------------------------
##StartRight --------------------------------------------------------------------------
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

##StartRight Prime--------------------------------------------------------------------------
###Model 1
SR_50_m12_data <- prepare_prediction_data(SR_50_m12_data)
X <- model.matrix(
  m1_formula,
  data = SR_50_m12_data
)
colnames(X)
missing_cols <- setdiff(names(m1_beta), colnames(X))

for(col in missing_cols) {
  X <- cbind(X, 0)
  colnames(X)[ncol(X)] <- col
}

X <- X[, names(m1_beta), drop = FALSE]
linpred <- as.vector(X %*% m1_beta)

SR_50_m12_data<- SR_50_m12_data %>%
  mutate(m1_pp = plogis(linpred))


###Model 1 ROW --------------------------------------------------------------------------
model_info(pred_prob_var = "m1_pp",
           test_data = SR_m12_data,
           outcome = "anti_pos",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m1")

###Model 1 ROW --------------------------------------------------------------------------
model_info(pred_prob_var = "m1_pp",
           test_data = SR_50_m12_data,
           outcome = "anti_pos",
           saving_name = "temp_dataset_ss_outcome_mY",
           manual_plotting = TRUE,
           manual_plot_name = "m1_50")

###Calibration prep for model 1 by anti_status ----------------------------------------
SR_m12_data <- SR_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0,
                              "0",
                              "1"))
brks_SR_nm1 <- quantile(SR_m12_data$m1_pp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE)
brks_SR_nm1[1] <- 0.99 * brks_SR_nm1[1]
brks_SR_nm1[length(brks_SR_nm1)] <- 1.1 * brks_SR_nm1[length(brks_SR_nm1)]
dec_SR_nm1 <- cut(
  SR_m12_data$m1_pp,
  breaks = brks_SR_nm1,
  include_lowest = TRUE
)

dec_SR_nm1 <- data.frame(y = SR_m12_data$SRoutcome,
                         pred = SR_m12_data$m1_pp,
                         anti_status = SR_m12_data$anti_status,
                         dec = dec_SR_nm1) %>%
  group_by(anti_status, dec) %>%
  mutate(prob_obs = sum(y) / n(),
         obs = sum(y),
         n_group = n(),
         mnpred = mean(pred),
         lower = lapply(sum(y), prop.test, n = n()),
         upper = sapply(lower, function(x) x$conf.int[2]),
         lower = sapply(lower, function(x) x$conf.int[1]))
#StartRight Prime -------------------------------------------------------------------
SR_50_m12_data <- SR_50_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0,
                              "0",
                              "1"))
brks_50_SR_nm1 <- quantile(SR_50_m12_data$m1_pp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE)
brks_50_SR_nm1[1] <- 0.99 * brks_50_SR_nm1[1]
brks_50_SR_nm1[length(brks_50_SR_nm1)] <- 1.1 * brks_50_SR_nm1[length(brks_50_SR_nm1)]
dec_50_SR_nm1 <- cut(
  SR_50_m12_data$m1_pp,
  breaks = brks_50_SR_nm1,
  include_lowest = TRUE
)

dec_50_SR_nm1 <- data.frame(y = SR_50_m12_data$SRoutcome,
                         pred = SR_50_m12_data$m1_pp,
                         anti_status = SR_50_m12_data$anti_status,
                         dec = dec_50_SR_nm1) %>%
  group_by(anti_status, dec) %>%
  mutate(prob_obs = sum(y) / n(),
         obs = sum(y),
         n_group = n(),
         mnpred = mean(pred),
         lower = lapply(sum(y), prop.test, n = n()),
         upper = sapply(lower, function(x) x$conf.int[2]),
         lower = sapply(lower, function(x) x$conf.int[1]))

## --- Compute dataset counts for model headings ----------------------------
m12_n_start <- nrow(SR_m12_data)
n12_T1D_start <- SR_m12_data %>%
  filter(SRoutcome == 1) %>%
  nrow()
m12_n_prime <- nrow(SR_50_m12_data)
n12_T1D_prime <- SR_50_m12_data %>%
  filter(SRoutcome == 1) %>%
  nrow()



## --- Title grobs (right-aligned) -----------------------------------------
model1_text_start <- paste0("StartRight: Clinical features only model (n=", m12_n_start, "; Type 1=", n12_T1D_start, ")")
model1_text_prime <- paste0("StartRight Prime: Clinical features only model (n=", m12_n_prime, "; Type 1=", n12_T1D_prime, ")")

title_m1_start <- patchwork::wrap_elements(ggpubr::text_grob(model1_text_start, face = "bold", size = 20, hjust = 0.61))
title_m1_prime <- patchwork::wrap_elements(ggpubr::text_grob(model1_text_prime, face = "bold", size = 20, hjust = 0.57))


##PDF ---------------------------------------------------------------------------------
# pdf("figures/Supp_Figure20.pdf", height = 10, width = 22)
# model_display_item <- patchwork::wrap_plots(
#   #title_m1_start,
#   #calibration plot for model 1 predict antipos
#   ggplot(cal_prep_m1, aes(x = mnpred, y = prob_obs)) +
#     geom_point(size = 3) +
#     xlab("Mean predicted probability in each decile") +
#     ylab("Proportion of Antibody positive in each decile") +
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#     ylim(c(0, 1)) + xlim(c(0, 1)) +
#     geom_errorbar(aes(ymin = lower, ymax = upper),
#                   width = 0.1,
#                   size = 0.7)+
#     theme_bw() +
#     theme(text = element_text(size = 20)),
#   #Calibration plot for model 1 split by antibody positivity
#   ggplot(dec_SR_nm1, aes(x = mnpred,
#                          y = prob_obs,
#                          color = factor(anti_status))) +
#     geom_point(size = 3) +
#     xlab("Mean predicted probability of clinical features model") +
#     ylab("Proportion of T1D in each decile") +
#     labs(color = "Antibody Status") +
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#     ylim(c(0, 1)) + xlim(c(0, 1)) +
#     geom_errorbar(aes(ymin = lower,
#                       ymax = upper,
#                       color = factor(anti_status)),
#                   width = 0.1,
#                   size = 0.7) +
#     theme_bw() +
#     theme(
#       text = element_text(size = 20),
#       legend.position = "none"
#     ),
#   ncol = 2, nrow = 1
# ) + patchwork::plot_annotation(tag_levels = list(c("A", "B")))
# print(model_display_item)
# dev.off()

#Plot prep --------------------------------------------------------------------------
DESIGN_abc <- "
11111#
  222333
  222333
  222333
  222333
  222333
"
model_display_item1 <- patchwork::wrap_plots(
  title_m1_start,
  #calibration plot for model 1 predict antipos
  ggplot(cal_prep_m1, aes(x = mnpred, y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of islet- \n autoantibody positive \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.1,
                  size = 0.7)+
    theme_bw() +
    theme(text = element_text(size = 20)),
  #Calibration plot for model 1 split by antibody positivity
  ggplot(dec_SR_nm1, aes(x = mnpred,
                         y = prob_obs,
                         color = factor(anti_status))) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features model") +
    ylab("Proportion of T1D \n in each decile") +
    labs(color = "Antibody Status") +
    geom_vline(xintercept = 0.1) +
    geom_vline(xintercept = 0.9) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper,
                      color = factor(anti_status)),
                  width = 0.1,
                  size = 0.7) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )) + patchwork::plot_layout(design = DESIGN_abc)
model_display_item2 <- patchwork::wrap_plots(
  #StartRight Prime
  title_m1_prime,
  #calibration plot for model 1 predict antipos
  ggplot(cal_prep_m1_50, aes(x = mnpred, y = prob_obs)) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n in each decile") +
    ylab("Proportion of islet- \n autoantibody positive \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.1,
                  size = 0.7)+
    theme_bw() +
    theme(text = element_text(size = 20)),
  #Calibration plot for model 1 split by antibody positivity
  ggplot(dec_50_SR_nm1, aes(x = mnpred,
                            y = prob_obs,
                            color = factor(anti_status))) +
    geom_point(size = 3) +
    xlab("Mean predicted probability \n of clinical features model") +
    ylab("Proportion of T1D \n in each decile") +
    labs(color = "Antibody Status") +
    geom_vline(xintercept = 0.1) +
    geom_vline(xintercept = 0.9) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper,
                      color = factor(anti_status)),
                  width = 0.1,
                  size = 0.7) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )) + patchwork::plot_layout(design = DESIGN_abc)

pdf("figures/Supp_Figurex28.pdf", height = 12, width = 14)
model_display_item <- patchwork::wrap_plots(
  model_display_item1,
  model_display_item2,
  ncol = 1, nrow = 2
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "", "C", "D")))
print(model_display_item)
dev.off()
ggsave("figures/Supp_Figurex28.jpeg", model_display_item, height = 12, width = 14)
## Threshold analysis -------------------------------------------------------------------
##StartRight ---------------------------------------------------------------------------
SR_m12_data <- SR_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0,
                              "0",
                              "1"))

# SR_m12_data %>%
#   filter(m1_pp < 0.05) %>%
#   group_by(anti_status) %>%
#   summarise(
#     n = n(),
#     n_T1D = sum(SRoutcome == 1),
#     perc_T1D = (sum(SRoutcome == 1)/n)*100
#   )
# SR_m12_data %>%
#   filter(m1_pp > 0.95) %>%
#   group_by(anti_status) %>%
#   summarise(
#     n = n(),
#     n_T1D = sum(SRoutcome == 1),
#     perc_T1D = (sum(SRoutcome == 1)/n)*100
#   )

SR_m12_data %>%
  filter(m1_pp < 0.1) %>%
  group_by(anti_status) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_m12_data %>%
  filter(m1_pp < 0.1) %>%
  mutate(anti_status2 = ifelse(num_anti == 0, "0", ifelse(num_anti == 1, "1", "2+"))) %>%
  group_by(anti_status2) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_m12_data %>%
  filter(m1_pp > 0.9) %>%
  group_by(anti_status) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
#StartRight Prime ----------------------------------------------------------------------
SR_50_m12_data <- SR_50_m12_data %>%
  mutate(anti_status = ifelse(num_anti == 0,
                              "0",
                              "1"))

# SR_m12_data %>%
#   filter(m1_pp < 0.05) %>%
#   group_by(anti_status) %>%
#   summarise(
#     n = n(),
#     n_T1D = sum(SRoutcome == 1),
#     perc_T1D = (sum(SRoutcome == 1)/n)*100
#   )
# SR_m12_data %>%
#   filter(m1_pp > 0.95) %>%
#   group_by(anti_status) %>%
#   summarise(
#     n = n(),
#     n_T1D = sum(SRoutcome == 1),
#     perc_T1D = (sum(SRoutcome == 1)/n)*100
#   )

SR_50_m12_data %>%
  filter(m1_pp < 0.1) %>%
  group_by(anti_status) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )

SR_50_m12_data %>%
  filter(m1_pp > 0.9) %>%
  group_by(anti_status) %>%
  summarise(
    n = n(),
    n_T1D = sum(SRoutcome == 1),
    perc_T1D = (sum(SRoutcome == 1)/n)*100
  )
