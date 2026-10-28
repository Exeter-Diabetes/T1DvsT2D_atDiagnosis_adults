#####################################################################################

#Paper figures

#To get the two final paper complete case model datasets

#Supplementary Table 11:
#Optimism tables for
#Model 1
#Model 2
#Model 3
#Model 4

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/model_comparison4.R")
#source("functions/var_characteristics_1.R")
#source("functions/model_continuous1.R")
#source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_ccc_20_3_2025.RData")
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
  "Hypertension_on_meds_v1",
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
  "Hypertension_on_meds_v1",
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
table(SR_SRout_ccc$Eth_4cat, useNA = "ifany")
table(SR_SRout_ccc$Eth_5cat, useNA = "ifany")
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)
#Model12
SR_m12_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_12))
table(SR_m12_data$Eth_4cat, useNA = "ifany")
table(SR_m12_data$Eth_5cat, useNA = "ifany")
#Model3
SR_m3_data <- SR_SRout_ccc %>%
  drop_na(all_of(all_vars_3))
table(SR_m3_data$Eth_4cat, useNA = "ifany")
table(SR_m3_data$Eth_5cat, useNA = "ifany")
##Models ----------------------------------------------------------------------------
##Optimism tables----------------------------------------------------------------------
###Model 1 ----------------------------------------------------------------------------------
SR_m12_data <- SR_m12_data %>%
  mutate(id = 1:n())
table(SR_m12_data$Eth_4cat, useNA = "ifany")
table(SR_m12_data$Eth_5cat, useNA = "ifany")
class(SR_m12_data$Eth_4cat)
varlist1 <- c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat1 <- c(
  "Eth_4cat",
  "Gender_v1",
  "DKA",
  "osmotic",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "famhisnoninsdiab",
  "Hypertension_on_meds_v1")

base_model_vars1 <- c("AgeatDiagnosis",
                      "bmi_model")
base_model_vars1_CAT <- NULL
SR_m12_data$Eth_4cat <- factor(SR_m12_data$Eth_4cat, levels = c("White", "Black", "South Asian", "Other/Mixed"))
table(SR_m12_data$Eth_4cat, useNA = "ifany")
table(SR_m12_data$Eth_5cat, useNA = "ifany")
class(SR_m12_data$Eth_4cat)
SR_12_dd <- SR_m12_data %>%
  dplyr::select(all_of(all_vars_12), "Eth_4cat", "SRoutcome") %>%
  mutate(Eth_4cat = factor(Eth_4cat, levels = c("White", "Black", "South Asian", "Other/Mixed"))) %>%
  as.data.frame()
dd <- datadist(SR_12_dd); options(datadist = 'dd')
table(SR_12_dd$Eth_4cat, useNA = "ifany")
table(SR_12_dd$Eth_5cat, useNA = "ifany")
class(SR_12_dd$Eth_4cat)
set.seed(123)
n_boot <- 100

dataset_bootstrap <- modelr::bootstrap(SR_m12_data, n_boot)
selected_vars_list <- vector("list", n_boot)
optimism_results <- vector("list", n_boot)

bootstrapped_estimations <- NULL
original_model_estimations <- NULL
print("Model 1")
for (i in 1:100) {
  print(i)
  #sample new dataset based on bootstrap iteration
  new_dataset <- SR_m12_data[dataset_bootstrap$strap[[i]]$idx,]
  #new_dataset <- SR_m12_data[dataset_bootstrap$strap[[1]]$idx,]

  ## Variable selection: do they add to age and BMI at diagnosis?  ---------------------------------------------------------
  model_comparisons(varlist = varlist1,
                    varlist_cat = varlist_cat1,
                    base_model_vars = base_model_vars1,
                    base_model_vars_cat = base_model_vars1_CAT,
                    dataset = new_dataset,
                    outcome = "SRoutcome",
                    saving_name = "boot_stable",
                    complete_case = FALSE,
                    plots = FALSE)
  #extract variables that add to age and BMI
  boot_stable_list <- boot_stable %>%
    filter(lrtest < 0.05) %>%
    dplyr::select(model) %>%
    mutate(variable = str_remove(
      model,
      #fixed("scale( AgeatDiagnosis ) + scale( bmi_model ) + ")
      fixed("AgeatDiagnosis + bmi_model + ")
    )) %>%
    mutate(
      variable = str_remove_all(variable, '"')
    )
  vars <- boot_stable_list$variable
  #save variable list to matrix to examine n of times selected
  selected_vars_list[[i]] <- vars
  vars_rms <- vars

  vars <- case_when(
    vars == "Eth_4cat" ~ 'relevel(factor(Eth_4cat), ref = "White")',
    TRUE ~ vars
  )
  #Create model formula string
  formula_string <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + ",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + ",
    paste(vars, collapse = " + ")
  )

  formula <- as.formula(formula_string)

  formula
  print(formula)
  #run formula in original and bootstrapped datasets
  model_new <- glm(formula,
                   data = new_dataset,
                   family = binomial)
  model_old <- glm(formula,
                   data = SR_m12_data,
                   family = binomial)
  ##Optimism ------------------------------------------------------------------------------
  #browser()
  formula_string_rms <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + ",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + ",
    paste(vars_rms, collapse = " + ")
  )

  formula_rms <- as.formula(formula_string_rms)

  formula_rms
  print(formula_rms)
  ###Convert to rms structure
  m1_rms <- lrm(formula_rms,
                data = SR_12_dd,
                x=TRUE,
                y = TRUE)
  ###Validate function
  m1_optimism_rms <- rms::validate(m1_rms,
                                   B = 100,
                                   maxit = 20)
  print("rms validate optimims complete")
  m1_optimism <- unclass(m1_optimism_rms)
  m1_optimism <- data.frame(`Performance metric` = row.names(m1_optimism), m1_optimism)
  colnames(m1_optimism) <- c("Performance metric",
                             "Full",
                             "Bootstrap:training",
                             "Bootstrap:test",
                             "Optimism",
                             "Full:corrected",
                             "Bootstrap:n")

  ###AUCROC
  #predict bootstrapped model to bootstrapped sample
  new_predictions <- predict(model_new,
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)
  #predict original model to original data
  original_predictions1 <- predict(model_old,
                                   newdata = SR_m12_data,
                                   type = "response") %>%
    cbind(id = SR_m12_data$id)
  #ROC of original model to original data
  full_roc_model <- roc(SR_m12_data$SRoutcome,
                        original_predictions1[,1],
                        plot = FALSE,
                        ci = TRUE)

  set.seed(345)
  dataset_bootstrap_roc <- modelr::bootstrap(SR_m12_data, n_boot)

  bootstrapped_ROC <- data.frame(training = NA, test = NA)

  for (x in 1:100) {
    print(paste("AUCROC loop:", x))
    #This is the bootstrap sample
    new_dataset <- SR_m12_data[dataset_bootstrap_roc$strap[[x]]$idx,]
    ROC <- data.frame(training = NA, test = NA)
    #Estimate model in bootstrap sample
    model <- glm(formula,
                 data = new_dataset,
                 family = binomial)
    #Predict bootstrap model to bootstrap sample
    boot_predictions_boot <- predict(model,
                                     newdata = new_dataset,
                                     type = "response")
    #Predict bootstrap model to full/original data
    boot_predictions_full <- predict(model,
                                     newdata = SR_m12_data,
                                     type = "response")

    training_roc_model <- roc(new_dataset$SRoutcome,
                              boot_predictions_boot,
                              plot = FALSE,
                              ci = TRUE)
    #Extract valuable summary information from ROC object

    ROC$training <- training_roc_model$auc
    test_roc_model <- roc(SR_m12_data$SRoutcome,
                          boot_predictions_full,
                          plot = FALSE,
                          ci = TRUE)


    ROC$test <- test_roc_model$auc

    bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
  }
  m1_optimism_rocs <- data.frame(
    `Performance metric` = "ROCAUC",
    Full = full_roc_model$auc,
    `Bootstrap.training` = mean(bootstrapped_ROC$training, na.rm = TRUE),
    `Bootstrap.test` = mean(bootstrapped_ROC$test, na.rm = TRUE)
  ) %>%
    mutate(
      Optimism = `Bootstrap.training` - `Bootstrap.test`,
      `Full.corrected`= Full - Optimism,
      `Bootstrap.n` = 1000
    )
  #browser()
  colnames(m1_optimism_rocs) <- c("Performance metric",
                                 "Full",
                                 "Bootstrap:training",
                                 "Bootstrap:test",
                                 "Optimism",
                                 "Full:corrected",
                                 "Bootstrap:n")

  m1_optimism <- rbind(m1_optimism,
                       m1_optimism_rocs)
  optimism_results[[i]] <- m1_optimism
  }
print("Model 1 complete -------------------------------------------------------------")
###Frequency of clinical variables in bootstraps -----------------------------------------
n_boot <- 100
variable_freq_m1 <- tibble(
  bootstrap = seq_along(selected_vars_list),
  variables = selected_vars_list
) %>%
  unnest_longer(variables) %>%
  count(variables, name = "n_selected") %>%
  arrange(desc(n_selected))

variable_freq_m1

###construct optimism table across bootstraps --------------------------------------
all_optimism <- bind_rows(
  optimism_results,
  .id = "bootstrap"
)
mean_optimism_m1 <- all_optimism %>%
  group_by(`Performance metric`) %>%
  summarise(
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
save(mean_optimism_m1, file = "m1_optimism.RData")
summary_optimism_m1 <- all_optimism %>%
  group_by(`Performance metric`) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        p2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
        p97.5 = ~quantile(.x, 0.975, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  )




###Model 2 ----------------------------------------------------------------------------------
varlist1 <- c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat1 <- c(
  "Eth_4cat",
  "Gender_v1",
  "DKA",
  "osmotic",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "famhisnoninsdiab",
  "Hypertension_on_meds_v1")

base_model_vars1 <- c("AgeatDiagnosis",
                      "bmi_model")
base_model_vars1_CAT <- NULL

set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m12_data, 100)
selected_vars_list <- vector("list", 100)
bootstrapped_estimations <- NULL
original_model_estimations <- NULL

for (i in 1:100) {
print(i)
  new_dataset <- SR_m12_data[dataset_bootstrap$strap[[i]]$idx,]
  #new_dataset <- SR_m12_data[dataset_bootstrap$strap[[1]]$idx,]


  ## Variable selection: do they add to age and BMI at diagnosis?  ---------------------------------------------------------
  model_comparisons(varlist = varlist1,
                    varlist_cat = varlist_cat1,
                    base_model_vars = base_model_vars1,
                    base_model_vars_cat = base_model_vars1_CAT,
                    dataset = new_dataset,
                    outcome = "SRoutcome",
                    saving_name = "boot_stable",
                    complete_case = FALSE,
                    plots = FALSE)
  boot_stable_list <- boot_stable %>%
    filter(lrtest < 0.05) %>%
    dplyr::select(model) %>%
    mutate(variable = str_remove(
      model,
      #fixed("scale( AgeatDiagnosis ) + scale( bmi_model ) + ")
      fixed("AgeatDiagnosis + bmi_model + ")
    )) %>%
    mutate(
      variable = str_remove_all(variable, '"')
    )
  vars <- boot_stable_list$variable
  selected_vars_list[[i]] <- vars
  vars_rms <- vars

  vars <- case_when(
    vars == "Eth_4cat" ~ 'relevel(factor(Eth_4cat), ref = "White")',
    TRUE ~ vars
  )


  formula_string <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + num_anti +",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + num_anti +",
    paste(vars, collapse = " + ")
  )

  formula <- as.formula(formula_string)

  print(formula)
  #apply model_comparison logic to building models - in new data and then in original

  model_new <- glm(formula,
                   data = new_dataset,
                   family = binomial)
  model_old <- glm(formula,
                   data = SR_m12_data,
                   family = binomial)

  ##Optimism ------------------------------------------------------------------------------
  print("Optimism")
  ###Convert to rms structure
  formula_string_rms <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + num_anti + ",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + num_anti + ",
    paste(vars_rms, collapse = " + ")
  )

  formula_rms <- as.formula(formula_string_rms)

  formula_rms
  print(formula_rms)
  m2_rms <- lrm(formula_rms,
                data = SR_12_dd,
                x=TRUE,
                y = TRUE)
  ###Validate function
  m2_optimism_rms <- rms::validate(m2_rms,
                                   B = 100,
                                   maxit = 20)
  m2_optimism <- unclass(m2_optimism_rms)
  m2_optimism <- data.frame(`Performance metric` = row.names(m2_optimism), m2_optimism)
  colnames(m2_optimism) <- c("Performance metric",
                             "Full",
                             "Bootstrap:training",
                             "Bootstrap:test",
                             "Optimism",
                             "Full:corrected",
                             "Bootstrap:n")

  ###AUCROC
  #predict bootstrapped model to bootstrapped sample
  new_predictions <- predict(model_new,
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)
  #predict original model to original data
  original_predictions1 <- predict(model_old,
                                   newdata = SR_m12_data,
                                   type = "response") %>%
    cbind(id = SR_m12_data$id)
  #ROC of original model to original data
  full_roc_model <- roc(SR_m12_data$SRoutcome,
                        original_predictions1[,1],
                        plot = FALSE,
                        ci = TRUE)

  set.seed(345)
  dataset_bootstrap_roc <- modelr::bootstrap(SR_m12_data, n_boot)

  bootstrapped_ROC <- data.frame(training = NA, test = NA)

  for (x in 1:100) {
    print(paste("AUC loop:", x))
    #This is the bootstrap sample
    new_dataset <- SR_m12_data[dataset_bootstrap_roc$strap[[x]]$idx,]
    ROC <- data.frame(training = NA, test = NA)
    #Estimate model in bootstrap sample
    model <- glm(formula,
                 data = new_dataset,
                 family = binomial)
    #Predict bootstrap model to bootstrap sample
    boot_predictions_boot <- predict(model,
                                     newdata = new_dataset,
                                     type = "response")
    #Predict bootstrap model to full/original data
    boot_predictions_full <- predict(model,
                                     newdata = SR_m12_data,
                                     type = "response")

    training_roc_model <- roc(new_dataset$SRoutcome,
                              boot_predictions_boot,
                              plot = FALSE,
                              ci = TRUE)
    #Extract valuable summary information from ROC object

    ROC$training <- training_roc_model$auc
    test_roc_model <- roc(SR_m12_data$SRoutcome,
                          boot_predictions_full,
                          plot = FALSE,
                          ci = TRUE)


    ROC$test <- test_roc_model$auc

    bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
  }
  m2_optimism_rocs <- data.frame(
    `Performance metric` = "ROCAUC",
    Full = full_roc_model$auc,
    `Bootstrap.training` = mean(bootstrapped_ROC$training, na.rm = TRUE),
    `Bootstrap.test` = mean(bootstrapped_ROC$test, na.rm = TRUE)
  ) %>%
    mutate(
      Optimism = `Bootstrap.training` - `Bootstrap.test`,
      `Full.corrected`= Full - Optimism,
      `Bootstrap.n` = 1000
    )
  colnames(m2_optimism_rocs) <- c("Performance metric",
                                  "Full",
                                  "Bootstrap:training",
                                  "Bootstrap:test",
                                  "Optimism",
                                  "Full:corrected",
                                  "Bootstrap:n")

  #browser()

  m2_optimism <- rbind(m2_optimism,
                       m2_optimism_rocs)
  optimism_results[[i]] <- m2_optimism
}
print("Model 2 complete -------------------------------------------------------------")
###Frequency of clinical variables in bootstraps -----------------------------------------
#n_boot <- 100
variable_freq_m2 <- tibble(
  bootstrap = seq_along(selected_vars_list),
  variables = selected_vars_list
) %>%
  unnest_longer(variables) %>%
  count(variables, name = "n_selected") %>%
  arrange(desc(n_selected))

variable_freq_m2

###construct optimism table across bootstraps --------------------------------------
all_optimism <- bind_rows(
  optimism_results,
  .id = "bootstrap"
)
mean_optimism_m2 <- all_optimism %>%
  group_by(`Performance metric`) %>%
  summarise(
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
save(mean_optimism_m2, file = "m2_optimism.RData")
summary_optimism_m2 <- all_optimism %>%
  group_by(`Performance metric`) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        p2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
        p97.5 = ~quantile(.x, 0.975, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  )


###Model 3 ----------------------------------------------------------------------------------
SR_m3_data <- SR_m3_data %>%
  mutate(id = 1:n())

varlist1 <- c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat1 <- c(
  "Eth_4cat",
  "Gender_v1",
  "DKA",
  "osmotic",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "famhisnoninsdiab",
  "Hypertension_on_meds_v1")

base_model_vars1 <- c("AgeatDiagnosis",
                      "bmi_model")
base_model_vars1_CAT <- NULL
SR_m3_data$Eth_4cat <- factor(SR_m3_data$Eth_4cat, levels = c("White", "Black", "South Asian", "Other/Mixed"))
SR_3_dd <- SR_m3_data %>%
  dplyr::select(all_of(all_vars_3), "Eth_4cat", "SRoutcome") %>%
  mutate(Eth_4cat = factor(Eth_4cat, levels = c("White", "Black", "South Asian", "Other/Mixed"))) %>%
  as.data.frame()
dd <- datadist(SR_3_dd); options(datadist = 'dd')

set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m3_data, 100)
selected_vars_list <- vector("list", 100)
bootstrapped_estimations <- NULL
original_model_estimations <- NULL

for (i in 1:100) {
  print(i)

  new_dataset <- SR_m3_data[dataset_bootstrap$strap[[i]]$idx,]
  #new_dataset <- SR_m12_data[dataset_bootstrap$strap[[1]]$idx,]


  ## Variable selection: do they add to age and BMI at diagnosis?  ---------------------------------------------------------
  model_comparisons(varlist = varlist1,
                    varlist_cat = varlist_cat1,
                    base_model_vars = base_model_vars1,
                    base_model_vars_cat = base_model_vars1_CAT,
                    dataset = new_dataset,
                    outcome = "SRoutcome",
                    saving_name = "boot_stable",
                    complete_case = FALSE,
                    plots = FALSE)
  boot_stable_list <- boot_stable %>%
    filter(lrtest < 0.05) %>%
    dplyr::select(model) %>%
    mutate(variable = str_remove(
      model,
      #fixed("scale( AgeatDiagnosis ) + scale( bmi_model ) + ")
      fixed("AgeatDiagnosis + bmi_model + ")
    )) %>%
    mutate(
      variable = str_remove_all(variable, '"')
    )
  vars <- boot_stable_list$variable
  selected_vars_list[[i]] <- vars
  vars_rms <- vars

  vars <- case_when(
    vars == "Eth_4cat" ~ 'relevel(factor(Eth_4cat), ref = "White")',
    TRUE ~ vars
  )

  formula_string <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + num_anti + scale(T1DGRS2_z) +",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + num_anti + T1DGRS2_z +",
    paste(vars, collapse = " + ")
  )

  formula <- as.formula(formula_string)

  print(formula)
  #apply model_comparison logic to building models - in new data and then in original

  model_new <- glm(formula,
                   data = new_dataset,
                   family = binomial)
  model_old <- glm(formula,
                   data = SR_m3_data,
                   family = binomial)

  ##Optimism ------------------------------------------------------------------------------
  ###Convert to rms structure
  print("Optimism")

  formula_string_rms <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + num_anti + scale(T1DGRS2_z) + ",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + num_anti + T1DGRS2_z + ",
    paste(vars_rms, collapse = " + ")
  )

  formula_rms <- as.formula(formula_string_rms)

  formula_rms
  print(formula_rms)
  m3_rms <- lrm(formula_rms,
                data = SR_3_dd,
                x=TRUE,
                y = TRUE)
  ###Validate function
  m3_optimism_rms <- rms::validate(m3_rms,
                                   B = 100,
                                   maxit = 20)
  m3_optimism <- unclass(m1_optimism_rms)
  m3_optimism <- data.frame(`Performance metric` = row.names(m3_optimism), m3_optimism)
  colnames(m3_optimism) <- c("Performance metric",
                             "Full",
                             "Bootstrap:training",
                             "Bootstrap:test",
                             "Optimism",
                             "Full:corrected",
                             "Bootstrap:n")

  ###AUCROC
  #predict bootstrapped model to bootstrapped sample
  new_predictions <- predict(model_new,
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)
  #predict original model to original data
  original_predictions1 <- predict(model_old,
                                   newdata = SR_m3_data,
                                   type = "response") %>%
    cbind(id = SR_m3_data$id)
  #ROC of original model to original data
  full_roc_model <- roc(SR_m3_data$SRoutcome,
                        original_predictions1[,1],
                        plot = FALSE,
                        ci = TRUE)

  set.seed(345)
  dataset_bootstrap_roc <- modelr::bootstrap(SR_m3_data, n_boot)

  bootstrapped_ROC <- data.frame(training = NA, test = NA)

  for (x in 1:100) {
    print(paste("AUC loop:", x))
    #This is the bootstrap sample
    new_dataset <- SR_m3_data[dataset_bootstrap_roc$strap[[x]]$idx,]
    ROC <- data.frame(training = NA, test = NA)
    #Estimate model in bootstrap sample
    model <- glm(formula,
                 data = new_dataset,
                 family = binomial)
    #Predict bootstrap model to bootstrap sample
    boot_predictions_boot <- predict(model,
                                     newdata = new_dataset,
                                     type = "response")
    #Predict bootstrap model to full/original data
    boot_predictions_full <- predict(model,
                                     newdata = SR_m3_data,
                                     type = "response")

    training_roc_model <- roc(new_dataset$SRoutcome,
                              boot_predictions_boot,
                              plot = FALSE,
                              ci = TRUE)
    #Extract valuable summary information from ROC object

    ROC$training <- training_roc_model$auc
    test_roc_model <- roc(SR_m3_data$SRoutcome,
                          boot_predictions_full,
                          plot = FALSE,
                          ci = TRUE)


    ROC$test <- test_roc_model$auc

    bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
  }
  m3_optimism_rocs <- data.frame(
    `Performance metric` = "ROCAUC",
    Full = full_roc_model$auc,
    `Bootstrap.training` = mean(bootstrapped_ROC$training, na.rm = TRUE),
    `Bootstrap.test` = mean(bootstrapped_ROC$test, na.rm = TRUE)
  ) %>%
    mutate(
      Optimism = `Bootstrap.training` - `Bootstrap.test`,
      `Full.corrected`= Full - Optimism,
      `Bootstrap.n` = 1000
    )
  colnames(m3_optimism_rocs) <- c("Performance metric",
                                  "Full",
                                  "Bootstrap:training",
                                  "Bootstrap:test",
                                  "Optimism",
                                  "Full:corrected",
                                  "Bootstrap:n")

  m3_optimism <- rbind(m3_optimism,
                       m3_optimism_rocs)
  optimism_results[[i]] <- m3_optimism
}
print("Model 3 complete -------------------------------------------------------------")
###Frequency of clinical variables in bootstraps -----------------------------------------
#n_boot <- 100
variable_freq_m3 <- tibble(
  bootstrap = seq_along(selected_vars_list),
  variables = selected_vars_list
) %>%
  unnest_longer(variables) %>%
  count(variables, name = "n_selected") %>%
  arrange(desc(n_selected))

variable_freq_m3

###construct optimism table across bootstraps --------------------------------------
all_optimism <- bind_rows(
  optimism_results,
  .id = "bootstrap"
)
mean_optimism_m3 <- all_optimism %>%
  group_by(`Performance metric`) %>%
  summarise(
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
save(mean_optimism_m3, file = "m3_optimism.RData")
summary_optimism_m3 <- all_optimism %>%
  group_by(`Performance metric`) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        p2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
        p97.5 = ~quantile(.x, 0.975, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  )

###Model 4 ----------------------------------------------------------------------------------
varlist1 <- c("AgeatDiagnosis",
              "bmi_model",
              "HbA1c_at_diagnosis_v1"
)
#create varlist_cat (categorical variables of interest names)
varlist_cat1 <- c(
  "Eth_4cat",
  "Gender_v1",
  "DKA",
  "osmotic",
  "Unintentional_weight_loss_v1",
  "autoimmune",
  "famhisnoninsdiab",
  "Hypertension_on_meds_v1")

base_model_vars1 <- c("AgeatDiagnosis",
                      "bmi_model")
base_model_vars1_CAT <- NULL

set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m3_data, 100)
selected_vars_list <- vector("list", 100)
bootstrapped_estimations <- NULL
original_model_estimations <- NULL

for (i in 1:100) {
  print(i)

  new_dataset <- SR_m3_data[dataset_bootstrap$strap[[i]]$idx,]
  #new_dataset <- SR_m12_data[dataset_bootstrap$strap[[1]]$idx,]


  ## Variable selection: do they add to age and BMI at diagnosis?  ---------------------------------------------------------
  model_comparisons(varlist = varlist1,
                    varlist_cat = varlist_cat1,
                    base_model_vars = base_model_vars1,
                    base_model_vars_cat = base_model_vars1_CAT,
                    dataset = new_dataset,
                    outcome = "SRoutcome",
                    saving_name = "boot_stable",
                    complete_case = FALSE,
                    plots = FALSE)
  boot_stable_list <- boot_stable %>%
    filter(lrtest < 0.05) %>%
    dplyr::select(model) %>%
    mutate(variable = str_remove(
      model,
      #fixed("scale( AgeatDiagnosis ) + scale( bmi_model ) + ")
      fixed("AgeatDiagnosis +  bmi_model + ")
    )) %>%
    mutate(
      variable = str_remove_all(variable, '"')
    )
  vars <- boot_stable_list$variable
  selected_vars_list[[i]] <- vars
  vars_rms <- vars

  vars <- case_when(
    vars == "Eth_4cat" ~ 'relevel(factor(Eth_4cat), ref = "White")',
    TRUE ~ vars
  )

  formula_string <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + scale(T1DGRS2_z) +",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + T1DGRS2_z +",
    paste(vars, collapse = " + ")
  )

  formula <- as.formula(formula_string)

  print(formula)
  #apply model_comparison logic to building models - in new data and then in original

  model_new <- glm(formula,
                   data = new_dataset,
                   family = binomial)
  model_old <- glm(formula,
                   data = SR_m3_data,
                   family = binomial)

  ##Optimism ------------------------------------------------------------------------------
  ###Convert to rms structure
  print("Optimism")
  formula_string_rms <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + scale(T1DGRS2_z) + ",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + T1DGRS2_z + ",
    paste(vars_rms, collapse = " + ")
  )

  formula_rms <- as.formula(formula_string_rms)

  formula_rms
  print(formula_rms)
  m4_rms <- lrm(formula_rms,
                data = SR_3_dd,
                x=TRUE,
                y = TRUE)
  ###Validate function
  m4_optimism_rms <- rms::validate(m4_rms,
                                   B = 100,
                                   maxit = 20)
  m4_optimism <- unclass(m4_optimism_rms)
  m4_optimism <- data.frame(`Performance metric` = row.names(m4_optimism), m4_optimism)
  colnames(m4_optimism) <- c("Performance metric",
                             "Full",
                             "Bootstrap:training",
                             "Bootstrap:test",
                             "Optimism",
                             "Full:corrected",
                             "Bootstrap:n")

  ###AUCROC
  #predict bootstrapped model to bootstrapped sample
  new_predictions <- predict(model_new,
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)
  #predict original model to original data
  original_predictions1 <- predict(model_old,
                                   newdata = SR_m3_data,
                                   type = "response") %>%
    cbind(id = SR_m3_data$id)
  #ROC of original model to original data
  full_roc_model <- roc(SR_m3_data$SRoutcome,
                        original_predictions1[,1],
                        plot = FALSE,
                        ci = TRUE)

  set.seed(345)
  dataset_bootstrap_roc <- modelr::bootstrap(SR_m3_data, n_boot)

  bootstrapped_ROC <- data.frame(training = NA, test = NA)

  for (x in 1:100) {
    print(paste("AUC loop:", x))
    #This is the bootstrap sample
    new_dataset <- SR_m3_data[dataset_bootstrap_roc$strap[[x]]$idx,]
    ROC <- data.frame(training = NA, test = NA)
    #Estimate model in bootstrap sample
    model <- glm(formula,
                 data = new_dataset,
                 family = binomial)
    #Predict bootstrap model to bootstrap sample
    boot_predictions_boot <- predict(model,
                                     newdata = new_dataset,
                                     type = "response")
    #Predict bootstrap model to full/original data
    boot_predictions_full <- predict(model,
                                     newdata = SR_m3_data,
                                     type = "response")

    training_roc_model <- roc(new_dataset$SRoutcome,
                              boot_predictions_boot,
                              plot = FALSE,
                              ci = TRUE)
    #Extract valuable summary information from ROC object

    ROC$training <- training_roc_model$auc
    test_roc_model <- roc(SR_m3_data$SRoutcome,
                          boot_predictions_full,
                          plot = FALSE,
                          ci = TRUE)


    ROC$test <- test_roc_model$auc

    bootstrapped_ROC <- rbind(bootstrapped_ROC, ROC)
  }
  m4_optimism_rocs <- data.frame(
    `Performance metric` = "ROCAUC",
    Full = full_roc_model$auc,
    `Bootstrap.training` = mean(bootstrapped_ROC$training, na.rm = TRUE),
    `Bootstrap.test` = mean(bootstrapped_ROC$test, na.rm = TRUE)
  ) %>%
    mutate(
      Optimism = `Bootstrap.training` - `Bootstrap.test`,
      `Full.corrected`= Full - Optimism,
      `Bootstrap.n` = 1000
    )
  colnames(m4_optimism_rocs) <- c("Performance metric",
                                  "Full",
                                  "Bootstrap:training",
                                  "Bootstrap:test",
                                  "Optimism",
                                  "Full:corrected",
                                  "Bootstrap:n")

  m4_optimism <- rbind(m4_optimism,
                       m4_optimism_rocs)
  optimism_results[[i]] <- m4_optimism
}
print("Model 4 complete -------------------------------------------------------------")
###Frequency of clinical variables in bootstraps -----------------------------------------
#n_boot <- 100
variable_freq_m4 <- tibble(
  bootstrap = seq_along(selected_vars_list),
  variables = selected_vars_list
) %>%
  unnest_longer(variables) %>%
  count(variables, name = "n_selected") %>%
  arrange(desc(n_selected))

variable_freq_m4

###construct optimism table across bootstraps --------------------------------------
all_optimism <- bind_rows(
  optimism_results,
  .id = "bootstrap"
)
mean_optimism_m4 <- all_optimism %>%
  group_by(`Performance metric`) %>%
  summarise(
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
save(mean_optimism_m4, file = "m4_optimism.RData")
summary_optimism_m4 <- all_optimism %>%
  group_by(`Performance metric`) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        p2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
        p97.5 = ~quantile(.x, 0.975, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  )


##Saving Optimism tables ----------------------------------------------------------------
summary_optimism_m1 <- summary_optimism_m1 %>%
  mutate(`Full (95% CI)` = paste0(round(Full_mean, 3), " (", round(Full_p2.5, 3), ";", round(Full_p97.5, 3), ")"),
         `Bootstrap:training (95% CI)` = paste0(round(`Bootstrap:training_mean`, 3), " (", round(`Bootstrap:training_p2.5`, 3), ";", round(`Bootstrap:training_p97.5`, 3), ")"),
         `Bootstrap:test (95% CI)` = paste0(round(`Bootstrap:test_mean`, 3), " (", round(`Bootstrap:test_p2.5`, 3), ";", round(`Bootstrap:test_p97.5`, 3), ")"),
         `Optimism (95% CI)` = paste0(round(Optimism_mean, 3), " (", round(Optimism_p2.5, 3), ";", round(Optimism_p97.5, 3), ")"),
         `Full:corrected (95% CI)` = paste0(round(`Full:corrected_mean`, 3), " (", round(`Full:corrected_p2.5`, 3), ";", round(`Full:corrected_p97.5`, 3), ")"),
         ) %>%
  dplyr::select(`Performance metric`,
         `Full (95% CI)`,
         `Bootstrap:training (95% CI)`,
         `Bootstrap:test (95% CI)`,
         `Optimism (95% CI)`,
         `Full:corrected (95% CI)`)

summary_optimism_m2 <- summary_optimism_m2 %>%
  mutate(`Full (95% CI)` = paste0(round(Full_mean, 3), " (", round(Full_p2.5, 3), ";", round(Full_p97.5, 3), ")"),
         `Bootstrap:training (95% CI)` = paste0(round(`Bootstrap:training_mean`, 3), " (", round(`Bootstrap:training_p2.5`, 3), ";", round(`Bootstrap:training_p97.5`, 3), ")"),
         `Bootstrap:test (95% CI)` = paste0(round(`Bootstrap:test_mean`, 3), " (", round(`Bootstrap:test_p2.5`, 3), ";", round(`Bootstrap:test_p97.5`, 3), ")"),
         `Optimism (95% CI)` = paste0(round(Optimism_mean, 3), " (", round(Optimism_p2.5, 3), ";", round(Optimism_p97.5, 3), ")"),
         `Full:corrected (95% CI)` = paste0(round(`Full:corrected_mean`, 3), " (", round(`Full:corrected_p2.5`, 3), ";", round(`Full:corrected_p97.5`, 3), ")"),
  ) %>%
  dplyr::select(`Performance metric`,
         `Full (95% CI)`,
         `Bootstrap:training (95% CI)`,
         `Bootstrap:test (95% CI)`,
         `Optimism (95% CI)`,
         `Full:corrected (95% CI)`)

summary_optimism_m3 <- summary_optimism_m3 %>%
  mutate(`Full (95% CI)` = paste0(round(Full_mean, 3), " (", round(Full_p2.5, 3), ";", round(Full_p97.5, 3), ")"),
         `Bootstrap:training (95% CI)` = paste0(round(`Bootstrap:training_mean`, 3), " (", round(`Bootstrap:training_p2.5`, 3), ";", round(`Bootstrap:training_p97.5`, 3), ")"),
         `Bootstrap:test (95% CI)` = paste0(round(`Bootstrap:test_mean`, 3), " (", round(`Bootstrap:test_p2.5`, 3), ";", round(`Bootstrap:test_p97.5`, 3), ")"),
         `Optimism (95% CI)` = paste0(round(Optimism_mean, 3), " (", round(Optimism_p2.5, 3), ";", round(Optimism_p97.5, 3), ")"),
         `Full:corrected (95% CI)` = paste0(round(`Full:corrected_mean`, 3), " (", round(`Full:corrected_p2.5`, 3), ";", round(`Full:corrected_p97.5`, 3), ")"),
  ) %>%
  dplyr::select(`Performance metric`,
         `Full (95% CI)`,
         `Bootstrap:training (95% CI)`,
         `Bootstrap:test (95% CI)`,
         `Optimism (95% CI)`,
         `Full:corrected (95% CI)`)

summary_optimism_m4 <- summary_optimism_m4 %>%
  mutate(`Full (95% CI)` = paste0(round(Full_mean, 3), " (", round(Full_p2.5, 3), ";", round(Full_p97.5, 3), ")"),
         `Bootstrap:training (95% CI)` = paste0(round(`Bootstrap:training_mean`, 3), " (", round(`Bootstrap:training_p2.5`, 3), ";", round(`Bootstrap:training_p97.5`, 3), ")"),
         `Bootstrap:test (95% CI)` = paste0(round(`Bootstrap:test_mean`, 3), " (", round(`Bootstrap:test_p2.5`, 3), ";", round(`Bootstrap:test_p97.5`, 3), ")"),
         `Optimism (95% CI)` = paste0(round(Optimism_mean, 3), " (", round(Optimism_p2.5, 3), ";", round(Optimism_p97.5, 3), ")"),
         `Full:corrected (95% CI)` = paste0(round(`Full:corrected_mean`, 3), " (", round(`Full:corrected_p2.5`, 3), ";", round(`Full:corrected_p97.5`, 3), ")"),
  ) %>%
  dplyr::select(`Performance metric`,
         `Full (95% CI)`,
         `Bootstrap:training (95% CI)`,
         `Bootstrap:test (95% CI)`,
         `Optimism (95% CI)`,
         `Full:corrected (95% CI)`)

writexl::write_xlsx(
  list(Model_1_summary  = summary_optimism_m1,
       Model_1_mean  = mean_optimism_m1,
       Model_2_summary  = summary_optimism_m2,
       Model_2_mean  = mean_optimism_m2,
       Model_3_summary  = summary_optimism_m3,
       Model_3_mean  = mean_optimism_m3,
       Model_4_summary  = summary_optimism_m4,
       Model_4_mean  = mean_optimism_m4),
  "tables/Supp_Table11_Optimism_with_var_select.xlsx"
)
writexl::write_xlsx(
  list(Model_1  = variable_freq_m1,
       Model_2  = variable_freq_m2,
       Model_3  = variable_freq_m3,
       Model_4  = variable_freq_m4),
  "tables/Supp_Table11_Optimism_var_selection_bootstap_n_tables.xlsx"
)
