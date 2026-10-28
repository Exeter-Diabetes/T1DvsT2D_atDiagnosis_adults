#####################################################################################

#Paper figures

#To get the two final paper complete case model datasets

#Supplementary Figure 7:
#Stability for
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
##Models ----------------------------------------------------------------------------
##Stability plots----------------------------------------------------------------------
###Model 1 ----------------------------------------------------------------------------------
SR_m12_data <- SR_m12_data %>%
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
SR_m12_data$Eth_4cat <- factor(SR_m12_data$Eth_4cat, levels = c("White", "Black", "South Asian", "Other/Mixed"))


set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m12_data, 100)
selected_vars_list <- vector("list", 100)
bootstrapped_estimations <- NULL
original_model_estimations <- NULL

for (i in 1:100) {

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

  vars <- case_when(
    vars == "Eth_4cat" ~ 'relevel(factor(Eth_4cat), ref = "White")',
    TRUE ~ vars
  )

  formula_string <- paste0(
    #"SRoutcome ~ scale(AgeatDiagnosis) + scale(bmi_model) + ",
    "SRoutcome ~ AgeatDiagnosis + bmi_model + ",
    paste(vars, collapse = " + ")
  )

  formula <- as.formula(formula_string)

  formula
  #apply model_comparison logic to building models - in new data and then in original

  model_new <- glm(formula,
               data = new_dataset,
               family = binomial)
  model_old <- glm(formula,
                   data = SR_m12_data,
                   family = binomial)

  new_predictions <- predict(model_new,
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)

  original_predictions1 <- predict(model_old,
                                   newdata = SR_m12_data,
                                   type = "response") %>%
    cbind(id = SR_m12_data$id)

  bootstrapped_estimations <- rbind(bootstrapped_estimations, new_predictions)
  original_model_estimations <- rbind(original_model_estimations, original_predictions1)

}

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
###Plots -------------------------------------------------------------------------------
stability_ci_1 <- as_tibble(bootstrapped_estimations) %>%
  group_by(id) %>%
  summarise(q1 = quantile(.,probs = 0.025, na.rm = TRUE),
            q3 = quantile(.,probs = 0.975, na.rm = TRUE))


stability_plot1 <- original_model_estimations %>%
  as.data.frame() %>%
  group_by(id) %>%
  summarise(original_pred = median(., na.rm = TRUE)) %>%
  #rename("original_pred" = ".") %>%
  left_join(
    bootstrapped_estimations %>%
      as.data.frame() %>%
      rename("boot_pred" = "."),
    by = c("id")
  ) %>%
  left_join(stability_ci_1,
            by = c("id")) %>%
  ggplot(aes(x = original_pred, y = boot_pred)) +
  geom_point(size = 0.1, alpha = 0.5, colour='grey') +
  geom_abline(aes(intercept = 0, slope = 1)) +
  stat_smooth(aes(x = original_pred, y = q1),
              se = FALSE,
              method = "loess",
              span = 0.3,
              linetype = 2,
              colour = "black",
              linewidth = 0.5) +
  stat_smooth(aes(x = original_pred, y = q3),
              se = FALSE,
              method = "loess",
              span = 0.3,
              linetype = 2,
              colour = "black",
              linewidth = 0.5) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab('Estimated risk from the developed model') +
  ylab('Estimated risk in the bootstrap samples') +
  theme_bw() +
  theme(text = element_text(size = 20))


###Model 2 ----------------------------------------------------------------------------------
SR_m12_data <- SR_m12_data %>%
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
SR_m12_data$Eth_4cat <- factor(SR_m12_data$Eth_4cat, levels = c("White", "Black", "South Asian", "Other/Mixed"))


set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m12_data, 100)
selected_vars_list <- vector("list", 100)
bootstrapped_estimations <- NULL
original_model_estimations <- NULL

for (i in 1:100) {

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

  formula
  #apply model_comparison logic to building models - in new data and then in original

  model_new <- glm(formula,
                   data = new_dataset,
                   family = binomial)
  model_old <- glm(formula,
                   data = SR_m12_data,
                   family = binomial)

  new_predictions <- predict(model_new,
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)

  original_predictions1 <- predict(model_old,
                                   newdata = SR_m12_data,
                                   type = "response") %>%
    cbind(id = SR_m12_data$id)

  bootstrapped_estimations <- rbind(bootstrapped_estimations, new_predictions)
  original_model_estimations <- rbind(original_model_estimations, original_predictions1)

}

###Frequency of clinical variables in bootstraps -----------------------------------------
n_boot <- 100
variable_freq_m2 <- tibble(
  bootstrap = seq_along(selected_vars_list),
  variables = selected_vars_list
) %>%
  unnest_longer(variables) %>%
  count(variables, name = "n_selected") %>%
  arrange(desc(n_selected))

variable_freq_m2
###Plots -------------------------------------------------------------------------------
stability_ci_2 <- as_tibble(bootstrapped_estimations) %>%
  group_by(id) %>%
  summarise(q1 = quantile(.,probs = 0.025, na.rm = TRUE),
            q3 = quantile(.,probs = 0.975, na.rm = TRUE))


stability_plot2 <- original_model_estimations %>%
  as.data.frame() %>%
  group_by(id) %>%
  summarise(original_pred = median(., na.rm = TRUE)) %>%
  #rename("original_pred" = ".") %>%
  left_join(
    bootstrapped_estimations %>%
      as.data.frame() %>%
      rename("boot_pred" = "."),
    by = c("id")
  ) %>%
  left_join(stability_ci_2,
            by = c("id")) %>%
  ggplot(aes(x = original_pred, y = boot_pred)) +
  geom_point(size = 0.1, alpha = 0.5, colour='grey') +
  geom_abline(aes(intercept = 0, slope = 1)) +
  stat_smooth(aes(x = original_pred, y = q1),
              se = FALSE,
              method = "loess",
              span = 0.3,
              linetype = 2,
              colour = "black",
              linewidth = 0.5) +
  stat_smooth(aes(x = original_pred, y = q3),
              se = FALSE,
              method = "loess",
              span = 0.3,
              linetype = 2,
              colour = "black",
              linewidth = 0.5) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab('Estimated risk from the developed model') +
  ylab('Estimated risk in the bootstrap samples') +
  theme_bw() +
  theme(text = element_text(size = 20))


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


set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m3_data, 100)
selected_vars_list <- vector("list", 100)
bootstrapped_estimations <- NULL
original_model_estimations <- NULL

for (i in 1:100) {

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

  formula
  #apply model_comparison logic to building models - in new data and then in original

  model_new <- glm(formula,
                   data = new_dataset,
                   family = binomial)
  model_old <- glm(formula,
                   data = SR_m3_data,
                   family = binomial)

  new_predictions <- predict(model_new,
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)

  original_predictions1 <- predict(model_old,
                                   newdata = SR_m3_data,
                                   type = "response") %>%
    cbind(id = SR_m3_data$id)

  bootstrapped_estimations <- rbind(bootstrapped_estimations, new_predictions)
  original_model_estimations <- rbind(original_model_estimations, original_predictions1)

}

###Frequency of clinical variables in bootstraps -----------------------------------------
n_boot <- 100
variable_freq_m3 <- tibble(
  bootstrap = seq_along(selected_vars_list),
  variables = selected_vars_list
) %>%
  unnest_longer(variables) %>%
  count(variables, name = "n_selected") %>%
  arrange(desc(n_selected))

variable_freq_m3
###Plots -------------------------------------------------------------------------------
stability_ci_3 <- as_tibble(bootstrapped_estimations) %>%
  group_by(id) %>%
  summarise(q1 = quantile(.,probs = 0.025, na.rm = TRUE),
            q3 = quantile(.,probs = 0.975, na.rm = TRUE))


stability_plot3 <- original_model_estimations %>%
  as.data.frame() %>%
  group_by(id) %>%
  summarise(original_pred = median(., na.rm = TRUE)) %>%
  #rename("original_pred" = ".") %>%
  left_join(
    bootstrapped_estimations %>%
      as.data.frame() %>%
      rename("boot_pred" = "."),
    by = c("id")
  ) %>%
  left_join(stability_ci_3,
            by = c("id")) %>%
  ggplot(aes(x = original_pred, y = boot_pred)) +
  geom_point(size = 0.1, alpha = 0.5, colour='grey') +
  geom_abline(aes(intercept = 0, slope = 1)) +
  stat_smooth(aes(x = original_pred, y = q1),
              se = FALSE,
              method = "loess",
              span = 0.3,
              linetype = 2,
              colour = "black",
              linewidth = 0.5) +
  stat_smooth(aes(x = original_pred, y = q3),
              se = FALSE,
              method = "loess",
              span = 0.3,
              linetype = 2,
              colour = "black",
              linewidth = 0.5) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab('Estimated risk from the developed model') +
  ylab('Estimated risk in the bootstrap samples') +
  theme_bw() +
  theme(text = element_text(size = 20))

###Model 4 ----------------------------------------------------------------------------------
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


set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m3_data, 100)
selected_vars_list <- vector("list", 100)
bootstrapped_estimations <- NULL
original_model_estimations <- NULL

for (i in 1:100) {

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

  formula
  #apply model_comparison logic to building models - in new data and then in original

  model_new <- glm(formula,
                   data = new_dataset,
                   family = binomial)
  model_old <- glm(formula,
                   data = SR_m3_data,
                   family = binomial)

  new_predictions <- predict(model_new,
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)

  original_predictions1 <- predict(model_old,
                                   newdata = SR_m3_data,
                                   type = "response") %>%
    cbind(id = SR_m3_data$id)

  bootstrapped_estimations <- rbind(bootstrapped_estimations, new_predictions)
  original_model_estimations <- rbind(original_model_estimations, original_predictions1)

}

###Frequency of clinical variables in bootstraps -----------------------------------------
n_boot <- 100
variable_freq_m4 <- tibble(
  bootstrap = seq_along(selected_vars_list),
  variables = selected_vars_list
) %>%
  unnest_longer(variables) %>%
  count(variables, name = "n_selected") %>%
  arrange(desc(n_selected))

variable_freq_m4
###Plots -------------------------------------------------------------------------------
stability_ci_4 <- as_tibble(bootstrapped_estimations) %>%
  group_by(id) %>%
  summarise(q1 = quantile(.,probs = 0.025, na.rm = TRUE),
            q3 = quantile(.,probs = 0.975, na.rm = TRUE))


stability_plot4 <- original_model_estimations %>%
  as.data.frame() %>%
  group_by(id) %>%
  summarise(original_pred = median(., na.rm = TRUE)) %>%
  #rename("original_pred" = ".") %>%
  left_join(
    bootstrapped_estimations %>%
      as.data.frame() %>%
      rename("boot_pred" = "."),
    by = c("id")
  ) %>%
  left_join(stability_ci_4,
            by = c("id")) %>%
  ggplot(aes(x = original_pred, y = boot_pred)) +
  geom_point(size = 0.1, alpha = 0.5, colour='grey') +
  geom_abline(aes(intercept = 0, slope = 1)) +
  stat_smooth(aes(x = original_pred, y = q1),
              se = FALSE,
              method = "loess",
              span = 0.3,
              linetype = 2,
              colour = "black",
              linewidth = 0.5) +
  stat_smooth(aes(x = original_pred, y = q3),
              se = FALSE,
              method = "loess",
              span = 0.3,
              linetype = 2,
              colour = "black",
              linewidth = 0.5) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab('Estimated risk from the developed model') +
  ylab('Estimated risk in the bootstrap samples') +
  theme_bw() +
  theme(text = element_text(size = 20))


### Joint Stability PDF -----------------------------------------------------------
pdf("figures/Supp_Figure7_var_selection.pdf", height = 20, width = 21)
stability_plots <- patchwork::wrap_plots(
  stability_plot1,
  stability_plot2,
  stability_plot3,
  stability_plot4,
  nrow = 2, ncol = 2
)+ patchwork::plot_annotation(tag_levels = list(c("A", "B", "C", "D")))
print(stability_plots)
dev.off()
ggsave("figures/Supp_Figurex8_var_selection.jpeg", stability_plots, height = 20, width = 21)


writexl::write_xlsx(
  list(Model_1  = variable_freq_m1,
       Model_2  = variable_freq_m2,
       Model_3  = variable_freq_m3,
       Model_4  = variable_freq_m4),
  "tables/Supp_Figx8_Stability_var_selection_bootstap_n_tables.xlsx"
)
