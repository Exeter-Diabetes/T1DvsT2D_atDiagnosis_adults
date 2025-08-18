#####################################################################################

#Paper figures

#To get the two final paper complete case model datasets

#Supplementary Table 12:
#Optimism metrics for 
#Model 1
#Model 2
#Model 3

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
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
##Models ----------------------------------------------------------------------------
##Stability plots----------------------------------------------------------------------
###Model 1 ----------------------------------------------------------------------------------
SR_m12_data <- SR_m12_data %>%
  mutate(id = 1:n())

original_predictions1 <- predict(m1, 
                                 newdata = SR_m12_data, 
                                 type = "response") %>%
  cbind(id = SR_m12_data$id)


set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m12_data, 100)

bootstrapped_estimations <- NULL

for (i in 1:100) {
  
  new_dataset <- SR_m12_data[dataset_bootstrap$strap[[i]]$idx,]
  
  model <- glm(SRoutcome ~ scale(AgeatDiagnosis) + 
                 scale(bmi_model) + 
                 scale(HbA1c_at_diagnosis_v1) +
                 #scale(c_peptide_v1) +
                 Gender_v1 + 
                 osmotic + 
                 autoimmune + 
                 Unintentional_weight_loss_v1 + 
                 DKA + 
                 famhisnoninsdiab,
                 #famhisauto, 
               data = new_dataset, 
               family = binomial)
  
  new_predictions <- predict(model, 
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)
  
  bootstrapped_estimations <- rbind(bootstrapped_estimations, new_predictions)
  
}

stability_ci_1 <- as_tibble(bootstrapped_estimations) %>%
  group_by(id) %>%
  summarise(q1 = quantile(.,probs = 0.025, na.rm = TRUE), 
            q3 = quantile(.,probs = 0.975, na.rm = TRUE))


stability_plot1 <- original_predictions1 %>%
  as.data.frame() %>%
  rename("original_pred" = ".") %>%
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
  theme(axis.text = element_text(size = 6))

###Model 2 ----------------------------------------------------------------------------------
original_predictions2 <- predict(m2, 
                                 newdata = SR_m12_data, 
                                 type = "response") %>%
  cbind(id = SR_m12_data$id)


set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m12_data, 100)

bootstrapped_estimations <- NULL

for (i in 1:100) {
  
  new_dataset <- SR_m12_data[dataset_bootstrap$strap[[i]]$idx,]
  
  model <- glm(SRoutcome ~ scale(AgeatDiagnosis) + 
                 scale(bmi_model) + 
                 scale(HbA1c_at_diagnosis_v1) +
                 Gender_v1 + 
                 osmotic + 
                 autoimmune + 
                 Unintentional_weight_loss_v1 + 
                 DKA + 
                 famhisnoninsdiab +
                 #famhisauto +
                 num_anti, 
               data = new_dataset, 
               family = binomial)
  
  new_predictions <- predict(model, 
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)
  
  bootstrapped_estimations <- rbind(bootstrapped_estimations, new_predictions)
  
}

stability_ci2 <- as_tibble(bootstrapped_estimations) %>%
  group_by(id) %>%
  summarise(q1 = quantile(.,probs = 0.025, na.rm = TRUE), 
            q3 = quantile(.,probs = 0.975, na.rm = TRUE))


stability_plot2 <- original_predictions2 %>%
  as.data.frame() %>%
  rename("original_pred" = ".") %>%
  left_join(
    bootstrapped_estimations %>%
      as.data.frame() %>%
      rename("boot_pred" = "."), 
    by = c("id")
  ) %>%
  left_join(stability_ci2, 
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
  theme(axis.text = element_text(size = 6))
#stability_plot2

###Model 3 ----------------------------------------------------------------------------------
SR_m3_data <- SR_m3_data %>%
  mutate(id = 1:n())


original_predictions3 <- predict(m3, 
                                 newdata = SR_m3_data, 
                                 type = "response") %>%
  cbind(id = SR_m3_data$id)


set.seed(123)
dataset_bootstrap <- modelr::bootstrap(SR_m3_data, 100)

bootstrapped_estimations <- NULL

for (i in 1:100) {
  
  new_dataset <- SR_m3_data[dataset_bootstrap$strap[[i]]$idx,]
  
  model <- glm(SRoutcome ~ scale(AgeatDiagnosis) + 
                 scale(bmi_model) + 
                 scale(HbA1c_at_diagnosis_v1) +
                 Gender_v1 + 
                 osmotic + 
                 autoimmune + 
                 Unintentional_weight_loss_v1 + 
                 DKA + 
                 famhisnoninsdiab +
                 #famhisauto + 
                 num_anti +
                 scale(T1DGRS2_z), 
               data = new_dataset, 
               family = binomial)
  
  new_predictions <- predict(model, 
                             newdata = new_dataset,
                             type = "response") %>%
    cbind(id = dataset_bootstrap$strap[[i]]$idx)
  
  bootstrapped_estimations <- rbind(bootstrapped_estimations, new_predictions)
  
}

stability_ci3 <- as_tibble(bootstrapped_estimations) %>%
  group_by(id) %>%
  summarise(q1 = quantile(.,probs = 0.025, na.rm = TRUE), 
            q3 = quantile(.,probs = 0.975, na.rm = TRUE))


stability_plot3 <- original_predictions3 %>%
  as.data.frame() %>%
  rename("original_pred" = ".") %>%
  left_join(
    bootstrapped_estimations %>%
      as.data.frame() %>%
      rename("boot_pred" = "."), 
    by = c("id")
  ) %>%
  left_join(stability_ci3, 
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
  theme(axis.text = element_text(size = 6))
#stability_plot3

### Joint Stability PDF -----------------------------------------------------------
pdf("figures/Supp_Figure9.pdf", height = 21, width = 8)
stability_plots <- patchwork::wrap_plots(
  stability_plot1,
  stability_plot2, 
  stability_plot3,
  nrow = 3, ncol = 1
)+ patchwork::plot_annotation(tag_levels = list(c("A", "B", "C")))
print(stability_plots)
dev.off()
