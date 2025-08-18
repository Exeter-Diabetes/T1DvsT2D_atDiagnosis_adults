#####################################################################################

#Paper figures

#To get the two final paper complete case model datasets

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(rms)
library(naniar)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")

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
##Models -------------------------------------------------------------------------------
###Model 1
m1 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
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
          data = SR_m12_data,
          family = binomial)
save(m1, file = "m1.RData")

m1_alt <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
                scale(bmi_model) +
                scale(HbA1c_at_diagnosis_v1) +
                #scale(c_peptide_v1) +
                Gender_v1 +
                osmotic +
                autoimmune +
                Unintentional_weight_loss_v1 +
                DKA +
                famhisdiab,
              #famhisauto,
              data = SR_m12_data,
              family = binomial)
save(m1_alt, file = "m1_alt.RData")

SR_12_dd <- SR_m12_data %>%
  dplyr::select(all_of(all_vars_12), "SRoutcome") %>%
  as.data.frame()
dd <- datadist(SR_12_dd); options(datadist = 'dd')
m1_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) + 
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
              data = SR_12_dd,
              x=TRUE, 
              y = TRUE)

###Model 2
m2 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) + 
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 + 
            osmotic + 
            autoimmune + 
            Unintentional_weight_loss_v1 + 
            DKA + 
            famhisnoninsdiab +
            #famhisauto +
            num_anti, 
          data = SR_m12_data, 
          family = binomial)

save(m2, file = "m2.RData")

m2_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) + 
                scale(bmi_model) + 
                scale(HbA1c_at_diagnosis_v1) +
                #scale(c_peptide_v1) +
                Gender_v1 + 
                osmotic + 
                autoimmune + 
                Unintentional_weight_loss_v1 + 
                DKA + 
                famhisnoninsdiab +
                #famhisauto +
                num_anti,
              data = SR_12_dd,
              x=TRUE, 
              y = TRUE)

###Model 3
m3 <- glm(SRoutcome ~ scale(AgeatDiagnosis) +
            scale(bmi_model) + 
            scale(HbA1c_at_diagnosis_v1) +
            #scale(c_peptide_v1) +
            Gender_v1 + 
            osmotic + 
            autoimmune + 
            Unintentional_weight_loss_v1 + 
            DKA + 
            famhisnoninsdiab +
            #famhisauto +
            num_anti +
            scale(T1DGRS2_z), 
          data = SR_m3_data, 
          family = binomial)
save(m3, file = "m3.RData")

SR_3_dd <- SR_m3_data %>%
  dplyr::select(all_of(all_vars_3), "SRoutcome") %>%
  as.data.frame()
dd3 <- datadist(SR_3_dd); options(datadist = 'dd')
m3_rms <- lrm(SRoutcome ~ scale(AgeatDiagnosis) + 
                scale(bmi_model) + 
                scale(HbA1c_at_diagnosis_v1) +
                #scale(c_peptide_v1) +
                Gender_v1 + 
                osmotic + 
                autoimmune + 
                Unintentional_weight_loss_v1 + 
                DKA + 
                famhisnoninsdiab +
                #famhisauto +
                num_anti +
                scale(T1DGRS2_z),
              data = SR_3_dd,
              x=TRUE, 
              y = TRUE)

##Display item---------------------------------------------------------------------------
###Model 1 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m1, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm1", 
           manual_plotting = TRUE, 
           manual_plot_name = "m1")
model_info(model = m1_alt, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm1_alt", 
           manual_plotting = TRUE, 
           manual_plot_name = "m1_alt")

####Forest estimates plots
coefs_clean_m1 <- coefs_m1 %>%
  mutate(
    var_type = if_else(str_detect(variable, 
                                  "^scale\\("), 
                       "continuous", 
                       "categorical"),
    # Clean continuous: "scale(x)" → "x"
    variable_clean = str_replace(variable, 
                                 "^scale\\((.*)\\)$", 
                                 "\\1"),
    
    # Handle categorical: split into y: z where z starts with capital letter or digit
    variable_clean = case_when(
      var_type == "categorical" ~ str_replace(variable, 
                                              "(^[A-Za-z0-9_]+)([A-Z0-9].*)",  # Match prefix (variable) + group (z)
                                              "\\1: \\2"),  # Insert ": " between y and z
      TRUE ~ variable_clean
    ),
    # Optionally capitalize the first character in z if it's a letter
    variable_clean = str_replace(variable_clean, 
                                 ": ([a-z])", 
                                 ~paste0(": ", 
                                         toupper(.x[2]))),
    # For sorting: lowercase everything to ensure alphabetical order
    sort_key = str_to_lower(variable_clean)
  )

# Step 2: Split into continuous and categorical
cont_m1 <- coefs_clean_m1 %>%
  filter(var_type == "continuous") %>%
  arrange(desc(sort_key))
cat_m1 <- coefs_clean_m1 %>%
  filter(var_type == "categorical") %>%
  arrange(desc(sort_key))

# Step 3: Create break row using a copy of an existing row
break_row_m1 <- cont_m1[1, ]
break_row_m1[] <- NA  # set all values to NA
break_row_m1$variable_clean <- " "
break_row_m1$var_type <- "break"
break_row_m1$sort_key <- "zzzzzzzz"  # ensures it's last if sorted again

# Step 4: Bind and set factor levels
coefs_ordered_m1 <- bind_rows(cont_m1, break_row_m1, cat_m1) %>%
  mutate(variable_clean = fct_inorder(c("HbA1c at diagnosis (mmol/mol)",
                                        #"C-peptide at visit 1 (pmol/l)",
                                        "BMI at diagnosis (kg/m2)",
                                        "Age at diagnosis (years)",
                                        " ",
                                        "Unintentional weight-loss", 
                                        "Presence of osmotic symptoms",
                                        "Male sex",
                                        "Parent history of non-insulin-treated diabetes",
                                        #"Parent history of autoimmune disease", 
                                        "Presence of DKA",
                                        "Presence of other autoimmune disease", 
                                        "Intercept")))
coefs_clean_m1_alt <- coefs_m1_alt %>%
  mutate(
    var_type = if_else(str_detect(variable, 
                                  "^scale\\("), 
                       "continuous", 
                       "categorical"),
    # Clean continuous: "scale(x)" → "x"
    variable_clean = str_replace(variable, 
                                 "^scale\\((.*)\\)$", 
                                 "\\1"),
    
    # Handle categorical: split into y: z where z starts with capital letter or digit
    variable_clean = case_when(
      var_type == "categorical" ~ str_replace(variable, 
                                              "(^[A-Za-z0-9_]+)([A-Z0-9].*)",  # Match prefix (variable) + group (z)
                                              "\\1: \\2"),  # Insert ": " between y and z
      TRUE ~ variable_clean
    ),
    # Optionally capitalize the first character in z if it's a letter
    variable_clean = str_replace(variable_clean, 
                                 ": ([a-z])", 
                                 ~paste0(": ", 
                                         toupper(.x[2]))),
    # For sorting: lowercase everything to ensure alphabetical order
    sort_key = str_to_lower(variable_clean)
  )

# Step 2: Split into continuous and categorical
cont_m1_alt <- coefs_clean_m1_alt %>%
  filter(var_type == "continuous") %>%
  arrange(desc(sort_key))
cat_m1_alt <- coefs_clean_m1_alt %>%
  filter(var_type == "categorical") %>%
  arrange(desc(sort_key))

# Step 3: Create break row using a copy of an existing row
break_row_m1_alt <- cont_m1_alt[1, ]
break_row_m1_alt[] <- NA  # set all values to NA
break_row_m1_alt$variable_clean <- " "
break_row_m1_alt$var_type <- "break"
break_row_m1_alt$sort_key <- "zzzzzzzz"  # ensures it's last if sorted again

# Step 4: Bind and set factor levels
coefs_ordered_m1_alt <- bind_rows(cont_m1_alt, break_row_m1_alt, cat_m1_alt) %>%
  mutate(variable_clean = fct_inorder(c("HbA1c at diagnosis (mmol/mol)",
                                        #"C-peptide at visit 1 (pmol/l)",
                                        "BMI at diagnosis (kg/m2)",
                                        "Age at diagnosis (years)",
                                        " ",
                                        "Unintentional weight-loss", 
                                        "Presence of osmotic symptoms",
                                        "Male sex",
                                        "Parent history of diabetes",
                                        #"Parent history of autoimmune disease", 
                                        "Presence of DKA",
                                        "Presence of other autoimmune disease", 
                                        "Intercept")))

coef_table_m1 <- coefs_ordered_m1_alt %>%
  filter(var_type != "break") %>%
  dplyr::select(variable_clean, variable, estimate)
write_xlsx(coef_table_m1,"tables/model1_coef_table.xlsx")
###Model 2 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m2, 
           test_data = SR_m12_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm2", 
           manual_plotting = TRUE, 
           manual_plot_name = "m2")

####Forest estimates plots
coefs_clean_m2 <- coefs_m2 %>%
  mutate(
    var_type = if_else(str_detect(variable, 
                                  "^scale\\("), 
                       "continuous", 
                       "categorical"),
    # Clean continuous: "scale(x)" → "x"
    variable_clean = str_replace(variable, 
                                 "^scale\\((.*)\\)$", 
                                 "\\1"),
    
    # Handle categorical: split into y: z where z starts with capital letter or digit
    variable_clean = case_when(
      var_type == "categorical" ~ str_replace(variable, 
                                              "(^[A-Za-z0-9_]+)([A-Z0-9].*)",  # Match prefix (variable) + group (z)
                                              "\\1: \\2"),  # Insert ": " between y and z
      TRUE ~ variable_clean
    ),
    # Optionally capitalize the first character in z if it's a letter
    variable_clean = str_replace(variable_clean, 
                                 ": ([a-z])", 
                                 ~paste0(": ", 
                                         toupper(.x[2]))),
    # For sorting: lowercase everything to ensure alphabetical order
    sort_key = str_to_lower(variable_clean)
  )

# Step 2: Split into continuous and categorical
cont_m2 <- coefs_clean_m2 %>%
  filter(var_type == "continuous") %>%
  arrange(desc(sort_key))
cat_m2 <- coefs_clean_m2 %>%
  filter(var_type == "categorical") %>%
  arrange(desc(sort_key))

# Step 3: Create break row using a copy of an existing row
break_row_m2 <- cont_m2[1, ]
break_row_m2[] <- NA  # set all values to NA
break_row_m2$variable_clean <- " "
break_row_m2$var_type <- "break"
break_row_m2$sort_key <- "zzzzzzzz"  # ensures it's last if sorted again

# Step 4: Bind and set factor levels
coefs_ordered_m2 <- bind_rows(cont_m2, break_row_m2, cat_m2) %>%
  mutate(variable_clean = fct_inorder(c("HbA1c at diagnosis (mmol/mol)",
                                        #"C-peptide at visit 1 (pmol/l)",
                                        "BMI at diagnosis (kg/m2)",
                                        "Age at diagnosis (years)",
                                        " ",
                                        "Unintentional weight-loss", 
                                        "Presence of osmotic symptoms",
                                        "Three positive antibodies",
                                        "Two positive antibodies",
                                        "Single positive antibody",
                                        "Male sex",
                                        "Parent history of non-insulin-treated diabetes",
                                        #"Parent history of autoimmune disease", 
                                        "Presence of DKA",
                                        "Presence of other autoimmune disease", 
                                        "Intercept")))


###Model 3 ROW --------------------------------------------------------------------------
####Get large amount of model info automated
model_info(model = m3, 
           test_data = SR_m3_data, 
           outcome = "SRoutcome",
           saving_name = "03_04_sm3", 
           manual_plotting = TRUE, 
           manual_plot_name = "m3")

####Forest estimates plots
coefs_clean_m3 <- coefs_m3 %>%
  mutate(
    var_type = if_else(str_detect(variable, 
                                  "^scale\\("), 
                       "continuous", 
                       "categorical"),
    # Clean continuous: "scale(x)" → "x"
    variable_clean = str_replace(variable, 
                                 "^scale\\((.*)\\)$", 
                                 "\\1"),
    
    # Handle categorical: split into y: z where z starts with capital letter or digit
    variable_clean = case_when(
      var_type == "categorical" ~ str_replace(variable, 
                                              "(^[A-Za-z0-9_]+)([A-Z0-9].*)",  # Match prefix (variable) + group (z)
                                              "\\1: \\2"),  # Insert ": " between y and z
      TRUE ~ variable_clean
    ),
    # Optionally capitalize the first character in z if it's a letter
    variable_clean = str_replace(variable_clean, 
                                 ": ([a-z])", 
                                 ~paste0(": ", 
                                         toupper(.x[2]))),
    # For sorting: lowercase everything to ensure alphabetical order
    sort_key = str_to_lower(variable_clean)
  )

# Step 2: Split into continuous and categorical
cont_m3 <- coefs_clean_m3 %>%
  filter(var_type == "continuous") %>%
  arrange(desc(sort_key))
cat_m3 <- coefs_clean_m3 %>%
  filter(var_type == "categorical") %>%
  arrange(desc(sort_key))

# Step 3: Create break row using a copy of an existing row
break_row_m3 <- cont_m3[1, ]
break_row_m3[] <- NA  # set all values to NA
break_row_m3$variable_clean <- " "
break_row_m3$var_type <- "break"
break_row_m3$sort_key <- "zzzzzzzz"  # ensures it's last if sorted again

# Step 4: Bind and set factor levels
coefs_ordered_m3 <- bind_rows(cont_m3, break_row_m3, cat_m3) %>%
  mutate(variable_clean = fct_inorder(c("T1D GRS",
                                        "HbA1c at diagnosis (mmol/mol)",
                                        #"C-peptide at visit 1 (pmol/l)",
                                        "BMI at diagnosis (kg/m2)",
                                        "Age at diagnosis (years)",
                                        " ",
                                        "Unintentional weight-loss", 
                                        "Presence of osmotic symptoms",
                                        "Three positive antibodies",
                                        "Two positive antibodies",
                                        "Single positive antibody",
                                        "Male sex",
                                        "Parent history of non-insulin-treated diabetes",
                                        #"Parent history of autoimmune disease", 
                                        "Presence of DKA",
                                        "Presence of other autoimmune disease", 
                                        "Intercept")))






###Compiled pdf -----------------------------------------------------------------------
DESIGN <- "
11111###
  22334455
  22334455
  22334455
  22334455
  22334455
"
DESIGN_abc <- "
11111#
  223344
  223344
  223344
  223344
  223344
"

m12_n <- nrow(SR_m12_data)
m3_n <- nrow(SR_m3_data)
model1_text <- paste0("Model 1: Clinical features (n=",m12_n,")")
model2_text <- paste0("Model 2: Clinical features + number of positive antibodies (n=",m12_n,")")
model3_text <- paste0("Model 3: Clinical features + number of positive antibodies + T1DGRS (n=",m3_n,")")
#### Model1 row output
m1_plots_abcd <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.125)
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
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines")
    ),
  #boxplot
  ggplot(SR_m12_data, aes(x=factor(SRoutcome, 
                                   levels = c("1","0"), 
                                   labels = c("Type 1", "Type 2")), 
                          y = pred_prob_m1)) +
    geom_violin() + 
    geom_boxplot(width = .15, 
                 outlier.size = 1) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw(),
  #calibration plot
  ggplot(cal_prep_m1, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw(),
  #model_coefficients
  coefs_ordered_m1 %>%
    filter(variable_clean != "Intercept") %>%
    ggplot(aes(x=variable_clean, 
               y=as.numeric(estimate), 
               ymin=as.numeric(lower), 
               ymax=as.numeric(upper))) +
    geom_pointrange(aes(x=variable_clean, 
                        y=estimate, 
                        ymin=lower, 
                        ymax=upper)) + 
    geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Model variables") + 
    ylab("Estimate (95% CI)") +
    theme_bw() #,  # use a white background
  #ncol = 4, nrow = 1 
) + patchwork::plot_layout(design = DESIGN)
# + patchwork::plot_annotation(
#   title = "Model 1: Clinical features only (n=774)"
# )
m1_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model1_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 1.4)
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
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines")
    ),
  #boxplot
  ggplot(SR_m12_data, aes(x=factor(SRoutcome, 
                                   levels = c("1","0"), 
                                   labels = c("Type 1", "Type 2")), 
                          y = pred_prob_m1)) +
    geom_violin() + 
    geom_boxplot(width = .15, 
                 outlier.size = 1) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw(),
  #calibration plot
  ggplot(cal_prep_m1, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw()
) + patchwork::plot_layout(design = DESIGN_abc)


#### Model2
m2_plots_abcd <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model2_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.125)
  ),
  #roc_plot
  roc_curves_m2 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m2,
      mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines")
    ),
  #boxplot
  ggplot(SR_m12_data, aes(x=factor(SRoutcome, 
                                   levels = c("1","0"), 
                                   labels = c("Type 1", "Type 2")), 
                          y = pred_prob_m2)) +
    geom_violin() + 
    geom_boxplot(width = .15, 
                 outlier.size = 1) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw(),
  #calibration plot
  ggplot(cal_prep_m2, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw(),
  #model_coefficients
  coefs_ordered_m2 %>%
    filter(variable_clean != "Intercept") %>%
    ggplot(aes(x=variable_clean, 
               y=as.numeric(estimate), 
               ymin=as.numeric(lower), 
               ymax=as.numeric(upper))) +
    geom_pointrange(aes(x=variable_clean, 
                        y=estimate, 
                        ymin=lower, 
                        ymax=upper)) + 
    geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Model variables") + 
    ylab("Estimate (95% CI)") +
    theme_bw() #,  # use a white background
  #ncol = 4, nrow = 1 
)  + patchwork::plot_layout(design = DESIGN)

m2_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model2_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.71)
  ),
  #roc_plot
  roc_curves_m2 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m2,
      mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines")
    ),
  #boxplot
  ggplot(SR_m12_data, aes(x=factor(SRoutcome, 
                                   levels = c("1","0"), 
                                   labels = c("Type 1", "Type 2")), 
                          y = pred_prob_m2)) +
    geom_violin() + 
    geom_boxplot(width = .15, 
                 outlier.size = 1) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw(),
  #calibration plot
  ggplot(cal_prep_m2, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw()
)  + patchwork::plot_layout(design = DESIGN_abc)

####Model 3

m3_plots_abcd <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model3_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.125)
    ),
  #roc_plot
  roc_curves_m3 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m3,
      mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines")
    ),
  #boxplot
  ggplot(SR_m3_data, aes(x=factor(SRoutcome, 
                                  levels = c("1","0"), 
                                  labels = c("Type 1", "Type 2")), 
                         y = pred_prob_m3)) +
    geom_violin() + 
    geom_boxplot(width = .15, 
                 outlier.size = 1) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw(),
  #calibration plot
  ggplot(cal_prep_m3, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw(),
  #model_coefficients
  coefs_ordered_m3 %>%
    filter(variable_clean != "Intercept") %>%
    ggplot(aes(x=variable_clean, 
               y=as.numeric(estimate), 
               ymin=as.numeric(lower), 
               ymax=as.numeric(upper))) +
    geom_pointrange(aes(x=variable_clean, 
                        y=estimate, 
                        ymin=lower, 
                        ymax=upper)) + 
    geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Model variables") + 
    ylab("Estimate (95% CI)") +
    theme_bw() #,  # use a white background
  #ncol = 4, nrow = 2 
) + patchwork::plot_layout(design = DESIGN)

m3_plots_abc <- patchwork::wrap_plots(
  patchwork::wrap_elements(
    ggpubr::text_grob(model3_text,
                      face = "bold",
                      size = 20,
                      color = "black",
                      hjust = 0.61)
                   #x = 0.125,
                   #gp = grid::gpar(col = "black", fontsize = 20),
                   #just = "left"
    ),
  #roc_plot
  roc_curves_m3 %>%
    ggplot(aes(x = 1- specificities, y = sensitivities)) +
    geom_path() +
    theme_bw() +
    scale_y_continuous("Sensitivity", labels = scales::percent) +
    scale_x_continuous("1- Specificity", labels = scales::percent) +
    theme_bw() +
    geom_label(
      data = dat_text_m3,
      mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
      size = 7,
      label.r = unit(0, "pt"),
      label.padding=unit(0.4, "lines")
    ) +
    theme(
      panel.spacing.x = unit(1.5, "lines")
    ),
  #boxplot
  ggplot(SR_m3_data, aes(x=factor(SRoutcome, 
                                  levels = c("1","0"), 
                                  labels = c("Type 1", "Type 2")), 
                         y = pred_prob_m3)) +
    geom_violin() + 
    geom_boxplot(width = .15, 
                 outlier.size = 1) +
    xlab("Type of diabetes")+
    ylab("Predicted probability \n of T1D vs T2D") +
    theme_bw(),
  #calibration plot
  ggplot(cal_prep_m3, aes(x = mnpred, y = prob_obs)) +
    geom_point() +
    xlab("Mean predicted probability in each decile") +
    ylab("Proportion of T1D \n in each decile") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(c(0, 1)) + xlim(c(0, 1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper))+
    theme_bw()
) + patchwork::plot_layout(design = DESIGN_abc)


####PDF
pdf("03_04_model_display_item_abcd.pdf", height = 15, width = 20)
model_display_item_abcd <- patchwork::wrap_plots(
  m1_plots_abcd,
  m2_plots_abcd,
  m3_plots_abcd,
  ncol = 1, nrow = 3
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C", "D", 
                                                   "", "A", "B", "C", "D", 
                                                   "", "A", "B", "C", "D")))
print(model_display_item_abcd)
dev.off()

pdf("figures/Figure1.pdf", height = 15, width = 16)
model_display_item <- patchwork::wrap_plots(
  m1_plots_abc,
  m2_plots_abc,
  m3_plots_abc,
  ncol = 1, nrow = 3
) + patchwork::plot_annotation(tag_levels = list(c("", "A", "B", "C", 
                                                   "", "A", "B", "C", 
                                                   "", "A", "B", "C")))
print(model_display_item)
dev.off()
