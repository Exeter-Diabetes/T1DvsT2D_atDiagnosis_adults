#####################################################################################

#Paper figures

#Supplementary Table 14

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





##Table of coefficients---------------------------------------------------------------
#Need to transpose remerge coefs_ordered_m with neatened names and column bind
coef_table_m1 <- coefs_ordered_m1 %>%
  mutate(coef_95 = paste0(estimate, " (", round(lower,2), ", ", 
                          round(upper, 2), ")"),
         OR_95 = paste0(OR, " (", round(OR_lower,2), ", ", 
                        round(OR_upper, 2), ")")) %>%
  filter(var_type != "break") %>%
  dplyr::select(variable_clean, coef_95, OR_95, p_value) %>%
  rename(`Model Feature` = variable_clean, 
         `Model 1 Beta coefficient (95% CI)` = coef_95,
         `Model 1 Odds Ratio (95% CI)` = OR_95,
         `Model 1 p-value` = p_value)

coef_table_m2 <- coefs_ordered_m2 %>%
  mutate(coef_95 = paste0(estimate, " (", round(lower,2), ", ", 
                          round(upper, 2), ")"),
         OR_95 = paste0(OR, " (", round(OR_lower,2), ", ", 
                        round(OR_upper, 2), ")")) %>%
  filter(var_type != "break") %>%
  dplyr::select(variable_clean, coef_95, OR_95, p_value) %>%
  rename(`Model Feature` = variable_clean, 
         `Model 2 Beta coefficient (95% CI)` = coef_95,
         `Model 2 Odds Ratio (95% CI)` = OR_95,
         `Model 2 p-value` = p_value)

coef_table_m3 <- coefs_ordered_m3 %>%
  mutate(coef_95 = paste0(estimate, " (", round(lower,2), ", ", 
                          round(upper, 2), ")"),
         OR_95 = paste0(OR, " (", round(OR_lower,2), ", ", 
                        round(OR_upper, 2), ")")) %>%
  filter(var_type != "break") %>%
  dplyr::select(variable_clean, coef_95, OR_95, p_value) %>%
  rename(`Model Feature` = variable_clean, 
         `Model 3 Beta coefficient (95% CI)` = coef_95,
         `Model 3 Odds Ratio (95% CI)` = OR_95,
         `Model 3 p-value` = p_value)

model_coef_table <- left_join(coef_table_m3, coef_table_m2)
model_coef_table <- left_join(model_coef_table, coef_table_m1)

write_xlsx(model_coef_table,"tables/Supp_Table14.xlsx")
