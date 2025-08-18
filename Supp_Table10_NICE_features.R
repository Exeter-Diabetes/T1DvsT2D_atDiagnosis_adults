#####################################################################################

#NICE Clinical combinations

#Supplementary Table 10

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(pROC)
#load functions ------------------------------------------------------------------
source("functions/model_info1.R")
source("functions/var_characteristics_1.R")
source("functions/model_continuous1.R")
source("functions/cat_contingency.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/data/SR_50_SRout_ccc_20_3_2025.RData")
#Prepare data -----------------------------------------------------------------------
table(SR_SRout_ccc$SRoutcome)
table(SR_50_SRout_ccc$SRoutcome)
table(SR_50_SRout_ccc$Eth_5cat, useNA = "ifany")
SR_SRout_ccc <- SR_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto)
  ) 
SR_50_SRout_ccc <- SR_50_SRout_ccc %>%
  mutate(bmi_model = ifelse(is.na(bmi_diag),
                            bmi,
                            bmi_diag),
         famhisnoninsdiab = ifelse(is.na(famhisnoninsdiab), "No", famhisnoninsdiab),
         famhisauto = ifelse(is.na(famhisauto), "0", famhisauto)
  ) 
SR_SRout_ccc$SRoutcome <- as.numeric(SR_SRout_ccc$SRoutcome)
SR_SRout_ccc$DKA <- as.character(SR_SRout_ccc$DKA)
SR_SRout_ccc$osmotic <- as.character(SR_SRout_ccc$osmotic)
SR_SRout_ccc$autoimmune <- as.character(SR_SRout_ccc$autoimmune)
SR_50_SRout_ccc$SRoutcome <- as.numeric(SR_50_SRout_ccc$SRoutcome)
SR_50_SRout_ccc$DKA <- as.character(SR_50_SRout_ccc$DKA)
SR_50_SRout_ccc$osmotic <- as.character(SR_50_SRout_ccc$osmotic)
SR_50_SRout_ccc$autoimmune <- as.character(SR_50_SRout_ccc$autoimmune)
SR_joint_cc <- full_join(SR_SRout_ccc, SR_50_SRout_ccc)

## Define NICEfeatures outcome
SR_joint_cc <- SR_joint_cc %>%
  mutate(NICEfeatures1 = ifelse((!is.na(DKA) & DKA == "1") 
                                | (!is.na(Unintentional_weight_loss_v1) & Unintentional_weight_loss_v1 == "Yes") 
                                | (!is.na(AgeatDiagnosis) & AgeatDiagnosis < 50) 
                                | (!is.na(bmi_model) & bmi_model < 25) 
                                | (!is.na(autoimmune) & autoimmune == "1" 
                                   | !is.na(famhisauto) & famhisauto == "1"), 
                                "1+ NICE features", 
                                ifelse(is.na(DKA) & is.na(Unintentional_weight_loss_v1) & is.na(AgeatDiagnosis) & is.na(bmi_model) & is.na(autoimmune) & is.na(famhisauto),
                                       NA,
                                       "No NICE features")),
         NICEfeatures2 = ifelse((!is.na(AgeatDiagnosis) & AgeatDiagnosis < 50) | (!is.na(bmi_model) & bmi_model < 25), 
                                "type 1", 
                                ifelse(is.na(AgeatDiagnosis) & is.na(bmi_model), NA, "type 2")),
         pos_anti = ifelse(num_anti >= 1, "1+ pos anti", "Negative antibodies"))
#look at those who have NICE outcome vs SR outcome
table(SR_joint_cc$NICEfeatures1, useNA = "ifany")
table(SR_joint_cc$NICEfeatures1, SR_joint_cc$SRoutcome, useNA = "ifany")
table(SR_joint_cc$NICEfeatures2, useNA = "ifany")
table(SR_joint_cc$NICEfeatures2, SR_joint_cc$SRoutcome, useNA = "ifany")
table(SR_joint_cc$pos_anti, useNA = "ifany")

table(SR_joint_cc$pos_anti, SR_joint_cc$SRoutcome, useNA = "ifany")
# ---------------------------------------------------------------------------------------


varlist_cat <- c( 
  "pos_anti",
  "NICEfeatures1", 
  "NICEfeatures2"
)
NICE_m12 <- SR_joint_cc %>%
  dplyr::select(SRoutcome, NICEfeatures1, NICEfeatures2, pos_anti) %>%
  filter(!is.na(pos_anti)) %>%
  mutate(NICEfeatures1 = ifelse(NICEfeatures1 == "1+ NICE features", 1, 0),
         NICEfeatures2 = ifelse(NICEfeatures2 == "type 1", 1, 0),
         pos_anti = ifelse(pos_anti == "1+ pos anti", 1, 0),
         NICE1_pos = ifelse(NICEfeatures1 == 1 & pos_anti ==1, 1, 0), 
         NICE2_pos = ifelse(NICEfeatures2 == 1 & pos_anti ==1, 1, 0))
table(NICE_m12$NICEfeatures1, useNA = "ifany")
table(NICE_m12$NICEfeatures1, NICE_m12$SRoutcome, useNA = "ifany")
table(NICE_m12$NICEfeatures2, useNA = "ifany")
table(NICE_m12$pos_anti, useNA = "ifany")
table(NICE_m12$NICE1_pos, NICE_m12$SRoutcome, useNA = "ifany")
table(NICE_m12$NICE2_pos, NICE_m12$SRoutcome, useNA = "ifany")


# cat_contingency(varlist_cat, 
#                 dataset = NICE_m12, 
#                 outcome = "SRoutcome", 
#                 saving_name = "tables/Supp_Table10_NICE",
#                 complete_case = TRUE, 
#                 plots = FALSE, 
#                 decimals = 3)

###########################################################################################
ANTI1 <- NICE_m12 %>%
  summarise(
    n = n(),
    totalover = n(),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(pos_anti == 1),
    PPV1 = (sum(pos_anti == 1)/totalover)*100,
    # nmissed_anti_pos = sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1),
    # Missedcases = ((sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1))/sum(pos_anti == 1))*100,
    all_three = sum(pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum(pos_anti == 0 & SRoutcome == 0),
    nmissed_antineg = sum(pos_anti == 0),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    #n_total_antineg = sum(pos_anti == 0),
    Spec = (all_three_under/n_T2D)*100)

ANTI1 <- ANTI1 %>%
  mutate(
    `% Meet criteria for antibody testing` = paste0(
      round(perc_testing,2), " (", totalover, "/", n, ")"),
    `Proportion of those eligible for testing, antibody positive` = paste0(
      round(PPV1,2), " (", n_anti_pos_pickedup, "/", totalover, ")"),
    PPV = paste0(
      round(PPV2,2), " (",all_three, "/", n_anti_pos_pickedup, ")"),
    NPV = paste0(
      round(NPV2,2), " (", all_three_under, "/", nmissed_antineg, ")"),
    Sensitivity = paste0(
      round(Sens,2), " (", all_three, "/", n_T1D,")"), 
    Specificity = paste0(
      round(Spec,2), " (", all_three_under, "/", n_T2D, ")")
  ) %>%
  select(
    `% Meet criteria for antibody testing`,
    `Proportion of those eligible for testing, antibody positive`,
    PPV,
    NPV,
    Sensitivity, 
    Specificity
  )
NICE1 <- NICE_m12 %>%
  summarise(
    n = n(),
    totalover = sum(NICEfeatures1 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(NICEfeatures1 == 1 & pos_anti == 1),
    PPV1 = (sum(NICEfeatures1 == 1 & pos_anti == 1)/sum(NICEfeatures1 == 1))*100,
    # nmissed_anti_pos = sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1),
    # Missedcases = ((sum(pos_anti == 1) - sum(NICEfeatures1 == 1 & pos_anti == 1))/sum(pos_anti == 1))*100,
    all_three = sum(NICEfeatures1 == 1 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((NICEfeatures1 == 0 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(NICEfeatures1 == 0 | pos_anti == 0),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    #n_total_antineg = sum(pos_anti == 0),
    Spec = (all_three_under/n_T2D)*100)

NICE1 <- NICE1 %>%
  mutate(
    `% Meet criteria for antibody testing` = paste0(
      round(perc_testing,2), " (", totalover, "/", n, ")"),
    `Proportion of those eligible for testing, antibody positive` = paste0(
      round(PPV1,2), " (", n_anti_pos_pickedup, "/", totalover, ")"),
    PPV = paste0(
      round(PPV2,2), " (",all_three, "/", n_anti_pos_pickedup, ")"),
    NPV = paste0(
      round(NPV2,2), " (", all_three_under, "/", nmissed_antineg, ")"),
    Sensitivity = paste0(
      round(Sens,2), " (", all_three, "/", n_T1D,")"), 
    Specificity = paste0(
      round(Spec,2), " (", all_three_under, "/", n_T2D, ")")
  ) %>%
  select(
    `% Meet criteria for antibody testing`,
    `Proportion of those eligible for testing, antibody positive`,
    PPV,
    NPV,
    Sensitivity, 
    Specificity
  )

NICE2 <- NICE_m12 %>%
  summarise(
    n = n(),
    totalover = sum(NICEfeatures2 == 1),
    perc_testing = (totalover/n)*100,
    n_anti_pos_pickedup = sum(NICEfeatures2 == 1 & pos_anti == 1),
    PPV1 = (sum(NICEfeatures2 == 1 & pos_anti == 1)/sum(NICEfeatures2 == 1))*100,
    # nmissed_anti_pos = sum(pos_anti == 1) - sum(NICEfeatures2 == 1 & pos_anti == 1),
    # Missedcases = ((sum(pos_anti == 1) - sum(NICEfeatures2 == 1 & pos_anti == 1))/sum(pos_anti == 1))*100,
    all_three = sum(NICEfeatures2 == 1 & pos_anti == 1 & SRoutcome == 1),
    all_three_under = sum((NICEfeatures2 == 0 | pos_anti == 0) & SRoutcome == 0),
    nmissed_antineg = sum(NICEfeatures2 == 0 | pos_anti == 0),
    PPV2 = (all_three/n_anti_pos_pickedup)*100,
    NPV2 = (all_three_under/sum(nmissed_antineg))*100,
    n_T1D = sum(SRoutcome == 1),
    n_T2D = sum(SRoutcome == 0),
    Sens = (all_three/n_T1D)*100,
    #n_total_antineg = sum(pos_anti == 0),
    Spec = (all_three_under/n_T2D)*100)

NICE2 <- NICE2 %>%
  mutate(
    `% Meet criteria for antibody testing` = paste0(
      round(perc_testing,2), " (", totalover, "/", n, ")"),
    `Proportion of those eligible for testing, antibody positive` = paste0(
      round(PPV1,2), " (", n_anti_pos_pickedup, "/", totalover, ")"),
    PPV = paste0(
      round(PPV2,2), " (",all_three, "/", n_anti_pos_pickedup, ")"),
    NPV = paste0(
      round(NPV2,2), " (", all_three_under, "/", nmissed_antineg, ")"),
    Sensitivity = paste0(
      round(Sens,2), " (", all_three, "/", n_T1D,")"), 
    Specificity = paste0(
      round(Spec,2), " (", all_three_under, "/", n_T2D, ")")
  ) %>%
  select(
    `% Meet criteria for antibody testing`,
    `Proportion of those eligible for testing, antibody positive`,
    PPV,
    NPV,
    Sensitivity, 
    Specificity
  )

NICE_criteria_table <- rbind(ANTI1, NICE1, NICE2)
write_xlsx(NICE_criteria_table,"tables/Supp_Table10_NICE_criteria.xlsx")

