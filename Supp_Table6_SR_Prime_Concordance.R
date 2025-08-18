#####################################################################################

#StartRight Concordance

#Supplementary Table 6

###################################################################################

#Libraries -----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions ------------------------------------------------------------------
source("functions/var_characteristics_1.R")
#Load data -------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_50_SRout_ccc_20_3_2025.RData")
load("~/PhD/StartRight_paper/data/SR_50_ro_cc_2_2025.RData")

#Create variable lists -----------------------------------------------------------------
SR_50_ro_cc$Urea_v1 <- as.numeric(SR_50_ro_cc$Urea_v1)
SR_50_ro_cc$LDL_v1 <- as.numeric(SR_50_ro_cc$LDL_v1)
SR_50_ro_cc$Haemoglobin_v1 <- as.numeric(SR_50_ro_cc$Haemoglobin_v1)
SR_50_ro_cc$white_blood_cell_count_v1 <- as.numeric(SR_50_ro_cc$white_blood_cell_count_v1)
SR_50_ro_cc$Platelets_v1 <- as.numeric(SR_50_ro_cc$Platelets_v1)
SR_50_ro_cc$num_anti <- as.character(SR_50_ro_cc$num_anti)
SR_50_ro_cc$autoimmune <- as.numeric(SR_50_ro_cc$autoimmune)
SR_50_ro_cc$osmotic <- as.numeric(SR_50_ro_cc$osmotic)
SR_50_ro_cc$DKA <- as.numeric(SR_50_ro_cc$DKA)
###COMPLETE CASE FOR BOTH OUTCOMES & VARS OF INTEREST
SR_cc <- full_join(SR_50_SRout_ccc, SR_50_ro_cc) %>%
  drop_na(SRoutcome, robust_outcome)

#Contingency table
table(SR_cc$SRoutcome, SR_cc$robust_outcome)

SR_cc %>%
  summarise(
    Concordant = sum((SRoutcome == "1" & robust_outcome == "T1D") | (SRoutcome == "0" & robust_outcome == "T2D"))/n()
  )
#Concordant = 99.3% 