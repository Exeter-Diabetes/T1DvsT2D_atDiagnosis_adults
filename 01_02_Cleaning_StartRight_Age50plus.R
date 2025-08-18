#################################################################################
#STARTRIGHT CLEANING: Ages 50+

#This provides cleaning from UNIVERSAL STARTRIGHT to those meeting paper
#inclusion criteria and age of diagnosis > 50

#1 Create SR >50s
##A For SRoutcome
##B Fore robust_outcome
#2 Do step-by-step for flow diagram
##A For SRoutcome
##B Fore robust_outcome
#3 Investigate missing outcome
##A For SRoutcome
##B Fore robust_outcome
#4 Save files
##A For SRoutcome
##B Fore robust_outcome
#################################################################################
#load libraries----------------------------------------------------------------------------
library(tidyverse)
library(writexl)
#load functions --------------------------------------------------------------------------
source("functions/var_characteristics.R")
source("functions/distri_plot5.R")

#load data-------------------------------------------------------------------------
load("~/PhD/StartRight_paper/data/SR_30_6_2025.RData")
#1) Filtering --------------------------------------------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------------
#CREATE SR 18-50s
SR_50_SRout <- SR %>%
  filter(AgeatDiagnosis > 50) %>%
  filter(!grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                Other_type_of_diabetes_v1)) %>%
  filter(!is.na(SRoutcome))

## B) For robust_outcome -------------------------------------------------------------------------
SR_50_ro <- SR %>%
  filter(AgeatDiagnosis > 50) %>%
  filter(!grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                Other_type_of_diabetes_v1)) %>%
  filter(!is.na(robust_outcome))
#2) Do step by step in order to get numbers for flow diagram -----------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------------
SR %>%
  filter(AgeatDiagnosis > 50) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))

SR %>%
  filter(AgeatDiagnosis > 50 & 
           grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                 Other_type_of_diabetes_v1)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(AgeatDiagnosis > 50 & 
           !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                  Other_type_of_diabetes_v1) & is.na(SRoutcome)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))

## B) For robust_outcome -------------------------------------------------------------------------
SR %>%
  filter(AgeatDiagnosis > 50) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))

SR %>%
  filter(AgeatDiagnosis > 50 & 
           grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                 Other_type_of_diabetes_v1)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(AgeatDiagnosis > 50 & 
           !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                  Other_type_of_diabetes_v1) & is.na(robust_outcome)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
#3) Investigate missing SRoutcome -------------------------------------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------------
SR %>%
  filter(AgeatDiagnosis > 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                                      Other_type_of_diabetes_v1) 
         & is.na(SRoutcome)) %>%
  group_by(define) %>%
  count()
###1) Investigate "Missing V4 insulin"
table_01_02_03_01_detail <- SR %>%
  filter(AgeatDiagnosis > 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                                      Other_type_of_diabetes_v1) 
         & is.na(SRoutcome)) %>%
  filter(define == "Missing V4 insulin") %>%
  select(Study_ID, Insulin_v1, Insulin_v2, Insulin_v3, Insulin_v4, UCPCR_v3, 
         UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3,
         Insulin_Long_acting_v3, Insulin_Mix_v3, Insulin_Rapid_acting_v3, 
         Stopped_Type_1_v3, Stopped_Type_2_v3, Started_Type_1_v3, 
         Started_Type_2_v3, DIET_only_v3, SGLT2_v3, MFN_v3, DPP4i_v3, GLP1_v3, 
         SU_v3, Participant_Continuation_v2, Participant_Continuation_v3, 
         `Visit Type_v4`, Insulin_Long_acting_v4, Insulin_Mix_v4, Insulin_Rapid_acting_v4, 
         Stopped_Type_1_v4, Stopped_Type_2_v4, Started_Type_1_v4, 
         Started_Type_2_v4, DIET_only_v4, SGLT2_v4, MFN_v4, DPP4i_v4, GLP1_v4, 
         SU_v4)

write_xlsx(table_01_02_03_01_detail,"tables/table_01_02_03_01_detail.xlsx")

###2) Investigate "V4>3, NO V4 BIO, V3 <3 years")
table_01_02_03_02_detail <- SR %>%
  filter(AgeatDiagnosis > 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                                      Other_type_of_diabetes_v1) 
         & is.na(SRoutcome)) %>%
  filter(define == "V4>3, NO V4 BIO, V3 <3 years") %>%
  select(Study_ID, Insulin_v3, Insulin_v4, 
         UCPCR_v3, UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3)

write_xlsx(table_01_02_03_02_detail,"tables/table_01_02_03_02_detail.xlsx")

###3) Investigate "Missing V4 insulin"
table_01_02_03_03_detail <- SR %>%
  filter(AgeatDiagnosis > 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                                      Other_type_of_diabetes_v1) 
         & is.na(SRoutcome)) %>%
  filter(define == "V4>3, NO V4 BIO, V3 >3 but Missing v3 UCPCR") %>%
  select(Study_ID, Insulin_v3, Insulin_v4, 
         UCPCR_v3, UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3)

write_xlsx(table_01_02_03_03_detail,"tables/table_01_02_03_03_detail.xlsx")

###4) Investigate "v3 and v4 less than 3"
table_01_02_03_04_detail <- SR %>%
  filter(AgeatDiagnosis > 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                                      Other_type_of_diabetes_v1) 
         & is.na(SRoutcome)) %>%
  filter(define == "v3 and v4 less than 3") %>%
  select(Study_ID, Insulin_v3, Insulin_v4, 
         UCPCR_v3, UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3)

write_xlsx(table_01_02_03_04_detail,"tables/table_01_02_03_04_detail.xlsx")

###5) Investigate "NA"
table_01_02_03_05_detail <- SR %>%
  filter(AgeatDiagnosis > 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
                                      Other_type_of_diabetes_v1) 
         & is.na(SRoutcome)) %>%
  filter(is.na(define)) %>%
  select(Study_ID, Insulin_v3, Insulin_v4, 
         UCPCR_v3, UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3)

write_xlsx(table_01_02_03_05_detail,"tables/table_01_02_03_05_detail.xlsx")

## B) For robust_outcome -------------------------------------------------------------------------

#4 Save data ---------------------------------------------------------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------------
# Rename for ease of use ------------------------------------------------------------------------
SR_50_SRout <- SR_50_SRout %>%
  rename(
    #Study_ID = StartRight_Study_ID_v1,
    #AgeatDiagnosis = `Age at Diagnosis_v1`,
    bmi = bmi_calc_v1
  )
save(SR_50_SRout, file = "data/SR_50_SRout_10_2_2025.RData")
## B) For robust_outcome -------------------------------------------------------------------------
SR_50_ro <- SR_50_ro %>%
  rename(
    #Study_ID = StartRight_Study_ID_v1,
    #AgeatDiagnosis = `Age at Diagnosis_v1`,
    bmi = bmi_calc_v1
  )
save(SR_50_ro, file = "data/SR_50_ro_10_2_2025.RData")