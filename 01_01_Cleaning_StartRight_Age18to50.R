#################################################################################
#STARTRIGHT CLEANING: Ages 18-50

#This provides cleaning from UNIVERSAL STARTRIGHT to those meeting paper
#inclusion criteria and age of diagnosis 18-50

#1 Create SR 18-50s
##A For SRoutcome
##B Fore robust_outcome
#2 Do step-by-step for flow diagram
##A For SRoutcome
##B Fore robust_outcome
#3 Investigate missing outcome
##A For SRoutcome
##B Fore robust_outcome
#4 Investigate those < 50 & recruited after "2018-10-31"
#5 Save datasets
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
SR_SRout <- SR %>%
  filter(AgeatDiagnosis <= 50) %>%
  #filter(Date_Visit_v1 <= "2018-10-31") %>%
  filter(!grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                Other_type_of_diabetes_v1)) %>%
  filter(!is.na(SRoutcome)) %>%
  filter(define %in% c("T1D Cpep v4", 
                       "T2D Cpep v4 ", 
                       "T2D no insulin v3",
                       "T2D no insulin v4"))

## B) For robust_outcome -------------------------------------------------------------------------
#CREATE SR 18-50s
SR_ro <- SR %>%
  filter(AgeatDiagnosis <= 50) %>%
  #filter(Date_Visit_v1 <= "2018-10-31") %>%
  filter(!grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                Other_type_of_diabetes_v1)) %>%
  filter(!is.na(robust_outcome))
#2) Do step by step in order to get numbers for flow diagram -----------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------------
SR %>%
  filter(AgeatDiagnosis > 50) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(
    Date_Visit_v1 > "2018-10-31" | 
      AgeatDiagnosis > 50) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(
    #Date_Visit_v1 > "2018-10-31" | 
           AgeatDiagnosis > 50 | 
           grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                Other_type_of_diabetes_v1)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(
    #Date_Visit_v1 > "2018-10-31" | 
      AgeatDiagnosis > 50 | 
      grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
            Other_type_of_diabetes_v1) | 
      is.na(SRoutcome)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))

## B) For robust_outcome -------------------------------------------------------------------------
SR %>%
  filter(AgeatDiagnosis > 50) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(
    #Date_Visit_v1 > "2018-10-31" | 
      AgeatDiagnosis > 50) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(
    #Date_Visit_v1 > "2018-10-31" | 
      AgeatDiagnosis > 50 | 
      grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
            Other_type_of_diabetes_v1)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(
    #Date_Visit_v1 > "2018-10-31" | 
      AgeatDiagnosis > 50 | 
      grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A",
            Other_type_of_diabetes_v1) | 
      is.na(robust_outcome)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))

#3) Investigate missing outcomes -------------------------------------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------
SR %>%
  filter(Date_Visit_v1 <= "2018-10-31" & AgeatDiagnosis <= 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                                                                    Other_type_of_diabetes_v1) & is.na(SRoutcome)) %>%
  group_by(define) %>%
  count()
###1) Investigate "Missing V4 insulin"
table_01_01_03_01_detail <- SR %>%
  filter(Date_Visit_v1 <= "2018-10-31" & AgeatDiagnosis <= 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                                                                       Other_type_of_diabetes_v1) & is.na(SRoutcome)) %>%
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

write_xlsx(table_01_01_03_01_detail,"tables/table_01_01_03_01_detail.xlsx")

###2) Investigate "V4>3, NO V4 BIO, V3 <3 years")
table_01_01_03_02_detail <- SR %>%
  filter(Date_Visit_v1 <= "2018-10-31" & AgeatDiagnosis <= 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                                                                       Other_type_of_diabetes_v1) & is.na(SRoutcome)) %>%
  filter(define == "V4>3, NO V4 BIO, V3 <3 years") %>%
  select(Study_ID, Insulin_v3, Insulin_v4, 
         UCPCR_v3, UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3)

write_xlsx(table_01_01_03_02_detail,"tables/table_01_01_03_02_detail.xlsx")

###3) Investigate "Missing V4 insulin"
table_01_01_03_03_detail <- SR %>%
  filter(Date_Visit_v1 <= "2018-10-31" & AgeatDiagnosis <= 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                                                                       Other_type_of_diabetes_v1) & is.na(SRoutcome)) %>%
  filter(define == "V4>3, NO V4 BIO, V3 >3 but Missing v3 UCPCR") %>%
  select(Study_ID, Insulin_v3, Insulin_v4, 
         UCPCR_v3, UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3)

write_xlsx(table_01_01_03_03_detail,"tables/table_01_01_03_03_detail.xlsx")

###4) Investigate "v3 and v4 less than 3"
table_01_01_03_04_detail <- SR %>%
  filter(Date_Visit_v1 <= "2018-10-31" & AgeatDiagnosis <= 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                                                                       Other_type_of_diabetes_v1) & is.na(SRoutcome)) %>%
  filter(define == "v3 and v4 less than 3") %>%
  select(Study_ID, Insulin_v3, Insulin_v4, 
         UCPCR_v3, UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3)

write_xlsx(table_01_01_03_04_detail,"tables/table_01_01_03_04_detail.xlsx")

###5) Investigate "NA"
table_01_01_03_05_detail <- SR %>%
  filter(Date_Visit_v1 <= "2018-10-31" & AgeatDiagnosis <= 50 & !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                                                                       Other_type_of_diabetes_v1) & is.na(SRoutcome)) %>%
  filter(is.na(define)) %>%
  select(Study_ID, Insulin_v3, Insulin_v4, 
         UCPCR_v3, UCPCR_v4, c_peptide_v4, dur_diab_joined_v4, dur_diab_yr_v3, 
         dur_diab_sample_yr_v4, dur_diab_sample_yr_v3)

write_xlsx(table_01_01_03_05_detail,"tables/table_01_01_03_05_detail.xlsx")

## B) For robust_outcome -------------------------------------------------------------------------

#4) Investigate < 50 years & Visit date post "2018-10-31" -------------------------------
## Make dataset that could be added to < 50s
SR_post_2018_10_31 <- SR %>%
  filter(AgeatDiagnosis <= 50 & Date_Visit_v1 > "2018-10-31")

SR_post_2018_10_31 %>%
  filter(!grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                Other_type_of_diabetes_v1)) %>%
  count()

SR_post_2018_10_31 %>%
  filter(!is.na(SRoutcome)) %>%
  count()

SR_post_2018_10_31 %>%
  filter(is.na(SRoutcome)) %>%
  group_by(define) %>%
  count()

SR_post_2018_10_31 <- SR_post_2018_10_31 %>%
  filter(!is.na(SRoutcome))

## Look at characteristics
varlist = c("AgeatDiagnosis", "bmi_calc_v1", "c_peptide_v1", "HbA1c_at_diagnosis_v1",
            "exeter_hba1c_v1", "T1DGRS2", "T2DGRS_suzuki24")
#create varlist_cat (categorical variables of interest names)
varlist_cat = c("clinical_diagnosis_v1", "define","num_anti", "GAD_bin_v1", "IA2_bin_v1", 
                "ZNT8_bin_v1", "famhisdiab", "ethnicity", "DKA", 
                "Unintentional_weight_loss_v1", "Gender_v1", "autoimmune", 
                "osmotic")

##a) create table of information ----------------------------------------------------
var_characteristics(varlist = varlist, varlist_cat = varlist_cat, dataset = SR_post_2018_10_31, numeric_option = "medianIQR", group = "SRoutcome")
#save as
table_01_01_4_characteristics_medIQR <- as.data.frame(summaryTable_GROUP_missing)
write_xlsx(table_01_01_4_characteristics_medIQR,"tables/table_01_01_4_characteristics_medIQR.xlsx")


##b) Figures for above ----------------------------------------------------------------
SR_post_2018_10_31$SRoutcome <- factor(SR_post_2018_10_31$SRoutcome, 
                                       levels = c("1","0"), 
                                       labels = c("Type 1", "Type 2"))
distri_plot(dataset = SR_post_2018_10_31, 
            vars = varlist, 
            pdf_name = "plots_01_01_4_bySRoutcome", 
            group = "SRoutcome")



#5) Save data ---------------------------------------------------------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------
# Rename for ease of use ------------------------------------------------------------------------
SR_SRout <- SR_SRout %>%
  rename(
    #Study_ID = StartRight_Study_ID_v1,
    #AgeatDiagnosis = `Age at Diagnosis_v1`,
    bmi = bmi_calc_v1
  )
save(SR_SRout, file = "data/SR_SRout_16_1_2025.RData")

## B) For robust_outcome -------------------------------------------------------------------------
# Rename for ease of use ------------------------------------------------------------------------
SR_ro <- SR_ro %>%
  rename(
    #Study_ID = StartRight_Study_ID_v1,
    #AgeatDiagnosis = `Age at Diagnosis_v1`,
    bmi = bmi_calc_v1
  )
save(SR_ro, file = "data/SR_ro_16_1_2025.RData")