#################################################################################
#STARTRIGHT CLEANING: Ages 18-50

#This provides sense checks to 01_01

#1 Load in data, libraries & functions
#2 Sense checks
## I) Biochemical sense checks
###i) Checking all participants eaten within 5hrs of biochem test
####A For SRoutcome
####B For robust_outcome
###ii) Glucose at time of C-Peptide reading < 4,
####A For SRoutcome
####B For robust_outcome
###iii) Exclude UCPCR if time between collection & analysis is >7days
####A For SRoutcome
####B For robust_outcome
## II) OUTCOME sense checks
###i)Look at characteristics between how T1D and T2D defined
####A For SRoutcome
####B For robust_outcome
###ii) Looking at T2D Cpep >= 600 with no antibodies
####A For SRoutcome
####B For robust_outcome
###iii) Looking at autoimmune diabetes:
####A For SRoutcome
####B For robust_outcome
#3 Remove invalid
##A For SRoutcome
##B For robust_outcome


#################################################################################
#load libraries----------------------------------------------------------------------------
library(tidyverse)

#load functions --------------------------------------------------------------------------
source("functions/var_characteristics.R")
source("functions/distri_plot5.R")

#load data-------------------------------------------------------------------------
#load dataset from 01_01
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_SRout_16_1_2025.RData")
load("~/PhD/StartRight_paper/T1DvsT2D_atDiagnosis_adults/data/SR_ro_16_1_2025.RData")


# SENSE CHECKS ---------------------------------------------------------------------
##Check if all SR_ro in SR_SRout
#If so, only need to check SR_SRout and not both datasets
SR_ro %>%
  filter(!(Study_ID %in% SR_SRout$Study_ID)) %>%
  select(Study_ID, robust_outcome, robust_define, define, SRoutcome) %>%
  print(n=44)
#There are 44 individuals in SR_ro that are not in SR_SRout
#Therefore need to do checks in both datasets
#These individuals are robust_outcome T1D, due to on insulin at 3 yrs and 2+ antis
#They can't have SRoutcome as no C-peptide at visit 3 or 4

## I) BIOCHEMICAL ----------------------------------------------------------------
###i) Checking all participants eaten within 5hrs of biochem test
###ii) Glucose at time of C-Peptide reading < 4, CHECK IF BLOOD c-pEPTIDE IS < 600, if there are any they will need to be redefined based on ucpcr (if the ucpcr is >0.6/1.1)
###iii) Exclude UCPCR if time between collection & analysis is >7days (unless UCPCR is >1.1)

###i) Checking all participants eaten within 5hrs of biochem test -----------------------

#make new variable (eat5h) to check if participant eaten within 5hrs of biochem test:
#"Okay"                               - eaten within 5 hours of biochem test
#                                     (date is the same for the test and meal
#                                     /snack within 5 hours of the biochem |
#                                     date is the previous day, but the snack
#                                     is still within 5 hours of bloods)
#"Out time >5h"                       - eaten outside of 5 hours of biochem test
#                                     (date is the same for the test and meal
#                                     /snack outside 5 hours of the biochem)
#"Out day diff & time >5h"            -date of bloods and food are different &
#                                     snack/meal time is greater than blood time
#                                     (i.e. later in the previous day) but is
#                                      outside of 5hours
#"Meal date after stored plasma date" - last meal date after plasma stored (investigate)
#"no"                                 -last meal date before bloods taken but
#                                     before 24hours before bloods
#### A) For SRoutcome -------------------------------------------------------------------------------
SR_SRout <- SR_SRout %>%
  mutate(eat5h =
           #if date stored plasma is the same as the date of the last meal
           ifelse(Date_Stored_Plasma_collected_v4 == Date_last_meal_v4,
                  #then check if time difference between time stored plasma and
                  #time of last meal is <= 5yrs
                  ifelse(as.numeric(difftime(Time_of_last_meal_v4,
                                             Time_Stored_Plasma_collected_v4,
                                             units = "hours")) <= 5,
                         #if this is true then == good/okay
                         "Okay",
                         #if same days but time of last meal is more than 5 hours
                         #check if the time difference between the last snack/drink
                         #and time stored plasma is <= 5hrs
                         ifelse(as.numeric(difftime(Time_of_last_snack_and_drink_v4,
                                                    Time_Stored_Plasma_collected_v4,
                                                    units = "hours")) <= 5,
                                #if this is true then == good/okay
                                "Okay",
                                #same day but both snack/drink and last meal is > 5hrs
                                "Out time >5h")),
                  #if date stored plasma is not the same as the date of the last meal
                  #check if time of last meal is later than time stored plasma
                  ifelse(Time_of_last_meal_v4 > Time_Stored_Plasma_collected_v4,
                         #if yes, check if not missing and
                         #the difference between the last snack/drink and
                         #plasma is <= 5h
                         ifelse(!is.na(Time_of_last_snack_and_drink_v4) &
                                  (as.numeric(difftime(Time_of_last_snack_and_drink_v4,
                                                       Time_Stored_Plasma_collected_v4,
                                                       units = "hours"))) <= 5,
                                #if this is true then == good/okay
                                #(while the meal might have been on the previous day the snack was before the blood within 5hrs)
                                "Okay",
                                #last meal previous day and snack too late (> 5h)
                                "Out day diff & time >5h"),
                         #date stored plasma not the same as date as last meal
                         #and time of last meal not later than time stored plasma
                         #look at if the date of the last meal is after the
                         #plasma was stored (impossible)
                         ifelse(Date_last_meal_v4 > Date_Stored_Plasma_collected_v4,
                                #if yes
                                "Meal date after stored plasma date",
                                #if no, this means that the date last meal
                                #must have been before the date stored plasma
                                #but the time of the last meal
                                #must not have been larger then the time of the plasma storage
                                "no"
                         ))))
#counts per category
table(SR_SRout$eat5h, useNA = "ifany")

#closer look at those with different date and greater time
table_01_11_I_1_a_A_details <- SR_SRout %>%
  filter(eat5h == "no") %>%
  dplyr::select(Study_ID, Time_Stored_Plasma_collected_v4, Time_of_last_meal_v4,
                Time_of_last_snack_and_drink_v4, Date_Stored_Plasma_collected_v4,
                Date_last_meal_v4)
write_xlsx(table_01_11_I_1_a_A_details,"tables/table_01_11_I_1_a_A_details.xlsx")

#closer look at those with different date and greater time
table_01_11_I_1_b_A_details <- SR_SRout %>%
  filter(eat5h == "Meal date after stored plasma date") %>%
  dplyr::select(Study_ID, Time_Stored_Plasma_collected_v4, Time_of_last_meal_v4,
                Time_of_last_snack_and_drink_v4, Date_Stored_Plasma_collected_v4,
                Date_last_meal_v4)
write_xlsx(table_01_11_I_1_b_A_details,"tables/table_01_11_I_1_b_A_details.xlsx")


#closer look at those with different date and greater time
table_01_11_I_1_c_A_details <- SR_SRout %>%
  filter(eat5h == "Out day diff & time >5h") %>%
  dplyr::select(Study_ID, Time_Stored_Plasma_collected_v4, Time_of_last_meal_v4,
                Time_of_last_snack_and_drink_v4, Date_Stored_Plasma_collected_v4,
                Date_last_meal_v4)
write_xlsx(table_01_11_I_1_c_A_details,"tables/table_01_11_I_1_c_A_details.xlsx")

#closer look at those with missing time
table_01_11_I_1_d_A_details <- SR_SRout %>%
  filter(is.na(eat5h)) %>%
  dplyr::select(Study_ID, Time_Stored_Plasma_collected_v4, Time_of_last_meal_v4,
                Time_of_last_snack_and_drink_v4, Date_Stored_Plasma_collected_v4,
                Date_last_meal_v4, `Visit Type_v4`, Stored_Plasma_collected_v4)
write_xlsx(table_01_11_I_1_d_A_details,"tables/table_01_11_I_1_d_A_details.xlsx")


table_01_11_I_1_d_A_details %>%
  filter(is.na(Time_of_last_snack_and_drink_v4) &
           is.na(Time_of_last_meal_v4) &
           is.na(Time_Stored_Plasma_collected_v4) &
           is.na(Date_Stored_Plasma_collected_v4) &
           is.na(Date_last_meal_v4)) %>%
  count()

table_01_11_I_1_d_A_somenotna <- table_01_11_I_1_d_A_details %>%
  filter(!is.na(Time_of_last_snack_and_drink_v4) |
           !is.na(Time_of_last_meal_v4) |
           !is.na(Time_Stored_Plasma_collected_v4) |
           !is.na(Date_Stored_Plasma_collected_v4) |
           !is.na(Date_last_meal_v4))

table_01_11_I_1_d_A_somenotna <- table_01_11_I_1_d_A_somenotna %>%
  mutate(missing_eat5h = ifelse(!is.na(Date_Stored_Plasma_collected_v4) & !is.na(Date_last_meal_v4),
                                ifelse(!is.na(Time_of_last_meal_v4) & is.na(Time_Stored_Plasma_collected_v4),
                                       "Both dates, have time of last meal, no plasma time",
                                       ifelse(is.na(Time_of_last_meal_v4) & !is.na(Time_Stored_Plasma_collected_v4),
                                              "Both dates, have plasma time, no last meal time",
                                              "Both dates, no times")),
                                ifelse(!is.na(Date_Stored_Plasma_collected_v4) & is.na(Date_last_meal_v4),
                                       ifelse(is.na(Time_of_last_meal_v4) & is.na(Time_of_last_snack_and_drink_v4),
                                              "Plasma date, All last meal info missing",
                                              "Plasma date, no meal date, some meal times"),
                                       "Meal date, no plasma date")))
table(table_01_11_I_1_d_A_somenotna$missing_eat5h, useNA = "ifany")
table(table_01_11_I_1_d_A_somenotna$missing_eat5h, table_01_11_I_1_d_A_somenotna$Stored_Plasma_collected_v4, useNA = "ifany")

table_01_11_I_1_d_A_somenotna_11_details <- table_01_11_I_1_d_A_somenotna %>%
  filter((missing_eat5h == "Meal date, no plasma date"|
            missing_eat5h == "Plasma date, no meal date, some meal times") &
           Stored_Plasma_collected_v4 == "Yes")
write_xlsx(table_01_11_I_1_d_A_somenotna_11_details,"tables/table_01_11_I_1_d_A_somenotna_11_details.xlsx")

table_01_11_I_1_d_A_somenotna_2_details <- table_01_11_I_1_d_A_somenotna %>%
  filter(missing_eat5h == "Both dates, have plasma time, no last meal time"|
            missing_eat5h == "Both dates, have time of last meal, no plasma time")
write_xlsx(table_01_11_I_1_d_A_somenotna_2_details,"tables/table_01_11_I_1_d_A_somenotna_2_details.xlsx")

#### B) For robust_outcome -------------------------------------------------------------------------------
SR_ro <- SR_ro %>%
  mutate(eat5h =
           #if date stored plasma is the same as the date of the last meal
           ifelse(Date_Stored_Plasma_collected_v4 == Date_last_meal_v4,
                  #then check if time difference between time stored plasma and
                  #time of last meal is <= 5yrs
                  ifelse(as.numeric(difftime(Time_of_last_meal_v4,
                                             Time_Stored_Plasma_collected_v4,
                                             units = "hours")) <= 5,
                         #if this is true then == good/okay
                         "Okay",
                         #if same days but time of last meal is more than 5 hours
                         #check if the time difference between the last snack/drink
                         #and time stored plasma is <= 5hrs
                         ifelse(as.numeric(difftime(Time_of_last_snack_and_drink_v4,
                                                    Time_Stored_Plasma_collected_v4,
                                                    units = "hours")) <= 5,
                                #if this is true then == good/okay
                                "Okay",
                                #same day but both snack/drink and last meal is > 5hrs
                                "Out time >5h")),
                  #if date stored plasma is not the same as the date of the last meal
                  #check if time of last meal is later than time stored plasma
                  ifelse(Time_of_last_meal_v4 > Time_Stored_Plasma_collected_v4,
                         #if yes, check if not missing and
                         #the difference between the last snack/drink and
                         #plasma is <= 5h
                         ifelse(!is.na(Time_of_last_snack_and_drink_v4) &
                                  (as.numeric(difftime(Time_of_last_snack_and_drink_v4,
                                                       Time_Stored_Plasma_collected_v4,
                                                       units = "hours"))) <= 5,
                                #if this is true then == good/okay
                                #(while the meal might have been on the previous day the snack was before the blood within 5hrs)
                                "Okay",
                                #last meal previous day and snack too late (> 5h)
                                "Out day diff & time >5h"),
                         #date stored plasma not the same as date as last meal
                         #and time of last meal not later than time stored plasma
                         #look at if the date of the last meal is after the
                         #plasma was stored (impossible)
                         ifelse(Date_last_meal_v4 > Date_Stored_Plasma_collected_v4,
                                #if yes
                                "Meal date after stored plasma date",
                                #if no, this means that the date last meal
                                #must have been before the date stored plasma
                                #but the time of the last meal
                                #must not have been larger then the time of the plasma storage
                                "no"
                         ))))
#counts per category
table(SR_ro$eat5h, useNA = "ifany")

#closer look at those with different date and greater time
table_01_11_I_1_a_B_details <- SR_ro %>%
  filter(eat5h == "no") %>%
  dplyr::select(Study_ID, Time_Stored_Plasma_collected_v4, Time_of_last_meal_v4,
                Time_of_last_snack_and_drink_v4, Date_Stored_Plasma_collected_v4,
                Date_last_meal_v4)
write_xlsx(table_01_11_I_1_a_B_details,"tables/table_01_11_I_1_a_B_details.xlsx")

#closer look at those with different date and greater time
table_01_11_I_1_b_B_details <- SR_ro %>%
  filter(eat5h == "Meal date after stored plasma date") %>%
  dplyr::select(Study_ID, Time_Stored_Plasma_collected_v4, Time_of_last_meal_v4,
                Time_of_last_snack_and_drink_v4, Date_Stored_Plasma_collected_v4,
                Date_last_meal_v4)
write_xlsx(table_01_11_I_1_b_B_details,"tables/table_01_11_I_1_b_B_details.xlsx")


#closer look at those with different date and greater time
table_01_11_I_1_c_B_details <- SR_ro %>%
  filter(eat5h == "Out day diff & time >5h") %>%
  dplyr::select(Study_ID, Time_Stored_Plasma_collected_v4, Time_of_last_meal_v4,
                Time_of_last_snack_and_drink_v4, Date_Stored_Plasma_collected_v4,
                Date_last_meal_v4)
write_xlsx(table_01_11_I_1_c_B_details,"tables/table_01_11_I_1_c_B_details.xlsx")

#closer look at those with missing time
table_01_11_I_1_d_B_details <- SR_ro %>%
  filter(is.na(eat5h)) %>%
  dplyr::select(Study_ID, Time_Stored_Plasma_collected_v4, Time_of_last_meal_v4,
                Time_of_last_snack_and_drink_v4, Date_Stored_Plasma_collected_v4,
                Date_last_meal_v4, `Visit Type_v4`, Stored_Plasma_collected_v4)
write_xlsx(table_01_11_I_1_d_B_details,"tables/table_01_11_I_1_d_B_details.xlsx")


table_01_11_I_1_d_B_details %>%
  filter(is.na(Time_of_last_snack_and_drink_v4) &
           is.na(Time_of_last_meal_v4) &
           is.na(Time_Stored_Plasma_collected_v4) &
           is.na(Date_Stored_Plasma_collected_v4) &
           is.na(Date_last_meal_v4)) %>%
  count()

table_01_11_I_1_d_B_somenotna <- table_01_11_I_1_d_B_details %>%
  filter(!is.na(Time_of_last_snack_and_drink_v4) |
           !is.na(Time_of_last_meal_v4) |
           !is.na(Time_Stored_Plasma_collected_v4) |
           !is.na(Date_Stored_Plasma_collected_v4) |
           !is.na(Date_last_meal_v4))

table_01_11_I_1_d_B_somenotna <- table_01_11_I_1_d_B_somenotna %>%
  mutate(missing_eat5h = ifelse(!is.na(Date_Stored_Plasma_collected_v4) & !is.na(Date_last_meal_v4),
                                ifelse(!is.na(Time_of_last_meal_v4) & is.na(Time_Stored_Plasma_collected_v4),
                                       "Both dates, have time of last meal, no plasma time",
                                       ifelse(is.na(Time_of_last_meal_v4) & !is.na(Time_Stored_Plasma_collected_v4),
                                              "Both dates, have plasma time, no last meal time",
                                              "Both dates, no times")),
                                ifelse(!is.na(Date_Stored_Plasma_collected_v4) & is.na(Date_last_meal_v4),
                                       ifelse(is.na(Time_of_last_meal_v4) & is.na(Time_of_last_snack_and_drink_v4),
                                              "Plasma date, All last meal info missing",
                                              "Plasma date, no meal date, some meal times"),
                                       "Meal date, no plasma date")))
table(table_01_11_I_1_d_B_somenotna$missing_eat5h, useNA = "ifany")
table(table_01_11_I_1_d_B_somenotna$missing_eat5h,
      table_01_11_I_1_d_B_somenotna$Stored_Plasma_collected_v4, useNA = "ifany")

table_01_11_I_1_d_B_somenotna_11_details <- table_01_11_I_1_d_B_somenotna %>%
  filter((missing_eat5h == "Meal date, no plasma date"|
            missing_eat5h == "Plasma date, no meal date, some meal times") &
           Stored_Plasma_collected_v4 == "Yes")
write_xlsx(table_01_11_I_1_d_B_somenotna_11_details,"tables/table_01_11_I_1_d_B_somenotna_11_details.xlsx")

table_01_11_I_1_d_B_somenotna_2_details <- table_01_11_I_1_d_B_somenotna %>%
  filter(missing_eat5h == "Both dates, have plasma time, no last meal time"|
           missing_eat5h == "Both dates, have time of last meal, no plasma time")
write_xlsx(table_01_11_I_1_d_B_somenotna_2_details,"tables/table_01_11_I_1_d_B_somenotna_2_details.xlsx")

###ii) Glucose at time of C-Peptide reading < 4 -----------------------------------
#CHECK IF BLOOD c-pEPTIDE IS < 600,
#if there are any they will need to be redefined based on ucpcr
#(if the ucpcr is >0.6/1.1)
#### A) For SRoutcome -------------------------------------------------------------------------------
#make variable (glu_cp) that looks at glucose and c-peptide status
SR_SRout <- SR_SRout %>%
  mutate(glu_cp =
           ifelse(as.numeric(venous_glucose_v4) >= 4,
                  "Glu >= 4", #this says glucose is >= 4
                  ifelse(as.numeric(c_peptide_v4) < 600,
                         "glu < 4 & Cpep < 600",
                         "glu < 4 & Cpep >= 600")
           )
  )
#look at category counts
table(SR_SRout$glu_cp, useNA = "ifany")
#compare missing categry with missing glucose
SR_SRout %>%
  filter(is.na(venous_glucose_v4)) %>%
  count()
#look at those with low glucose and low c-peptide more closely
table_01_11_I_2_A_details <- SR_SRout %>%
  filter(glu_cp == "glu < 4 & Cpep < 600") %>%
  dplyr::select(Study_ID, SRoutcome, define, venous_glucose_v4, c_peptide_v4, UCPCR_v4)
write_xlsx(table_01_11_I_2_A_details,"tables/table_01_11_I_2_A_details.xlsx")

#### B) For robust_outcome -------------------------------------------------------------------------------
#make variable (glu_cp) that looks at glucose and c-peptide status
SR_ro <- SR_ro %>%
  mutate(glu_cp =
           ifelse(as.numeric(venous_glucose_v4) >= 4,
                  "Glu >= 4", #this says glucose is >= 4
                  ifelse(as.numeric(c_peptide_v4) < 600,
                         "glu < 4 & Cpep < 600",
                         "glu < 4 & Cpep >= 600")
           )
  )
#look at category counts
table(SR_ro$glu_cp, useNA = "ifany")
#compare missing categry with missing glucose
SR_ro %>%
  filter(is.na(venous_glucose_v4)) %>%
  count()
#look at those with low glucose and low c-peptide more closely
table_01_11_I_2_B_details <- SR_ro %>%
  filter(glu_cp == "glu < 4 & Cpep < 600") %>%
  dplyr::select(Study_ID, SRoutcome, define, venous_glucose_v4, c_peptide_v4, UCPCR_v4)
write_xlsx(table_01_11_I_2_B_details,"tables/table_01_11_I_2_B_details.xlsx")

###iii) Exclude UCPCR if time between collection & analysis is >7days ---------------
#(unless UCPCR is >1.1)
#look at those with a urine time in transit > 7 and their UCPCR
#### A) For SRoutcome -------------------------------------------------------------------------------
SR_SRout <- SR_SRout %>%
  mutate(urine_days_in_transit_v4 = as.numeric(difftime(V4DateUrineReceive_v4,
                                                        V4DateUrineCollect_v4,
                                                        units = "days")))

table_01_11_I_3_A_details <- SR_SRout %>%
  filter(as.numeric(urine_days_in_transit_v4) > 7) %>%
  dplyr::select(Study_ID, urine_days_in_transit_v4, SRoutcome, define, UCPCR_v4)
write_xlsx(table_01_11_I_3_A_details,"tables/table_01_11_I_3_A_details.xlsx")

#### B) For robust_outcome -------------------------------------------------------------------------------
SR_ro <- SR_ro %>%
  mutate(urine_days_in_transit_v4 = as.numeric(difftime(V4DateUrineReceive_v4,
                                                        V4DateUrineCollect_v4,
                                                        units = "days")))

table_01_11_I_3_B_details <- SR_ro %>%
  filter(as.numeric(urine_days_in_transit_v4) > 7) %>%
  dplyr::select(Study_ID, urine_days_in_transit_v4, SRoutcome, define, UCPCR_v4)
write_xlsx(table_01_11_I_3_B_details,"tables/table_01_11_I_3_B_details.xlsx")

## II) OUTCOME sense checks ----------------------------------------------------------
###i)Look at characteristics between how T1D and T2D defined --------------------------
#(broad catergories: cpep, ucpcr, insulin)
#### A) For SRoutcome -------------------------------------------------------------------------------
#group by how T1 & T2 defined
SR_SRout <- SR_SRout %>%
  mutate(def3 = ifelse(str_detect(define, "T1D Cpep"),
                       "T1D Cpep < 600",
                       ifelse(str_detect(define, "T1D UCPCR"),
                              "T1D UCPCR < 1.1",
                              ifelse(str_detect(define, "T2D no insulin"),
                                     "T2D no insulin",
                                     ifelse(str_detect(define, "T2D Cpep"),
                                            "T2D Cpep >= 600",
                                            ifelse(str_detect(define, "T2D UCPCR"),
                                                   "T2D UCPCR >= 1.1",
                                                   NA))
                              )
                       )
  )
  )

SR_SRout$autoimmune <- as.numeric(SR_SRout$autoimmune)
SR_SRout$osmotic <- as.numeric(SR_SRout$osmotic)
SR_SRout$DKA <- as.numeric(SR_SRout$DKA)
SR_SRout$num_anti <- factor(SR_SRout$num_anti)
SR_SRout$LDL_v1 <- as.numeric(SR_SRout$LDL_v1)
SR_SRout$Urea_v1 <- as.numeric(SR_SRout$Urea_v1)
SR_SRout$Haemoglobin_v1 <- as.numeric(SR_SRout$Haemoglobin_v1)
SR_SRout$white_blood_cell_count_v1 <- as.numeric(SR_SRout$white_blood_cell_count_v1)
SR_SRout$Platelets_v1 <- as.numeric(SR_SRout$Platelets_v1)

#create table of information
#create varlist (numeric variables of interest names)
varlist = c("AgeatDiagnosis", "bmi", "bmi_diag", "c_peptide_v1", "Hips_v1",
            "Waist_v1", "wh_ratio_v1", "Systolic_BP_v1", "Diastolic_BP_v1",
            "Fat_Percentage_v1", "Alcohol_Units_per_week_v1", "HDL_at_diag_v1",
            "LDL_at_diag_v1", "HbA1c_at_diagnosis_v1", "Cholesterol_at_diag_v1",
            "Triglycerides_at_diag_v1", "Glucose_at_diagnosis_v1", "HDL_v1",
            "LDL_v1", "exeter_hba1c_v1", "Cholestrol_v1", "Triglycerides_v1",
            "Glucose_v1", "GFR_v1", "Sodium_v1", "Potassium_v1", "Creatinine_v1",
            "Urea_v1", "Bilirubin_v1", "ALT_v1", "ALP_v1", "Albumin_v1",
            "Haemoglobin_v1", "white_blood_cell_count_v1", "Platelets_v1",
            "T1DGRS2", "T2DGRS_suzuki24")
#create varlist_cat (categorical variables of interest names)
varlist_cat = c("num_anti", "AntibodyStatus", "GAD_bin_v1", "IA2_bin_v1",
                "ZNT8_bin_v1", "famhisdiab", "ethnicity", "DKA",
                "Unintentional_weight_loss_v1", "Gender_v1", "autoimmune",
                "osmotic", "Ketoacidosis_at_diag_v1", "Smoker_v1",
                "Acanthosis_Nigricans_v1", "At_diagnosis_Admitted_to_hosp_v1",
                "Hypertension_on_meds_v1", "famhisauto", "famhisinsdiab",
                "famhisnoninsdiab")

var_characteristics(varlist = varlist, varlist_cat = varlist_cat, dataset = SR_SRout, numeric_option = "medianIQR", group = "def3")
#save as
table_01_11_II_1_characteristics_medIQR <- as.data.frame(summaryTable_GROUP_missing)
write_xlsx(table_01_11_II_1_characteristics_medIQR,"tables/table_01_11_II_1_characteristics_medIQR.xlsx")


#Figures for above
distri_plot(dataset = SR_SRout, vars = varlist, pdf_name = "plots_01_11_II_1_bydef3", group = "def3")




#ant
table(SR_SRout$num_anti)
table(SR_SRout$AntibodyStatus)
#visualising bmi distribution

SR_SRout$num_anti <- as.character(SR_SRout$num_anti)

ggplot(SR_SRout, aes(num_anti)) +
  geom_bar() +
  facet_wrap(vars(def3))

SR_SRout$AntibodyStatus <- as.character(SR_SRout$AntibodyStatus)

ggplot(SR_SRout, aes(AntibodyStatus)) +
  geom_bar() +
  facet_wrap(vars(def3))

#### B) For robust_outcome -------------------------------------------------------------------------------

###ii) Looking at T2D Cpep >= 600 with no antibodies ----------------------------------
#and what their ucpcr values should be (in broad groups)
#- this is to help decide whether our cut-off of 1.1 makes sense
#### A) For SRoutcome -------------------------------------------------------------------------------
#Filter new dataset with only T2D CPEP>= 600, no antibodies
d_01_11_II_2 <- SR_SRout %>%
  filter(def3 == "T2D Cpep >= 600" & num_anti == "0")
#make new variable of UCPCR category
d_01_11_II_2 <- d_01_11_II_2 %>%
  mutate(ucpcr_cat = ifelse(UCPCR_v4 < 0.6,
                            "<0.6",
                            ifelse(UCPCR_v4 >= 0.6 & UCPCR_v4 < 1.1,
                                   "0.6 - <1.1",
                                   ">=1.1")))
#look at new variable category counts
table(d_01_11_II_2$ucpcr_cat, useNA = "ifany")

#create table of information
#create varlist (numeric variables of interest names)
#varlist = c("AgeatDiagnosis", "bmi", "wh_ratio_v1", "HDL_at_diag_v1",
            #"HbA1c_at_diagnosis_v1", "Triglycerides_at_diag_v1", "T1DGRS2",
            #"T2DGRS_suzuki24")
#create varlist_cat (categorical variables of interest names)
#varlist_cat = c("num_anti", "AntibodyStatus", "GAD_bin_v1", "IA2_bin_v1",
               # "ZNT8_bin_v1", "famhisdiab", "ethnicity", "DKA",
               # "Unintentional_weight_loss_v1", "Gender", "autoimmune",
               #"osmotic")

var_characteristics(varlist = varlist, varlist_cat = varlist_cat,
                    dataset = d_01_11_II_2, numeric_option = "medianIQR",
                    group = "ucpcr_cat")
#save as
table_01_11_II_2_characteristics_medIQR <- as.data.frame(summaryTable_GROUP_missing)
write_xlsx(table_01_11_II_2_characteristics_medIQR,"tables/table_01_11_II_2_characteristics_medIQR.xlsx")


#Figures for above
distri_plot(dataset = d_01_11_II_2, vars = varlist, pdf_name = "plots_01_11_II_2_byucpcr_cat", group = "ucpcr_cat")

#look more closely at those "<0.6"
table_01_11_II_2_a_details <- d_01_11_II_2 %>%
  filter(ucpcr_cat == "<0.6") %>%
  dplyr::select(clinical_diagnosis_v1, UCPCR_v3, UCPCR_v4, AgeatDiagnosis, bmi, famhisdiab, ethnicity,
                DKA, Unintentional_weight_loss_v1, Gender, autoimmune,
                osmotic, HDL_at_diag_v1, HbA1c_at_diagnosis_v1, T1DGRS2,
                T2DGRS_suzuki24)
write_xlsx(table_01_11_II_2_a_details,"tables/table_01_11_II_2_a_details.xlsx")

#look more closely at those "0.6 - <1.1"
table_01_11_II_2_b_details <-d_01_11_II_2 %>%
  filter(ucpcr_cat == "0.6 - <1.1") %>%
  dplyr::select(clinical_diagnosis_v1, UCPCR_v3, UCPCR_v4, AgeatDiagnosis, bmi, famhisdiab, ethnicity,
                DKA, Unintentional_weight_loss_v1, Gender, autoimmune,
                osmotic, HDL_at_diag_v1, HbA1c_at_diagnosis_v1, T1DGRS2,
                T2DGRS_suzuki24)
write_xlsx(table_01_11_II_2_b_details,"tables/table_01_11_II_2_b_details.xlsx")
#look more closely at those ">=1.1"
table_01_11_II_2_c_details <- d_01_11_II_2 %>%
  filter(ucpcr_cat == ">=1.1") %>%
  dplyr::select(clinical_diagnosis_v1, UCPCR_v3, UCPCR_v4, Other_type_of_diabetes_v1)
write_xlsx(table_01_11_II_2_c_details,"tables/table_01_11_II_2_c_details.xlsx")


#look at UCPCR distribution of those in ">=1.1"
d_01_11_II_2 %>%
  filter(ucpcr_cat == ">=1.1") %>%
  ggplot(aes(y=UCPCR_v4)) +
  geom_boxplot()

#### B) For robust_outcome -------------------------------------------------------------------------------

###iii) Looking at autoimmune diabetes: -----------------------------------------------
#how many do we miss (greater than 2 antibodies & on insulin >3yrs) and what
#their ucpcr values should be (in broad groups) - this is to help decide
#whether our cut-off of 1.1 makes sense
#autoimmune diabetes:
#how many do we miss (greater than 2 antibodies & on insulin >3yrs)
#### A) For SRoutcome -------------------------------------------------------------------------------
#Make new dataset of only those with 2+ antibodies
#and new variable lookin at insulin status at 3 years
d_01_11_II_3 <- SR_SRout %>%
  filter(num_anti == "2" | num_anti == "3") %>%
  mutate(ins3 =
           ifelse(!is.na(dur_diab_yr_v4) & dur_diab_yr_v4 >= 2.86,
                  ifelse(!is.na(Insulin_v4) & Insulin_v4 == "No"|Insulin_v4 == "no"|Insulin_v4 == "NO",
                         "no insulin v4",
                         ifelse(!is.na(Insulin_v4) & Insulin_v4 == "Yes",
                                "insulin v4",
                                "missing insulin v4")
                  ),
                  ifelse(!is.na(dur_diab_yr_v3) & dur_diab_yr_v3 >= 2.86,
                         ifelse(!is.na(Insulin_v3) & Insulin_v3 == "No",
                                "no insulin v3",
                                ifelse(!is.na(Insulin_v3) & Insulin_v3 == "Yes",
                                       "insulin v3",
                                       "missing diab_y4 or v4 < 3y & missing insulin v3")),
                         ifelse(is.na(dur_diab_yr_v3) | dur_diab_yr_v3 < 2.86,
                                ifelse(!is.na(dur_diab_sample_yr_v4) & dur_diab_sample_yr_v4 >= 2.86,
                                       ifelse(!is.na(Insulin_v4) & Insulin_v4 == "No"|Insulin_v4 == "no"|Insulin_v4 == "NO",
                                              "no insulin v4 up",
                                              ifelse(!is.na(Insulin_v4) & Insulin_v4 == "Yes",
                                                     "insulin v4 up",
                                                     "missing insulin v4 up")),
                                       "missing dur_diab_sample_v4 or <3y"),
                                "missing dur_diab_y3 & sample_v4")
                  )
           )
  )
#make UCPCR category variable
d_01_11_II_3 <- d_01_11_II_3 %>%
  mutate(ucpcr_cat = ifelse(UCPCR_v4 < 0.6,
                            "<0.6",
                            ifelse(UCPCR_v4 >= 0.6 & UCPCR_v4 < 1.1,
                                   "0.6 - <1.1",
                                   ">1.1")))

#filter only those on insulin
d_01_11_II_3 <- d_01_11_II_3 %>%
  filter(ins3 == "insulin v3" | ins3 == "insulin v4" | ins3 == "insulin v4 up")

#characteristics table
var_characteristics(varlist = varlist,
                    varlist_cat = varlist_cat,
                    dataset = d_01_11_II_3,
                    numeric_option = "medianIQR",
                    group = "ucpcr_cat")
#save as
table_01_11_II_3_characteristics_medIQR <- as.data.frame(summaryTable_GROUP_missing)
write_xlsx(table_01_11_II_3_characteristics_medIQR,"tables/table_01_11_II_3_characteristics_medIQR.xlsx")


#Figures for above
distri_plot(dataset = d_01_11_II_3,
            vars = varlist,
            pdf_name = "plots_01_11_II_3_byucpcr_cat",
            group = "ucpcr_cat")


#check SRoutcome & how those with autoimmune T1D & UCPCR > 0.6 defined
table_01_11_II_3_a_details <- d_01_11_II_3 %>%
  filter(ins3 == "insulin v3" | ins3 == "insulin v4" | ins3 == "insulin v4 up") %>%
  filter(ucpcr_cat == ">1.1" | ucpcr_cat == "0.6 - <1.1") %>%
  dplyr:: select(SRoutcome, define, ucpcr_cat)
write_xlsx(table_01_11_II_3_a_details,"tables/table_01_11_II_3_a_details.xlsx")
#

#### B) For robust_outcome -------------------------------------------------------------------------------


#3) remove invalid -------------------------------------------------------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------------
SR_SRout_cc <- SR_SRout
save(SR_SRout_cc, file = "data/SR_SRout_cc_1_2025.RData")

## B) For robust_outcome -------------------------------------------------------------------------------
SR_ro_cc <- SR_ro
save(SR_ro_cc, file = "data/SR_ro_cc_1_2025.RData")

#4) Final dataset ---------------------------------------------------------
## A) For SRoutcome -------------------------------------------------------------------------------
SR_SRout_ccc <- SR_SRout_cc %>%
  filter(define %in% c("T1D Cpep v4",
                       "T2D Cpep v4 ",
                       "T2D no insulin v3",
                       "T2D no insulin v4"))
save(SR_SRout_ccc, file = "data/SR_SRout_ccc_20_3_2025.RData")
# table(SR_SRout_ccc$define)
# SR_dup <- SR_SRout_ccc %>%
#   group_by(Study_ID) %>%
#   mutate(id_count=n())
# dups <- SR_dup %>%
#   filter(id_count==2) %>%
#   select(Study_ID,
#          id_count)
