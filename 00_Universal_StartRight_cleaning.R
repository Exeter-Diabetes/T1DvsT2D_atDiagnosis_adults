########################################################################################
#STARTRIGHT CLEANING

#This script is to produce final, cleaned StartRight dataset for all to use
############################################################################################
#set working directory

#load libraries ------------------------------------------------------------------------
library(readxl)
library(writexl)
library(readr)
library(hms)
library(chron)
library(tidyverse)

#Read in visit data -----------------------------------------------------------------------------
SR_v1 <- read_xlsx("C:/Users/jk704/University of Exeter/StartRight/OneDrive_1_26-03-2024/StartRight Baseline Data (Visit 1)/1 - Visit 1 Dataset 02Feb2022 CLEANED Aug23.xlsx")
SR_v2 <- read_xlsx("C:/Users/jk704/University of Exeter/StartRight/OneDrive_1_26-03-2024/StartRight Visit 2 Data/2 - SR Visit 2 DCF Dataset 27july2022 CLEANED Aug23.xlsx")
SR_v3 <- read_xlsx("C:/Users/jk704/University of Exeter/StartRight/OneDrive_1_26-03-2024/StartRight Visit 3 Data/3 - V3 DCF & UCPCR Dataset 25Oct2022 CLEANED Aug23.xlsx")
SR_v4 <- read_xlsx("~/StartRight/2024/2024Sept24 StartRight Visit 4 Updated Data.xlsx")
#READ IN LIPIDS FOR V1
SR_lipids <- read_xlsx("data/SR Lipids Batch Analysis 2024 with Pt ID .xlsx")
#read in adiponectin data
adi <- read_xlsx("data/C25-134 CBAL 25.016 Sample Manifest - Exeter StartRight Plasma 27.11.25.xlsx")
#Read in additional data
#SRdka <- read_excel("data/data for J DKA excell.xls")
grs <- read_excel("data/data for J excell.xls")
SRt2dgrs <- read.csv("data/t2dgrs_sr_mvp_ta_kp_21oct2022.csv")
tcf7l2 <- read_excel("data/tcf7l2_updated.xlsx")
t2dgrs <- read_excel("data/t2dgrs_forangus_200924.xlsx")
T2DgrsZ <- read_csv("data/T2DgrsDosageScoresAndZscoresTopMedStartRight.csv")
part_T2D_scores <- read_excel("data/T2D_clusters_sr.xlsx")
GRS_standardised <- read.csv("data/SR_standardised_GRS_scores.csv")

#Prep to join --------------------------------------------------------------------
#Add suffixes
colnames(SR_v1) <- paste(colnames(SR_v1),"v1",sep="_")
colnames(SR_v2) <- paste(colnames(SR_v2),"v2",sep="_")
colnames(SR_v3) <- paste(colnames(SR_v3),"v3",sep="_")


#Make sure all in same format ------------------------------------------------------
#these are NB for the new visit 4
SR_v4$Date_Stored_Plasma_collected_v4 <- as.integer(SR_v4$Date_Stored_Plasma_collected_v4)
SR_v4$Date_Stored_Plasma_collected_v4 <- as.Date(SR_v4$Date_Stored_Plasma_collected_v4, origin = "1899-12-30")

SR_v4$Date_glucose_collected_v4 <- as.integer(SR_v4$Date_glucose_collected_v4)
SR_v4$Date_glucose_collected_v4 <- as.Date(SR_v4$Date_glucose_collected_v4, origin = "1899-12-30")

SR_v4$Date_Research_Serum_collected_v4 <- as.integer(SR_v4$Date_Research_Serum_collected_v4)
SR_v4$Date_Research_Serum_collected_v4 <- as.Date(SR_v4$Date_Research_Serum_collected_v4, origin = "1899-12-30")

SR_v4$Date_Research_Hba1c_collected_v4 <- as.integer(SR_v4$Date_Research_Hba1c_collected_v4)
SR_v4$Date_Research_Hba1c_collected_v4 <- as.Date(SR_v4$Date_Research_Hba1c_collected_v4, origin = "1899-12-30")

#Join
SRdata <- full_join(SR_v1, SR_v2, by = c("StartRight_Study_ID_v1" = "StartRight_Study_ID_v2"))
SRdata <- full_join(SRdata, SR_v3, by = c("StartRight_Study_ID_v1" = "StartRight_Study_ID_v3"))
SRdata <- full_join(SRdata, SR_v4, by = c("StartRight_Study_ID_v1" = "Study_ID"))

#identify duplicate IDs
SRdata %>% group_by(StartRight_Study_ID_v1) %>% mutate(id_count=n())%>% filter(id_count==2)

#duplicate entries on SR0284; SR375 & SR1262 (REMOVING SECOND ENTRY)
SRdata <- SRdata[c(-162, -225, -820),]
#only need to do the above once, from here onwards just load SRdata
save(SRdata, file = "SRdata.RData")

#Join in additional data ------------------------------------------------------------
T2DgrsZ$...1 <- NULL
GRS <- full_join(grs, T2DgrsZ, by = c("n_eid" = "IID"))
GRS <- full_join(GRS, part_T2D_scores, by = c("n_eid" = "shortid"))
GRS <- full_join(GRS, t2dgrs, by = c("n_eid" = "shortid"))


tcf7l2 <- tcf7l2 %>%
  filter(Cohort == "Start_Right")
GRS3 <- full_join(GRS, tcf7l2, by = c("n_eid" = "labno"))

GRS2 <- GRS %>%
  group_by(n_eid) %>%
  mutate(id_count=n())
dups <- GRS2 %>% filter(id_count==2)
write_xlsx(dups,"duplicated_T2DGRS.xlsx")
GRS1 <- GRS3 %>%
  distinct(n_eid, .keep_all = TRUE)
GRS %>%
  filter(StartRightID == "SR0852")

SR <- left_join(SRdata, SRt2dgrs[ , c("startrightid",
                                      "t2dgrsstdmvpta",
                                      "grst2dtamvpall_tp",
                                      "ancestry")],
                by = c("StartRight_Study_ID_v1" = "startrightid"))
SR <- left_join(SR, GRS1[ , c("StartRightID",
                              "t1d_grs",
                              "SCORE",
                              "T2DGRS",
                              "T2DGRSperAllele",
                              "T2DGRSzScore",
                              "chr10:112994329",
                              "chr10:112996282",
                              "chr10:112998590",
                              "chr10:113047288",
                              "chr10:113049143",
                              "Ancestry",
                              "rawt2dsuzuki24",
                              "t2d_liverlipid_hc",
                              "t2d_metabolicsyn_hc",
                              "t2d_obesity_hc",
                              "t2d_residualglycaemic_hc",
                              "t2d_betacell_neg_hc",
                              "t2d_betacell_pos_hc",
                              "t2d_bodyfat_hc",
                              "t2d_lipodys_hc",
                              "t2d_alpneg_sc",
                              "t2d_beta1_sc",
                              "t2d_beta2_sc",
                              "t2d_bilirubin_sc",
                              "t2d_chol_sc",
                              "t2d_hyperins_sc",
                              "t2d_lipo1_sc",
                              "t2d_lipo2_sc",
                              "t2d_liverlipid_sc",
                              "t2d_obesity_sc",
                              "t2d_proins_sc",
                              "t2d_shbglpa_sc")],
                by = c("StartRight_Study_ID_v1" = "StartRightID"))


#there is a duplication for SR0852, the second one needs to be removed
SR <- SR[-474,]
#SR <- left_join(SR, SRdka, by = c("StartRight_Study_ID_v1" = "StartRightID"))

SR <- left_join(SR, SR_lipids[ , c("Patient Code",
                                   "CHOL (mmol/L)",
                                   "HDL (mmol/L)")],
                by = c("StartRight_Study_ID_v1" = "Patient Code"))

SR_lipid_check <- SR_lipids %>%
  filter(`Patient Code` %in% c("SR0372", "SR0431", "SR0857", "SR1417", "SR1808", "SR2618"))
#write_xlsx(SR_lipid_check,"SR_lipid_check.xlsx")

SR %>%
  filter(StartRight_Study_ID_v1 == "SR0076") %>%
  select(StartRight_Study_ID_v1, Cholestrol_v1, HDL_v1)

SR <- SR %>%
  mutate(Cholestrol_mmol_L_v1 = ifelse(!is.na(Cholestrol_v1),
                                       Cholestrol_v1,
                                       ifelse(`CHOL (mmol/L)` == "Insufficient Volume"
                                              | `CHOL (mmol/L)` == "Haemolysed",
                                              NA,
                                              `CHOL (mmol/L)`)),
         HDL_mmol_L_v1 = ifelse(!is.na(HDL_v1),
                                HDL_v1,
                                ifelse(`HDL (mmol/L)` == "Insufficient Volume"
                                       | `HDL (mmol/L)` == "Haemolysed",
                                       NA,
                                       `HDL (mmol/L)`)))

SR$Cholestrol_mmol_L_v1 <- as.numeric(SR$Cholestrol_mmol_L_v1)
SR$HDL_mmol_L_v1 <- as.numeric(SR$HDL_mmol_L_v1)

SR_dup <- SR %>%
  group_by(StartRight_Study_ID_v1) %>%
  mutate(id_count=n())
dups <- SR_dup %>%
  filter(id_count==2) %>%
  select(StartRight_Study_ID_v1,
         id_count,
         `CHOL (mmol/L)`,
         Cholestrol_mmol_L_v1,
         `HDL (mmol/L)`,
         HDL_mmol_L_v1)

#These lines are from delayed samples and need to be removed
#For IDs SR0372, SR0431, SR1808, SR2618
dups <- dups[-c(2, 3, 10, 11), ]
#This one is mislabelled and actually belongs to SR0076, which has been added
#and merged in
dups <- dups[-5, ]

#The remaining one SR0857 has two values (QC) can be averaged
dups_chol <- aggregate(dups$Cholestrol_mmol_L_v1,
                    by=list(StartRight_Study_ID_v1=dups$StartRight_Study_ID_v1
                            ),
                    data=dups,
                    FUN=mean) %>%
  rename(chol = x)
dups_hdl <- aggregate(dups$HDL_mmol_L_v1,
                       by=list(StartRight_Study_ID_v1=dups$StartRight_Study_ID_v1
                       ),
                       data=dups,
                       FUN=mean) %>%
  rename(hdl = x)

dups <- left_join(dups_chol, dups_hdl)
SR <- left_join(SR, dups)

SR <- SR %>%
mutate(`CHOL (mmol/L)` = ifelse(StartRight_Study_ID_v1 %in% SR_lipid_check$`Patient Code`,
                                chol,
                                `CHOL (mmol/L)`),
       `HDL (mmol/L)` = ifelse(StartRight_Study_ID_v1 %in% SR_lipid_check$`Patient Code`,
                               hdl,
                               `HDL (mmol/L)`),
       Cholestrol_mmol_L_v1 = ifelse(StartRight_Study_ID_v1 %in% SR_lipid_check$`Patient Code`,
                                chol,
                                Cholestrol_mmol_L_v1),
       HDL_mmol_L_v1 = ifelse(StartRight_Study_ID_v1 %in% SR_lipid_check$`Patient Code`,
                               hdl,
                              HDL_mmol_L_v1),
       )

SR %>%
  filter(StartRight_Study_ID_v1 %in% c("SR0076","SR0372", "SR0431", "SR0857", "SR1417", "SR1808", "SR2618")) %>%
  select(StartRight_Study_ID_v1, `CHOL (mmol/L)`,Cholestrol_mmol_L_v1, `HDL (mmol/L)`, HDL_mmol_L_v1)
SR <- SR %>%
  distinct(StartRight_Study_ID_v1, .keep_all = TRUE) %>%
  select(-hdl, -chol)
#
# aggregate(SR$`CHOL (mmol/L)`,
#           by=list(StartRight_Study_ID_v1=SR$StartRight_Study_ID_v1,
#                   etc1=SR$etc1,
#                   etc2=SR$etc2),
#           data=SR,
#           FUN=mean)

SR <- left_join(SR, GRS_standardised,
                by = c("StartRight_Study_ID_v1" = "Study_ID"))

SR %>%
  group_by(StartRight_Study_ID_v1) %>%
  mutate(id_count=n()) %>%
  filter(id_count==2) %>%
  select(StartRight_Study_ID_v1,
         id_count,
         T2DGRS_suzuki24_z,
         T1DGRS2_z
  )
SR <- SR %>%
  distinct(StartRight_Study_ID_v1, .keep_all = TRUE)

#Add in adipoenectin
adi <- adi %>%
  mutate(`Adiponectin_ug/mL` = ifelse(`Adiponectin_ug/mL` == "<0.3", "0.29", `Adiponectin_ug/mL`))
adi$`Adiponectin_ug/mL` <- as.numeric(adi$`Adiponectin_ug/mL`)
adi_dups <- adi %>%
  group_by(`Participant Code`) %>%
  mutate(id_count=n()) %>%
  filter(id_count==2) %>%
  select(`Run ID`,`Sample no.`, `Participant Code`, `Scanned 2D Tube Code`, `Adiponectin_ug/mL`, AnalysisDate)



adi_dups_mean <- aggregate(adi_dups$`Adiponectin_ug/mL`,
                       by=list(`Participant Code`=adi_dups$`Participant Code`),
                       data=adi_dups,
                       FUN=mean) %>%
  rename(adi = x)

adi <- adi %>%
  left_join(adi_dups_mean) %>%
  mutate(adiponectin_ug_mL_v1 = ifelse(`Participant Code` %in% adi_dups$`Participant Code`, adi, `Adiponectin_ug/mL`)) %>%
  rename(
    adiponectin_analysis_date = AnalysisDate
  )
adi %>%
  group_by(`Participant Code`) %>%
  mutate(id_count=n()) %>%
  filter(id_count==2) %>%
  select(`Run ID`,`Sample no.`, `Participant Code`, `Scanned 2D Tube Code`, `Adiponectin_ug/mL`,adiponectin_ug_mL_v1, adiponectin_analysis_date)
SR <- left_join(SR, adi[ , c("Participant Code",
                             "adiponectin_ug_mL_v1",
                             "adiponectin_analysis_date")],
                by = c("StartRight_Study_ID_v1" = "Participant Code"))
SR %>%
  filter(!(StartRight_Study_ID_v1 %in% SR$StartRight_Study_ID_v1)) %>%
  select(StartRight_Study_ID_v1, adiponectin_ug_mL_v1)

SR %>%
  group_by(StartRight_Study_ID_v1) %>%
  mutate(id_count=n()) %>%
  filter(id_count==2) %>%
  #filter(!(StartRight_Study_ID_v1 %in% SR$StartRight_Study_ID_v1)) %>%
  select(StartRight_Study_ID_v1, adiponectin_ug_mL_v1)

SR <- SR %>%
  distinct(StartRight_Study_ID_v1, .keep_all = TRUE)
#Save file
save(SR, file = "SR_15_1_25.RData")
write_xlsx(SR,"SR_15_1_25.xlsx")

#Rename variables ---------------------------------------------------------------------
SR <- SR %>%
  rename(
    Study_ID = StartRight_Study_ID_v1,
    recruitment_site_v1 = `Recruitment Site_v1`,
    full_or_pilot_v1 = `Full or Pilot_v1`,
    #date_of_birth_v1 = `Date of Birth_v1`,
    ethnic_origin = `Ethnic Origin_v1`,
    date_of_diagnosis_v1 = `Date of Diagnosis_v1`,
    AgeatDiagnosis = `Age at Diagnosis_v1`,
    clinical_diagnosis_v1 = Type_of_diabetes_v1,
    bmi_calc_v1 = `BMI Calc_v1`,
    time_of_last_snack_v1 = `Time-of-last snack_v1`,
    recruitment_visit_DCF_comments_v1 = `1 - Recruitment Visit DCF_Comments_v1`,
    exeter_hba1c_v1 = `Exeter HbA1c_v1`,
    c_peptide_v1 = `C-Peptide_v1`,
    urine_creatinine_v1 = `Urine Creatinine_v1`,
    urine_cpeptide_v1 = `Urine C-Peptide_v1`,
    local_hba1c_v1 = `Local HbA1c_v1`,
    white_blood_cell_count_v1 = `White Blood Cell Count_v1`,
    home_urine_date_collected_v1 = `Home Urine Date Collected_v1`,
    home_urine_sample_received_v1 = `Home Urine Sample Received_v1`,
    home_urine_days_in_transit_v1 = `Home Urine Days In Transit_v1`,
    urine_creatinine_v2 = `V2 Urine Creatinine_v2`,
    urine_cpeptide_v2 = `V2 Urine C-Peptide_v2`,
    UCPCR_v2 = `V2 UCPCR_v2`,
    #urine_days_in_transit_v2 = `V2 Urine Days in Transit_v2`,
    urine_creatinine_v3 = `V3 Urine Creatinine_v3`,
    urine_cpeptide_v3 = `V3 Urine C-Peptide_v3`,
    UCPCR_v3 = `V3 UCPCR_v3`,
    #days_urine_in_transit_v3 = `Days Urine In Transit_v3`,
    #DKA = studyDKA,
    T1DGRS1 = t1d_grs,
    T1DGRS2 = SCORE,
    rs7901695 = `chr10:112994329`,
    rs4506565 = `chr10:112996282`,
    rs7903146 = `chr10:112998590`,
    rs11196205 = `chr10:113047288`,
    rs12255372 = `chr10:113049143`,
    T2DGRS_suzuki24 = rawt2dsuzuki24,
    T2DGRSstd_vujkovic20 = t2dgrsstdmvpta,
    T2DGRS_vujkovic20 = grst2dtamvpall_tp,
    T2DGRS_mahajan18 = T2DGRS,
    T2DGRSzscore_mahajan18 = T2DGRSzScore,
    T2DGRSperallele_mahajan18 = T2DGRSperAllele
  )

#Fix consistency issues/spell check etc -------------------------------------------------
SR <- SR %>%
  mutate(recruitment_site_v1 = ifelse(recruitment_site_v1 == "CHES",
                                      "Chester",
                                      ifelse(recruitment_site_v1 == "EXET",
                                             "Exeter",
                                             ifelse(recruitment_site_v1 == "IPSW",
                                                    "Ipswich",
                                                    ifelse(recruitment_site_v1 == "PLYM",
                                                           "Plymouth",
                                                           ifelse(recruitment_site_v1 == "SHEF",
                                                                  "Sheffield",
                                                                  recruitment_site_v1))))))
SR <- SR %>%
  mutate(clinical_diagnosis_v1 = ifelse(clinical_diagnosis_v1 == "Type2",
                                        "Type 2",
                                        clinical_diagnosis_v1))
SR <- SR %>%
  mutate(SU_Type_v1 = ifelse(SU_Type_v1 == "GIICLAZIDE" | SU_Type_v1 == "GLICAZIDE" |SU_Type_v1 == "GLICLAZIDE MODIFIED RELEASE" |SU_Type_v1 == "GLICLAZIDE." |SU_Type_v1 == "GLICLAZIDE. Z ,CLE" |SU_Type_v1 == "GLICLIZIDE" |SU_Type_v1 == "LICLAZIDE",
                             "GLICLAZIDE",
                             SU_Type_v1))
SR <- SR %>%
  mutate(Glitazone_v1 = ifelse(Glitazone_v1 == "NO",
                               "No",
                               Glitazone_v1))
SR <- SR %>%
  mutate(DPP4_Type_v1 = ifelse(DPP4_Type_v1 == "ALOALPTIN" | DPP4_Type_v1 == "ALOGLIPTIN ." | DPP4_Type_v1 == "ALOGLIPTIN1 NH N" | DPP4_Type_v1 == "ALOGLIPTIN52--",
                               "ALOGLIPTIN",
                               ifelse(DPP4_Type_v1 == "LINAFLIPTIN" | DPP4_Type_v1 == "LINGALIPTIN" ,
                                      "LINAGLIPTIN",
                                      DPP4_Type_v1)))
SR <- SR %>%
  mutate(SGLT2_Type_v1 = ifelse(SGLT2_Type_v1 == "DAPAGLIFOZIN" | SGLT2_Type_v1 == "DAPAGUFLOZN" | SGLT2_Type_v1 == "DAPGLIFLOZIN",
                                "DAPAGLIFLOZIN",
                                ifelse(SGLT2_Type_v1 == "EMPAGLIHOZIN",
                                       "EMPAGLIFLOZIN",
                                       SGLT2_Type_v1)))

SR <- SR %>%
  mutate(Diagnosed_Ketoacidosis_v1 = ifelse(Diagnosed_Ketoacidosis_v1 == "N/K",
                                            "NK",
                                            Diagnosed_Ketoacidosis_v1))
SR <- SR %>%
  mutate(Acanthosis_Nigricans_v1 = ifelse(Acanthosis_Nigricans_v1 == "?",
                                          "No",
                                          Acanthosis_Nigricans_v1))
#hypertension
#smoker
#Ex-Smoker -> Ex-smoker
SR <- SR %>%
  mutate(Smoker_v1 = ifelse(Smoker_v1 == "Ex-Smoker",
                            "Ex-smoker",
                            Smoker_v1))
SR <- SR %>%
  mutate(Fasting_Hba1c_diag_v1 = ifelse(Fasting_Hba1c_diag_v1 == "YES",
                                        "Yes",
                                        Fasting_Hba1c_diag_v1))
SR <- SR %>%
  mutate(Fasting_glucose_diag_v1 = ifelse(Fasting_glucose_diag_v1 == "YES",
                                          "Yes",
                                          Fasting_glucose_diag_v1))
SR <- SR %>%
  mutate(Fasting_triglycerides_diag_v1 = ifelse(Fasting_triglycerides_diag_v1 == "K/K",
                                                "N/K",
                                                Fasting_triglycerides_diag_v1))
SR <- SR %>%
  mutate(pH_less_than_7_3_at_diag_v1 = ifelse(pH_less_than_7_3_at_diag_v1 == "Not Tested",
                                              "Not tested",
                                              pH_less_than_7_3_at_diag_v1))
SR <- SR %>%
  mutate(Bld_ketones_not_tested_at_diag_v1 = ifelse(Bld_ketones_not_tested_at_diag_v1 == "Not tested",
                                                    "Not Tested",
                                                    Bld_ketones_not_tested_at_diag_v1))
#father ins treated
#Nk -> NK
SR <- SR %>%
  mutate(Father_insulin_treated_v1 = ifelse(Father_insulin_treated_v1 == "Nk",
                                            "NK",
                                            Father_insulin_treated_v1))
SR <- SR %>%
  mutate(Pregnant_v2 = ifelse(Pregnant_v2 == "Don't Know",
                              "Don’t Know",
                              Pregnant_v2))
SR <- SR %>%
  mutate(SU_v2 = ifelse(SU_v2 == "YES",
                        "Yes",
                        SU_v2))
SR <- SR %>%
  mutate(SU_Type_v2 = ifelse(SU_Type_v2 == "GLICAZIDE" | SU_Type_v2 == "GLICIAZIDE" |SU_Type_v2 == "Gliclazide" |SU_Type_v2 == "GLICLAZIDE (ZICRON)" |SU_Type_v2 == "GLICLAZIDE MODIFIED RELEASE" |SU_Type_v2 == "GLIDAZIDE",
                             "GLICLAZIDE",
                             SU_Type_v2))
SR <- SR %>%
  mutate(Glitazone_v2 = ifelse(Glitazone_v2 == "NO",
                               "No",
                               Glitazone_v2))
SR <- SR %>%
  mutate(DPP4i_v2 = ifelse(DPP4i_v2 == "NO",
                           "No",
                           DPP4i_v2))
SR <- SR %>%
  mutate(DPP4_Type_v2 = ifelse(DPP4_Type_v2 == "ALGOLIPTIN" | DPP4_Type_v2 == "ALOGIPTIN" | DPP4_Type_v2 == "ALOGLPTIN" | DPP4_Type_v2 == "ALOGLYPTIN",
                               "ALOGLIPTIN",
                               ifelse(DPP4_Type_v2 == "LINALIPTIN" ,
                                      "LINAGLIPTIN",
                                      ifelse(DPP4_Type_v2 == "SAXAGLIPTIN (ONGLYZA)",
                                             "SAXAGLIPTIN",
                                             ifelse(DPP4_Type_v2 == "Sitagliptin (Januvia)",
                                                    "SITAGLIPTIN",
                                                    DPP4_Type_v2)))))
SR <- SR %>%
  mutate(GLP1_v2 = ifelse(GLP1_v2 == "NO",
                          "No",
                          ifelse(GLP1_v2 == "YES",
                                 "Yes",
                                 GLP1_v2)))
SR <- SR %>%
  mutate(SGLT2_v2 = ifelse(SGLT2_v2 == "YES",
                           "Yes",
                           SGLT2_v2))
SR <- SR %>%
  mutate(Other_non_insulin_v2 = ifelse(Other_non_insulin_v2 == "NO",
                                       "No",
                                       Other_non_insulin_v2))
SR <- SR %>%
  mutate(Admission_Reason_1_v2 = ifelse(Admission_Reason_1_v2 == "Ketacidosis",
                                        "Ketoacidosis",
                                        Admission_Reason_1_v2))
SR <- SR %>%
  mutate(Admission_Reason_2_v2 = ifelse(Admission_Reason_2_v2 == "Ketacidosis",
                                        "Ketoacidosis",
                                        Admission_Reason_2_v2))
SR <- SR %>%
  mutate(Admission_Reason_3_v2 = ifelse(Admission_Reason_3_v2 == "Ketacidosis",
                                        "Ketoacidosis",
                                        Admission_Reason_3_v2))
SR <- SR %>%
  mutate(Participant_Continuation_v3 = ifelse(Participant_Continuation_v3 == "YES",
                                              "Yes",
                                              Participant_Continuation_v3))
SR <- SR %>%
  mutate(GP_Nurse_v3 = ifelse(GP_Nurse_v3 == "YES",
                              "Yes",
                              GP_Nurse_v3))
SR <- SR %>%
  mutate(GP_Doctor_v3 = ifelse(GP_Doctor_v3 == "YES",
                               "Yes",
                               GP_Doctor_v3))
SR <- SR %>%
  mutate(Hospital_DSD_v3 = ifelse(Hospital_DSD_v3 == "NO",
                                  "No",
                                  Hospital_DSD_v3))
SR <- SR %>%
  mutate(Hospital_DSN_v3 = ifelse(Hospital_DSN_v3 == "NO",
                                  "No",
                                  Hospital_DSN_v3))
SR <- SR %>%
  mutate(Community_CDC_v3 = ifelse(Community_CDC_v3 == "NO",
                                   "No",
                                   Community_CDC_v3))
SR <- SR %>%
  mutate(Community_CDN_v3 = ifelse(Community_CDN_v3 == "NO",
                                   "No",
                                   Community_CDN_v3))
SR <- SR %>%
  mutate(Monitor_Blood_Glucose_v3 = ifelse(Monitor_Blood_Glucose_v3 == "YES",
                                           "Yes",
                                           Monitor_Blood_Glucose_v3))
SR <- SR %>%
  mutate(Pregnant_v3 = ifelse(Pregnant_v3 == "NO",
                              "No",
                              Pregnant_v3))
SR <- SR %>%
  mutate(DIET_only_v3 = ifelse(DIET_only_v3 == "NO",
                               "No",
                               DIET_only_v3))
SR <- SR %>%
  mutate(MFN_v3 = ifelse(MFN_v3 == "YES",
                         "Yes",
                         MFN_v3))
SR <- SR %>%
  mutate(MFN_Type_v3 = ifelse(MFN_Type_v3 == "STANDARD",
                              "Standard",
                              MFN_Type_v3))
SR <- SR %>%
  mutate(SU_v3 = ifelse(SU_v3 == "NO",
                        "No",
                        SU_v3))
SR <- SR %>%
  mutate(SU_Type_v3 = ifelse(SU_Type_v3 == "G;ICLAZIDE" | SU_Type_v3 == "Gliclazide" |SU_Type_v3 == "GLICLAZIDE." |SU_Type_v3 == "GLICLAZINE",
                             "GLICLAZIDE",
                             ifelse(SU_Type_v3 == "GLIMEPINDE",
                                    "GLIMEPIRIDE",
                                    SU_Type_v3)))
SR <- SR %>%
  mutate(Glitazone_v3 = ifelse(Glitazone_v3 == "NO",
                               "No",
                               Glitazone_v3))
SR <- SR %>%
  mutate(DPP4i_v3 = ifelse(DPP4i_v3 == "NO",
                           "No",
                           DPP4i_v3))
SR <- SR %>%
  mutate(DPP4_Type_v3 = ifelse(DPP4_Type_v3 == "ALAGLIPTIN" | DPP4_Type_v3 == "Alogliptin" | DPP4_Type_v3 == "ANAGLIPTIN" | DPP4_Type_v3 == "VIPIDIA (ALOGLIPTIN)",
                               "ALOGLIPTIN",
                               ifelse(DPP4_Type_v3 == "Linagliptin" ,
                                      "LINAGLIPTIN",
                                      ifelse(DPP4_Type_v3 == "SAXOGLIPTIN",
                                             "SAXAGLIPTIN",
                                             DPP4_Type_v3))))
SR <- SR %>%
  mutate(GLP1_v3 = ifelse(GLP1_v3 == "NO",
                          "No",
                          GLP1_v3))
SR <- SR %>%
  mutate(SGLT2_v3 = ifelse(SGLT2_v3 == "NO",
                           "No",
                           SGLT2_v3))
SR <- SR %>%
  mutate(Other_non_insulin_v3 = ifelse(Other_non_insulin_v3 == "NO",
                                       "No",
                                       Other_non_insulin_v3))
SR <- SR %>%
  mutate(Insulin_v3 = ifelse(Insulin_v3 == "NO",
                             "No",
                             Insulin_v3))
SR <- SR %>%
  mutate(Missed_Doses_v3 = ifelse(Missed_Doses_v3 == "YES",
                                  "Yes",
                                  Missed_Doses_v3))
SR <- SR %>%
  mutate(Admitted_to_Hosp_v3 = ifelse(Admitted_to_Hosp_v3 == "NO",
                                      "No",
                                      Admitted_to_Hosp_v3))
SR <- SR %>%
  mutate(Other_Medical_Conditions_v3 = ifelse(Other_Medical_Conditions_v3 == "NO",
                                              "No",
                                              Other_Medical_Conditions_v3))
SR <- SR %>%
  mutate(Changes_to_Non_DM_Meds_v3 = ifelse(Changes_to_Non_DM_Meds_v3 == "NO",
                                            "No",
                                            Changes_to_Non_DM_Meds_v3))
SR <- SR %>%
  mutate(Other_Studies_v3 = ifelse(Other_Studies_v3 == "NO",
                                   "No",
                                   ifelse(Other_Studies_v3 == "yes",
                                          "Yes",
                                          Other_Studies_v3)))
SR <- SR %>%
  mutate(Home_Urine_Collection_v3 = ifelse(Home_Urine_Collection_v3 == "YES",
                                           "Yes",
                                           Home_Urine_Collection_v3))
SR <- SR %>%
  mutate(Hypo_Questionnaire_v3 = ifelse(Hypo_Questionnaire_v3 == "YES",
                                        "Yes",
                                        Hypo_Questionnaire_v3))

##from visit 4
#Insulin_v4 (no _> No)
SR <- SR %>%
  mutate(Insulin_v4 = ifelse(Insulin_v4 == "no",
                             "No",
                             Insulin_v4))


#Fix dates and times ---------------------------------------------------------------------
##Dates
###Mixed dates
#DBS_date_received_v4


#DBS_date_collected_v4
#Date_urine_v4
#Date_neoteryx_v4
SR <- SR %>%
  mutate(Date_neoteryx_v4 = if_else(str_detect(Date_neoteryx_v4, "UTC"),
                                    as.Date(str_replace(Date_neoteryx_v4, "00:00:00 UTC", "")),
                                    as.Date(Date_neoteryx_v4, "%d.%m.%y")),
         Date_urine_v4 = if_else(str_detect(Date_urine_v4, "UTC"),
                                 as.Date(str_replace(Date_urine_v4, "00:00:00 UTC", "")),
                                 as.Date(Date_urine_v4, "%d.%m.%y")),
         DBS_date_collected_v4 = if_else(str_detect(DBS_date_collected_v4, "UTC"),
                                         as.Date(str_replace(DBS_date_collected_v4, "00:00:00 UTC", "")),
                                         as.Date(DBS_date_collected_v4, "%d/%m/%Y")),
         DBS_date_received_v4 = if_else(str_detect(DBS_date_received_v4, "UTC"),
                                         as.Date(str_replace(DBS_date_received_v4, "00:00:00 UTC", "")),
                                         as.Date(DBS_date_received_v4, "%d/%m/%Y")))

##Times
###Decimals
#Time_of_last_snack_and_drink_v4
#Time_of_last_meal_v4
SR <- SR %>%
  mutate(Time_of_last_meal_v4 = hms::hms(days =Time_of_last_meal_v4),
         Time_of_last_snack_and_drink_v4 = hms::hms(days = as.numeric(Time_of_last_snack_and_drink_v4)))
SR <- SR %>%
  mutate_at("Time_of_last_meal_v4", str_replace, ".000000", "")
SR <- SR %>%
  mutate_at("Time_of_last_snack_and_drink_v4", str_replace, ".000000", "")
SR <- SR %>%
  mutate(Time_of_last_meal_v4 = as.POSIXct(paste0("1899-12-31 ",Time_of_last_meal_v4), format = "%Y-%m-%d %H:%M:%S"),
         Time_of_last_snack_and_drink_v4 = as.POSIXct(paste0("1899-12-31 ",Time_of_last_snack_and_drink_v4), format = "%Y-%m-%d %H:%M:%S"))

###mixed types
#Capillary_glucose_time_v4
SR <- SR %>%
  mutate(Capillary_glucose_time_v4 = ifelse(str_detect(Capillary_glucose_time_v4, "UTC"),
                             Capillary_glucose_time_v4,
                             format(times(as.numeric(Capillary_glucose_time_v4)))))

SR <- SR %>%
  mutate(Capillary_glucose_time_v4 = ifelse(str_detect(Capillary_glucose_time_v4, "UTC"),
                                            str_replace(Capillary_glucose_time_v4, "1900-01-00", "1899-12-31"),
                                            Capillary_glucose_time_v4))

SR <- SR %>%
  mutate(Capillary_glucose_time_v4 = ifelse(str_detect(Capillary_glucose_time_v4, "UTC"),
                                            str_replace(Capillary_glucose_time_v4, "UTC", ""),
                                            paste0("1899-12-31 ",Capillary_glucose_time_v4)))
#Time_urine_v4
SR <- SR %>%
  mutate(Time_urine_v4 = ifelse(str_detect(Time_urine_v4, "UTC"),
                                Time_urine_v4,
                                format(times(as.numeric(Time_urine_v4)))))

SR <- SR %>%
  mutate(Time_urine_v4 = ifelse(str_detect(Time_urine_v4, "UTC"),
                                str_replace(Time_urine_v4, "1900-01-00", "1899-12-31"),
                                Time_urine_v4))

SR <- SR %>%
  mutate(Time_urine_v4 = ifelse(str_detect(Time_urine_v4, "UTC"),
                                str_replace(Time_urine_v4, "UTC", ""),
                                paste0("1899-12-31 ",Time_urine_v4)))
#Time_neoteryx_v4
SR <- SR %>%
  mutate(Time_neoteryx_v4 = ifelse(str_detect(Time_neoteryx_v4, "UTC"),
                                   Time_neoteryx_v4,
                                format(times(as.numeric(Time_neoteryx_v4)))))

SR <- SR %>%
  mutate(Time_neoteryx_v4 = ifelse(str_detect(Time_neoteryx_v4, "UTC"),
                                str_replace(Time_neoteryx_v4, "1900-01-00", "1899-12-31"),
                                Time_neoteryx_v4))

SR <- SR %>%
  mutate(Time_neoteryx_v4 = ifelse(str_detect(Time_neoteryx_v4, "UTC"),
                                str_replace(Time_neoteryx_v4, "UTC", ""),
                                paste0("1899-12-31 ",Time_neoteryx_v4)))
SR$Time_neoteryx_v4
#Make individual date corrections

#Check missing key variables -----------------------------------------------------------
missingingsex <- SR %>%
  filter(is.na(Gender_v1)) %>%
  select(Study_ID, AgeatDiagnosis, bmi_calc_v1, Gender, Gender_v1)
#Make additional variables ------------------------------------------------------
#create variables needed for outcome definition
SR <- SR %>%
  mutate(dur_diab_wks_v1 = as.numeric(difftime(Date_Visit_v1, date_of_diagnosis_v1, units = "weeks")),
         dur_diab_wks_v2 = as.numeric(difftime(Date_Contacted_v2, date_of_diagnosis_v1,units = "weeks")),
         dur_diab_wks_v3 = as.numeric(difftime(Date_Contacted_v3, date_of_diagnosis_v1, units = "weeks")),
         dur_diab_wks_v4 = as.numeric(difftime(Visit_Date_v4, date_of_diagnosis_v1, units = "weeks")),
         dur_diab_sample_wks_v4 = ifelse(is.na(DBS_date_received_v4),
                                         as.numeric(difftime(V4DateUrineCollect_v4, date_of_diagnosis_v1, units = "weeks")),
                                         as.numeric(difftime(DBS_date_received_v4, date_of_diagnosis_v1, units = "weeks"))),
         dur_diab_sample_wks_v3 = ifelse(is.na(Date_form_completed_v3),
                                         as.numeric(difftime(V3DateUrineReceive_v3, date_of_diagnosis_v1, units = "weeks")),
                                         as.numeric(difftime(Date_form_completed_v3, date_of_diagnosis_v1, units = "weeks")))
  )



SR <- SR %>%
  mutate(dur_diab_yr_v1 = dur_diab_wks_v1/52.14,
         dur_diab_yr_v2 = dur_diab_wks_v2/52.14,
         dur_diab_yr_v3 = dur_diab_wks_v3/52.14,
         dur_diab_yr_v4 = dur_diab_wks_v4/52.14,
         dur_diab_sample_yr_v4 = dur_diab_sample_wks_v4/52.14,
         dur_diab_sample_yr_v3 = dur_diab_sample_wks_v3/52.14)

SR <- SR %>%
  mutate(dur_date_form_v4 = (as.numeric(difftime(Date_form_completed_v4, date_of_diagnosis_v1, units = "weeks")))/52.14,
         dur_visit_date_v4 = (as.numeric(difftime(Visit_Date_v4, date_of_diagnosis_v1, units = "weeks")))/52.14,
         dur_date_urine_collect_v4 = (as.numeric(difftime(V4DateUrineCollect_v4, date_of_diagnosis_v1, units = "weeks")))/52.14,
         dur_date_research_serum_v4 = (as.numeric(difftime(Date_Research_Serum_collected_v4, date_of_diagnosis_v1, units = "weeks")))/52.14,
         dur_date_stored_plasma_v4 = (as.numeric(difftime(Date_Stored_Plasma_collected_v4, date_of_diagnosis_v1, units = "weeks")))/52.14,
         dur_dbs_date_collected_v4 = (as.numeric(difftime(DBS_date_collected_v4, date_of_diagnosis_v1, units = "weeks")))/52.14,
         dur_dbs_date_received_v4 = (as.numeric(difftime(DBS_date_received_v4, date_of_diagnosis_v1, units = "weeks")))/52.14
  )



SR <- SR %>%
  mutate(longest_date_name = ifelse(!is.na(dur_date_form_v4) & dur_date_form_v4 >= 2.86,
                                    "Date_form_completed_v4",
                                    ifelse(!is.na(dur_visit_date_v4) & dur_visit_date_v4 >= 2.86,
                                           "Visit_date_v4",
                                           ifelse(!is.na(dur_date_urine_collect_v4) & dur_date_urine_collect_v4 >= 2.86,
                                                  "V4DateUrineCollect_v4",
                                                  ifelse(!is.na(dur_date_research_serum_v4) & dur_date_research_serum_v4 >= 2.86,
                                                         "Date_Research_Serum_collected_v4",
                                                         ifelse(!is.na(dur_date_stored_plasma_v4) & dur_date_stored_plasma_v4 >= 2.86,
                                                                "Date_Stored_Plasma_collected_v4",
                                                                ifelse(!is.na(dur_dbs_date_collected_v4) & dur_dbs_date_collected_v4 >= 2.86,
                                                                       "DBS_date_collected_v4",
                                                                       ifelse(!is.na(dur_dbs_date_received_v4) & dur_dbs_date_received_v4 >= 2.86,
                                                                              "DBS_date_received_v4",
                                                                              "V4 date/duration missing"))))))))

SR <- SR %>%
  mutate(dur_diab_joined_v4 = ifelse(!is.na(dur_date_form_v4) & dur_date_form_v4 >= 2.86,
                                     dur_date_form_v4,
                                     ifelse(!is.na(dur_visit_date_v4) & dur_visit_date_v4 >= 2.86,
                                            dur_visit_date_v4,
                                            ifelse(!is.na(dur_date_urine_collect_v4) & dur_date_urine_collect_v4 >= 2.86,
                                                   dur_date_urine_collect_v4,
                                                   ifelse(!is.na(dur_date_research_serum_v4) & dur_date_research_serum_v4 >= 2.86,
                                                          dur_date_research_serum_v4,
                                                          ifelse(!is.na(dur_date_stored_plasma_v4) & dur_date_stored_plasma_v4 >= 2.86,
                                                                 dur_date_stored_plasma_v4,
                                                                 ifelse(!is.na(dur_dbs_date_collected_v4) & dur_dbs_date_collected_v4 >= 2.86,
                                                                        dur_dbs_date_collected_v4,
                                                                        ifelse(!is.na(dur_dbs_date_received_v4) & dur_dbs_date_received_v4 >= 2.86,
                                                                               dur_dbs_date_received_v4,
                                                                               NA))))))))



table(SR$longest_date_name)

#antibodies
#checked
SR <- SR %>%
  mutate(
    GAD_bin_v1 = ifelse(GAD_v1 == "negative", "negative", "positive"),
    IA2_bin_v1 = ifelse(IA2_v1 == "negative", "negative", "positive"),
    ZNT8_bin_v1 = ifelse(ZNT8_v1 == "negative", "negative", "positive")
  )

SR$GAD_bin_v1 <- ifelse(SR$GAD_bin_v1 == "positive", "1", "0")
SR$GAD_bin_v1 <- as.numeric(SR$GAD_bin_v1)
SR$IA2_bin_v1 <- ifelse(SR$IA2_bin_v1 == "positive", "1", "0")
SR$IA2_bin_v1 <- as.numeric(SR$IA2_bin_v1)
SR$ZNT8_bin_v1 <- ifelse(SR$ZNT8_bin_v1 == "positive", "1", "0")
SR$ZNT8_bin_v1 <- as.numeric(SR$ZNT8_bin_v1)

SR <- SR %>%
  mutate(AntibodyStatus = case_when(
    GAD_bin_v1 == "1" & IA2_bin_v1 == "1" ~ "AntiStatus3",
    GAD_bin_v1 == "1" & IA2_bin_v1 == "0" ~ "AntiStatus1",
    GAD_bin_v1 == "0" & IA2_bin_v1 == "1" ~ "AntiStatus2",
    GAD_bin_v1 == "0" & IA2_bin_v1 == "0" ~ "AntiStatus0"
  ))

SR <- SR %>%
  mutate(num_anti = ifelse(!is.na(ZNT8_bin_v1), GAD_bin_v1 + IA2_bin_v1 + ZNT8_bin_v1, GAD_bin_v1 + IA2_bin_v1))

#WAIST-HIP RATIO
SR <- SR %>%
  mutate(wh_ratio_v1 = Waist_v1/Hips_v1)

#ethnicity
SR <- SR %>%
  mutate(
    ethnicity = ifelse(ethnic_origin == "White British" | ethnic_origin == "White Irish" | ethnic_origin == "Other White" | ethnic_origin == "Not Stated", "White", "Non-white"))
#family history
#check
table(SR$Mother_diabetes_v1)
table(SR$Father_diabetes_v1)

SR <- SR %>%
  mutate(
    famhisdiab = ifelse(Mother_diabetes_v1 == "Yes" | Father_diabetes_v1 == "Yes",
                        "Yes",
                        "No"),
    famhisinsdiab = ifelse((Mother_diabetes_v1 == "Yes" &
                              Mother_insulin_treated_v1 == "Yes") |
                             (Father_diabetes_v1 == "Yes" &
                                Father_insulin_treated_v1 == "Yes"),
                           "Yes",
                           "No"),
    famhisnoninsdiab = ifelse((Mother_diabetes_v1 == "Yes" &
                                 Mother_insulin_treated_v1 == "No") |
                                (Father_diabetes_v1 == "Yes" &
                                   Father_insulin_treated_v1 == "No"),
                              "Yes",
                              "No")
  )

SR <- SR %>%
  mutate(
    obese = ifelse(bmi_calc_v1 >= 30, 1, 0),
    c_obese = ifelse(Gender == "Female",
                     ifelse(Waist_v1 >= 88, "Yes", "No"),
                     ifelse(Waist_v1 >= 102, "Yes", "No"))

  )
# family history of autoimmune
SR <- SR %>%
  mutate(
    famhisauto = ifelse(Mother_Coeliac_v4 == "Yes" |
                          Mother_Viteligo_v4 == "Yes"|
                          Mother_Addisons_disease_v4 == "Yes" |
                          Mother_Overactive_Thyroid_v4 == "Yes" |
                          Mother_Underactive_Thyroid_v4 == "Yes" |
                          Mother_Pernicious_Anaemia_v4 == "Yes" |
                          Mother_Rheumatoid_Arthritis_v4 == "Yes" |
                          Father_Coeliac_v4 == "Yes" |
                          Father_Viteligo_v4 == "Yes"|
                          Father_Addisons_Disease_v4 == "Yes" |
                          Father_Overactive_Thyroid_v4 == "Yes" |
                          Father_Underactive_Thyroid_v4 == "Yes" |
                          Father_Pernicious_Anaemia_v4 == "Yes" |
                          Father_Rheumatoid_Arthritis_v4 == "Yes",
                        "1",
                        "0" ))

#autoimmune
SR <- SR %>%
  mutate(
    autoimmune = ifelse(
      (!is.na(Coeliac_v1) & Coeliac_v1 == "Yes") |
        (!is.na(Viteligo_v1) & Viteligo_v1 == "Yes") |
        (!is.na(Addisons_disease_v1) & Addisons_disease_v1 == "Yes") |
        (!is.na(Overactive_Thyroid_v1) & Overactive_Thyroid_v1 == "Yes") |
        (!is.na(Underactive_Thyroid_v1) & Underactive_Thyroid_v1 == "Yes") |
        (!is.na(Pernicious_Anaemia_v4) & Pernicious_Anaemia_v4 == "Yes") |
        (!is.na(Rheumatoid_Arthritis_v4) & Rheumatoid_Arthritis_v4 == "Yes"),
      "1",
      "0" )
  )
table(SR$autoimmune)
#osmotic
SR <- SR %>%
  mutate(
    osmotic = ifelse(At_Diagnosis_Feeling_thirsty_v1 == "Yes" |
                       Inc_freq_passing_urine_daytime_v1 == "Yes"|
                       Inc_freq_passing_urine_night_v1 == "Yes",
                     "1",
                     "0" )
  )

#bmi at diagnosis
SR <- SR %>%
  mutate(
    bmi_diag = Weight_at_diagnosis_v1/(Height_v1^2)
  )

#make ethnicity categories
SR <- SR %>%
  mutate(Eth_5cat = ifelse(ethnic_origin %in% c("White British",
                                                "White Irish",
                                                "Other White"),
                           "White",
                           ifelse(ethnic_origin %in% c("Pakistani",
                                                       "Indian",
                                                       "Other Asian"),
                                  "South Asian",
                                  ifelse(ethnic_origin %in% c("Black African",
                                                              "Black Caribbean",
                                                              "Other Black"),
                                         "Black",
                                         ifelse(ethnic_origin %in% c("Other Mixed",
                                                                     "White & Black African",
                                                                     "White & Black Caribbean",
                                                                     "White Asian"),
                                                "Mixed",
                                                ifelse(ethnic_origin %in% c("Any Other Group",
                                                                            "Not Known",
                                                                            "Not Stated",
                                                                            "Chinese"),
                                                       "Other",
                                                       NA
                                                )
                                         )
                                  )
                           )
  )
  )

#Make DKA and ketosis_without_acidosis
##Prep
###Blood ketones
SR <- SR %>%
  mutate(
    Blood_ketone_result_at_diag_v1 =
      ifelse(Blood_ketone_result_at_diag_v1 %in% c(">7", ">7.0", "HI"), "7.1",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c("N/K", "N/A", "NK", "No Result", "NO RESULT", "NOT AVAILABLE", "NOT KNOWN", "UNKNOWN", "'SOME'", "+"),
      "result_unknown",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c("NEG", "NEGATIVE", "NO KETONES", "<0.1", "<1MMOL/L", "0"),
      "value_available",
      ifelse(is.na(Blood_ketone_result_at_diag_v1), "not_tested",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c(">1.5", "1.5MMOL/L"), "1.5",
      ifelse(Blood_ketone_result_at_diag_v1 == ">20", "20.1",
      ifelse(Blood_ketone_result_at_diag_v1 == "8+", "8.1",
      ifelse(Blood_ketone_result_at_diag_v1 == "5.2MMOL", "5.2",
      ifelse(Blood_ketone_result_at_diag_v1 == "4.5MMOLS", "4.5",
      ifelse(Blood_ketone_result_at_diag_v1 == "4.4MMOL", "4.4",
      ifelse(Blood_ketone_result_at_diag_v1 == "2.3MMOL/L", "2.3",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c("2.2MMOL/L", "2.2MOLS"), "2.2",
      ifelse(Blood_ketone_result_at_diag_v1 == "1.5MMOL/L", "1.5",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c("0.6MMOL", "0.6MMOL/L"), "0.6",
      ifelse(Blood_ketone_result_at_diag_v1 == "0.5MMOL/L", "0.5",
      ifelse(Blood_ketone_result_at_diag_v1 == "0.8 MMOL/L", "0.8",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c("1.0MMOL/L", "1"), "1.0",
      ifelse(Blood_ketone_result_at_diag_v1 == "1.2MMOL/L", "1.2",
      ifelse(Blood_ketone_result_at_diag_v1 == "1+", "1.1",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c("2 TRACE", "2.0 MMOL/L", "2"), "2.0",
      ifelse(Blood_ketone_result_at_diag_v1 == "25MMOLS", "25.0",
      ifelse(Blood_ketone_result_at_diag_v1 == "25 1", "25.1",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c("3+", "HIGH"), "3.1",
      ifelse(Blood_ketone_result_at_diag_v1 %in% c("LEVEL 3","3"), "3.0",
      Blood_ketone_result_at_diag_v1
      )))))))))))))))))))))))),
    #Blood_ketone_status_v1 = whether tested, available or unknown
    Blood_ketone_status_v1 =
      ifelse(Blood_ketone_result_at_diag_v1 == "not_tested", "not_tested",
      ifelse(Blood_ketone_result_at_diag_v1 == "result_unknown", "result_unknown",
      "value_available")))
#Remove characters and turn to as numeric
SR <- SR %>%
  mutate(
    Blood_ketone_result_at_diag_v1 = ifelse(Blood_ketone_result_at_diag_v1
                                            %in% c("not_tested", "result_unknown", "value_available"), NA,
                                            as.numeric(Blood_ketone_result_at_diag_v1)))
###Urine ketones
SR <- SR %>%
  mutate(
    #Code to pluses, negative and not available
    urine_ketones_at_diagnosis_v1 =
      ifelse(urine_ketones_at_diagnosis_v1 %in% c("+1", "1", "1+", "1.5MMOLS", "15MG/DL", "15MG/LOL"), "+",
      ifelse(urine_ketones_at_diagnosis_v1 %in% c("2+", "YES++", "2", "0.4", "40MG/DL", "716++"), "++",
      ifelse(urine_ketones_at_diagnosis_v1 %in% c("3+", "3++", "3", "KETONES +++", "8MMOLS","80MG/MMOL", "80MG/DL", "50MG", "8", "0.8", "3+++", "LARGE"), "+++",
      ifelse(urine_ketones_at_diagnosis_v1 %in% c("+4", "4+", "4+ DIPSTICK", "4+++", "4", "LARGE 16", "16", "HI", "HIGH"), "++++",
      ifelse(urine_ketones_at_diagnosis_v1 %in% c("NEG", "NEGATIVE", "NIL KETONES", "NORMAL", "0", "NAD", "NONE", "NO"), "negative",
      ifelse(urine_ketones_at_diagnosis_v1 %in% c("RESULT NOT KNOWN", "UNKNOWN","N/A", "NO RESULT", "NO RESULT DOCUMENTED BUT TESTED", "NK", "N/K", "NOT KNOWN", "NOT AVAILABLE", "NOT RECORDED", "TESTED AT GP, NO RESULT", "TESTED BUT NO RESULT", "POSITIVE ? AMOUNT", "KETONE CONCENTRATION", "NGC:", "N86", "YES", "YES (30)", "Ketones Urine (as said in medical notes)", "5"), "unknown",
      ifelse(urine_ketones_at_diagnosis_v1 %in% c("1.5", "TRACE", "POS", "POSITIVE"), "trace",
      urine_ketones_at_diagnosis_v1))))))),
    #change to numeric only version of this
    urine_ketones_at_diagnosis2_v1 = as.numeric(urine_ketones_at_diagnosis_v1),
    #Have two different units (pluses in urine_ketones_at_diagnosis_v1 and values in urine_ketones_at_diagnosis2_v1)
    #The latter also has two different units: looking for > 3 and > 17 (mg/dl) (and > ++ for pluses)
    high_urine_ketones_v1 =
      ifelse(!is.na(urine_ketones_at_diagnosis_v1) & urine_ketones_at_diagnosis_v1 %in% c("+++", "++++"), "1",
      ifelse(
        !is.na(urine_ketones_at_diagnosis2_v1) &
               (urine_ketones_at_diagnosis2_v1 > 17
             | (urine_ketones_at_diagnosis2_v1 > 3 & urine_ketones_at_diagnosis2_v1 <= 6)),
             "1",
             "0")))
#Want high blood ketones >= 3
SR <- SR %>%
  mutate(
    high_blood_ketones_v1 = ifelse(!is.na(Blood_ketone_result_at_diag_v1) & Blood_ketone_result_at_diag_v1 >= 3,
                     "1",
                     "0")
  )

##DKA
# SR <- SR %>%
#   mutate(
#     DKA2 = ifelse((high_blood_ketones_v1 == "1" & pH_less_than_7_3_at_diag_v1 == "Yes" & At_diagnosis_Admitted_to_hosp_v1 == "Yes")
#                       | (high_blood_ketones_v1 == "1" & pH_less_than_7_3_at_diag_v1 == "No" & At_diagnosis_Admitted_to_hosp_v1 == "Yes" & Ketoacidosis_at_diag_v1 == "Yes")
#                       | (high_blood_ketones_v1 == "1" & pH_less_than_7_3_at_diag_v1 == "Not tested" & At_diagnosis_Admitted_to_hosp_v1 == "Yes" & Ketoacidosis_at_diag_v1 == "Yes")
#                       | (high_urine_ketones_v1 == "1" & pH_less_than_7_3_at_diag_v1 == "Yes" & At_diagnosis_Admitted_to_hosp_v1 == "Yes")
#                       | (high_urine_ketones_v1 == "1" & pH_less_than_7_3_at_diag_v1 == "No" & At_diagnosis_Admitted_to_hosp_v1 == "Yes" & Ketoacidosis_at_diag_v1 == "Yes")
#                       | (high_urine_ketones_v1 == "1" & pH_less_than_7_3_at_diag_v1 == "Not tested" & At_diagnosis_Admitted_to_hosp_v1 == "Yes" & Ketoacidosis_at_diag_v1 == "Yes"),
#                       "1",
#                       "0")
#   )
SR <- SR %>%
  mutate(
    #based on criteria Ph + ketone blood>3 urin >++ or reported as dka and supportive ketone but no PH
    DKA = ifelse((high_blood_ketones_v1 == "1"
                       | high_urine_ketones_v1 == "1")
                      & ((pH_less_than_7_3_at_diag_v1 == "Yes" & At_diagnosis_Admitted_to_hosp_v1 == "Yes")
                         | ((pH_less_than_7_3_at_diag_v1 == "No"| pH_less_than_7_3_at_diag_v1 == "Not tested") & At_diagnosis_Admitted_to_hosp_v1 == "Yes" & Ketoacidosis_at_diag_v1 == "Yes")),
                      "1",
                      ifelse(Blood_ketone_status_v1 == "not_tested" & is.na(urine_ketones_at_diagnosis_v1) & is.na(pH_less_than_7_3_at_diag_v1) & is.na(At_diagnosis_Admitted_to_hosp_v1) & is.na(Ketoacidosis_at_diag_v1),NA,"0"))
  )

# dka_clean <- SR %>%
#   filter(is.na(DKA)) %>%
#   select(Study_ID, DKA, studyDKA, high_blood_ketones_v1, pH_less_than_7_3_at_diag_v1,
#          At_diagnosis_Admitted_to_hosp_v1, high_urine_ketones_v1,
#          Ketoacidosis_at_diag_v1, urine_ketones_at_diagnosis_v1,
#          Blood_ketone_result_at_diag_v1, Blood_ketone_status_v1)
# dka_cleanED <- SR %>%
#   filter(!is.na(DKA)) %>%
#   select(Study_ID, DKA, studyDKA, high_blood_ketones_v1, pH_less_than_7_3_at_diag_v1,
#          At_diagnosis_Admitted_to_hosp_v1, high_urine_ketones_v1,
#          Ketoacidosis_at_diag_v1, urine_ketones_at_diagnosis_v1,
#          Blood_ketone_result_at_diag_v1, Blood_ketone_status_v1)
# SR %>%
#   filter(Blood_ketone_status_v1 == "not_tested" & is.na(urine_ketones_at_diagnosis_v1)) %>%
#   select(Study_ID, DKA, studyDKA, high_blood_ketones_v1, pH_less_than_7_3_at_diag_v1,
#          At_diagnosis_Admitted_to_hosp_v1, high_urine_ketones_v1,
#          Ketoacidosis_at_diag_v1, urine_ketones_at_diagnosis_v1,
#          Blood_ketone_result_at_diag_v1, Blood_ketone_status_v1)
#
# SR %>%
#   filter(Blood_ketone_status_v1 == "not_tested" & is.na(urine_ketones_at_diagnosis_v1) & is.na(pH_less_than_7_3_at_diag_v1) & is.na(At_diagnosis_Admitted_to_hosp_v1) & is.na(Ketoacidosis_at_diag_v1)) %>%
#   select(Study_ID, DKA, studyDKA, ketone3, pH_less_than_7_3_at_diag_v1,
#          At_diagnosis_Admitted_to_hosp_v1, high_urine_ketones,
#          Ketoacidosis_at_diag_v1, urine_ketones_at_diagnosis_v1,
#          Blood_ketone_result_at_diag_v1, Blood_ketone_status_v1)
#
# SR %>%
#   filter((DKA == "1" & studyDKA == "0") | (DKA == "0" & studyDKA == "1")) %>%
#   select(Study_ID, DKA, studyDKA, ketone3, pH_less_than_7_3_at_diag_v1,
#          At_diagnosis_Admitted_to_hosp_v1, high_urine_ketones,
#          Ketoacidosis_at_diag_v1, urine_ketones_at_diagnosis_v1,
#          Blood_ketone_result_at_diag_v1, Blood_ketone_status_v1)
#
# table(SR$DKA, SR$studyDKA, useNA = "ifany")
# table(SR$DKA2, SR$studyDKA, useNA = "ifany")
##ketosis_without_acidosis
# SR <- SR %>%
#   mutate(ketosis_v1 = ifelse((!is.na(Blood_ketone_result_at_diag_v1) & Blood_ketone_result_at_diag_v1 >= 0.7)
#                              | ((!is.na(urine_ketones_at_diagnosis_v1) & urine_ketones_at_diagnosis_v1 %in% c("++", "+++", "++++") )
#                                 | (!is.na(urine_ketones_at_diagnosis2_v1) & (urine_ketones_at_diagnosis2_v1 >= 17 | (urine_ketones_at_diagnosis2_v1 >= 3 & urine_ketones_at_diagnosis2_v1 <= 6)))
#                                 ),
#                              "1", "0"))
SR <- SR %>%
  mutate(ketosis_v1 = ifelse((!is.na(Blood_ketone_result_at_diag_v1) & Blood_ketone_result_at_diag_v1 >= 0.7)
                             | ((!is.na(urine_ketones_at_diagnosis_v1) & urine_ketones_at_diagnosis_v1 %in% c("++", "+++", "++++") )
                                | (!is.na(urine_ketones_at_diagnosis2_v1) & (urine_ketones_at_diagnosis2_v1 >= 17 | (urine_ketones_at_diagnosis2_v1 >= 3 & urine_ketones_at_diagnosis2_v1 <= 6)))
                             ),
                             "1",
                             ifelse(is.na(Blood_ketone_result_at_diag_v1) & (is.na(urine_ketones_at_diagnosis_v1) | urine_ketones_at_diagnosis_v1 %in% c("negative", "trace", "unknown")), NA, "0")))

table(SR$ketosis_v1,
      SR$DKA,
      useNA = "ifany")
SR <- SR %>%
  mutate(ketosis_no_acidosis_v1 = ifelse(ketosis_v1 == "1" & DKA == "0", "1",
                                  ifelse(ketosis_v1 == "1" & DKA == "1", NA, "0")))
table(SR$ketosis_no_acidosis_v1, useNA = "ifany")

#Make relevant outcomes -------------------------------------------------------------
##Insulin requirement at 3 years ----------------------------------------------------
SR <- SR %>%
  mutate(ins_outcome = ifelse(is.na(Initial_diabetes_Insulin_v1),
                              ifelse(!is.na(dur_diab_joined_v4) & dur_diab_joined_v4 >= 2.86,
                                     ifelse(!is.na(Insulin_v4) & (Insulin_v4 == "No"|Insulin_v4 == "no"|Insulin_v4 == "NO"),
                                            "T2D no insulin v4",
                                            ifelse(!is.na(Insulin_v4) & Insulin_v4 == "Yes",
                                                   "T1D yes insulin v4 ",
                                                   "missing insulin v4"
                                            )
                                     ),
                                     ifelse((!is.na(dur_diab_yr_v3) & dur_diab_yr_v3 >= 2.86) | (!is.na(dur_diab_sample_yr_v3) & dur_diab_sample_yr_v3 >= 2.86),
                                            ifelse(!is.na(Insulin_v3) & Insulin_v3 == "No",
                                                   "T2D no insulin v3",
                                                   ifelse(!is.na(Insulin_v3) & Insulin_v3 == "Yes",
                                                          "T1D yes insulin v3",
                                                          "missing insulin v3"
                                                   )
                                            ),
                                            "missing dur_diab_v3 & v4 or both <3y"
                                     )
                              ),
                              "Insulin at diagnosis"
  )
  )

SR <- SR %>%
  mutate(insulinRequire = ifelse(str_detect(ins_outcome, "T1D"),
                                 "1",
                                 ifelse(str_detect(ins_outcome, "T2D"),
                                        "0",
                                        NA
                                 )
  )
  )
##StartRight 3 year outcome --------------------------------------------------------
#This is the outcome where:
#T1D = insulin requirement at 3 years and c-peptide < 600 pmol/l
#T2D = no insulin requirment at 3 years or if, c-peptide >= 600 pmol/l

SR$c_peptide_v4 <- as.numeric(SR$c_peptide_v4)
#SR$valid_ucpcr_v4 <- as.numeric(SR$valid_ucpcr_v4)
SR$UCPCR_v3 <- as.numeric(SR$UCPCR_v3)
SR$UCPCR_v4 <- as.numeric(SR$UCPCR_v4)
#SR$urine_days_in_transit_v4 <- as.numeric(SR$urine_days_in_transit_v4)

table(SR$bloodspot_cpeptide_v4)

SR %>%
  filter(is.na(c_peptide_v4) & !is.na(bloodspot_cpeptide_v4)) %>%
  count()

SR %>%
  filter(!is.na(c_peptide_v4) & is.na(bloodspot_cpeptide_v4)) %>%
  count()
SR %>%
  filter(!is.na(c_peptide_v4) & !is.na(bloodspot_cpeptide_v4)) %>%
  dplyr::select(Study_ID, c_peptide_v4, bloodspot_cpeptide_v4)

SR <- SR %>%
  mutate(bloodspot_cpeptide_v4 = ifelse(bloodspot_cpeptide_v4 == "<100",
                                        "99",
                                        ifelse(bloodspot_cpeptide_v4 == "<200",
                                               "199",
                                               ifelse(bloodspot_cpeptide_v4 == "IQE" | bloodspot_cpeptide_v4 == "Lab Error" | bloodspot_cpeptide_v4 == "Mis sampled" | bloodspot_cpeptide_v4 == "No result" | bloodspot_cpeptide_v4 == "No Result" | bloodspot_cpeptide_v4 == "Not Collected" | bloodspot_cpeptide_v4 == "Not returned" | bloodspot_cpeptide_v4 == "Not Returned",
                                                      NA,
                                                      bloodspot_cpeptide_v4))))

SR$bloodspot_cpeptide_v4 <- as.numeric(SR$bloodspot_cpeptide_v4)

SR <- SR %>%
  mutate(c_peptide_v4 = ifelse(!is.na(c_peptide_v4) & !is.na(bloodspot_cpeptide_v4) & c_peptide_v4 == bloodspot_cpeptide_v4,
                               mean(c_peptide_v4, bloodspot_cpeptide_v4),
                               ifelse(is.na(c_peptide_v4),
                                      bloodspot_cpeptide_v4,
                                      c_peptide_v4)))


SR %>%
  filter(Study_ID %in% c("SR2601","SR1003" )) %>%
  select(Study_ID, Insulin_v1, Insulin_v2, Insulin_v3, Insulin_v4,
         Insulin_Rapid_acting_v4, Insulin_Mix_v4, Insulin_Pump_v4,
         Insulin_Long_acting_v4)

# "SR0278",
# "SR2175"

##do this for now
SR <- SR %>%
  mutate(
    Insulin_v4 = ifelse(Study_ID %in% c("SR1764","SR3401"), "No",
                        ifelse(
                          #this is to reduce missing Insulin_v4,
                          #this patient on insulin on all other visits
                          Study_ID %in% c("SR0245","SR1061","SR1018","SR1602",
                                          "SR2601","SR1003"),
                          "Yes",
                          Insulin_v4)),
    Insulin_v3 = ifelse(Study_ID %in% c("SR2327","SR0139"),
                        "Yes",
                        Insulin_v3),
    Insulin_v2 = ifelse(Study_ID %in% c("SR0278"),
                        "Yes",
                        Insulin_v2),
    Insulin_v1 = ifelse(Study_ID %in% c("SR2175"),
                        "Yes",
                        Insulin_v1))



SR <- SR %>%
  #create a new variable called "define"
  mutate(define =
           # if "dur_diab_year_v4" (duration of diabetes at visit 4) is not missing and >= 2.86
           ifelse(!is.na(dur_diab_joined_v4) & dur_diab_joined_v4 >= 2.86,
                  #look at if Insulin at visit 4 is not missing and == "No"
                  ifelse(!is.na(Insulin_v4) & (Insulin_v4 == "No"|Insulin_v4 == "no"|Insulin_v4 == "NO"),
                         #dur_diab_year_v4 >= 2.86 and insulin = no at visit 4
                         "T2D no insulin v4",
                         #otherwise look if insulin at visit 4 is not missing and == "Yes"
                         ifelse(!is.na(Insulin_v4) & Insulin_v4 == "Yes",
                                #if is is "Yes" check that v4 Cpep is not missing and >= 600
                                ifelse(!is.na(c_peptide_v4) & c_peptide_v4 >= 600,
                                       #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" and Cpep >= 600
                                       "T2D Cpep v4 ",
                                       #otherwise check if c-pep is not missing and < 600
                                       ifelse(!is.na(c_peptide_v4) & c_peptide_v4 < 600,
                                              #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" and Cpep < 600
                                              "T1D Cpep v4",
                                              #otherwise check if V4 UCPCR is not missing and >= 1.1
                                              ifelse(!is.na(UCPCR_v4) & UCPCR_v4 >= 1.1,
                                                     #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" and UCPCR >= 1.1
                                                     "T2D UCPCR v4",
                                                     #lastly check if UCPCR V4 is missing
                                                     ifelse(!is.na(UCPCR_v4),
                                                            #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" but missing both v4 Cpep and UCPCR
                                                            "T1D UCPCR v4",
                                                            ifelse((!is.na(dur_diab_yr_v3) & dur_diab_yr_v3 >= 2.86) | (!is.na(dur_diab_sample_yr_v3) & dur_diab_sample_yr_v3 >= 2.86),
                                                                   #check if insulin is non-missing and == "No"
                                                                   ifelse(!is.na(Insulin_v3) & Insulin_v3 == "No",
                                                                          #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "No"
                                                                          "T2D no insulin v3",
                                                                          #check if v3 insulin is non-missing and Insulin == "Yes"
                                                                          ifelse(!is.na(Insulin_v3) & Insulin_v3 == "Yes",
                                                                                 #if so check v3 UCPCR >= 1.1
                                                                                 ifelse(!is.na(UCPCR_v3) & UCPCR_v3 >= 1.1,
                                                                                        #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "Yes", V3 UCPCR >=1.1
                                                                                        "T2D UCPCR v3",
                                                                                        #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "Yes", V3 UCPCR <1.1
                                                                                        ifelse(is.na(UCPCR_v3),
                                                                                               "V4>3, NO V4 BIO, V3 >3 but Missing v3 UCPCR",
                                                                                               "T1D UCPCR v3"
                                                                                        )
                                                                                 ),
                                                                                 #this is the "false" option for whether "insulin is non-missing and == Yes", as prev checked if "NO" only option here
                                                                                 #is that v3 insulin is missing
                                                                                 "V4>3, NO V4 BIO, V3 >3 but Missing v3 insulin"
                                                                          )
                                                                   ),
                                                                   "V4>3, NO V4 BIO, V3 <3 years"
                                                            )
                                                     )
                                              )
                                       )
                                ),
                                #this is the "false" option for whether "insulin is non-missing and == Yes", as prev checked if "NO" only option here
                                #is that v4 insulin is missing
                                ifelse(!is.na(c_peptide_v4) & c_peptide_v4 >= 600,
                                       #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" and Cpep >= 600
                                       "T2D Cpep v4 ",
                                       #otherwise check if c-pep is not missing and < 600
                                       ifelse(!is.na(c_peptide_v4) & c_peptide_v4 < 600,
                                              #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" and Cpep < 600
                                              "T1D Cpep v4",
                                              #otherwise check if V4 UCPCR is not missing and >= 1.1
                                              ifelse(!is.na(UCPCR_v4) & UCPCR_v4 >= 1.1,
                                                     #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" and UCPCR >= 1.1
                                                     "T2D UCPCR v4",
                                                     #lastly check if UCPCR V4 is missing
                                                     ifelse(!is.na(UCPCR_v4),
                                                            #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" but missing both v4 Cpep and UCPCR
                                                            "T1D UCPCR v4",
                                                            ifelse((!is.na(dur_diab_yr_v3) & dur_diab_yr_v3 >= 2.86) | (!is.na(dur_diab_sample_yr_v3) & dur_diab_sample_yr_v3 >= 2.86),
                                                                   #check if insulin is non-missing and == "No"
                                                                   ifelse(!is.na(Insulin_v3) & Insulin_v3 == "No",
                                                                          #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "No"
                                                                          "T2D no insulin v3",
                                                                          #check if v3 insulin is non-missing and Insulin == "Yes"
                                                                          ifelse(!is.na(Insulin_v3) & Insulin_v3 == "Yes",
                                                                                 #if so check v3 UCPCR >= 1.1
                                                                                 ifelse(!is.na(UCPCR_v3) & UCPCR_v3 >= 1.1,
                                                                                        #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "Yes", V3 UCPCR >=1.1
                                                                                        "T2D UCPCR v3",
                                                                                        #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "Yes", V3 UCPCR <1.1
                                                                                        ifelse(is.na(UCPCR_v3),
                                                                                               "V4>3, NO V4 BIO, V3 >3 but Missing v3 UCPCR",
                                                                                               "T1D UCPCR v3"
                                                                                        )
                                                                                 ),
                                                                                 #this is the "false" option for whether "insulin is non-missing and == Yes", as prev checked if "NO" only option here
                                                                                 #is that v3 insulin is missing
                                                                                 "V4>3, NO V4 BIO, V3 >3 but Missing v3 insulin"
                                                                          )
                                                                   ),
                                                                   "V4>3, NO V4 BIO, V3 <3 years"
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
                  ),
                  #this is the false option for if "dur_diab_year_v4 is non-missing and >=2.86"
                  #so if dur_diab_year_v4 is missing or <2.86 the function will come here
                  #if dur_diab_yr_v3 is non-missing and >=2.86
                  ifelse(!is.na(dur_diab_yr_v3) & dur_diab_yr_v3 >= 2.86,
                         #check if insulin is non-missing and == "No"
                         ifelse(!is.na(Insulin_v3) & Insulin_v3 == "No",
                                #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "No"
                                "T2D no insulin v3",
                                #check if v3 insulin is non-missing and Insulin == "Yes"
                                ifelse(!is.na(Insulin_v3) & Insulin_v3 == "Yes",
                                       #if so check v3 UCPCR >= 1.1
                                       ifelse(UCPCR_v3 >= 1.1,
                                              #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "Yes", V3 UCPCR >=1.1
                                              "T2D UCPCR v3",
                                              #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "Yes", V3 UCPCR <1.1
                                              "T1D UCPCR v3"
                                       ),
                                       #this is the "false" option for whether "insulin is non-missing and == Yes", as prev checked if "NO" only option here
                                       #is that v3 insulin is missing
                                       "Missing v3 insulin"
                                )
                         ),
                         #this is the false option for if "dur_diab_year_v3 is non-missing and >=2.86"
                         #so if dur_diab_year_v3 is missing or <2.86 (which also implies dur_diab_year_v4 is either missing or <2.86)
                         #check if dur_diab_yr_v3 is non-missing and < 2.86 (this is essentially to check if its missing)
                         ifelse(!is.na(dur_diab_sample_yr_v3) & dur_diab_sample_yr_v3 >= 2.86,
                                #check if insulin is non-missing and == "No"
                                ifelse(!is.na(Insulin_v3) & Insulin_v3 == "No",
                                       #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "No"
                                       "T2D no insulin v3",
                                       #check if v3 insulin is non-missing and Insulin == "Yes"
                                       ifelse(!is.na(Insulin_v3) & Insulin_v3 == "Yes",
                                              #if so check v3 UCPCR >= 1.1
                                              ifelse(!is.na(UCPCR_v3) & UCPCR_v3 >= 1.1,
                                                     #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "Yes", V3 UCPCR >=1.1
                                                     "T2D UCPCR v3",
                                                     #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "Yes", V3 UCPCR <1.1
                                                     ifelse(is.na(UCPCR_v3),
                                                            "V4<3, v3>3 but Missing v3 ucpcr",
                                                            "T1D UCPCR v3"
                                                     )
                                              ),
                                              #this is the "false" option for whether "insulin is non-missing and == Yes", as prev checked if "NO" only option here
                                              #is that v3 insulin is missing
                                              "V4<3, v3>3 but Missing v3 insulin"
                                       )
                                ),
                                "v3 and v4 less than 3"
                         )
                         #this is the false option for if "dur_diab_year_v3 is non-missing and <2.86"
                         #as we previously checked if it was >=2.86, the only option here is that dur_diab_yr_v3 is missing
                         #to get to this point we must have dur_diab_year_v4 < 2.86 or been missing, so we can say
                         #anything that reaches this point must have missing v3 data and v4 either <2.86 or be also missing
                         #"no(less than 3y) v4 and no v3 data"
                  )
           )
  )


#define outcome variable
SR <- SR %>%
  mutate(SRoutcome =
           #if detect "T1D" in the string in variable "definition"
           ifelse(str_detect(define, "T1D"),
                  #assign "1" to new variable "outcome" (these are type 1 diabetes according to our definition)
                  "1",
                  #if dont detect "T1D"
                  #check if detect "T2D" in string in variable "definition"
                  ifelse(str_detect(define, "T2D"),
                         #assign "0" to new variable "outcome" (these are type 2 diabetes according to our definition)
                         "0",
                         #since have detected neither "T1D" or "T2D" assign NA (these we cannot define)
                         NA
                  )
           )
  )

#Robust_definition
SR <- SR %>%
  #create a new variable called "define"
  mutate(robust_define =
  # if "dur_diab_year_v4" (duration of diabetes at visit 4) is not missing and >= 2.86
  ifelse(!is.na(dur_diab_joined_v4) & dur_diab_joined_v4 >= 2.86,
         ifelse(!is.na(Insulin_v4) & (Insulin_v4 == "No"),
                #dur_diab_year_v4 >= 2.86 and insulin = no at visit 4
                ifelse(!is.na(num_anti) & num_anti == 0,
                       "T2D no insulin v4 no antis",
                       ifelse(!is.na(num_anti),
                              "Unclassified, no ins, 1+ pos anti",
                              "Unclassified, no ins, missing antis"
                       )
                ),
                ifelse(!is.na(Insulin_v4) & Insulin_v4 == "Yes",
                       #if is is "Yes" check that v4 Cpep is not missing and >= 600
                       ifelse((!is.na(c_peptide_v4) & c_peptide_v4 < 200) | (!is.na(num_anti) & num_anti >= 2),
                              #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" and Cpep >= 600
                              ifelse(!is.na(c_peptide_v4) & c_peptide_v4 < 200 & !is.na(num_anti) & num_anti >= 2,
                                     "T1D on ins (Cpep < 200 & 2+ antis) v4",
                                     ifelse(!is.na(c_peptide_v4) & c_peptide_v4 < 200,
                                            "T1D on ins (Cpep < 200) v4",
                                            "T1D on ins (2+ antis) v4"
                                     )
                              ),
                              #otherwise c-pep is missing/>200pmol OR num_anti missing| <2
                              ifelse(!is.na(num_anti) & num_anti == 0 & !is.na(c_peptide_v4) & c_peptide_v4 >= 600,
                                     #dur_diab_year_v4 >=2.86, insulin at v4 == "Yes" and Cpep < 600
                                     "T2D ins, no antis, Cpep >= 600 v4",
                                     #otherwise check if V4 UCPCR is not missing and >= 1.1
                                     ifelse(!is.na(num_anti) & !is.na(c_peptide_v4),
                                            ifelse(num_anti == 0,
                                                   "Unclassified on ins, anti neg, 200pmol < Cpep < 600 pmol",
                                                   "Unclassified on ins, Cpep > 200, 1 anti pos"),
                                            ifelse(!is.na(num_anti),
                                                   "Unclassified on ins, missing c-peptide, 1 antis",
                                                   "Unclassified on ins, missing antis and c-peptide missing/200pmol < Cpep < 600 pmol"
                                            )
                                     )
                              )
                       ),
                       "Unclassified missing v4 insulin info"
                )
         ),
         #"visit 4 duration missing or < 3 yrs"
         ifelse((!is.na(dur_diab_yr_v3) & dur_diab_yr_v3 >= 2.86) | (!is.na(dur_diab_sample_yr_v3) & dur_diab_sample_yr_v3 >= 2.86),
                #check if insulin is non-missing and == "No"
                ifelse(!is.na(Insulin_v3) & Insulin_v3 == "No",
                       #dur_diab_yr_v4 <2.86, dur_diab_yr_v3 >= 2.86 and Insulin == "No"
                       ifelse(!is.na(num_anti) & num_anti == 0,
                              "T2D no insulin v3 no antis",
                              ifelse(!is.na(num_anti),
                                     "Unclassified, no ins, 1+ pos anti",
                                     "Unclassified no ins v3, missing antis")
                       ),
                       #check if v3 insulin is non-missing and Insulin == "Yes"
                       ifelse(!is.na(Insulin_v3) & Insulin_v3 == "Yes",
                              "Unclassified v3 on ins & no c-pep @v3",
                              "Unclassified v3 missing insulin v3 & no c-pep @v3"
                       )
                ),
                "Unclassified v4 and v3 duration missing|<3 years"
         )
  ),
robust_outcome =
  #if detect "T1D" in the string in variable "definition"
  ifelse(str_detect(robust_define, "T1D"),
         #assign "1" to new variable "outcome" (these are type 1 diabetes according to our definition)
         "T1D",
         #if dont detect "T1D"
         #check if detect "T2D" in string in variable "definition"
         ifelse(str_detect(robust_define, "T2D"),
                #assign "0" to new variable "outcome" (these are type 2 diabetes according to our definition)
                "T2D",
                #since have detected neither "T1D" or "T2D" assign NA (these we cannot define)
                NA
         )
  )
)

#define outcome variable
#SR <- SR %>%
  #mutate(robust_outcome =
           #if detect "T1D" in the string in variable "definition"
           #ifelse(str_detect(define, "T1D"),
                  #assign "1" to new variable "outcome" (these are type 1 diabetes according to our definition)
                  #"T1D",
                  #if dont detect "T1D"
                  #check if detect "T2D" in string in variable "definition"
                  #ifelse(str_detect(define, "T2D"),
                         #assign "0" to new variable "outcome" (these are type 2 diabetes according to our definition)
                        # "T2D",
                         #since have detected neither "T1D" or "T2D" assign NA (these we cannot define)
                         #NA
                  #)
           #)
 # )


#Save ---------------------------------------------------------------------------------
save(SR, file = "data/SR_30_6_2025.RData")
write_xlsx(SR,"data/SR_30_6_2025.xlsx")
