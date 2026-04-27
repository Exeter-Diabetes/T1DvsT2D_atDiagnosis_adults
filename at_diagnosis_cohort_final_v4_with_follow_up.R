
# Inclusion:
## diagnosed >3 months after registration start date (n=1,232,500)
## with hospital data
## diagnosed ≥01/01/2018 (n=284,500)
## diagnosed ≥age 18 years (n=278,382)

## V1 and V2 additionally had:
## not on insulin at diagnosis (have used window of up to 1 month after diagnosis but could reduce further)
## Version 3 doesn't have this exclusion

## V1, V2 and V3 additionally had:
## on insulin within 3 years of diagnosis (‘type 1’) or with at least 3 years follow-up (‘type 2’)
## V4: T1 still defined in the same way but 3 years of follow up not required


# Model variables:
## Sex
## Age at diagnosis
## BMI at diagnosis - 2 year window either side
## HbA1c at diagnosis (mmol/mol) - -6 months to +7 days
## Glucose and fasting glucose at diagnosis (mmol/L) - -6 months to +7 days (v4 only)
## Parent history of non-insulin-treated diabetes (i.e. T2D diabetes). NOTE: if unable to get, Parent history of diabetes is fine (Yes = 1, No = 0) - Code for positive or negative diabetes family history in mother/father/parent/unspecified first degree relative/’family history’ recorded any time, excluding if specified as gestational/type 1.
## DKA (Yes = 1, No = 0) - Hospital admission with DKA as the primary cause within 30 days before or after diagnosis date.
## Unintentional weight-loss (Yes = 1, No = 0) - Code for weight loss in GP records within 30 days before or after diagnosis date.
## Presence of osmotic symptoms (Yes = 1, No = 0)* - Code for polydipsia or polyuria in GP records within 30 days before or after diagnosis date.
## Presence of other autoimmune disorder (Yes = 1, No = 0)** - Code for any of the listed autoimmune disorders prior to/at diagnosis

## *Presence of osmotic symptoms is defined by the presence of at least one of the following at presentation: feeling thirsty, increased frequency of passing urine during the daytime, increased frequency of passing urine during the night.
## **Presence of additional autoimmune disorder/s is defined by at least one of the following: Coeliac, Vitiligo, Addisons disease, Overactive Thyroid, Underactive Thyroid, Pernicious Anaemia, Rheumatoid Arthritis

## Outcome: Progressed to insulin in 3 years (1/T1D), not progressed to insulin in 3 years (0/T2D)

## Plus DKA, T1 code ever, basal-bolus insulin at last follow-up

############################################################################################

# Setup
library(tidyverse)
library(aurum)
library(EHRBiomarkr)
rm(list=ls())

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")
codesets = cprd$codesets()
codes = codesets$getAllCodeSetVersion(v = "01/06/2024")

analysis <- cprd$analysis("jk_at_diag")


############################################################################################

# Define cohort IDs
## And add type 1 and type 2 definitions
## NB have checked and no diagnoses are after death in ONS death table

analysis = cprd$analysis("at_diag")
at_diag_cohort <- at_diag_cohort %>% analysis$cached("final_20250404")

analysis <- cprd$analysis("jk_at_diag")

cohort <- at_diag_cohort %>%
  filter(with_hes==1 &
           dm_diag_date>=as.Date("2018-01-01") &
           dm_diag_age>=18) %>%
  #(is.na(dm_diag_insdate) | datediff(dm_diag_insdate, dm_diag_date)>30) &
  #(datediff(gp_end_date, dm_diag_date)>=(3*365.25) | (!is.na(dm_diag_insdate) & datediff(dm_diag_insdate, dm_diag_date)<=(3*365.25)))) %>%
  mutate(follow_up_3yrs=ifelse(datediff(gp_end_date, dm_diag_date)>=(3*365.25), 1L, 0L),
         followup_duration = datediff(gp_end_date, dm_diag_date),
         ins_3yrs=ifelse(!is.na(dm_diag_insdate) & datediff(dm_diag_insdate, dm_diag_date)<=(3*365.25), 1L, 0L)) %>%
  select(patid, gender, ethnicity_5cat, ethnicity_16cat, imd_decile, dob, dm_diag_date, dm_diag_age, dm_diag_insdate, follow_up_3yrs, followup_duration, ins_3yrs, contains("prehba1c")) %>%
  analysis$cached("cohort_interim_1_v4", unique_indexes="patid")

cohort %>% count()
#222636


############################################################################################

# Add in other variables:
## BMI at diagnosis (currently contains BMI -2 years to +7 days only)
## Parent history of non-insulin-treated diabetes
## DKA
## Unintentional weight loss
## Presence of osmotic symptoms at diagnosis
## Presence of other autoimmune disorder at diagnosis


# Add in BMI within 2 year window of diagnosis

analysis = cprd$analysis("jk")
bmi_2yrs <- bmi_2yrs %>% analysis$cached("at_diag_bmi_2yrs")

analysis <- cprd$analysis("jk_at_diag")

cohort <- cohort %>%
  left_join(bmi_2yrs, by="patid") %>%
  analysis$cached("cohort_interim_2_v4", unique_indexes="patid")

cohort %>% count()
#222636 as above


# Remove from cohort if missing HbA1c or BMI at diagnosis

cohort <- cohort %>%
  filter(!is.na(prebmi) & !is.na(prehba1c)) %>%
  analysis$cached("cohort_interim_3_v4", unique_indexes="patid")

cohort %>% count()
#189756


# Add in parental diabetes (fh_diabetes is more broad)

analysis = cprd$analysis("all_patid")

## Raw FH of diabetes
raw_fh_diabetes_medcodes <- cprd$tables$observation %>%
  inner_join(codes$fh_diabetes, by="medcodeid") %>%
  analysis$cached("raw_fh_diabetes_medcodes", indexes=c("patid", "obsdate"))

## Clean FH of diabetes
clean_fh_diabetes_medcodes <- raw_fh_diabetes_medcodes %>%
  inner_join(cprd$tables$validDateLookup, by="patid") %>%
  filter(obsdate>=min_dob & obsdate<=gp_end_date) %>%
  select(patid, date=obsdate, fh_diabetes_cat) %>%
  analysis$cached("clean_fh_diabetes_medcodes", indexes=c("patid", "date", "fh_diabetes_cat"))

analysis = cprd$analysis("jk_at_diag")

# Exclude gestational and type 1, and sister/brother/child
# Then determine whether positive or negative
# For people with positive and negative codes:
## If all negative codes are earlier than positive codes, fine - use positive
## Otherwise, treat as missing

fh_positive_negative_interim <- clean_fh_diabetes_medcodes %>%
  filter(fh_diabetes_cat!="positive - gestational" & fh_diabetes_cat!="positive - type 1" & fh_diabetes_cat!="positive - sister" & fh_diabetes_cat!="positive - brother" & fh_diabetes_cat!="positive - child") %>%
  mutate(fh_diabetes_cat=ifelse(fh_diabetes_cat=="negative", "negative", "positive")) %>%
  group_by(patid, fh_diabetes_cat) %>%
  summarise(earliest_date=min(date, na.rm=TRUE),
            latest_date=max(date, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(patid) %>%
  pivot_wider(id_cols=patid, names_from = c(fh_diabetes_cat), names_glue = "{fh_diabetes_cat}_{.value}", values_from=c(earliest_date, latest_date)) %>%
  ungroup() %>%
  analysis$cached("fh_positive_negative_interim", unique_indexes="patid")

fh_positive_negative <- fh_positive_negative_interim %>%
  mutate(fh_diabetes=ifelse(is.na(positive_earliest_date), 0L,
                            ifelse(is.na(negative_earliest_date), 1L,
                                   ifelse(!is.na(positive_earliest_date) & !is.na(negative_earliest_date) & negative_latest_date<positive_earliest_date, 1L, NA)))) %>%
  analysis$cached("fh_positive_negative", unique_indexes="patid")

cohort <- cohort %>%
  left_join(fh_positive_negative, by="patid") %>%
  analysis$cached("cohort_interim_4_v4", unique_indexes="patid")

cohort %>% count()
#189756 as above


# DKA at diagnosis

dka_at_diag <- cprd$tables$hesDiagnosisEpi %>%
  inner_join(codes$icd10_dka, by=c("ICD"="icd10")) %>%
  inner_join(cohort, by="patid") %>%
  filter(d_order==1 & abs(datediff(epistart, dm_diag_date))<=30) %>%
  distinct(patid) %>%
  mutate(dka_at_diag=1L) %>%
  analysis$cached("dka_at_diag_v4", unique_indexes="patid")

cohort <- cohort %>%
  left_join(dka_at_diag, by="patid") %>%
  mutate(dka_at_diag=ifelse(is.na(dka_at_diag), 0L, 1L)) %>%
  analysis$cached("cohort_interim_5_v4", unique_indexes="patid")

cohort %>% count()
#189756 as above


# Weight loss at diagnosis

analysis = cprd$analysis("all_patid")

## Raw weight loss medcodes - don't need to clean as noone in cohort will have diagnosis date before birth/after death/deregistration
raw_weight_loss_medcodes <- cprd$tables$observation %>%
  inner_join(codes$weight_loss, by="medcodeid") %>%
  analysis$cached("raw_weight_loss_medcodes", indexes=c("patid", "obsdate"))

analysis = cprd$analysis("jk_at_diag")

weight_loss_at_diag <- cohort %>%
  inner_join(raw_weight_loss_medcodes, by="patid") %>%
  filter(abs(datediff(obsdate, dm_diag_date))<=30) %>%
  distinct(patid) %>%
  mutate(weight_loss_at_diag=1L) %>%
  analysis$cached("weight_loss_at_diag_v4", unique_indexes="patid")

cohort <- cohort %>%
  left_join(weight_loss_at_diag, by="patid") %>%
  mutate(weight_loss_at_diag=ifelse(is.na(weight_loss_at_diag), 0L, 1L)) %>%
  analysis$cached("cohort_interim_6_v4", unique_indexes="patid")

cohort %>% count()
#189756 as above


# Osmotic symptoms at diagnosis

analysis = cprd$analysis("all_patid")

## Raw polydipsia medcodes - don't need to clean as noone in cohort will have diagnosis date before birth/after death/deregistration
raw_polydipsia_medcodes <- cprd$tables$observation %>%
  inner_join(codes$polydipsia, by="medcodeid") %>%
  analysis$cached("raw_polydipsia_medcodes", indexes=c("patid", "obsdate"))

## Raw urinary frequency medcodes - don't need to clean as noone in cohort will have diagnosis date before birth/after death/deregistration
raw_urinary_frequency_medcodes <- cprd$tables$observation %>%
  inner_join(codes$urinary_frequency, by="medcodeid") %>%
  analysis$cached("raw_urinary_frequency_medcodes", indexes=c("patid", "obsdate"))

osmotic_symptoms <- raw_polydipsia_medcodes %>%
  union_all(raw_urinary_frequency_medcodes)

analysis = cprd$analysis("jk_at_diag")

osmotic_symptoms_at_diag <- cohort %>%
  inner_join(osmotic_symptoms, by="patid") %>%
  filter(abs(datediff(obsdate, dm_diag_date))<=30) %>%
  distinct(patid) %>%
  mutate(osmotic_symptoms_at_diag=1L) %>%
  analysis$cached("osmotic_symptoms_at_diag_v4", unique_indexes="patid")

cohort <- cohort %>%
  left_join(osmotic_symptoms_at_diag, by="patid") %>%
  mutate(osmotic_symptoms_at_diag=ifelse(is.na(osmotic_symptoms_at_diag), 0L, 1L)) %>%
  analysis$cached("cohort_interim_7_v4", unique_indexes="patid")

cohort %>% count()
#189756 as above


# Fasting glucose at diagnosis

analysis = cprd$analysis("all_patid")

## Clean fasting glucose medcodes
clean_fastingglucose_medcodes <- cprd$tables$observation %>%
  inner_join(codes$fastingglucose, by="medcodeid") %>%
  analysis$cached("clean_fastingglucose_medcodes", indexes=c("patid", "obsdate"))

analysis = cprd$analysis("jk_at_diag")

fastingglucose_at_diag <- cohort %>%
  inner_join(clean_fastingglucose_medcodes, by="patid") %>%
  mutate(datediff=datediff(date, dm_diag_date)) %>%
  filter(datediff<=7 & datediff>=-730) %>%
  group_by(patid) %>%
  mutate(min_timediff=min(abs(datediff), na.rm=TRUE)) %>%
  filter(abs(datediff)==min_timediff) %>%
  mutate(prefastingglucose=min(testvalue, na.rm=TRUE)) %>%
  filter(prefastingglucose==testvalue) %>%
  dbplyr::window_order(datediff) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  rename(prefastingglucosedate=date,
         prefastingglucosedatediff=datediff) %>%
  select(patid, prefastingglucose, prefastingglucosedate, prefastingglucosedatediff) %>%
  analysis$cached("fastingglucose_at_diag_v4", unique_indexes="patid")

cohort <- cohort %>%
  left_join(fastingglucose_at_diag, by="patid") %>%
  analysis$cached("cohort_interim_8_v4", unique_indexes="patid")

cohort %>% count()
#189756 as above


## Random glucose medcodes
raw_random_glucose <- cohort %>%
  select(patid) %>%
  inner_join(cprd$tables$observation) %>%
  filter(medcodeid==109701000006110 | medcodeid==224191000006110 | medcodeid==517791000006115 | medcodeid==518541000006118 | medcodeid==259584017 | medcodeid==1897851000006115 | medcodeid==259597015 | medcodeid==259307010 | medcodeid==518531000006111 | medcodeid==129841000000118 | medcodeid==134471000000110 | medcodeid==128091000000112 | medcodeid==977341000006114 | medcodeid==8412491000006111 | medcodeid==502060018 | medcodeid==3674501000006115 | medcodeid==3674511000006117 | medcodeid==3043011000006116 | medcodeid==3043021000006112 | medcodeid==3043041000006117 | medcodeid==3080861000006112) %>%
  analysis$cached("raw_random_glucose_v4", indexes=c("patid", "obsdate"))

### Check max, min and unit codes to decide how to clean

raw_random_glucose %>% filter(!is.na(testvalue) & testvalue!=0) %>% summarise(min=min(testvalue, na.rm=TRUE), max=max(testvalue, na.rm=TRUE))
# min is 0.05 and max is 1530 - need to clean

units <- raw_random_glucose %>% group_by(numunitid) %>% count() %>% collect()
# can just include missing and 218 as per fasting glucose - and use same units

clean_random_glucose <- raw_random_glucose %>%
  clean_biomarker_values(testvalue, "fastingglucose") %>%
  clean_biomarker_units(numunitid, "fastingglucose") %>%
  group_by(patid, obsdate) %>%
  summarise(testvalue=mean(testvalue, na.rm=TRUE)) %>%
  ungroup() %>%
  select(patid, date=obsdate, testvalue) %>%
  analysis$cached("clean_random_glucose_v4", indexes=c("patid", "date", "testvalue"))

randomglucose_at_diag <- cohort %>%
  inner_join(clean_random_glucose, by="patid") %>%
  mutate(datediff=datediff(date, dm_diag_date)) %>%
  filter(datediff<=7 & datediff>=-730) %>%
  group_by(patid) %>%
  mutate(min_timediff=min(abs(datediff), na.rm=TRUE)) %>%
  filter(abs(datediff)==min_timediff) %>%
  mutate(prerandomglucose=min(testvalue, na.rm=TRUE)) %>%
  filter(prerandomglucose==testvalue) %>%
  dbplyr::window_order(datediff) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  rename(prerandomglucosedate=date,
         prerandomglucosedatediff=datediff) %>%
  select(patid, prerandomglucose, prerandomglucosedate, prerandomglucosedatediff) %>%
  analysis$cached("randomglucose_at_diag_v4", unique_indexes="patid")

cohort <- cohort %>%
  left_join(randomglucose_at_diag, by="patid") %>%
  analysis$cached("cohort_interim_9_v4", unique_indexes="patid")

cohort %>% count()
#189756 as above  


# Other autoimmune disease at diagnosis

ai_conditions <- c("coeliac", "vitiligo", "addisons", "graves", "hashimotos", "pernicious_anaemia", "rheumatoidarthritis")

for (i in ai_conditions) {
  
  print(i)
  
  if (i=="rheumatoidarthritis") {
    codelist <- codes[[i]]
  }
  else {
    codelist <- read_delim(paste0("C:/Users/ky279/OneDrive - University of Exeter/CPRD/2025/Julieanne progression/Final/autoimmune codelists/", i, ".txt")) %>%
      rename(medcodeid=MedCodeId)
  }
  
  code_occurrences <- cprd$tables$observation %>%
    inner_join(codelist, by="medcodeid", copy=TRUE) %>%
    inner_join(cprd$tables$validDateLookup, by="patid") %>%
    filter(obsdate>=min_dob) %>%
    select(patid, obsdate) %>%
    analysis$cached(paste0("clean_", i, "_medcodes"), indexes=c("patid", "obsdate"))
  
  code_occurrences_at_diag <- cohort %>%
    inner_join(code_occurrences, by="patid") %>%
    filter(obsdate<=dm_diag_date) %>%
    distinct(patid) %>%
    mutate({{i}}:=1L) %>%
    analysis$cached(paste0(i, "_at_diag_v4"), unique_indexes="patid")
  
  assign(i, code_occurrences_at_diag)
  
}


cohort <- cohort %>%
  left_join(coeliac, by="patid") %>%
  left_join(vitiligo, by="patid") %>%
  left_join(addisons, by="patid") %>%
  left_join(graves, by="patid") %>%
  left_join(hashimotos, by="patid") %>%
  left_join(pernicious_anaemia, by="patid") %>%
  left_join(rheumatoidarthritis, by="patid") %>%
  mutate(across(c(coeliac, vitiligo, addisons, graves, hashimotos, pernicious_anaemia, rheumatoidarthritis),~ if_else(is.na(.), 0, .))) %>%
  analysis$cached("cohort_interim_10_v4", unique_indexes="patid")



# Add DKA ever (more than 30 days after diagnosis), T1 code ever, basal-bolus at final followup

## DKA
analysis = cprd$analysis("all_patid")
raw_dka_any_cause <- raw_dka_any_cause %>% analysis$cached("raw_dka_icd10")
analysis = cprd$analysis("jk_at_diag")
dka_after_diag <- raw_dka_any_cause %>%
  inner_join(cohort, by="patid") %>%
  filter(d_order==1 & datediff(epistart, dm_diag_date)>30) %>%
  distinct(patid) %>%
  mutate(dka=1L) %>%
  analysis$cached("dka_after_diag_v4", unique_indexes="patid")

## T1 code
analysis = cprd$analysis("all_patid")
raw_diabetes_medcodes <- raw_diabetes_medcodes %>% analysis$cached("raw_diabetes_medcodes")
analysis = cprd$analysis("jk_at_diag")
t1_code <- raw_diabetes_medcodes %>%
  filter(all_diabetes_cat=="Type 1") %>%
  inner_join(cprd$tables$validDateLookup, by="patid") %>%
  filter(obsdate>=min_dob & obsdate<=gp_end_date) %>%
  distinct(patid) %>%
  mutate(t1_code=1L) %>%
  analysis$cached("t1_code", unique_indexes="patid")

## Basal-bolus at final followup
analysis = cprd$analysis("all_patid")
clean_insulin_prodcodes <- clean_insulin_prodcodes %>% analysis$cached("clean_insulin_prodcodes")
basal_bolus <- clean_insulin_prodcodes %>% filter(insulin_cat=="Bolus insulin")

analysis = cprd$analysis("jk_at_diag")
basal_bolus_follow_up <- cprd$tables$validDateLookup %>%
  left_join(cprd$tables$onsDeath, by="patid") %>%
  inner_join(basal_bolus, by="patid") %>%
  mutate(end_date=ifelse(is.na(reg_date_of_death) | (!is.na(reg_date_of_death) & reg_date_of_death>gp_end_date), gp_end_date, reg_date_of_death)) %>%
  filter(date<end_date & datediff(end_date, date)<365.25) %>%
  distinct(patid) %>%
  mutate(basal_bolus=1L) %>%
  analysis$cached("basal_bolus_follow_up", unique_indexes="patid")


cohort <- cohort %>%
  left_join(dka_after_diag, by="patid") %>%
  mutate(dka=ifelse(is.na(dka), 0L, 1L)) %>%
  left_join(t1_code, by="patid") %>%
  mutate(t1_code=ifelse(is.na(t1_code), 0L, 1L)) %>%
  left_join(basal_bolus_follow_up, by="patid") %>%
  mutate(basal_bolus=ifelse(is.na(basal_bolus), 0L, 1L)) %>%
  analysis$cached("cohort_v4")

