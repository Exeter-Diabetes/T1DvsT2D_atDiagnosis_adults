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
## A) For SRoutcome -------------------------------------------------------------------------------
SR %>%
  filter(AgeatDiagnosis > 50) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(AgeatDiagnosis <=50) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
# SR %>%
#   filter(
#     Date_Visit_v1 > "2018-10-31" | 
#       AgeatDiagnosis > 50) %>%
#   summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(AgeatDiagnosis <=50 & 
           grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                 Other_type_of_diabetes_v1)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))

SR %>%
  filter(AgeatDiagnosis <=50 & 
           !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                 Other_type_of_diabetes_v1)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
SR %>%
  filter(AgeatDiagnosis <=50 & 
           !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                 Other_type_of_diabetes_v1) & is.na(SRoutcome)) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))

SR %>%
  filter(AgeatDiagnosis <=50 & 
           !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                  Other_type_of_diabetes_v1) & !is.na(SRoutcome) & !is.na(define) & !define %in% 
           c("T1D Cpep v4", "T2D Cpep v4 ", "T2D no insulin v3",
             "T2D no insulin v4")) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))

SR %>%
  filter(AgeatDiagnosis <=50 & 
           !grepl("MIDD (MATERNAL INHERITED DIABETES & DEAFNESS)|MODY|MODY HNF1A", 
                  Other_type_of_diabetes_v1) & !is.na(SRoutcome) & 
           define %in% c("T1D Cpep v4", "T2D Cpep v4 ", "T2D no insulin v3",
                         "T2D no insulin v4")) %>%
  summarise(n = sum(!is.na(Study_ID) == TRUE))
