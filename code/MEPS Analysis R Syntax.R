
# #Install Packages <- only need to install once 
# install.packages("foreign")
# install.packages("survey")
# install.packages("tidyr")
# install.packages("reshape")
# install.packages("reshape2")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("icd")
# install.packages("packagename")
# install.packages("ggplot2")
# install.packages("pscl")
# install.packages("boot")
# install.packages("lmtest")
# install.packages("nonnest2")
# install.packages("sjstats")
# install.packages("survey")
# install.packages("rmarkdown")
# install.packages("weights")
# install.packages("jtools")
# install.packages("pscl")
# install.packages("mhurdle")
# install.packages("stringi")
# install.packages("magrittr")
# install.packages("devtools")
# install.packages("margins")
#devtools::install_github("jackwasey/icd")

###################
## Load packages ##
###################
library(foreign)
library(survey)
library(tidyr)
library(tidyselect) 
library(dplyr)
library(magrittr) 
library(ggplot2)
library(jtools)
library(weights)
library(readr)
library(stringi)
library(sjstats)
library(pscl)
library(mhurdle)
library(usethis)
library(devtools)
library(SASxport)
library(stringi)
library(magrittr)
library(icd)
library(devtools)
library(margins)
library(devtools)
library(tidyverse)
library(readr)
library(readxl)
library(haven)
library(icd)

#To do: Change directory if needed
setwd("C:/Users/berkaa01/OneDrive - The Mount Sinai Hospital/MEPS analysis/Asem/code")

#Inflation Excel File - used to inflate all expenditures to 2020 values;
#this is set up to inflate family (out-of-pocket) expenditures by the consumer price index (CPI) 
#and other medical expenditures by the personal consumer expenditures (PCE) health index 
inflation <- read.csv("../data/Inflation - 2020.csv",row.names = 1) #import inflation csv file
inflation$ratePCE_health <- 1/(inflation$PCE_Health/inflation$PCE_Health[max(nrow(inflation))]) #create ratePCE_health variable to inflate other medical expenditures
inflation$rateCPI <- 1/(inflation$CPI/inflation$CPI[max(nrow(inflation))]) #create rateCPI variable to inflate family (out-of-pocket) expenditures

#######################
## LONGITUDINAL DATA ##
#######################

#2012-2017 data (change directory if needed)
directory <- "C:/Users/berkaa01/OneDrive - The Mount Sinai Hospital/MEPS analysis/Shangqing's R and data/"
setwd(directory)

#Step0: Create year vector
year_longitudinal_panel<-c(seq(16,21))

#Step1: Create directories for loading and save data
directory_Longitudinal_load<-paste0(directory,"/longitudinal weights/raw/")
directory_Longitudinal_save<-paste0(directory, "/longitudinal weights/rdata/")

#Step2: Create vectors for extracting names and assigning names
Longitudinal_file_name_load_ssp<-list.files(path=directory_Longitudinal_load)
Longitudinal_file_name_load<-stringr::str_remove(Longitudinal_file_name_load_ssp,".ssp")
Longitudinal_file_name_save<-paste0("LONGWT_PANEL",year_longitudinal_panel)

#Step3: Load and save files
counter=1
for (i in Longitudinal_file_name_load){
  data1=read.xport(paste0(directory_Longitudinal_load, i, ".ssp"))
  assign(Longitudinal_file_name_save[counter],data1)
  counter<-counter+1}

#Step 4: Load 2018 and 2019 data separately (change directory if needed)
setwd("C:/Users/berkaa01/OneDrive - The Mount Sinai Hospital/MEPS analysis/Asem/code")
LONGWT_PANEL22 <- read.csv('../data/raw/longitudinal/h210.csv')
LONGWT_PANEL23 <- read.csv('../data/raw/longitudinal/h217.csv')

#############################
## MEDICAL CONDITIONS #######
#############################

#To do: Change directory if needed
directory <- "C:/Users/berkaa01/OneDrive - The Mount Sinai Hospital/MEPS analysis/Asem/"

#Notes: ICD 9 codes are applicable to 2011-2015 files, ICD 10 codes are applicable starting from 2016.#

#Step0: Create year vectors based on ICD codes
year_ICD_10<-c(seq(2016, 2018))
year_ICD_09<-c(seq(2011, 2015))
year_MedicalCondition<-seq(2011, 2018)

#Step1: Create directories for loading and save data
directory_MedicalCondition_load<-paste0(directory,"/data/raw/medical conditions/raw/")
directory_MedicalCondition_save<-paste0(directory,"/data/raw/medical conditions/rdata/")

#Step2: Create vectors for extracting names and assigning names
MedicalCondition_file_name_load_ssp<-list.files(path = directory_MedicalCondition_load)
MedicalCondition_file_name_load<-stringr::str_remove(MedicalCondition_file_name_load_ssp,".ssp")
MedicalCondition_file_name_save_all<-paste0("MedicalConditions_", year_MedicalCondition)

MedicalCondition_file_name_save_ICD10<-paste0("MedicalConditions_",year_ICD_10)
MedicalCondition_file_name_save_ICD10_grouped<-paste0("MedicalConditions_Grouped_",year_ICD_10)

MedicalCondition_file_name_save_ICD9<-paste0("MedicalConditions_",year_ICD_09)
MedicalCondition_file_name_save_ICD9_grouped<-paste0("MedicalConditions_Grouped_",year_ICD_09)

#Step3: Load and save files
#For ICD 10 and ICD 9
counter=1
for(i in MedicalCondition_file_name_load) {
  data1=read.xport(paste0(directory_MedicalCondition_load,i,".ssp"))
  assign(MedicalCondition_file_name_save_all[counter], data1)
  counter<-counter+1}

#Manually read in 2018 file
MedicalConditions_2018 <- read_excel('../data/raw/medical conditions/excel/h207.xlsx')

#Step4: Data management
#For ICD 10
counter=1
for (i in MedicalCondition_file_name_save_ICD10){
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, ICD10CDX) %>% 
    mutate(MedicalCondition_MI = ifelse(ICD10CDX == "I21" | ICD10CDX == "I22", 1, 0), #Myocardial Infarction
           MedicalCondition_STRK = ifelse(ICD10CDX == "I61" | ICD10CDX == "I63"  | ICD10CDX == "I64" | ICD10CDX == "G45", 1, 0), # Stroke
           MedicalCondition_HF = ifelse(ICD10CDX == "I50", 1, 0), #Heart Failure
           MedicalCondition_DM = ifelse(ICD10CDX == "E11" | ICD10CDX == "E10" | ICD10CDX == "E13", 1, 0)) #Diabetes Mellitus
  assign(MedicalCondition_file_name_save_ICD10[counter], data1)
  counter<-counter+1}

counter=1
for (i in MedicalCondition_file_name_save_ICD10) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    group_by(DUPERSID) %>% 
    summarise(MedicalCondition_MI_Grouped = max(MedicalCondition_MI), #Medical conditions with the same DUPERSID are summed, values > 0 represent that the medical condition is present
              MedicalCondition_STRK_Grouped = max(MedicalCondition_STRK),
              MedicalCondition_HF_Grouped = max(MedicalCondition_HF),
              MedicalCondition_DM_Grouped = max(MedicalCondition_DM))
  assign(MedicalCondition_file_name_save_ICD10_grouped[counter], data1)
  save(data1,file=paste0(directory_MedicalCondition_save,MedicalCondition_file_name_save_ICD10_grouped[counter],".rdata"))
  counter<-counter+1}

#For ICD 9
counter=1
for (i in MedicalCondition_file_name_save_ICD9){
  data1 = get(i) %>% 
    select(DUPERSID, ICD9CODX) %>% 
    mutate(MedicalCondition_MI = ifelse(ICD9CODX == "410" | ICD9CODX == "412", 1, 0), #Myocardial Infarction
           MedicalCondition_STRK = ifelse(ICD9CODX == "431" | ICD9CODX == "433" | ICD9CODX == "434" | ICD9CODX == "436", 1, 0), #Stroke
           MedicalCondition_HF = ifelse(ICD9CODX == "428", 1, 0), #Heart Failure
           MedicalCondition_DM = ifelse(ICD9CODX == "250", 1, 0)) #Diabetes Mellitus
  assign(MedicalCondition_file_name_save_ICD9[counter], data1)
  counter<-counter+1}

counter=1
for (i in MedicalCondition_file_name_save_ICD9) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    group_by(DUPERSID) %>% 
    summarise(MedicalCondition_MI_Grouped = max(MedicalCondition_MI), #Medical conditions with the same DUPERSID are summed, values > 0 represent that the medical condition is present
              MedicalCondition_STRK_Grouped = max(MedicalCondition_STRK),
              MedicalCondition_HF_Grouped = max(MedicalCondition_HF),
              MedicalCondition_DM_Grouped = max(MedicalCondition_DM))
  assign(MedicalCondition_file_name_save_ICD9_grouped[counter], data1)
  save(data1,file=paste0(directory_MedicalCondition_save,MedicalCondition_file_name_save_ICD9_grouped[counter],".rdata"))
  counter<-counter+1}

################################
## CHARLSON COMORBIDITY INDEX ##
################################

#Note: CCIs are derived from MedicalConditions files.

#Step0: Create year vectors based on ICD codes
year_ICD_10<-c(seq(2016, 2018))
year_ICD_09<-c(seq(2011, 2015))

#Load ICD codes and corresponding comorbidities
codes_ICD10 <- readRDS('../data/raw/charlson/icd/icd10.Rdata')
codes_ICD9 <- readRDS('../data/raw/charlson/icd/icd9.Rdata')

#Step1: Create directories for loading and save data
directory_Charlson_save<-paste0(directory,"data/raw/charlson/rdata/")

#Step2: Create vectors for extracting names and assigning names
Charlson_file_name_save_ICD10<-paste0("Charlson_",year_ICD_10)
Charlson_file_name_save_ICD9<-paste0("Charlson_",year_ICD_09)

PrimaryConditions_ICD10 <- c("I21", "I22", "I61", "I63", "I64", "G45", "I50", "E11", "E10", "E13")
PrimaryConditions_ICD9 <- c("410", "412", "431", "433", "434", "436", "428", "250")

#Step3: Load and save files 
#For ICD 10
counter=1
for (i in MedicalCondition_file_name_save_ICD10){
  data1 = get(i) %>%
    select(DUPERSID, ICD10CDX) %>%
    filter(!(ICD10CDX %in% PrimaryConditions_ICD10)) %>% #exclude the 4 primary conditions from Charlson count
    mutate(Dementia=ifelse(ICD10CDX%in%codes_ICD10$Dementia,1,0) * 2,
           Pulmonary=ifelse(ICD10CDX%in%codes_ICD10$Pulmonary,1,0) * 1,
           Rheumatic=ifelse(ICD10CDX%in%codes_ICD10$Rheumatic,1,0) * 1,
           LiverMild=ifelse(ICD10CDX%in%codes_ICD10$LiverMild,1,0) * 2,
           Paralysis=ifelse(ICD10CDX%in%codes_ICD10$Paralysis,1,0) * 2,
           Renal=ifelse(ICD10CDX%in%codes_ICD10$Renal,1,0) * 1,
           Cancer=ifelse(ICD10CDX%in%codes_ICD10$Cancer,1,0) * 2,
           LiverSevere=ifelse(ICD10CDX%in%codes_ICD10$LiverSevere,1,0) * 4,
           Mets=ifelse(ICD10CDX%in%codes_ICD10$Mets,1,0) * 6,
           HIV=ifelse(ICD10CDX%in%codes_ICD10$HIV,1,0) * 4) %>%
    distinct(DUPERSID, Dementia, Pulmonary, Rheumatic, LiverMild, 
             Paralysis, Renal, Cancer, LiverSevere, Mets, HIV)
  data1$Row_Sum <- rowSums(data1[-c(1:2)])
  data1 <- data1 %>% group_by(DUPERSID) %>% dplyr::summarize(Charlson = sum(Row_Sum))
  assign(Charlson_file_name_save_ICD10[counter],data1)
  save(data1,file=paste0(directory_Charlson_save,MedicalCondition_file_name_save_ICD10[counter],".rdata"))
  counter<-counter+1}

#For ICD 9
counter=1
for (i in MedicalCondition_file_name_save_ICD9){
  data1 = get(i) %>%
    select(DUPERSID, ICD9CODX) %>%
    filter(!(ICD9CODX %in% PrimaryConditions_ICD9)) %>% #exclude the 4 primary conditions from Charlson count
    mutate(Dementia=ifelse(ICD9CODX%in%codes_ICD9$Dementia,1,0) * 2,
           Pulmonary=ifelse(ICD9CODX%in%codes_ICD9$Pulmonary,1,0) * 1,
           Rheumatic=ifelse(ICD9CODX%in%codes_ICD9$Rheumatic,1,0) * 1,
           LiverMild=ifelse(ICD9CODX%in%codes_ICD9$LiverMild,1,0) * 2,
           Paralysis=ifelse(ICD9CODX%in%codes_ICD9$Paralysis,1,0) * 2,
           Renal=ifelse(ICD9CODX%in%codes_ICD9$Renal,1,0) * 1,
           Cancer=ifelse(ICD9CODX%in%codes_ICD9$Cancer,1,0) * 2,
           LiverSevere=ifelse(ICD9CODX%in%codes_ICD9$LiverSevere,1,0) * 4,
           Mets=ifelse(ICD9CODX%in%codes_ICD9$Mets,1,0) * 6,
           HIV=ifelse(ICD9CODX%in%codes_ICD9$HIV,1,0) * 4)%>%
    distinct(DUPERSID, Dementia, Pulmonary, Rheumatic, LiverMild,
             Paralysis, Renal, Cancer, LiverSevere, Mets, HIV)
  data1$Row_Sum <- rowSums(data1[-c(1:2)])
  data1 <- data1 %>% group_by(DUPERSID) %>% dplyr::summarize(Charlson = sum(Row_Sum))
  assign(Charlson_file_name_save_ICD9[counter],data1)
  save(data1,file=paste0(directory_Charlson_save, MedicalCondition_file_name_save_ICD9[counter],".rdata"))
  counter<-counter+1}

########################
## MERGE WITH LONGDAT ##
########################

LONGWT_PANEL16 <- LONGWT_PANEL16 %>% 
  merge(MedicalConditions_Grouped_2011, all.x=TRUE) %>% #merge Panel 16 ('11-12) with med cond 2011
  merge(Charlson_2011, all.x=TRUE)
LONGWT_PANEL16$Year = 2012

LONGWT_PANEL17 <- LONGWT_PANEL17 %>% 
  merge(MedicalConditions_Grouped_2012, all.x=TRUE) %>% #merge Panel 17 ('12-13) with med cond 2012
  merge(Charlson_2012, all.x=TRUE)
LONGWT_PANEL17$Year = 2013

LONGWT_PANEL18 <- LONGWT_PANEL18 %>% 
  merge(MedicalConditions_Grouped_2013, all.x=TRUE) %>% #merge Panel 18 ('13-14) with med cond 2013
  merge(Charlson_2013, all.x=TRUE)
LONGWT_PANEL18$Year = 2014

LONGWT_PANEL19 <- LONGWT_PANEL19 %>% 
  merge(MedicalConditions_Grouped_2014, all.x=TRUE) %>% #merge Panel 19 ('14-15) with med cond 2014
  merge(Charlson_2014, all.x=TRUE)
LONGWT_PANEL19$Year = 2015

LONGWT_PANEL20 <- LONGWT_PANEL20 %>% 
  merge(MedicalConditions_Grouped_2015, all.x=TRUE) %>% #merge Panel 20 ('15-16) with med cond 2015
  merge(Charlson_2015, all.x=TRUE)
LONGWT_PANEL20$Year = 2016

LONGWT_PANEL21 <- LONGWT_PANEL21 %>% 
  merge(MedicalConditions_Grouped_2016, all.x=TRUE) %>% #merge Panel 21 ('16-17) with med cond 2016
  merge(Charlson_2016, all.x=TRUE)
LONGWT_PANEL21$Year = 2017

LONGWT_PANEL22$DUPERSID = as.numeric(substr(as.character(LONGWT_PANEL22$DUPERSID), 3, nchar(as.character(LONGWT_PANEL22$DUPERSID))))
LONGWT_PANEL22 <- LONGWT_PANEL22 %>% 
  merge(MedicalConditions_Grouped_2017, all.x=TRUE) %>% #merge Panel 22 ('17-18) with med cond 2017
  merge(Charlson_2017, all.x=TRUE)
LONGWT_PANEL22$Year = 2018

LONGWT_PANEL23 <- LONGWT_PANEL23 %>% 
  merge(MedicalConditions_Grouped_2018, all.x=TRUE) %>% #merge Panel 23 ('18-19) with med cond 2018
  merge(Charlson_2018, all.x=TRUE)
LONGWT_PANEL23$Year = 2019

######################
## RENAME VARIABLES ##
######################

#SF-6D
LONGWT_PANEL23 <- LONGWT_PANEL23 %>% rename(ADDOWN2=ADDPRS2, ADDOWN4=ADDPRS4, 
                                            ADMALS2=ADEMLS2, ADMALS4=ADEMLS4,
                                            ADNRGY2=ADENGY2, ADNRGY4=ADENGY4, 
                                            ADPWLM2=ADWKLM2, ADPWLM4=ADWKLM4)
LONGWT_PANEL22 <- LONGWT_PANEL22 %>% rename(ADDOWN2=ADDPRS2, ADDOWN4=ADDPRS4, 
                                            ADMALS2=ADEMLS2, ADMALS4=ADEMLS4,
                                            ADNRGY2=ADENGY2, ADNRGY4=ADENGY4, 
                                            ADPWLM2=ADWKLM2, ADPWLM4=ADWKLM4)
LONGWT_PANEL21 <- LONGWT_PANEL21 %>% rename(ADDOWN4=ADPRST4, ADMALS4=ADEMLS4, 
                                            ADPWLM4=ADWKLM4, ADNRGY4=ADENGY4)

#Primary Conditions
LONGWT_PANEL23 <- LONGWT_PANEL23 %>% rename(DIABDXY1=DIABDXY1_M18)

###############
## EDUCATION ##
###############

#Combine responses into categories

LONGWT_PANEL16 <- LONGWT_PANEL16 %>% 
                  mutate(EDUCATION=case_when(HIDEG==1 | HIDEG==2 | HIDEG==8 ~ 1, 
                                             HIDEG==3 | HIDEG==7 ~ 2, 
                                             HIDEG==4 | HIDEG==5 | HIDEG==6 ~ 3, TRUE~ 0))
LONGWT_PANEL17 <- LONGWT_PANEL17 %>% 
                  mutate(EDUCATION=case_when(EDUYRDG==1 | EDUYRDG==2 | EDUYRDG==3 | EDUYRDG==10 ~ 1,
                                             EDUYRDG>=4 & EDUYRDG<=7 ~ 2, 
                                             EDUYRDG==8 | EDUYRDG==9 ~ 3, TRUE ~ 0)) 
LONGWT_PANEL18 <- LONGWT_PANEL18 %>% 
                  mutate(EDUCATION=case_when(EDUYRDG==1 | EDUYRDG==2 | EDUYRDG==3 | EDUYRDG==10 ~ 1,
                                             EDUYRDG>=4 & EDUYRDG<=7 ~ 2, 
                                             EDUYRDG==8 | EDUYRDG==9 ~ 3, TRUE ~ 0))
LONGWT_PANEL19 <- LONGWT_PANEL19 %>% 
                  mutate(EDUCATION=case_when(EDUYRDG==1 | EDUYRDG==2 | EDUYRDG==3 | EDUYRDG==10 ~ 1,
                                             EDUYRDG>=4 & EDUYRDG<=7 ~ 2, 
                                             EDUYRDG==8 | EDUYRDG==9 ~ 3, TRUE ~ 0))
LONGWT_PANEL20 <- LONGWT_PANEL20 %>%
                  mutate(EDUCATION=case_when(HIDEG==1 | HIDEG==2 | HIDEG==8 ~ 1, 
                                             HIDEG==3 | HIDEG==7 ~ 2, 
                                             HIDEG==4 | HIDEG==5 | HIDEG==6 ~ 3, TRUE~ 0))
LONGWT_PANEL21 <- LONGWT_PANEL21 %>% 
                  mutate(EDUCATION=case_when(HIDEG==1 | HIDEG==2 | HIDEG==8 ~ 1, 
                                             HIDEG==3 | HIDEG==7 ~ 2, 
                                             HIDEG==4 | HIDEG==5 | HIDEG==6 ~ 3, TRUE~ 0))
LONGWT_PANEL22 <- LONGWT_PANEL22 %>% 
                  mutate(EDUCATION=case_when(HIDEG==1 | HIDEG==2 | HIDEG==8 ~ 1, 
                                             HIDEG==3 | HIDEG==7 ~ 2, 
                                             HIDEG==4 | HIDEG==5 | HIDEG==6 ~ 3, TRUE~ 0))
LONGWT_PANEL23 <- LONGWT_PANEL23 %>% 
                  mutate(EDUCATION=case_when(HIDEG==1 | HIDEG==2 | HIDEG==8 ~ 1, 
                                             HIDEG==3 | HIDEG==7 ~ 2, 
                                             HIDEG==4 | HIDEG==5 | HIDEG==6 ~ 3, TRUE~ 0))

############################
## COMBINE ALL YEARS #######
############################

#Extract variables of interest
var_id <- c('DUPERSID', 'PANEL', 'VARPSU', 'VARSTR', 'Year', 'YEARIND')
var_wt <- c('LONGWT')
var_dem <- c('AGEY1X', 'SEX', 'RACETHX', 'INSURCY1', 'INSCOVY1', 'MCAIDY1', 'EDUCATION', 'MARRYY1X', 'POVCATY1', 'FAMINCY1')
var_diag <- c('DIABDXY1','DIABAGY1', 'MIDXY1', 'MIAGY1', 'STRKDXY1', 'STRKAGY1')
var_com <- c(names(MedicalConditions_Grouped_2011)[-1], 'Charlson')
var_sf6d <- c('ADDAYA4', 'ADDOWN4', 'ADMALS4', 'ADNRGY4', 'ADPAIN4', 'ADPWLM4', 'ADSOCA4')
var_cost <- c('TOTSLFY2', 'TOTEXPY2')

LONGWT_PANEL16 <- LONGWT_PANEL16 %>% select(var_id, var_wt, var_dem, var_diag, var_com, var_sf6d, var_cost)
LONGWT_PANEL17 <- LONGWT_PANEL17 %>% select(var_id, var_wt, var_dem, var_diag, var_com, var_sf6d, var_cost)
LONGWT_PANEL18 <- LONGWT_PANEL18 %>% select(var_id, var_wt, var_dem, var_diag, var_com, var_sf6d, var_cost)
LONGWT_PANEL19 <- LONGWT_PANEL19 %>% select(var_id, var_wt, var_dem, var_diag, var_com, var_sf6d, var_cost)
LONGWT_PANEL20 <- LONGWT_PANEL20 %>% select(var_id, var_wt, var_dem, var_diag, var_com, var_sf6d, var_cost)
LONGWT_PANEL21 <- LONGWT_PANEL21 %>% select(var_id, var_wt, var_dem, var_diag, var_com, var_sf6d, var_cost)
LONGWT_PANEL22 <- LONGWT_PANEL22 %>% select(var_id, var_wt, var_dem, var_diag, var_com, var_sf6d, var_cost)
LONGWT_PANEL23 <- LONGWT_PANEL23 %>% select(var_id, var_wt, var_dem, var_diag, var_com, var_sf6d, var_cost)

#Create one dataset with all panels & years
Bind_Data <- rbind(LONGWT_PANEL16, LONGWT_PANEL17, LONGWT_PANEL18, LONGWT_PANEL19, LONGWT_PANEL20, LONGWT_PANEL21, LONGWT_PANEL22, LONGWT_PANEL23)
Bind_Data <- Bind_Data %>% filter(YEARIND == 1) #Take only those with responses in both years

######################
## COST + INFLATION ##
######################

#Inflate
Bind_Data$Family_Total <- Bind_Data$TOTSLFY2 * inflation[match(Bind_Data$Year,inflation$Year),"rateCPI"]
Bind_Data$AllPayers_Total <- ((Bind_Data$TOTEXPY2-Bind_Data$TOTSLFY2)*inflation[match(Bind_Data$Year,inflation$Year),"ratePCE_health"]) + (Bind_Data$TOTSLFY2*inflation[match(Bind_Data$Year,inflation$Year),"rateCPI"])

#Nonzero expenditures
Bind_Data <- Bind_Data %>% mutate(nonzero=ifelse(AllPayers_Total==0,0,1))
Bind_Data$nonzero<-relevel(factor(Bind_Data$nonzero), ref="0")
table(Bind_Data$nonzero)

#Nonzero OOP expenditures
Bind_Data <- Bind_Data %>% mutate(nonzero_OOP=ifelse(Family_Total==0,0,1))
Bind_Data$nonzero_OOP<-relevel(factor(Bind_Data$nonzero_OOP), ref="0")
table(Bind_Data$nonzero_OOP)

###########################################
## PRIORITY CONDITIONS: SURVEY RESPONSES ##
###########################################

#Survey priority conditions: time period between age at time of survey and age of diagnosis
Bind_Data$DIABAGY1[Bind_Data$DIABAGY1 < 0] <- NA
Bind_Data$MIAGY1[Bind_Data$MIAGY1 < 0] <- NA
Bind_Data$STRKAGY1[Bind_Data$STRKAGY1 < 0] <- NA
Bind_Data$AGEY1X_DIABAGED = Bind_Data$AGEY1X - Bind_Data$DIABAGY1 #Diabetes
Bind_Data$AGEY1X_MIAGED = Bind_Data$AGEY1X - Bind_Data$MIAGY1 #Myocardial Infarction 
Bind_Data$AGEY1X_STRKAGED = Bind_Data$AGEY1X - Bind_Data$STRKAGY1 #Stroke/TIA

#Add yes/no values for priority conditions - based on Age Diagnosis
Bind_Data$PCDiabetes = ifelse(Bind_Data$AGEY1X_DIABAGED == 0 | Bind_Data$AGEY1X_DIABAGED == 1, 1, ifelse(Bind_Data$AGEY1X_DIABAGED > 1, 2, 0))
Bind_Data$PCMI = ifelse(Bind_Data$AGEY1X_MIAGED == 0 | Bind_Data$AGEY1X_MIAGED == 1, 1, ifelse(Bind_Data$AGEY1X_MIAGED > 1, 2, 0))
Bind_Data$PCStroke = ifelse(Bind_Data$AGEY1X_STRKAGED == 0 | Bind_Data$AGEY1X_STRKAGED == 1, 1, ifelse(Bind_Data$AGEY1X_STRKAGED > 1, 2, 0))

#Survey priority conditions - based on Yes/No responses
Bind_Data$DIABDXY1[Bind_Data$DIABDXY1 < 0] <- NA
Bind_Data$MIDXY1[Bind_Data$MIDXY1 < 0] <- NA
Bind_Data$STRKDXY1[Bind_Data$STRKDXY1 < 0] <- NA
Bind_Data$PCDiabetesDX = ifelse(Bind_Data$DIABDXY1==1, 1, 0) #Diabetes
Bind_Data$PCMIDX = ifelse(Bind_Data$MIDXY1==1, 1, 0) #Myocardial Infarction
Bind_Data$PCStrokeDX = ifelse(Bind_Data$STRKDXY1==1, 1, 0) #Stroke/TIA

#############################################
## PRIORITY CONDITIONS: MEDICAL CONDITIONS ##
#############################################

# HF - based on Medical Conditions
Bind_Data$HF_baseline <- Bind_Data$MedicalCondition_HF_Grouped
Bind_Data$HF_baseline <- ifelse(is.na(Bind_Data$MedicalCondition_HF_Grouped)==T, 0, Bind_Data$HF_baseline)
summary(Bind_Data$HF_baseline); table(Bind_Data$HF_baseline); table(Bind_Data$MedicalCondition_HF_Grouped,useNA="always")

# Diabetes - based on Medical Conditions
Bind_Data$DIAB_baseline <- Bind_Data$MedicalCondition_DM_Grouped
Bind_Data$DIAB_baseline <- ifelse(is.na(Bind_Data$MedicalCondition_DM_Grouped)==T, 0, Bind_Data$DIAB_baseline)
summary(Bind_Data$DIAB_baseline); table(Bind_Data$DIAB_baseline); table(Bind_Data$MedicalCondition_DM_Grouped,useNA="always")

# Stroke - based on Medical Conditions
Bind_Data$STRK_baseline <- Bind_Data$MedicalCondition_STRK_Grouped
Bind_Data$STRK_baseline <- ifelse(is.na(Bind_Data$MedicalCondition_STRK_Grouped)==T, 0, Bind_Data$STRK_baseline); table(Bind_Data$MedicalCondition_STRK_Grouped,useNA="always")
summary(Bind_Data$STRK_baseline); table(Bind_Data$STRK_baseline)

# MI - based on Medical Conditions
Bind_Data$MI_baseline <- Bind_Data$MedicalCondition_MI_Grouped
Bind_Data$MI_baseline <- ifelse(is.na(Bind_Data$MedicalCondition_MI_Grouped)==T, 0, Bind_Data$MI_baseline); table(Bind_Data$MedicalCondition_MI_Grouped,useNA="always")
summary(Bind_Data$MI_baseline); table(Bind_Data$MI_baseline)

#########################
## PRIORITY CONDITIONS ##
#########################

#Combine responses from Medical Conditions + Survey Responses
Bind_Data <- Bind_Data %>% mutate(HF_yn = HF_baseline,
                                  DIAB_yn = ifelse(DIAB_baseline==1 | PCDiabetesDX==1, 1, 0),
                                  MI_yn = ifelse(MI_baseline==1 | PCMIDX==1, 1, 0),
                                  STRK_yn = ifelse(STRK_baseline==1 | PCStrokeDX==1, 1, 0))

#####################
## SF-6D RESPONSES ##
#####################

#SF-6D (SF-12V2)
Bind_Data$ADDAYA4[Bind_Data$ADDAYA4 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADPWLM4[Bind_Data$ADPWLM4 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADMALS4[Bind_Data$ADMALS4 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADPAIN4[Bind_Data$ADPAIN4 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADNRGY4[Bind_Data$ADNRGY4 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADDOWN4[Bind_Data$ADDOWN4 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADSOCA4[Bind_Data$ADSOCA4 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included

#Recode 5 item response in version2 to 2 item response of version1
Bind_Data$ADPWLM4_Recode = ifelse(Bind_Data$ADPWLM4 > 0 & Bind_Data$ADPWLM4 < 5, 1, ifelse(Bind_Data$ADPWLM4 == 5, 2, 0))
Bind_Data$ADMALS4_Recode = ifelse(Bind_Data$ADMALS4 > 0 & Bind_Data$ADMALS4 < 5, 1, ifelse(Bind_Data$ADMALS4 == 5, 2, 0))

Bind_Data$SFPhys = ifelse(Bind_Data$ADDAYA4 == 3, 1, ifelse(Bind_Data$ADDAYA4 == 2, 2, ifelse(Bind_Data$ADDAYA4 == 1, 3, 0)))
Bind_Data$SFRole = ifelse(Bind_Data$ADPWLM4_Recode == 2 & Bind_Data$ADMALS4_Recode == 2, 1, ifelse(Bind_Data$ADPWLM4_Recode == 1 & Bind_Data$ADMALS4_Recode == 2, 2, ifelse(Bind_Data$ADPWLM4_Recode == 2 & Bind_Data$ADMALS4_Recode == 1, 3, ifelse(Bind_Data$ADPWLM4_Recode == 1 & Bind_Data$ADMALS4_Recode == 1, 4, 0))))
Bind_Data$SFSocial = ifelse(Bind_Data$ADSOCA4 == 5, 1, ifelse(Bind_Data$ADSOCA4 == 4, 2, ifelse(Bind_Data$ADSOCA4 == 3, 3, ifelse(Bind_Data$ADSOCA4 == 2, 4, ifelse(Bind_Data$ADSOCA4 == 1, 5, 0)))))
Bind_Data$SFPain = Bind_Data$ADPAIN4
Bind_Data$SFMental = ifelse(Bind_Data$ADDOWN4 == 5, 1, ifelse(Bind_Data$ADDOWN4 == 4, 2, ifelse(Bind_Data$ADDOWN4 == 3, 3, ifelse(Bind_Data$ADDOWN4 == 2, 4, ifelse(Bind_Data$ADDOWN4 == 1, 5, 0)))))
Bind_Data$SFVital = Bind_Data$ADNRGY4

#Create Domains for SF-6D
Bind_Data$pf6d12 = ifelse(Bind_Data$SFPhys == 3, -.045, 0)
Bind_Data$rf6d12 = ifelse(Bind_Data$SFRole >= 2 & Bind_Data$SFRole <= 4, -.063, 0)
Bind_Data$s6d12 = ifelse(Bind_Data$SFSocial == 2, -.063, ifelse(Bind_Data$SFSocial == 3, -.066, ifelse(Bind_Data$SFSocial == 4, -.081, ifelse(Bind_Data$SFSocial == 5, -.093, 0))))
Bind_Data$bp6d12 = ifelse(Bind_Data$SFPain == 3, -.042, ifelse(Bind_Data$SFPain == 4, -.077, ifelse(Bind_Data$SFPain == 5, -.137, 0)))
Bind_Data$v6d12 = ifelse(Bind_Data$SFVital >= 2 & Bind_Data$SFVital <= 4, -.078, ifelse(Bind_Data$SFVital == 5, -.106, 0))
Bind_Data$mh6d12 = ifelse(Bind_Data$SFMental == 2 | Bind_Data$SFMental == 3, -.059, ifelse(Bind_Data$SFMental == 4, -.113, ifelse(Bind_Data$SFMental == 5, -.134, 0)))
Bind_Data$most = ifelse(Bind_Data$SFPhys == 3 | Bind_Data$SFRole >= 3 | Bind_Data$SFPain >= 4 | Bind_Data$SFVital >= 4 | Bind_Data$SFSocial >= 4 | Bind_Data$SFMental >= 4, -.077, 0)

#Obtain final score
Bind_Data$sf6d = 1 + Bind_Data$pf6d12 + Bind_Data$rf6d12 + Bind_Data$s6d12 + Bind_Data$bp6d12 + Bind_Data$v6d12 + Bind_Data$mh6d12 + Bind_Data$most
summary(Bind_Data$sf6d)

################################
## BIND DATA VARIABLE CLASSES ##
################################

# Age
Bind_Data$AGEY1X[Bind_Data$AGEY1X == -1] <- NA
Bind_Data$AGEY1X_Classes <- factor(ifelse(Bind_Data$AGEY1X >= 0 & Bind_Data$AGEY1X <= 4, 1, ifelse(Bind_Data$AGEY1X >= 5 & Bind_Data$AGEY1X <= 17, 2, ifelse(Bind_Data$AGEY1X >= 18 & Bind_Data$AGEY1X <= 44, 3, ifelse(Bind_Data$AGEY1X >= 45 & Bind_Data$AGEY1X <= 54, 4, ifelse(Bind_Data$AGEY1X >= 55 & Bind_Data$AGEY1X <= 64, 5, ifelse(Bind_Data$AGEY1X >= 65  & Bind_Data$AGEY1X <= 74, 6, ifelse(Bind_Data$AGEY1X >=75, 7, 0))))))), 
                                  labels = c("Ages 0-4", "Ages 5-17", "Ages 18-44", "Ages 45-54", "Ages 55-64", "Ages 65-74", "Ages 75+"))
table(Bind_Data$AGEY1X_Classes, useNA="always")

# Sex
Bind_Data$SEX_Classes <- factor(Bind_Data$SEX, labels = c("Male", "Female"))
table(Bind_Data$SEX_Classes, useNA="always")

# Race/Ethnicity (remove Other/Multiple race)
table(Bind_Data$RACETHX, useNA="always")
Bind_Data$RACETHX_Classes <- factor(ifelse(Bind_Data$RACETHX==1,2, ifelse(Bind_Data$RACETHX==2,1, ifelse(Bind_Data$RACETHX==3,3, ifelse(Bind_Data$RACETHX==4,4,5)))),
                                    labels = c("Non-Hispanic White", "Hispanic", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other Race or Multiple Race"))
table(Bind_Data$RACETHX_Classes, useNA="always")

# Marital Status
table(Bind_Data$MARRYY1X, useNA="always")
Bind_Data$MARRYY1X[Bind_Data$MARRYY1X < 0] <- NA
Bind_Data$MARRYY1X_Classes <- factor(ifelse(Bind_Data$MARRYY1X==1, 1 , ifelse(Bind_Data$MARRYY1X >= 2 & Bind_Data$MARRYY1X <= 4, 2, 3)), 
                                     labels = c("Married", "Widowed/divorced/separated", "Never married"))
table(Bind_Data$MARRYY1X_Classes, useNA="always")

# Full Year Insurance Coverage Status
table(Bind_Data$INSURCY1, useNA="always")
Bind_Data$INSURCY1[Bind_Data$INSURCY1 < 0] <- NA
Bind_Data$INSURC_Classes <- factor(ifelse(Bind_Data$INSURCY1 >= 4 & Bind_Data$INSURCY1 <= 6 , 2, 
                                   ifelse(Bind_Data$INSURCY1 == 1 | (Bind_Data$INSURCY1==8 & Bind_Data$INSCOVY1==1) , 1, 
                                   ifelse((Bind_Data$INSURCY1==2 & Bind_Data$MCAIDY1==1) | (Bind_Data$INSURCY1==8 & Bind_Data$INSCOVY1==2 & Bind_Data$MCAIDY1==1), 3, 
                                   ifelse(Bind_Data$INSURCY1 == 3 | Bind_Data$INSURCY1 == 7, 4,
                                   ifelse((Bind_Data$INSURCY1==2 & Bind_Data$MCAIDY1!=1) | (Bind_Data$INSURCY1==8 & !(Bind_Data$INSCOVY1%in%c(1,2))) | (Bind_Data$INSURCY1==8 & Bind_Data$INSCOVY1==2 & Bind_Data$MCAIDY1!=1), 5, 0))))), 
                                   labels=c("Private", "Medicare", "Medicaid", "Uninsured", "Other"))
table(Bind_Data$INSURC_Classes, useNA="always")

#Education (Highest Degree)
table(Bind_Data$EDUCATION, useNA="always")
Bind_Data$EDUCATION[Bind_Data$EDUCATION==0] <- NA
Bind_Data$EDUCATION_Classes <- factor(Bind_Data$EDUCATION, labels = c("Less Than HS", "HS/Other", "College/Higher"))
table(Bind_Data$EDUCATION_Classes, useNA="always")

#Charlson Comorditiy Index
summary(Bind_Data$Charlson)
Bind_Data$Charlson[is.na(Bind_Data$Charlson)] <- 0
Bind_Data$Charlson_Classes_0_1 <- factor(ifelse(Bind_Data$Charlson == 0, 0, 1), labels=c("0", "1+"))
table(Bind_Data$Charlson_Classes_0_1, useNA="always")

#Income
table(Bind_Data$POVCATY1, useNA="always")
Bind_Data$POVCATY1[Bind_Data$POVCATY1 < 0] <- NA
Bind_Data$POVCAT_Classes <- factor(Bind_Data$POVCATY1, labels = c("Poor/Negative","Near Poor","Low Income", "Middle Income","High Income"))
table(Bind_Data$POVCAT_Classes, useNA="always")

###########################
## SAVE BIND_DATA OBJECT ##
###########################

#Save for future use
#saveRDS(Bind_Data, "../data/cleaned/Bind_Data_2022_03_10.rds")

#To do: Uncomment to skip running all of the above
#Bind_Data  <- readRDS("../data/cleaned/Bind_Data_2022_03_10.rds")

#########################
## EXPLORE FREQUENCIES ##
#########################

#Subset by age (>=) and race/ethnicity (remove 'Other')
Bind_Data_subset <- Bind_Data %>% filter(AGEY1X>=45) %>% filter(RACETHX!=5)

#Primary Conditions: Medical Conditions file
table(Bind_Data_subset$Year, HF=Bind_Data_subset$HF_baseline, useNA="always")
table(Bind_Data_subset$Year, DIAB=Bind_Data_subset$DIAB_baseline, useNA="always")
table(Bind_Data_subset$Year, MI=Bind_Data_subset$MI_baseline, useNA="always")
table(Bind_Data_subset$Year, STRK=Bind_Data_subset$STRK_baseline, useNA="always")

#Primary Conditions: Survey Response
table(Bind_Data_subset$Year, DIAB=Bind_Data_subset$PCDiabetesDX, useNA="always")
table(Bind_Data_subset$Year, MI=Bind_Data_subset$PCMIDX, useNA="always")
table(Bind_Data_subset$Year, STRK=Bind_Data_subset$PCStrokeDX, useNA="always")

#Primary Conditions: Either/or using survey and med cond
table(Bind_Data_subset$Year, HF=Bind_Data_subset$HF_yn, useNA="always")
table(Bind_Data_subset$Year, DIAB=Bind_Data_subset$DIAB_yn, useNA="always")
table(Bind_Data_subset$Year, MI=Bind_Data_subset$MI_yn, useNA="always")
table(Bind_Data_subset$Year, STRK=Bind_Data_subset$STRK_yn, useNA="always")

#Remaining variables
summary(Bind_Data_subset$AGEY1X)
table(Bind_Data_subset$SEX_Classes, useNA="always")
table(Bind_Data_subset$RACETHX_Classes, useNA="always")
table(Bind_Data_subset$MARRYY1X_Classes, useNA="always") #3 NA
table(Bind_Data_subset$INSURC_Classes, useNA="always")
table(Bind_Data_subset$EDUCATION_Classes, useNA="always") #514 NA
table(Bind_Data_subset$Charlson_Classes_0_1, useNA="always")

#Outcomes
summary(Bind_Data_subset$sf6d)
table(Bind_Data_subset$nonzero, useNA="always")
summary(Bind_Data_subset$AllPayers_Total)

#Prepare data for Charlson analysis
charlson_variables <- c('DUPERSID', 'PANEL', 'VARPSU', 'VARSTR', 'Year', 'LONGWT', 'Charlson_Classes_0_1', 
                        'DIAB_yn', 'HF_yn', 'MI_yn', 'STRK_yn', 
                        'AGEY1X', 'SEX_Classes', 'RACETHX_Classes', 'INSURC_Classes', 'EDUCATION_Classes')
Bind_Data_Charlson <- Bind_Data_subset[,charlson_variables]
#write.csv(Bind_Data_Charlson, '../data/cleaned/MEPS_subset_for_CCI_2022-03-10.csv', row.names=FALSE)
#saveRDS(Bind_Data_Charlson, file='../data/cleaned/MEPS_subset_for_CCI_2022-03-10.Rds')

###################
## SURVEY DESIGN ##
###################

n_years = length(unique(Bind_Data$Year))
n_years

Bind_Data$LONGWT_n_years <- Bind_Data$LONGWT/n_years #LONGWT = Longitudinal Weights to weight patients for the 8 years of data 
Bind_Data$LONGWT_n_years <- ifelse(is.na(Bind_Data$LONGWT_n_years)==T,0,Bind_Data$LONGWT_n_years)

mepsdsgn = svydesign(id = ~VARPSU,
                     strata = ~VARSTR,
                     weights = ~LONGWT_n_years,
                     data = Bind_Data,
                     nest = TRUE)

#Subset by age and race/ethnicity
mepsdsgn_subset <- subset(mepsdsgn, AGEY1X>=45 & RACETHX!=5)

####################
## MODELING SF-6D ##
####################

#Note: Treat year as factor
#Fit model: initial terms
sf6d_model <- svyglm(sf6d ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                       as.factor(Year) + 
                       AGEY1X + I(AGEY1X^2) +
                       SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes + 
                       Charlson_Classes_0_1, design= mepsdsgn_subset)

#LRT for age
regTermTest(sf6d_model, ~AGEY1X, method="LRT", df=NULL)
regTermTest(sf6d_model, ~I(AGEY1X^2), method="LRT", df=NULL)

#Final model: keep age-squared
round(sf6d_model$coefficients, 2)
round(confint(sf6d_model), 2)
#summary(sf6d_model)

###################
## MODELING COST ##
###################

#Part 1: 
#Fit initial terms
cost_twopart1 <- svyglm(nonzero ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                          as.factor(Year) + 
                          AGEY1X + I(AGEY1X^2) + 
                          SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes + 
                          Charlson_Classes_0_1,
                        design=mepsdsgn_subset, family=binomial(link="logit"))

#LRT for age
regTermTest(cost_twopart1, ~AGEY1X, method="LRT", df=NULL)
regTermTest(cost_twopart1, ~I(AGEY1X^2), method="LRT", df=NULL)

#Final model: remove age-squared
cost_twopart1 <- svyglm(nonzero ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                          as.factor(Year) + 
                          AGEY1X + 
                          SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes +
                          Charlson_Classes_0_1,
                        design=mepsdsgn_subset, family=binomial(link="logit"))
format(round(exp(cost_twopart1$coefficients), digits=2), scientific=F)
round(exp(confint(cost_twopart1)), digits=2)
#summary(cost_twopart1)
#confint(cost_twopart1)
#length(predict(cost_twopart1))

#Part 2: 
mepsdsgntwopart_nonzero <- mepsdsgn_subset %>% subset(nonzero=="1")

#Fit initial terms
cost_twopart2 <- svyglm(AllPayers_Total ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                          as.factor(Year) + 
                          AGEY1X + I(AGEY1X^2) + 
                          SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes +
                          Charlson_Classes_0_1,
                        design=mepsdsgntwopart_nonzero, family=Gamma(link="log"))

#LRT for age 
regTermTest(cost_twopart2, ~AGEY1X, method="LRT", df=NULL)
regTermTest(cost_twopart2, ~I(AGEY1X^2), method="LRT", df=NULL)

#Final model: remove age-squared
cost_twopart2 <- svyglm(AllPayers_Total ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                          as.factor(Year) + 
                          AGEY1X + 
                          SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes + 
                          Charlson_Classes_0_1,
                        design=mepsdsgntwopart_nonzero, family=Gamma(link="log"))
format(round(exp(cost_twopart2$coefficients), digits=2), scientific=F)
round(exp(confint(cost_twopart2)), digits=2)
#options(digits=10)
#summary(cost_twopart2)
#confint(cost_twopart2)

#######################
## MODELING CHARLSON ##
#######################

#Fit model: initial terms
charlson_model <- svyglm(Charlson_Classes_0_1 ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                          as.factor(Year) + 
                           AGEY1X + I(AGEY1X^2) + 
                          SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes,
                        design=mepsdsgn_subset, family=binomial(link="logit"))

#LRT for age
regTermTest(charlson_model, ~AGEY1X, method="LRT", df=NULL)
regTermTest(charlson_model, ~I(AGEY1X^2), method="LRT", df=NULL)

#Final model: keep age-squared in model
format(round(exp(charlson_model$coefficients), digits=2), scientific=F)
round(exp(confint(charlson_model)), digits=2)
#summary(charlson_model)
#confint(charlson_model)
#length(predict(charlson_model))

#Save models for future use
#save(sf6d_model, file='../data/cleaned/MEPS_model_sf6d_2022-03-10.Rdata')
#save(cost_twopart1, file='../data/cleaned/MEPS_model_cost1_2022-03-10.Rdata')
#save(cost_twopart2, file='../data/cleaned/MEPS_model_cost2_2022-03-10.Rdata')
#save(charlson_model, file='../data/cleaned/MEPS_model_charlson_2022-03-10.Rdata')

########################
## ADVANCED BOOTSTRAP ##
########################

#Bootstrap for uncertainty

#Obtain Unique combos of VARSTR + VARPSU
strpsu_combos <- Bind_Data %>% 
                 distinct(VARSTR, VARPSU) %>% 
                 arrange(VARSTR) %>% select(VARSTR, VARPSU)

#Set number bootstraps
n_boot <- 1000

#Initialize matrices to hold bootstrapped coefficients
coefs_sf6d <- matrix(ncol=n_boot, nrow=25)
coefs_cost1 <- matrix(ncol=n_boot, nrow=24)
coefs_cost2 <- matrix(ncol=n_boot, nrow=24)
coefs_charlson <- matrix(ncol=n_boot, nrow=24)

set.seed(47)
for(i in 1:n_boot) {
  
  bootstrap_wts <- data.frame()
  
  for(j in unique(strpsu_combos$VARSTR)) {
    
    #PSUs within stratum j
    psu <- strpsu_combos %>% filter(VARSTR==j)
    n_psu <- nrow(psu)
    
    #Sample n-1 PSUs
    boot_psu <- sample(psu$VARPSU, size=(n_psu-1), replace=TRUE)
    
    #Bootstrap weights
    weights <- data.frame(table(boot_psu))
    names(weights) <- c('VARPSU', 'freq')
    psu <- merge(psu, weights, by='VARPSU', all.x=TRUE)
    psu$freq <- ifelse(is.na(psu$freq)==T, 0, psu$freq)
    psu$weight <- psu$freq * (n_psu/(n_psu-1))
    
    bootstrap_wts <- rbind(bootstrap_wts, psu)
  }
  
  #Obtain bootstrap weight
  bootstrap_dat <- merge(Bind_Data, bootstrap_wts, by=c('VARPSU', 'VARSTR'), all.x=TRUE)
  bootstrap_dat$bootstrap_wt <- bootstrap_dat$LONGWT * bootstrap_dat$weight
  bootstrap_dat$bootstrap_wt_n_years <- bootstrap_dat$bootstrap_wt/n_years
  
  mepsdsgn_i = svydesign(id = ~VARPSU,
                         strata = ~VARSTR,
                         weights = ~bootstrap_wt_n_years,
                         data = bootstrap_dat,
                         nest = TRUE)
  
  #Subset by age and race/ethnicity
  mepsdsgn_subset_i <- subset(mepsdsgn_i, AGEY1X>=45 & RACETHX!=5)
  
  #Fit SF-6D model
  beta_sf6d_i <- svyglm(sf6d ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                          as.factor(Year) + 
                          AGEY1X + I(AGEY1X^2) + 
                          SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes + 
                          Charlson_Classes_0_1, 
                        design=mepsdsgn_subset_i)
  
  #Fit Cost Part 1 model
  beta_cost1_i <- svyglm(nonzero ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                           as.factor(Year) + 
                           AGEY1X + 
                           SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes + 
                           Charlson_Classes_0_1,
                         design=mepsdsgn_subset_i, 
                         family=binomial(link="logit"))
  
  #Fit Cost Part 2 model
  mepsdsgntwopart_nonzero_i <- mepsdsgn_subset_i %>% subset(nonzero=="1")
  beta_cost2_i <- svyglm(AllPayers_Total ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                           as.factor(Year) + 
                           AGEY1X + 
                           SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes + 
                           Charlson_Classes_0_1,
                         design=mepsdsgntwopart_nonzero_i, 
                         family=Gamma(link="log"))
  
  #Fit Charlson model
  beta_charlson_i <- svyglm(Charlson_Classes_0_1 ~ DIAB_yn + HF_yn + MI_yn + STRK_yn + 
                             as.factor(Year) + 
                              AGEY1X + I(AGEY1X^2) + 
                             SEX_Classes + RACETHX_Classes + INSURC_Classes + EDUCATION_Classes,
                           design=mepsdsgn_subset_i,
                           family=binomial(link="logit"))
  
  #Store all coefficients
  coefs_sf6d[,i] <- beta_sf6d_i$coefficients
  coefs_cost1[,i] <- beta_cost1_i$coefficients
  coefs_cost2[,i] <- beta_cost2_i$coefficients
  coefs_charlson[,i] <- beta_charlson_i$coefficients

  print (i)
}

#Transpose matrix results
coefs_sf6d <- t(coefs_sf6d)
coefs_cost1 <- t(coefs_cost1)
coefs_cost2 <- t(coefs_cost2)
coefs_charlson <- t(coefs_charlson)

#Add original model coefficients
matrix_sf6d <- rbind(coefs_sf6d, coef(sf6d_model))
matrix_cost1 <- rbind(coefs_cost1, coef(cost_twopart1))
matrix_cost2 <- rbind(coefs_cost2, coef(cost_twopart2))
matrix_charlson <- rbind(coefs_charlson, coef(charlson_model))

#Save matrices as R objects
#save(matrix_sf6d, file='../data/cleaned/MEPS_bootstrap_matrix_sf6d_2022-03-15.Rdata')
#save(matrix_cost1, file='../data/cleaned/MEPS_bootstrap_matrix_cost1_2022-03-15.Rdata')
#save(matrix_cost2, file='../data/cleaned/MEPS_bootstrap_matrix_cost2_2022-03-15.Rdata')
#save(matrix_charlson, file='../data/cleaned/MEPS_bootstrap_matrix_charlson_2022-03-15.Rdata')

#Save matrices as CSVs
#write.csv(matrix_sf6d, '../data/cleaned/MEPS_bootstrap_matrix_sf6d_2022-03-15.csv', row.names=FALSE)
#write.csv(matrix_cost1, '../data/cleaned/MEPS_bootstrap_matrix_cost1_2022-03-15.csv', row.names=FALSE)
#write.csv(matrix_cost2, '../data/cleaned/MEPS_bootstrap_matrix_cost2_2022-03-15.csv', row.names=FALSE)
#write.csv(matrix_charlson, '../data/cleaned/MEPS_bootstrap_matrix_charlson_2022-03-15.csv', row.names=FALSE)

