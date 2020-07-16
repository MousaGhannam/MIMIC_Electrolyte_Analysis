# Load Libraries ------------------------------------------------------------------------------------------------------------------------------------------
library("RPostgreSQL")
library("ggplot2")
library("tidyverse")
library("dbplyr")
library("lubridate")
library("data.table")
library("dtplyr")

# Connect to RDB ------------------------------------------------------------------------------------------------------------------------------------

# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# create a cONnectiON to the postgres database
# set the search path to the mimiciii schema
con <- dbConnect(drv, dbname = "mimic",
                 host = "localhost", port = 5432,
                 user = "mousaghannam")
dbSendQuery(con, 'set search_path to mimiciii')

patients <- tbl(con, dbplyr::in_schema("mimiciii", "patients"))
object.size(patients)

# show a list of tables
tableList <- dbListTables(con)

#Memory conserative approaching for performing SQL queries on DB 
#(Rewrite this function to handle multiple inputs )
tbl_mimic <- function(table) {
  table <- as.character(substitute(table))
  tbl(con, dbplyr::in_schema("mimiciii", table))
}

# Pulling necessary tables (not in memory yet) ------------------------------------------------------------------------------------------------------------
labevents <- tbl_mimic(labevents)

d_labitems <- tbl_mimic(d_labitems)

d_icd_diagnoses <- tbl_mimic(d_icd_diagnoses) %>%
  select(icd9_code, short_title)

diagnoses_icd <-tbl_mimic(diagnoses_icd) %>%
  select(hadm_id, icd9_code, seq_num)

prescriptions <- tbl_mimic(prescriptions) %>%
  select(subject_id, hadm_id, startdate, enddate, drug, route)

d_icd_procedures <- tbl_mimic(d_icd_procedures)

procedures_icd <- tbl_mimic(procedures_icd)

patients <- tbl_mimic(patients)

admissions <- tbl_mimic(admissions)

#ICD-9 Codes + their descriptions
icd9_dict <- diagnoses_icd %>%
  inner_join(d_icd_diagnoses, by = "icd9_code")


# Load Electrolyte Lab Measurements -----------------------------------------------------------------------------------------------------------------------
d_labitems %>%
  filter(str_detect(tolower(label), "calcium")) %>%
  collect() -> lab_idsCa

d_labitems %>%
  filter(str_detect(tolower(label), "phosphate")) %>%
  collect() -> lab_idsPh

d_labitems %>%
  filter(str_detect(tolower(label), "magnesium")) %>%
  collect() -> lab_idsMg

d_labitems %>%
  filter(str_detect(tolower(label), "potassium")) %>%
  collect() -> lab_idsK

#Lab measurements to include
itemidsCa <- lab_idsCa$itemid[c(4)]
#Drugs to include for phosphate (tentative)
itemidsPh <- lab_idsPh$itemid[c(2,3,4)]
#Drugs to include for magnesium (tentative)
itemidsMg <- lab_idsMg$itemid[c(1,2,3)]
#Drugs to include for potassium (tentative)
itemidsK <- lab_idsK$itemid[c(1,2,3,4)]

#List of all of the selected lab values
labevents %>% 
  filter(itemid %in% itemidsCa) -> labEventsCa

labevents %>% 
  filter(itemid %in% itemidsPh) -> labEventsPh

labevents %>% 
  filter(itemid %in% itemidsMg) -> labEventsMg 

labevents %>% 
  filter(itemid %in% itemidsK) -> labEventsK

#Frequencies of different electrolyte drug prescriptions
getFreq <- function(myData){ 
  myData %>% 
    count(., drug, route, sort = TRUE) %>%
    print(n=Inf) -> x
  return(x)
}

#Calcium
collect(getFreq(repletionsCa)) -> freqDrugCa 
freqDrugCa %>% print(n = Inf)
#Phosphate
collect(getFreq(repletionsPh)) -> freqDrugPh 
#Magnesium
collect(getFreq(repletionsMg)) -> freqDrugMg 
freqDrugMg %>% print(n = Inf)
#Potassium
collect(getFreq(repletionsK)) -> freqDrugK 
freqDrugK %>% print(n = Inf)

# Drug Lists ----------------------------------------------------------------------------------------------------------------------------------------------
#Drugs to include for calcium (tentative)
includePrescCa <- freqDrugCa$drug[c(1,6,12,14)]
#Drugs to include for phosphate (tentative)
includePrescPh <- freqDrugPh$drug[c(1,2)]
#Drugs to include for magnesium (tentative)
includePrescMg <- freqDrugMg$drug[c(1,8,9,11,16,19)]
#Drugs to include for potassium (tentative)
includePrescK <- freqDrugK$drug[c(2,3,4,5,9,11,12)]

#Load all of the electrolyte repletions
electrolyte_search <- c("calcium", "magnesium", "m phosphate", "potassium")
electrolyte_names <-  c("calcium", "magnesium", "phosphate", "potassium")

get_prescription_basic <- function(electrolyte){
  tbl_mimic(prescriptions) %>%
    filter(str_detect(tolower(drug), electrolyte))
    select(drug, hadm_id, subject_id)
}

repletion_list <- lapply(electrolytes, get_prescription_basic)
names(repletion_list) <- electrolyte_names
lapply(repletion_list, count) #Count each of the lists

repletionsSkinny <- lapply(repletion_list, select(drug, hadm_id, subject_id))
#Get all of the repletions 12-24 hours after a lab draw 


# Perform Exclusions --------------------------------------------------------------------------------------------------------------------------------------
#Prescriptions
#K-wasters
prescriptions %>%
  filter( drug %ilike% "furosemide%" |  drug %ilike% "bumenatide%" |  drug %ilike% "hydrochlorothiazide%" 
          |  drug %ilike% "chlorothiazide%" |  drug %ilike% "methylchlothiazide%" |  drug %ilike% "metolazone%" 
          |  drug %ilike% "chlorthalidone%" |  drug %ilike% "ethacrynic acid%" |  drug %ilike% "torsemide%" 
          |  drug %ilike% "bendroflumethiazide%" |  drug %ilike% "polythiazide%" |  drug %ilike% "hydroflumethiazide%"
          |  drug %ilike% "acetazolamide%")

#Load up all of the relevant hadm_ids to exclude in a single table
#Pull ICD9 codes stored in CSVs from the last electrolyte paper 
setwd("~/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes/")
my_files = list.files(path = "/Users/mousaghannnam/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes/", pattern = "*.csv", full.names = FALSE)
icd9_codes <- lapply(my_files, read.csv)
names(icd9_codes) <- gsub("\\.spec.csv$", "", my_files)
icd9_codes_table <- plyr::ldply(my_codes, data.frame)


getCodes <- function(codeName){
  
  my_codes_table %>%
    filter(.id == codeName) %>%
    filter(code_standard_name == "ICD9") %>%
    select(code) %>%
    mutate_all(~str_remove_all(as.character(.x), '\\.')) %>% 
    unlist() -> codeVec
  return(codeVec)
}

#Create a running list of all ICD9 codes that we want to exclude
icd9codesList <- lapply(names(icd9_codes), getCodes)
names(icd9codesList) <- names(icd9_codes)

#Function to return ICD9 hospital admissions from ICD9 codes
get_hadm_IDs <- function(codeList){
  icd9_dict %>%
    filter(icd9_code %in% codeList) %>%
    select(hadm_id) -> hadm_id_list
  return(hadm_id_list)
}

#Create a running list of hospital admissions to exclude
hadm_id_List <- lapply(codesList, get_hadm_IDs)


#Some ICD9 codes could not be found in the previous files, or were retrieved using other methods, so we retrieved them here. 
#Transfusion of packed cells  
icd9_pRBC <- tbl_mimic(d_icd_procedures) %>%
  filter(short_title %ilike% "RBC%" | short_title %ilike% "packed%") 

tbl_mimic(procedures_icd) %>%
  filter(icd9_code == "9904") %>%
  mutate(is_pRBC = icd9_code == "9904")  -> procedures_icd

hadm_id_transfusion <- tbl_mimic(procedures_icd) %>%
  filter(icd9_code == "9904") %>%
  mutate(is_pRBC = icd9_code == "9904") %>%
  select(hadm_id) 
  
hadm_id_List[["prbc"]] = hadm_id_transfusion #Add to the list

##Nutritional deficiency 
icd9_dict %>%
  filter(short_title %ilike% "nutrition%") %>%
  distinct(icd9_code, short_title) 

#Codes for Nutritional Deficiency NEC and Nutritional Deficiency NOS
codes_ND <- c("2698", "2699")

icd9_dict %>%
  filter(icd9_code == "2698" | icd9_code == "2699") %>%
  select(hadm_id) -> hadm_id_NutDef

icd9_dict %>%
  filter(icd9_code %in% codes_ND) %>%
  mutate(is_PTN = (icd9_code == "2698" | icd9_code == "2699" ))

hadm_id_List[["nutrit-def"]] = hadm_id_NutDef #Add to the list

#Age is greater than 18
admissions %>%
  inner_join(patients, by = c("subject_id")) %>%
  select(hadm_id, admittime, dob) %>%
  print() -> all_admissions

all_admissions %>%
  dplyr::mutate(age = date_part("year", admittime) - date_part("year", dob)) %>%
  dplyr::mutate(age = age - ifelse(
    date_part("month", admittime) < date_part("month", dob) |
      (date_part("month", admittime) == date_part("month", dob) &
         date_part("day", admittime) < date_part("day", dob)
      ),
    1,
    0
  )) %>%
  filter(age < 18) %>%
  select(hadm_id) -> hadm_id_pediatrics

hadm_id_List[["pediatrics"]] = hadm_id_pediatrics #Add to the list

#Filter out patients with MDRD below 30 


hadm_id_table <- plyr::ldply(hadm_id_List, data.frame)



exclude_hadm_ids <- function(){
  
}
#Row bind all of the hadm_ids that you want to exlcude. Then, use distinct() to make sure there are no overlaps 

