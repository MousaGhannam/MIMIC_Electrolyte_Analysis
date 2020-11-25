# Load Libraries ------------------------------------------------------------------------------------------------------------------------------------------
library("RPostgreSQL")
library("ggplot2")
library("tidyverse")
library("dbplyr")
library("lubridate")
library("data.table")
library("dtplyr")
library("ggpubr")

# Connect to RDB ------------------------------------------------------------------------------------------------------------------------------------

# load the PostgreSQL driver
drv <- dbDriver("Postgres")

# create a connection to the postgres database
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

caregivers <- tbl_mimic(caregivers)
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
itemidsCa <- lab_idsCa$itemid[c(4)] #Do we keep "Calcium, total" (#5)? Or no...
#Drugs to include for phosphate (tentative)
itemidsPh <- lab_idsPh$itemid[c(2)]
#Drugs to include for magnesium (tentative)
itemidsMg <- lab_idsMg$itemid[c(1)]
#Drugs to include for potassium (tentative)
itemidsK <- lab_idsK$itemid[c(1,4)]

#List of all of the selected lab values
labevents %>% 
  filter(itemid %in% itemidsCa) %>%
  inner_join(select(tbl_mimic(d_labitems),-loinc_code, -row_id ), by = "itemid") %>%
  select(subject_id, hadm_id, itemid, charttime, valuenum, valueuom, label, flag, fluid, category) -> labEventsCa

labevents %>% 
  filter(itemid %in% itemidsPh) %>%
  inner_join(select(tbl_mimic(d_labitems),-loinc_code, -row_id ), by = "itemid") %>%
  select(subject_id, hadm_id, itemid, charttime, valuenum, valueuom, label, flag, fluid, category) -> labEventsPh

labevents %>% 
  filter(itemid %in% itemidsMg) %>%
  inner_join(select(tbl_mimic(d_labitems),-loinc_code, -row_id ), by = "itemid") %>%
  select(subject_id, hadm_id, itemid, charttime, valuenum, valueuom, label, flag, fluid, category) -> labEventsMg

labevents %>% 
  filter(itemid %in% itemidsK)  %>%
  inner_join(select(tbl_mimic(d_labitems),-loinc_code, -row_id ), by = "itemid") %>%
  select(subject_id, hadm_id, itemid, charttime, valuenum, valueuom, label, flag, fluid, category) -> labEventsK


# Drug Lists ----------------------------------------------------------------------------------------------------------------------------------------------
#Load all of the electrolyte repletions
electrolyte_search <- c("calcium", "magnesium", "m phosphate", "potassium")
electrolyte_names <-  c("calcium", "magnesium", "phosphate", "potassium")

get_prescription_basic <- function(electrolyte){
  tbl_mimic(prescriptions) %>%
    filter(str_detect(tolower(drug), electrolyte))
}

repletion_list <- lapply(electrolyte_search, get_prescription_basic)
names(repletion_list) <- electrolyte_names
lapply(repletion_list, count) #Count each of the lists

#Frequencies of different electrolyte drug prescriptions
getFreq <- function(myData){ 
  myData %>% 
    count(., drug, route, sort = TRUE) %>%
    print(n=Inf) 
}
#List of all frequencies for each electrolyte repletion
electrolyte_drug_freq <- lapply(repletion_list, getFreq)

#Calcium
collect(electrolyte_drug_freq$calcium) -> freqDrugCa 
freqDrugCa %>% print(n = Inf)
#Phosphate
collect(electrolyte_drug_freq$phosphate) -> freqDrugPh #all are valid
#Magnesium
collect(electrolyte_drug_freq$magnesium) -> freqDrugMg 
freqDrugMg %>% print(n = Inf) #Magnesium sulfide, (1-5)
#Potassium
collect(electrolyte_drug_freq$potassium) -> freqDrugK 
freqDrugK %>% print(n = Inf) #All Postassium chloride, IV/VO, phosphate too 

#Drugs to include for calcium (tentative)
includePrescCa <- freqDrugCa$drug[c(1:5,6,7,10,12,14)]
#Drugs to include for phosphate (tentative)
includePrescPh <- freqDrugPh$drug
#Drugs to include for magnesium (tentative)
includePrescMg <- freqDrugMg$drug[c(1:9)]
#Drugs to include for potassium (tentative)
includePrescK <- freqDrugK$drug[c(1:15,18:26,29:32)]


#Calcium
repletion_list$calcium %>%
  filter(drug %in% includePrescCa) %>% #Only include certain repletions
  select(-gsn, -starts_with("form"), -dose_unit_rx, -dose_val_rx, -prod_strength, -starts_with("drug_name_gen"),-row_id, subject_id, hadm_id, startdate,
         -drug_name_poe, -ndc, drug_type, -enddate) %>%
  inner_join(labEventsCa, by = c("hadm_id", "subject_id")) %>%
  select(-icustay_id, -drug_type, -route, -itemid, -valueuom, subject_id, -fluid, -category) -> repletion_list$calcium

#Magnesium
repletion_list$magnesium %>%
  filter(drug %in% includePrescMg) %>% #Only include certain repletions
  select(-gsn, -starts_with("form"), -dose_unit_rx, -dose_val_rx, -prod_strength, -starts_with("drug_name_gen"),-row_id, subject_id, hadm_id, startdate,
         -drug_name_poe, -ndc, drug_type, -enddate) %>%
  inner_join(labEventsMg, by = c("hadm_id", "subject_id")) %>%
  select(-icustay_id, -drug_type, -route, -itemid, -valueuom, subject_id, -fluid, -category) -> repletion_list$magnesium

#Phosphate
repletion_list$phosphate %>%
  filter(drug %in% includePrescPh) %>% #Only include certain repletions
  select(-gsn, -starts_with("form"), -dose_unit_rx, -dose_val_rx, -prod_strength, -starts_with("drug_name_gen"),-row_id, subject_id, hadm_id, startdate,
         -drug_name_poe, -ndc, drug_type, -enddate) %>%
  inner_join(labEventsPh, by = c("hadm_id", "subject_id")) %>%
  select(-icustay_id, -drug_type, -route, -itemid, -valueuom, subject_id, -fluid, -category) -> repletion_list$phosphate 

#Potassium
repletion_list$potassium %>%
  filter(drug %in% includePrescK) %>% #Only include certain repletions
  select(-gsn, -starts_with("form"), -dose_unit_rx, -dose_val_rx, -prod_strength, -starts_with("drug_name_gen"),-row_id, subject_id, hadm_id, startdate,
         -drug_name_poe, -ndc, drug_type, -enddate) %>%
  inner_join(labEventsK, by = c("hadm_id", "subject_id")) %>%
  select(-icustay_id, -drug_type, -route, -itemid, -valueuom, subject_id, -fluid, -category) -> repletion_list$potassium

#Put these tables in memory
collect(repletion_list$calcium) -> allRepletionsCa
collect(repletion_list$magnesium) -> allRepletionsMg
collect(repletion_list$phosphate) -> allRepletionsPh
collect(repletion_list$potassium) -> allRepletionsK

#Get all of the repletions 0-24 hours before and after a lab draw 

allRepletionsCa %>%
  mutate(recentRepletion = startdate - charttime ) %>%
  filter(recentRepletion >= -24*3600, recentRepletion <= 24*3600) %>%
  mutate(preVsPost = case_when(
    recentRepletion > 0 & recentRepletion <= 24*3600 ~ "pre-repletion", 
    recentRepletion < 0 & recentRepletion >= -24*3600 ~"post-repletion",
    recentRepletion < -24*3600 | recentRepletion > 24*3600 ~"notRepletion",
    recentRepletion == 0 ~ "noDiff")) %>%
  mutate(repletionRange = case_when(valuenum < 8.6 ~ "below normal", valuenum >= 8.6 & valuenum <= 10.3 ~ "normal range", valuenum > 10.3 ~ "above normal")) 

  
#allRepletionsCa %>% distinct(subject_id, hadm_id) -> allRepletionsCa
allRepletionsMg %>%
  mutate(recentRepletion = startdate - charttime ) %>%
  filter(recentRepletion >= -24*3600 & recentRepletion <= 24*3600) %>%
  mutate(preVsPost = case_when(
    recentRepletion > 0 & recentRepletion <= 24*3600 ~ "pre-repletion", 
    recentRepletion < 0 & recentRepletion >= -24*3600 ~"post-repletion",
    recentRepletion < -24*3600 | recentRepletion > 24*3600 ~"notRepletion",
    recentRepletion == 0 ~ "noDiff")) %>%
  mutate(repletionRange = case_when(valuenum < 1.8 ~ "below normal", valuenum >= 1.8 & valuenum <= 2.5 ~ "normal range", valuenum > 2.5 ~ "above normal"))-> recentRepletionsMg

#allRepletionsMg %>% distinct(hadm_id, drug, startdate) -> allRepletionsMg %>% distinct(hadm_id, drug) 
    
allRepletionsPh %>%
  mutate(recentRepletion = startdate - charttime ) %>%
  filter(recentRepletion >= -24*3600 & recentRepletion <= 24*3600) %>%
  mutate(preVsPost = case_when(
    recentRepletion > 0 & recentRepletion <= 24*3600 ~ "pre-repletion", 
    recentRepletion < 0 & recentRepletion >= -24*3600 ~"post-repletion",
    recentRepletion < -24*3600 | recentRepletion > 24*3600 ~"notRepletion",
    recentRepletion == 0 ~ "noDiff")) %>%
  mutate(repletionRange = case_when(valuenum < 2.5 ~ "below normal", valuenum >= 2.5 & valuenum <= 4.5 ~ "normal range", valuenum > 4.5 ~ "above normal")) -> recentRepletionsPh

#allRepletionsPh %>% distinct(hadm_id, drug, startdate) -> allRepletionsPh

allRepletionsK %>%
  mutate(recentRepletion = startdate - charttime) %>%
  filter(recentRepletion >= -24*3600 & recentRepletion <= 24*3600) %>%
  mutate(preVsPost = case_when(
    recentRepletion > 0 & recentRepletion <= 24*3600 ~ "pre-repletion", 
    recentRepletion < 0 & recentRepletion >= -24*3600 ~"post-repletion",
    recentRepletion < -24*3600 | recentRepletion > 24*3600 ~"notRepletion",
    recentRepletion == 0 ~ "noDiff")) %>%
  mutate(repletionRange = case_when(valuenum < 8.6 ~ "below normal", valuenum >= 8.6 & valuenum <= 10.3 ~ "normal range", valuenum > 10.3 ~ "above normal"))-> recentRepletionsK

allRepletionsK %>%
  mutate(recentRepletion = startdate - charttime) %>%
  filter(recentRepletion >= -24*3600 & recentRepletion <= 24*3600) %>%
  mutate(preVsPost = case_when(
    recentRepletion > 0 & recentRepletion <= 24*3600 ~ "pre-repletion", 
    recentRepletion < 0 & recentRepletion >= -24*3600 ~"post-repletion",
    recentRepletion < -24*3600 | recentRepletion > 24*3600 ~"notRepletion",
    recentRepletion == 0 ~ "noDiff")) %>%
  mutate(repletionRange = case_when(valuenum < 8.6 ~ "below normal", valuenum >= 8.6 & valuenum <= 10.3 ~ "normal range", valuenum > 10.3 ~ "above normal"))-> recentRepletionsK

#allRepletionsK %>% distinct(hadm_id, drug, startdate) -> allRepletionsK

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
icd9_codes_table <- plyr::ldply(icd9_codes, data.frame)

#Takes the compilation of CSVs in their original state, formats them, and extracts the ICD9 Code
getCodes <- function(codeName){
  icd9_codes_table %>%
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
hadm_id_List <- lapply(icd9codesList, get_hadm_IDs)


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

names(hadm_id_List)
hadm_id_List[["pediatrics"]] = hadm_id_pediatrics #Add to the list
#Filter out patients with MDRD below 30 
d_labitems %>%
  filter(str_detect(tolower(label), "gfr") |str_detect(tolower(label), "mdrd")) %>%
  select(itemid) %>%
  collect() -> MDRD_Code

labevents %>%
  filter(itemid == !!MDRD_Code$itemid) %>%
  select(valuenum) %>%
  filter(!is.na(valuenum)) %>%
  collect() -> MDRD

hadm_id_table <- plyr::ldply(hadm_id_List, data.frame)

#Use distinct() to make sure there are no overlaps 
hadm_id_table %>% distinct(hadm_id) -> distinct_hadm_id

#Perform Ca Exclusions
allRepletionsCa %>%
  count(drug, sort = TRUE) -> preExclusionCa

allRepletionsCa %>%
  anti_join(hadm_id_table, by = "hadm_id") -> postExclusionCa
#Perform Ph Exclusions
allRepletionsPh %>%
  count(drug, sort = TRUE) -> preExclusionPh

allRepletionsPh %>%
  anti_join(hadm_id_table, by = "hadm_id") -> postExclusionPh
#Perform Mg Exclusions
allRepletionsMg %>%
  count(drug, sort = TRUE) -> preExclusionMg

allRepletionsMg %>%
  anti_join(hadm_id_table, by = "hadm_id") -> postExclusionMg
#Perform K Exclusions
allRepletionsK %>%
  count(drug, sort = TRUE) -> preExclusionK

k_pre_post_repletions_MV_CV %>%
  anti_join(hadm_id_table, by = "hadm_id") -> postExclusionK


# Visualize Post-exclusion repletions ---------------------------------------------------------------------------------------------------------------------
#Pre-Repletion Values
thresholdsCa <- c(8.6,10.3)
ggplot(postExclusionCa, aes(x=valuenum)) + 
  geom_histogram(binwidth = 0.05, colour = "black", fill = "white") + 
  xlim(0,3) + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept= thresholdsCa,  color="blue", linetype="dashed", size=1) + labs(title = "Calcium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

thresholdsMg <- c(1.8, 2.5)
ggplot(postExclusionMg, aes(x=valuenum)) + 
  geom_histogram(binwidth = 0.05, colour = "black", fill = "white") + 
  xlim(0,8) + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept= thresholdsMg,  color="blue", linetype="dashed", size=1) + labs(title = "Magnesium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

thresholdsK <- c(3.6, 5.2)
ggplot(postExclusionK, aes(x=valuenum)) + 
  geom_histogram(binwidth = 0.05, colour = "black", fill = "white") + 
  xlim(0,8) + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept= thresholdsK,  color="blue", linetype="dashed", size=1) + labs(title = "Potassium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

thresholdsPhos <- c(2.5,4.5)
ggplot(postExclusionPh, aes(x=valuenum)) + 
  geom_histogram(binwidth = 0.1, colour = "black", fill = "white") + 
  xlim(0,9) + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept= thresholdsPhos,  color="blue", linetype="dashed", size=1) + labs(title = "Phosphate Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

#Look at if people replete differently depending on the drug?
ggdensity(postExclusionCa, "valuenum", fill = "drug", palette = "jco")  + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=0.3) + 
  geom_vline(xintercept= thresholdsCa,  color="blue", linetype="dashed", size=0.3) + labs(title = "Calcium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

# Further Analysis ----------------------------------------------------------------------------------------------------------------------------------------
#Try combining data-frames and computing summary statistics by group 
postExclusionAll <- bind_rows(list(Calcium = postExclusionCa, Potassium = postExclusionK, Phosphate = postExclusionPh, Magnesium = postExclusionMg), .id = "Electrolyte")
recrepExcAll <- bind_rows(list(Calcium = recentRepletionsCa, Potassium = recentRepletionsK, Phosphate = recentRepletionsPh, Magnesium = recentRepletionsMg), .id = "Electrolyte") %>% filter(!is.na(repletionRange))
#Calcium analysis
postExclusionAll %>%
  group_by(Electrolyte, ) %>%
  summarize(n(), mean(valuenum, na.rm = TRUE)) %>%
  
postExclusionAll %>%
    filter(Electrolyte == "Calcium") %>%
    group_by(label) %>%
    summarize(n(), mean(valuenum, na.rm = TRUE))

postExclusionAll %>%
  group_by(Electrolyte,label,repletionRange, preVsPost) %>%
  summarize(n(), mean(valuenum, na.rm = TRUE))

allRepletionsK %>%
  mutate(recentRepletion = startdate - charttime) %>%
  filter(recentRepletion >= -24*3600 & recentRepletion <= 24*3600) %>%
  mutate(preVsPost = case_when(
    recentRepletion > 0 & recentRepletion <= 24*3600 ~ "pre-repletion", 
    recentRepletion < 0 & recentRepletion >= -24*3600 ~"post-repletion",
    recentRepletion < -24*3600 | recentRepletion > 24*3600 ~"notRepletion",
    recentRepletion == 0 ~ "noDiff")) %>%
  mutate(repletionRange = case_when(valuenum < 8.6 ~ "below normal", valuenum >= 8.6 & valuenum <= 10.3 ~ "normal range", valuenum > 10.3 ~ "above normal"))-> recentRepletionsK


ggdensity(postExclusionCa, "valuenum", fill = "drug", palette = "jco")

postExclusionAll %>%
  filter(Electrolyte == "Calcium") %>%
  group_by(Electrolyte) %>%
  summarize(n(), mean(valuenum, na.rm = TRUE))

recrepExcAll %>%
  group_by(Electrolyte, preVsPost) %>%
  filter(label != "Free Calcium") %>%
  summarize(n = n(), mean = mean(valuenum, na.rm = TRUE)) %>%
  ungroup() -> preVsPostSummaries

ggtexttable(test, rows = NULL, 
            theme = ttheme("mBlue"))

recrepExcAll %>%
  filter(Electrolyte == "Potassium") %>%
  group_by(hadm_id, subject_id)

recentRepletionsK %>%
filter(!is.na(preVsPost)) %>%
gghistogram(recentRepletionsK, x="valuenum", fill = "preVsPost", palette = "jco", theme=theme_classic(), binwidth = 0.2)   -> p  
ggpar(p,xlim = c(0,10))
gghistogram(recrepExcAll, x = "recentRepletion", fill = "repletionRange", palette = "jco", facet.by = "Electrolyte", ggtheme = theme_classic2()) -> g + scale_x_time() + theme(axis.text.x = element_text(angle=90))

ggpar(p = g, xticks.by = "3600")

dev.copy(png, 'density_histograms_48hr_window.png')
dev.off()

#Across electrolytes 
postExclusionAll %>%
  group_by(Electrolyte) %>%
  filter(label != "Free Calcium") %>%
  summarize(n(), mean = mean(valuenum, na.rm = TRUE)) %>%
  ungroup()

postExclusionAll %>%
  group_by(Electrolyte) %>%
  filter(label != "Free Calcium") %>%
  count(repletionRange, na.rm=TRUE) %>%
  ungroup()

postExclusionAll %>%
  group_by(Electrolyte, belowNormal) 

postExclusionAll 

postExclusionCa %>% 
  count(belowNormal, withinNormal, aboveNormal) %>%
  mutate(repletionFrac = (n / sum(postExclusionCa$n[1:3])) * 100 )


# Re-Analyzing Repletions ---------------------------------------------------------------------------------------------------------------------------------
#"Input-events" related to potassium (includes repletions and lab events potentially)
#Careview DB
tbl_mimic(d_items) %>%
  filter(label %ilike% "%potassium%" | label %ilike% "%kcl%") %>%
  filter(str_detect(tolower(dbsource), "care")) %>%
  arrange(label) %>%
  print(n=Inf) -> kCVSearch
  
#Potassium input events 
tbl_mimic(inputevents_cv) %>%
  semi_join(kCVSearch, by = "itemid") %>%
  group_by(linkorderid, hadm_id, subject_id) %>%
  mutate(starttime = min(charttime), endtime = max(charttime), 
         med.duration = endtime - starttime, start_cgid = if_else(condition = (starttime == charttime), 
         true = cgid, false =  NULL),end_cgid = if_else(condition = (endtime == charttime), true = cgid, false =  NULL)) %>% #, interval(start = starttime, end = endtime)
  ungroup() %>%
  select(hadm_id, subject_id, icustay_id, itemid, charttime, starttime, endtime, med.duration, cgid, start_cgid, end_cgid, linkorderid) %>% 
  #distinct() %>% filter(!(is.na(start_cgid) & is.na(end_cgid))) %>%
  rename(itemid.repletion = itemid, charttime.repletion = charttime) %>% 
  left_join(labEventsK, by=c("subject_id", "hadm_id")) %>%
  arrange(charttime, charttime.repletion) %>%
  mutate(recentRepletion = charttime.repletion - charttime) %>%
  collect() -> dfPotassiumMethod1

dfPotassiumMethod1 %>%
  transmute(as.numeric(med.duration)) %>%
  ggdensity(dhours(as.duration(dfPotassiumMethod1$med.duration[1:100])))

#12 Hour window
dfPotassiumMethod1 %>%
  select(-recentRepletion) %>%
  mutate(recentRepletion = charttime.repletion - charttime) %>%
  mutate(preVsPost = case_when(
    charttime == charttime.repletion  ~ "sameTime",
    (recentRepletion > 0) & (recentRepletion <= 24*3600) ~ "pre-repletion",
    (recentRepletion < 0) & (recentRepletion >= -24*3600) ~"post-repletion",
    (recentRepletion > 24*3600) | (recentRepletion < -24*3600) ~ "notRecent"))%>%
  group_by(preVsPost) %>%
  count()
  
%>% count(preVsPost)
  filter(!is.na(preVsPost), preVsPost != "notRecent") -> dfPotassiumMethod1_filtered_24HrWindow

dfPotassiumMethod1 %>%
  select(-recentRepletion) %>%
  mutate(recentRepletion = charttime.repletion - charttime) %>%
  mutate(preVsPost = case_when((charttime == charttime.repletion)  ~ "sameTime",
    (recentRepletion >= 12*3600) & (recentRepletion <= 24*3600) ~ "pre-repletion",
    (recentRepletion <= -12*3600) & (recentRepletion >= -24*3600) ~"post-repletion",
    (recentRepletion < 12*3600 & recentRepletion > 0) | (recentRepletion > -12*3600 & recentRepletion < 0)  ~ "tooRecent",
    (recentRepletion > 24*3600) | (recentRepletion < -24*3600) ~ "notRecent")) %>%
  filter(!is.na(preVsPost) | preVsPost != "notRecent") -> dfPotassiumMethod1_filtered_12HrWindow

dfPotassiumMethod1_filtered_12HrWindow %>% count(preVsPost) %>%
  ggtexttable(theme = ttheme("classic"))
dfPotassiumMethod1_filtered_12HrWindow %>% count(preVsPost) -> window12Hour
#Pie chart representing frequencies of Pre Vs Post K
slices <- window12Hour$n
lbls <- window12Hour$preVsPost
pct = round(window12Hour$n / (sum(window12Hour$n)) * 100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(x = slices,labels =  lbls, main = "Frequency of Pre and Post Repletions (K)")


dfPotassiumMethod1 %>%
  select(-recentRepletion) %>%
  mutate(recentRepletion = charttime.repletion - charttime) %>%
  mutate(preVsPost = case_when(
    charttime == charttime.repletion  ~ "sameTime",
    (recentRepletion > 0) & (recentRepletion <= 24*3600) ~ "pre-repletion",
    (recentRepletion < 0) & (recentRepletion >= -24*3600) ~"post-repletion",
    (recentRepletion > 24*3600) | (recentRepletion < -24*3600) ~ "notRecent")) %>%
  filter(!is.na(preVsPost), preVsPost != "notRecent") %>% #Filter out NAs (lab value or repletion time doesn't exist )
  count(preVsPost) -> frequenciesPreVsPostK 

ggtexttable(frequenciesPreVsPostK, theme = ttheme("classic"))
#Pie chart representing frequencies of Pre Vs Post K
slices <- frequenciesPreVsPostK$n
lbls <- frequenciesPreVsPostK$preVsPost
pct = round(frequenciesPreVsPostK$n / (sum(frequenciesPreVsPostK$n)) * 100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(x = slices,labels =  lbls, main = "Frequency of Pre and Post Repletions (K)")

#Histogram of lab values 
ggdensity(dfPotassiumMethod1_filtered_12HrWindow, x = "recentRepletion", fill = "preVsPost") + 

ggdensity(recrepExcAll, x = "recentRepletion", fill = "preVsPost", palette = "RdBu", facet.by = "Electrolyte", ggtheme = theme_classic2()) -> g + scale_x_time() + theme(axis.text.x = element_text(angle=90))

    filter(is.na(preVsPost))
  
    filter((recentRepletion > 0) & (recentRepletion < 24*3600)) %>%
    select(recentRepletion) %>%
    top_n(n = 5)
    
    
    mutate(recentestRepletion = min(recentRepletion)) %>% print(n= 100, width = Inf)
    
  rename(itemid.labV = itemid, label.labV = label, valuenum.labV = valuenum) %>%
  # distinct(charttime.repletion) %>%
  print(n=200, width=Inf)

tbl_mimic(inputevents_cv) %>%
  semi_join(kCVSearch, by = "itemid") %>%
  select(subject_id, hadm_id, itemid, charttime,cgid, linkorderid) %>%
  rename(itemid.repletion = itemid, charttime.repletion = charttime) %>% 
  left_join(labEventsK, by=c("subject_id", "hadm_id")) -> inputEvents_LEs_K

inputEvents_LEs_K %>%
  group_by(linkorderid, charttime.repletion, charttime) %>%
  summarise(recentRepletion = charttime.repletion - charttime) %>%
  mutate(recentRepletion = charttime.repletion - charttime, mostRecentRepletion = if(min))
  
%>%
  group_by(linkorderid) %>%
  mutate(recentRepletion = charttime.repletion - charttime) %>%
  filter(recentRepletion >= duration(-24*3600) & recentRepletion <= 24*3600)
  
#Filtering down chartevents
kCVSearch %>% print(n=Inf)
  filter(str_detect(tolower(linksto), "chart")) %>% print(n=Inf)
  select(itemid) %>%
  collect() -> kCVSearch_Chartevents_LEs

kCVSearch_Chartevents_LEs$itemid[c(1,2,8:10)] -> kCVSearch_Chartevents_LEs

kCVSearch %>%
  filter(str_detect(tolower(linksto), "chart")) %>% 
  select(itemid) %>%
  collect() -> kCVSearch_Chartevents_Repletions

  kCVSearch_Chartevents_Repletions$itemid[-c(1,2,8:10)] -> kCVSearch_Chartevents_Repletions

tbl_mimic(chartevents) %>%
  filter(itemid %in% kCVSearch_Chartevents_LEs) %>% #count() = 589120
  
#Cross-reference the labEvents table with the chartevents table to see if their values are the same across same hadm_ids + subject_ids!!!

tbl_mimic(chartevents) %>%
  filter(itemid %in% kCVSearch_Chartevents_Repletions) %>% #count() 12324
  count()



  
  select(-charttime, -storetime, -orderid, -rate, -rateuom, -originalamount, -amount, -amountuom, originalamountuom, -originalrate, -originalrateuom, -originalsite, -stopped, -newbottle ) %>%
  count()
  print(n=50, width=Inf)
  ungroup() %>%
  


tbl_mimic(inputevents_cv) %>%
  semi_join(kCVSearch, by = "itemid") %>%
  mutate(starttime = min(charttime), endtime = max(charttime), mutate = endtime - starttime) %>% #, interval(start = starttime, end = endtime)
  print(n=50, width=Inf)


kCVSearch%>%
  filter(str_detect(tolower(linksto), "inputevents")) %>% 
  inner_join(select(tbl_mimic(inputevents_cv), itemid, subject_id, hadm_id, charttime, originalamount, originalamountuom, originalroute), by=c("itemid")) %>%
  select(-conceptid, -param_type, -unitname, -category, -dbsource, -linksto, -abbreviation) %>%
  rename(charttimeRepletion = charttime, itemidRepletion = itemid, drugDescription = label) %>% 
  inner_join(labEventsK,by=c("hadm_id", "subject_id")) %>% # Count before inner_join = 409,190
  rename(itemid.labV = itemid, label.labV = label, valuenum.labV = valuenum) %>% #Counter after inner_join = 15,491,893, semi_join = 406,199, inner_join %>% distinct() = 15,292,990
  collect() -> kCvDf
  

# Current -----------------------------------------------------------------


kCvDf %>%
  mutate(recentRepletion = as.numeric(charttimeRepletion - charttime )) %>% 
  filter(recentRepletion <= 24*3600 & recentRepletion >= 0) %>% print(width=Inf,n = 100)
  mutate(preVsPost = case_when((charttime - charttimeRepletion) < 24*3600 & (charttime - charttimeRepletion) > 0 ~ 'repleted',
                   (charttime - charttimeRepletion) > 24*3600 & (charttime - charttimeRepletion) < 0 ~ 'repleted'))

kCvDf %>%
  mutate(recentRepletion = as.numeric(charttimeRepletion - charttime )) %>%
  filter(recentRepletion >= -24*3600 & recentRepletion <= 24*3600) %>%
  mutate(preVsPost = case_when((charttime == charttime.repletion)  ~ "sameTime",
 (recentRepletion <= 24*3600 & recentRepletion > 0) ~ "pre-repletion",
 (recentRepletion >= -24*3600) & (recentRepletion < 0) ~"post-repletion",
 (recentRepletion > 24*3600) | (recentRepletion < -24*3600) ~ "notSoonEnough"))
  select(recentRepletion, preVsPost, hadm_id, charttime, charttimeRepletion) %>%
  ggdensity("recentRepletion", fill="preVsPost", palette = "jco")
  
kCvDf
  
  
  kCvDf %>% count(preVsPost)

kCVSearch%>%
  filter(str_detect(tolower(linksto), "inputevents")) %>% 
  inner_join(select(tbl_mimic(inputevents_cv), itemid, subject_id, hadm_id, charttime, originalamount, originalamountuom, originalroute), by=c("itemid")) %>%
  select(-conceptid, -param_type, -unitname, -category, -dbsource, -linksto, -abbreviation) %>%
  rename(charttimeRepletion = charttime, itemidRepletion = itemid, drugDescription = label) %>%
  inner_join(labEventsK,by=c("hadm_id", "subject_id")) %>%# Count before inner_join = 409,190
  rename(itemid.labV = itemid, label.labV = label, valuenum.labV = valuenum) %>%
  distinct() %>%
  count()#Counter after inner_join = 15,491,893, semi_join = 406,199
  
  
  collect() -> K_CV_IEs

labEventsK %>% count() #1038686
labEventsK %>% group_by(subject_id, hadm_id, ) %>% count()
labEventsK %>%
  select(hadm_id, subject_id) %>%
  distinct() %>% print() -> labEventKAdmissions

labEventKAdmissions %>%
  inner_join()



tbl_mimic(d_items) %>%
    filter(label %ilike% "%potassium%" | label %ilike% "%kcl%") %>%
    filter(str_detect(tolower(dbsource), "meta")) %>%
    arrange(label) %>%
    print(n=Inf)
  select(itemid) %>%
  collect() -> CVKRepletionsIds  


tbl_mimic(d_items) %>%
  filter(str_detect(tolower(label), "potassium")) %>%
  inner_join(select(tbl_mimic(inputevents_mv), itemid, subject_id, hadm_id, starttime,endtime,amount, amountuom, ordercategoryname,orderid, linkorderid ), by="itemid") %>%
  filter(itemid %in% kCodesMV) %>%
  collect() -> metavisionPotassiumRepletions

tbl_mimic(d_items) %>%
  filter(str_detect(tolower(label), "potassium")) %>%
  inner_join(select(tbl_mimic(inputevents_cv), itemid, subject_id, hadm_id, charttime, originalamount, originalamountuom, originalroute), by="itemid") %>%
  filter(itemid %in% CVKRepletionsIds) %>%
  collect() -> careviewPotassiumRepletions

allRepletionsK %>% group_by(hadm_id, subject_id)

careviewPotassiumRepletions %>%
  select(itemid, label, hadm_id, charttime, subject_id) %>%
  inner_join(select(allRepletionsK,hadm_id, charttime, valuenum, flag, subject_id), by=c("hadm_id", "subject_id")) %>%
  group_by()
  mutat

tbl_mimic(d_items) %>%
  filter(label %ilike% "%potassium%" | label %ilike% "%kcl%") %>%
  filter(str_detect(tolower(dbsource), "care")) %>%
  select(itemid) %>%
  collect() -> CVKRepletionsIds
  print(n=Inf)

  CVKRepletionsIds <- CVKRepletionsIds$itemid[c(-1, -4, -6, -7, -17, -64)]


tbl_mimic(d_items) %>%
  filter(str_detect(tolower(label), "potassium")) %>%
  filter(str_detect(tolower(dbsource), "care")) %>%
  select(itemid) %>%
  collect() -> CVKRepletionsIds
CVKRepletionsIds <- CVKRepletionsIds$itemid[c(2,4,7:11,13:29)]


CVKRepletionsIds %>%
  unlist() %>%
  as.vector() -> CVKRepletionsIds


metavisionPotassiumRepletions %>%
  group_by(linkorderid) %>%
  summarize(n())

metavisionPotassiumRepletions %>%
  distinct(category) 

kCodesMV <- c("225166","222139","225925" )
tbl_mimic(d_items) %>%
  filter(itemid %in% c("211","220045"))

