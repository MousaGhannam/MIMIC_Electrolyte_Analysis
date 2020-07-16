# install.packages("RPostgreSQL")
library("RPostgreSQL")
library("ggplot2")
library("tidyverse")

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
tbl_mimic <- function(table) {
  table <- as.character(substitute(table))
  tbl(con, dbplyr::in_schema("mimiciii", table))
}

#CALCIUM ANALYSIS
tbl_mimic(labevents) %>% 
  select(hadm_id, itemid,charttime, valuenum, valueuom, flag) %>%
  print() -> allLabEvents

tbl_mimic(d_labitems) %>%
  filter(str_detect(tolower(label), "calcium")) %>%
  select(itemid, label, fluid, category) %>%
  print() -> codesCa

d_icd_diagnoses <- tbl_mimic(d_icd_diagnoses) %>%
  select(icd9_code, short_title)

diagnoses_icd <-tbl_mimic(diagnoses_icd) %>%
  select(hadm_id, icd9_code, seq_num)

icd9_dict <- diagnoses_icd %>%
  inner_join(d_icd_diagnoses, by = "icd9_code")

icd9_dict %>%
    filter(icd9_code == "0030")

d_icd_diagnoses %>% 
  filter(icd9_code == "94127")

setwd("~/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes/Drugs")
my_files_1 = list.files(path = "/Users/mousaghannnam/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes/Drugs", pattern = "*.csv", full.names = FALSE)
my_drugs <- lapply(my_files_1, read.csv)
names(my_drugs) <- gsub("\\.spec.csv$", "", my_files_1)

setwd("~/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes/")
my_files_2 = list.files(path = "/Users/mousaghannnam/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes/", pattern = "*.csv", full.names = FALSE)
my_codes <- lapply(my_files_2, read.csv)
names(my_codes) <- gsub("\\.spec.csv$", "", my_files_2)

my_codes$`renal-codes`

testdf = data.frame()
for(df in my_data){
  df %>%
    filter(code_standard_name == "ICD9") %>%
    select(code) %>%
    mutate_all(~as.numeric(str_remove_all(as.character(.x), '\\.'))) %>%
    
}


setwd("~/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes")
  = list.files(path = "/Users/mousaghannnam/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes/", pattern = "*.csv", full.names = FALSE)
my_data <- lapply(my_files, read.csv)
names(my_data) <- gsub("\\.spec.csv$", "", my_files)


testdf = data.frame()
for(df in my_data){
  df %>%
    filter(code_standard_name == "ICD9") %>%
    select(code) %>%
    mutate_all(~as.numeric(str_remove_all(as.character(.x), '\\.'))) 
    
}

tbl <- list.files(pattern ="*.csv", path = "/Users/mousaghannnam/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes", full.names = TRUE) %>% 
    map_df(~read_csv(., col_types = cols(.default = "c"))) 

testCode <- "941.27"
gsub(c("."),"", testCode, fixed = TRUE)

path <- "/Users/mousaghannnam/Documents/Data Science/R Projects/MIMIC_Electrolyte_Analysis/Codes/"
files <- list.files(path=path, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}



#TEST TO SEE IF USING STR DETECT VS LABEL CHANGES OUTPUT 
tbl_mimic(prescriptions) %>%
  filter(str_detect(tolower(drug), "calcium")) %>%
  select(subject_id, hadm_id, startdate, enddate, drug, route) %>%
  print() -> prescriptionsCaTib




allLabEvents %>%
  inner_join(codesCa, by = "itemid") %>%
  inner_join(prescriptionsCaTib, by = "hadm_id") %>%
  print() -> fullCaSet 

dfJoinedCa <- collect(fullCaSet)

#Visualizing for all lab values of calcium 
thresholdsCa <- c(8.6,10.3)
ggplot(dfJoinedCa, aes(x=valuenum)) + 
  geom_histogram(bins = 150, colour = "black", fill = "white") + 
  xlim(0,14) + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept= thresholdsCa,  color="blue", linetype="dashed", size=1) + labs(title = "Calcium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

#We see a histogram around ~1-2, with many lab values. Should we exlcude these? 
#Only looking at values between 0 and 5 now, so let's make a new table
allLabEvents %>%
  inner_join(codesCa, by = "itemid") %>%
  inner_join(prescriptionsCaTib, by = "hadm_id") %>%
  inner_join(icd9_dict, by = "hadm_id") %>%
  filter(valuenum > 0 & valuenum < 5) %>%
  print() -> caSetAnomalies

df_caSetAnomalies <- collect(caSetAnomalies) 

df_caSetAnomalies %>%
  ggplot(aes(x = valuenum)) + geom_histogram(binwidth = 0.1)  + labs(title = "Calcium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

dfJoinedCa %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  mutate(belowNormal = valuenum < 8.6, withinNormal = valuenum >= 8.6 & valuenum <= 10.33, aboveNormal = valuenum > 10.3)
#Faster way to identify anomalies for calcium? 
dfJoinedCa %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  filter(valuenum > 0 & valuenum < 5) %>%
  mutate(belowNormal = valuenum < 8.6, withinNormal = valuenum >= 8.6 & valuenum <= 10.33, aboveNormal = valuenum > 10.3) %>%
  select(hadm_id, subject_id)-> CaSetAnomalies

caSetAnomalies %>%
  select(hadm_id, subject_id) -> CaAnomalies_hids

filter(seq_num <= 5) %>%
  group_by(subject_id, hadm_id) %>%
  top_n(1, wt = seq_num) %>%
  ungroup() %>%
  select(subject_id, hadm_id, icd9_code, seq_num) %>%
  print() -> mi_admissions

CaAnomalies_hids %>%
  inner_join(icd9_dict, by = "hadm_id") %>%
  filter(seq_num <= 5) %>%
  group_by(subject_id, hadm_id) %>%
  arrange(desc(seq_num)) %>%
  head(1) %>%
  ungroup() %>%
  select(subject_id, hadm_id, icd9_code, seq_num) %>%
  count(short_title, sort = TRUE) %>%
  print() 
  collect() -> diagnosesAnomalies
  
  df <- data.frame(x = c(10, 4, 1, 6, 3, 1, 1))
  df %>% arrange(desc(x)) %>% head(2)
  df %>% top_n(2)
  head
#Filtering
dfJoinedCa %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 &  recentRepletion <= 24*3600) %>%
  ggplot(aes(x = valuenum, fill = flag)) + geom_histogram(binwidth = 0.1) + xlim(4,15) + labs(title = "Calcium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

CaRepletionRanges <- dfJoinedCa %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  mutate(belowNormal = valuenum < 8.6, withinNormal = valuenum >= 8.6 & valuenum <= 10.33, aboveNormal = valuenum > 10.3) %>%
  count(belowNormal, withinNormal, aboveNormal)



CaRepletionRanges %>% 
  mutate(repletionFrac = (n / sum(CaRepletionRanges$n[1:3])) * 100 )
## REASON FOR THE WEIRDNESS IN DATA IS THE LARGE AMOUNT OF VALUES AROUND ZERO 

#POTASSIUM ANALYSIS
tbl_mimic(d_labitems) %>%
  filter(label %ilike% "%potassium%") %>%
  select(itemid, label, fluid, category) %>%
  print() -> codesK

tbl_mimic(prescriptions) %>%
  filter(drug %ilike% "%potassium%") %>%
  select(subject_id, hadm_id, startdate, enddate, drug, route) %>%
  print() -> prescriptionsKTib

allLabEvents %>%
  inner_join(codesK, by = "itemid") %>%
  inner_join(prescriptionsKTib, by = "hadm_id") %>%
  print() -> fullKSet 

dfJoinedK <- collect(fullKSet)

dfJoinedK %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  ggplot(aes(x = valuenum, fill = flag)) + geom_histogram(binwidth = 0.1) + xlim(1,8) + labs(title = "Potassium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

kRepletionRanges <- dfJoinedK %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  mutate(belowNormal = valuenum < 3.6) %>%
  mutate(withinNormal = valuenum >= 3.6 & valuenum <= 5.2) %>%
  mutate(aboveNormal = valuenum > 5.2) %>%
  count(withinNormal, aboveNormal, belowNormal)

kRepletionRanges %>% 
  mutate(repletionFrac = (n / sum(kRepletionRanges$n[1:3])) * 100 )
      
#Examining serum potassium versus whole blood. I read that whole blood can have falsely high values, due to RBC hemolysis. 
dfJoinedK %>%
  ggplot(., aes(x = valuenum, fill = category)) + geom_histogram(binwidth = 0.1) + xlim(1,8) + labs(title = "Potassium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

#PHOSPHATE ANALYSIS
tbl_mimic(d_labitems) %>%
  filter(label %ilike% "%phosphate%") %>%
  select(itemid, label, fluid, category) %>%
  print() -> codesPhos

tbl_mimic(prescriptions) %>%
  filter(drug %ilike% "%phosphate%") %>%
  select(subject_id, hadm_id, startdate, enddate, drug, route) %>%
  print() -> prescriptionsPhosTib

allLabEvents %>%
  inner_join(codesK, by = "itemid") %>%
  inner_join(prescriptionsKTib, by = "hadm_id") %>%
  print() -> fullPhosSet 

dfJoinedPhos <- collect(fullPhosSet)

dfJoinedPhos %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  ggplot(aes(x = valuenum, fill = flag)) + geom_histogram(binwidth = 0.1) + xlim(0,9) + labs(title = "Phosphate Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

PhosRepletionRanges <- dfJoinedPhos %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  mutate(belowNormal = valuenum < 2.5) %>%
  mutate(withinNormal = valuenum >= 2.5 & valuenum <= 4.5) %>%
  mutate(aboveNormal = valuenum > 4.5) %>%
  count(withinNormal, aboveNormal, belowNormal)

PhosRepletionRanges %>% 
  mutate(repletionFrac = (n / sum(PhosRepletionRanges$n[1:3])) * 100 )

#MAGNESIUM ANALYSIS
tbl_mimic(d_labitems) %>%
  filter(label %ilike% "%magnesium%") %>%
  select(itemid, label, fluid, category) %>%
  print() -> codesMg

tbl_mimic(prescriptions) %>%
  filter(drug %ilike% "%magnesium%") %>%
  select(subject_id, hadm_id, startdate, enddate, drug, route) %>%
  print() -> prescriptionsMgTib

allLabEvents %>%
  inner_join(codesMg, by = "itemid") %>%
  inner_join(prescriptionsMgTib, by = "hadm_id") %>%
  print() -> fullMgSet 

dfJoinedMg <- collect(fullMgSet)

dfJoinedMg %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  ggplot(aes(x = valuenum, fill = flag)) + geom_histogram(binwidth = 0.1) + xlim(0,5) + labs(title = "Magnesium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

MgRepletionRanges <- dfJoinedMg %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  mutate(belowNormal = valuenum < 1.8) %>%
  mutate(withinNormal = valuenum >= 1.8 & valuenum <= 2.5) %>%
  mutate(aboveNormal = valuenum > 2.5) %>%
  count(withinNormal, aboveNormal, belowNormal)

MgRepletionRanges %>% 
  mutate(repletionFrac = (n / sum(MgRepletionRanges$n[1:3])) * 100 )


# Smoother Setup? -----------------------------------------------------------------------------------------------------------------------------------------

d_labitems <- tbl_mimic(d_labitems)
prescriptions <- tbl_mimic(prescriptions) %>%

allLabEvents %>%
  left_join(select(tbl_mimic(d_labitems), label, fluid, category, itemid), by = "itemid") %>%
  filter(label %ilike% "RBC%" | label %ilike% "packed%") %>%
  distinct(label)

%>%
  left_join(select(tbl_mimic(prescriptions), subject_id, hadm_id, startdate, enddate, drug, route), by  = "hadm_id") %>%
  left_join(icd9_dict, by = "hadm_id") %>%
  left_join(select(tbl_mimic(patients), subject_id, dob), by = "subject_id") %>%
  left_join(select(tbl_mimic(admissions), admission_location, admittime, subject_id), by = "subject_id") %>%
  count()
  #left_join(select(tbl_mimic(chartevents), hadm_id, cgid), by = "hadm_id") %>%
  #left_join(select(tbl_mimic(caregivers), label, description, cgid), by = "cgid") -> df_mega
  
allLabEvents %>%
  mutate(age = date_part("year", admittime) - date_part("year", dob)) %>% # under 18
  filter(age >= 18) %>%
  
  
              
  inner_join(codesCa, by = "itemid") %>%
  inner_join(prescriptionsCaTib, by = "hadm_id") %>%
  inner_join(icd9_dict, by = "hadm_id") %>%
  inner_join(se)
  print() -> labevents


  tbl_mimic(d_labitems) %>%
    filter(str_detect(tolower(label), "calcium") | str_detect(tolower(label), "phosphate") | str_detect(tolower(label), "magnesium") | str_detect(tolower(label), "phosphate"))    
    
  allLabEvents %>%
    filter(str_detect(tolower(drug), "calcium") | str_detect(tolower(drug), "phosphate") | str_detect(tolower(drug), "magnesium") | str_detect(tolower(drug), "phosphate")) %>%
    select(itemid, label, fluid, category) %>%
    print() -> codesMg
  
  tbl_mimic(prescriptions) %>%
    filter(drug %ilike% "%magnesium%") %>%
    select(subject_id, hadm_id, startdate, enddate, drug, route) %>%
    print() -> prescriptionsMgTib
  
  ##Perform filters
  tbl_mimic(d_labitems) %>%
    filter(label %ilike% "acetazolamide%") %>%
    select(itemid, label, fluid, category) %>%
    print() -> codeA

  
  as.list(unlist(my_drugs$`k-wasters`$order_name))
aList = my_drugs$`loop-diuretics`

#Filtering K-wasters
  tbl_mimic(prescriptions) %>%
    filter(!drug %ilike% "furosemide%" | !drug %ilike% "bumenatide%" | !drug %ilike% "hydrochlorothiazide%" 
           | !drug %ilike% "chlorothiazide%" | !drug %ilike% "methylchlothiazide%" | !drug %ilike% "metolazone%" 
           | !drug %ilike% "chlorthalidone%" | !drug %ilike% "ethacrynic acid%" | !drug %ilike% "torsemide%" 
           | !drug %ilike% "bendroflumethiazide%" | !drug %ilike% "polythiazide%" | !drug %ilike% "hydroflumethiazide%"
           ! !drug %ilike% "acetazolamide%" )

#Transfusion of packed cells  
d_procedures <- tbl_mimic(d_icd_procedures) %>%
  filter(short_title %ilike% "RBC%" | short_title %ilike% "packed%") 
#Packed RBCs
procedures_icd <- tbl_mimic(procedures_icd) %>%
  filter(icd9_code == "9904") %>%
  count() 

procedures_icd <- tbl_mimic(procedures_icd) %>%
  filter(icd9_code == "9904") %>%
  mutate(is_transfusion = icd9_code == "9904") %>%
  select(hadm_id)

#Total parenteral nutrition
procedures_icd <- tbl_mimic(procedures_icd) %>%
  filter(short_title %ilike% "nutrition%")
d_procedures <- tbl_mimic(d_icd_procedures) %>%
  filter(short_title %ilike% "parenteral%")


#DONT FORGET YOU CAN EASILY USE MUTATE TO SAY "If_X_DRUG". 



##Nutritional deficiency 
icd9_dict %>%
  filter(short_title %ilike% "nutrition%") %>%
  distinct(icd9_code, short_title) 

  icd9_dict %>%
    filter(icd9_code == "2698" | icd9_code == "2699") %>%
    mutate(is_PTN = (icd9_code == "2698" | icd9_code == "2699" )) %>%
    count()
    
   
  
    filter(grepl(str_c("^(", str_c(my_drugs$`k-wasters`,  collapse="|"), ")"), drug))
    filter(drug %in%  as.list(my_drugs$`k-wasters`)) %>%
    select(subject_id, hadm_id, startdate, enddate, drug, route) %>%
    print() -> acetazolamide
    
  
  