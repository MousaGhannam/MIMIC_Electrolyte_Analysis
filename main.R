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

# Extract all lab events for Ca, Phosphate, Mg, K 
LabEventsCa = dbGetQuery(con, "SELECT le.itemid, le.hadm_id, le.subject_id, d.label, d.fluid, d.category, le.value, le.valuenum, le.valueuom, le.flag, le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE 'Calcium%'")
LabEventsPhos = dbGetQuery(con, "SELECT le.itemid, le.hadm_id, le.subject_id, d.label, d.fluid, d.category, le.value, le.valuenum, le.valueuom,le.flag, le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE '%phosphate%'")
LabEventsMg = dbGetQuery(con, "SELECT le.itemid, le.hadm_id, le.subject_id, d.label, d.fluid, d.category, le.value, le.valuenum, le.valueuom, le.flag, le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE '%magnesium%'")
LabEventsK = dbGetQuery(con, "SELECT le.itemid, le.hadm_id, le.subject_id, d.label, d.fluid, d.category, le.value, le.valuenum, le.valueuom, le.flag, le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE '%potassium%'")
datetime = dbGetQuery(con, "SELECT le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE 'Calcium%'")
#Looking at all the lab values 
#Red bar is the mean, blue bars are the normal value thresholds 
thresholdsCa <- c(8.6,10.3)
ggplot(LabEventsCa, aes(x=valuenum)) + 
  geom_histogram(binwidth = 0.2, colour = "black", fill = "white") + 
  xlim(4,14) + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept= thresholdsCa,  color="blue", linetype="dashed", size=1) + labs(title = "Calcium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

thresholdsMg <- c(1.8, 2.5)
ggplot(LabEventsMg, aes(x=valuenum)) + 
  geom_histogram(binwidth = 0.05, colour = "black", fill = "white") + 
  xlim(0,5) + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept= thresholdsMg,  color="blue", linetype="dashed", size=1) + labs(title = "Magnesium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

thresholdsK <- c(3.6, 5.2)
LabEventsKSerum <- LabEventsK %>%
  filter(grepl('Chemistry', .$category, ignore.case = TRUE))
LabEventsKWB <- LabEventsK %>%
  filter(!grepl('Chemistry', .$category, ignore.case = TRUE)) + labs(title = "Potassium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

#Examining serum potassium versus whole blood. I read that whole blood can have falsely high values, due to RBC hemolysis. 
ggplot(LabEventsK, aes(x = valuenum, fill = category)) + geom_histogram(binwidth = 0.1) + xlim(1,8) + labs(title = "Potassium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

thresholdsPhos <- c(2.5,4.5)
ggplot(LabEventsPhos, aes(x=valuenum)) + 
  geom_histogram(binwidth = 0.1, colour = "black", fill = "white") + 
  xlim(0,9) + 
  geom_vline(aes(xintercept= mean(valuenum, na.rm = T)),  color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept= thresholdsP,  color="blue", linetype="dashed", size=1)

#Can look exclusively at abnormal values
ggplot(LabEventsK, aes(x = valuenum, fill = flag)) + geom_histogram(binwidth = 0.1) + xlim(1,8) + labs(title = "Potassium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")
ggplot(LabEventsCa, aes(x = valuenum, fill = flag)) + geom_histogram(binwidth = 0.1) + xlim(4,14) + labs(title = "Calcium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

#Examining prescriptions given 
prescriptionsCa <- dbGetQuery(con, "SELECT * FROM prescriptions WHERE drug ILIKE 'Calcium%' ORDER BY drug")

levels(factor(prescriptionsCa$drug))

#Frequencies of different calcium repletions
freqDrugCa <- prescriptionsCa %>% 
  count(., drug, sort = TRUE)

#Frequencies of different calcium routes
freqRouteCa <- prescriptionsCa %>% 
  count(., route, sort = TRUE)

sum(is.na(LabEventsCa$hadm_id)) #Amount of Ca lab events with NO hadm_id
sum(!is.na(LabEventsCa$hadm_id)) #Amount of Ca lab events WITH hadm_id
LabEventsCa$charttime[1:20]

preCalciumRepletions <- LabEventsCa %>%
  left_join(., LabEventsCa, by = "hadm_id")
ggplot(prescriptionsCa, aes(x=fct_infreq(drug))) + geom_bar() #Visualizing the table

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
  
tbl_mimic(prescriptions) %>%
  filter(str_detect(tolower(drug), "calcium")) %>%
  select(subject_id, hadm_id, startdate, enddate, drug, route) %>%
  print() -> prescriptionsCaTib

allLabEvents %>%
  inner_join(codesCa, by = "itemid") %>%
  inner_join(prescriptionsCaTib, by = "hadm_id") %>%
  print() -> fullCaSet 

dfJoinedCa <- collect(fullCaSet)

dfJoinedCa %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 &  recentRepletion <= 24*3600) %>%
  ggplot(aes(x = valuenum, fill = flag)) + geom_histogram(binwidth = 0.1) + xlim(4,15) + labs(title = "Calcium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

CaRepletionRanges <- dfJoinedCa %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  mutate(belowNormal = valuenum < 8.6) %>%
  mutate(withinNormal = valuenum >= 8.6 & valuenum <= 10.33) %>%
  mutate(aboveNormal = valuenum > 10.3) %>%
  count(withinNormal, aboveNormal, belowNormal)

  
CaRepletionRanges %>% 
  mutate(repletionFrac = (n / sum(CaRepletionRanges$n[1:3])) * 100 )


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
      

#PHOSPHATE ANALYSIS
tbl_mimic(d_labitems) %>%
  filter(label %ilike% "%phosphate%") %>%
  select(itemid, label, fluid, category) %>%
  print() -> codesPhos

tbl_mimic(prescriptions) %>%
  filter(drug %ilike% "%potassium%") %>%
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

dfJoinedPhos %>%
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
  print() -> codesPhos

tbl_mimic(prescriptions) %>%
  filter(drug %ilike% "%magnesium%") %>%
  select(subject_id, hadm_id, startdate, enddate, drug, route) %>%
  print() -> prescriptionsPhosTib

allLabEvents %>%
  inner_join(codesMg, by = "itemid") %>%
  inner_join(prescriptionsMgTib, by = "hadm_id") %>%
  print() -> fullMgSet 

dfJoinedMg <- collect(fullMgSet)

dfJoinedMg %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  ggplot(aes(x = valuenum, fill = flag)) + geom_histogram(binwidth = 0.1) + xlim(0,5) + labs(title = "Magnesium Lab Values", x = "Lab Value mEq/L", y = "Lab Draws")

kRepletionRanges <- dfJoinedK %>%
  mutate(recentRepletion = charttime - startdate) %>%
  filter(recentRepletion >= 12*3600 & recentRepletion <= 24*3600) %>%
  mutate(belowNormal = valuenum < 1.8) %>%
  mutate(withinNormal = valuenum >= 1.8 & valuenum <= 2.5) %>%
  mutate(aboveNormal = valuenum > 2.5) %>%
  count(withinNormal, aboveNormal, belowNormal)

MgRepletionRanges %>% 
  mutate(repletionFrac = (n / sum(MgRepletionRanges$n[1:3])) * 100 )

