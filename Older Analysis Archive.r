# Extract all lab events for Ca, Phosphate, Mg, K 
LabEventsCa = dbGetQuery(con, "SELECT le.itemid, le.hadm_id, le.subject_id, d.label, d.fluid, d.category, le.value, le.valuenum, le.valueuom, le.flag, le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE 'Calcium%'")
LabEventsPhos = dbGetQuery(con, "SELECT le.itemid, le.hadm_id, le.subject_id, d.label, d.fluid, d.category, le.value, le.valuenum, le.valueuom,le.flag, le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE '%phosphate%'")
LabEventsMg = dbGetQuery(con, "SELECT le.itemid, le.hadm_id, le.subject_id, d.label, d.fluid, d.category, le.value, le.valuenum, le.valueuom, le.flag, le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE '%magnesium%'")
LabEventsK = dbGetQuery(con, "SELECT le.itemid, le.hadm_id, le.subject_id, d.label, d.fluid, d.category, le.value, le.valuenum, le.valueuom, le.flag, le.charttime FROM labevents le LEFT JOIN d_labitems d ON le.itemid = d.itemid WHERE d.label ILIKE '%potassium%'")

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