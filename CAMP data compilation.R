library(tidyverse)
library(lubridate)

site_list_filename = "X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/Community Air Monitoring Project/Community Air Monitoring Sites/CAMP sites list.csv"
criteria_filename = "H:/Data/TSP PM25 MN 2013-2017.txt"
AT_filename = "X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/Community Air Monitoring Project/Air Data/AT 2013-2016.csv"



site_list = read.csv(site_list_filename, stringsAsFactors = F)

criteria = read.table(criteria_filename, sep = ",", header = T)
criteria = filter(criteria, SITE.ID %in% site_list$MPCA.ID)
criteria2 = mutate(criteria, COLLECTION.DATE = ymd(COLLECTION.DATE), ARITHMETIC.MEAN = ifelse(DAILY.CRITERIA.IND == "Y", ARITHMETIC.MEAN, NA),
                   MDL = NA, Censored = FALSE, Pollutant = ifelse(PARAMETER.CODE == 11101, "TSP", "PM2.5"), Units = "ug/m3") %>%
            select(STATE.CODE, COUNTY.CODE, SITE.ID, PARAMETER.CODE, POC, COLLECTION.DATE, DURATION.CODE, ARITHMETIC.MEAN, Pollutant, MDL, Censored, Units)


AT = read.csv(AT_filename, stringsAsFactors = F)
AT2 = filter(AT, AQS_ID %% 10000 %in% site_list$MPCA.ID) %>% arrange(AQS_ID, Param_Code, POC, ymd(Date) ) %>%
      mutate(STATE.CODE = AQS_ID %/% 10e6, COUNTY.CODE = (AQS_ID - STATE.CODE * 10e6) %/% 10e3, SITE.ID = AQS_ID - STATE.CODE * 10e6 - COUNTY.CODE * 10e3,
            ARITHMETIC.MEAN = Concentration, DURATION.CODE = 7, Censored = Concentration < MDL, COLLECTION.DATE = ymd(Date), PARAMETER.CODE = Param_Code) %>%
      select(STATE.CODE, COUNTY.CODE, SITE.ID, PARAMETER.CODE, POC, COLLECTION.DATE, DURATION.CODE, ARITHMETIC.MEAN, Pollutant, MDL, Censored, Units)

all_CAMP = rbind(criteria2, AT2) %>% arrange(STATE.CODE, COUNTY.CODE, SITE.ID, PARAMETER.CODE, POC, COLLECTION.DATE) %>%
  filter(!(SITE.ID == 7549 & Pollutant != "PM2.5"), !(SITE.ID == 7554 & Pollutant == "PM2.5") )
names(all_CAMP) = c("State Code", "County Code", "Site ID", "Parameter Code", "POC", "Date", "Duration Code", "24-Hour Average", "Pollutant", "MDL", "Censored", "Units")

write.csv(all_CAMP, "X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/Community Air Monitoring Project/Air Data/CAMP Monitoring Results 2013-2016.csv", row.names = F, na = "")
