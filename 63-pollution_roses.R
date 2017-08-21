## Pollution roses and love poems



#Generates
pollution roses for all air toxics for a site and saves them in a folder
defined by output_path

 

#Currently does
not address POCs. Only use for single POC sites until fixed.

 

#Currently only
works for single site. Multi-site capability may be added in future.

 

#Make sure you
have all packages below installed.

 

#------------------------------------------------------------------------------------------------

 

#Enter all values
between the dashed lines

 

 

#monitoring data
must be a csv in ATDE format with columns "AQS_ID", "POC",
"Param_Code", "Date","Concentration",

  # "Null_Data_Code",
"MDL", "Pollutant", "Year", "CAS"

  # Date needs to be in yyyy-mm-dd format or
yyyymmdd

 

 

#met_data needs
to be filtered to site of interest (pull from Tableau)

 

monitoring_data_file
= ".csv" #complete path of csv file with monitoring data USE FORWARD
SLASHES /

met_data_file =
".csv" #complete path of csv file with met data USE FORWARD SLASHES /

output_path =
"" #path to output folder USE FORWARD SLASHES /

site_num =
123456789 # site number: enter complete 9 digit AQS ID without dashes

site_name =
"" #Enter site name in quotes as you want it to appear on the title
of roses use \n for new line

png_height = 600
#Image height

png_width = 600
#Image width

num_breaks = 5
#number of colors desired on pollution rose. Breaks are minimum to MDL, then
(num_breaks - 1) evenly spaced intervals from MDL to maximum value for
pollutant.

 

#-------------------------------------------------------------------------------------------------

 

library(dplyr)

library(lubridate)

library(tidyr)

library(openair)

library(reshape)

 

site_save =
gsub("[.]", "", site_name)

site_save =
gsub("\n", "", site_save)

site_save =
gsub("/", "-", site_save)

 

 

data = read.csv(monitoring_data_file,
stringsAsFactors = F)

names(data)[1:10]
<- c("AQS_ID", "POC", "Param_Code",
"Date","Concentration",

                 "Null_Data_Code",
"MDL", "Pollutant", "Year", "CAS")

data$date =
ymd(data$Date)

data$Pollutant =
gsub("/", "-", data$Pollutant) #R confuses forward slashes
for file paths when naming pngs

met_data =
read.csv(met_data_file)

 

data_site =
filter(data, AQS_ID %in% site_num & !is.na(Concentration) )

data_site =
data_site %>% group_by(Pollutant) %>% filter(max(Concentration) >
0.001 & max(Concentration) > max(MDL))

pollutants_site =
data_site %>% group_by(Pollutant) %>% summarise(MDL = max(MDL), minimum =
min(Concentration), maximum = max(Concentration))

data_site =
select(data_site, date, Pollutant, Concentration)

breaks_site =
NULL

for (i in
1:nrow(pollutants_site)) {

  breaks_site = rbind(breaks_site,
c(round_any(pollutants_site$minimum[i], 0.001, floor),

                      round_any(
c(pollutants_site$MDL[i], pollutants_site$MDL[i] + (pollutants_site$maximum[i]
- pollutants_site$MDL[i]) * (1:(num_breaks-1) / (num_breaks-1) ) ), 0.001,
ceiling ) ) )

}

  

 

 

data_site =
spread(data_site, key = Pollutant, value = Concentration)

 

 

names(met_data)[c(1,7,8)]
= c("Day","wd","ws")

met_data =
mutate(met_data, date =
paste0(Year,"/",Month,"/",Day,"
",Hour,":00"))

met_data$date =
ymd_hm(met_data$date)

met_data =
met_data[,-c(1:3, 9:11)]

met_data =
timeAverage(met_data, avg.time = "day")

met_data$date =
ymd(met_data$date)

 

data_site =
left_join(data_site, met_data, by = "date")

 

png(paste0(output_path,"/Wind
Rose for ", site_save,".png"), height=png_height,
width=png_width)

windRose(data_site,
paddle = F, breaks = 5 * 0:5, key.footer = "mph",
main=paste("Daily Average Wind Rose \n", site_name) )

dev.off()

 

for(i in
1:nrow(pollutants_site)) {

  png(paste0(output_path,"/Pollution Rose
for ", pollutants_site$Pollutant[i]," ",eight, width=png_width)

  pollutionRose(data_site, statistic =
"abs.count", pollutant = pollutants_site$Pollutant[i], breaks =
breaks_site[i,], key.footer="ug/m3", main=paste("Daily Average
Pollution Rose for", pollutants_site$Pollutant[i],"\n",
site_name) )

  dev.off()

}

site_save,".png"), height=png_h

