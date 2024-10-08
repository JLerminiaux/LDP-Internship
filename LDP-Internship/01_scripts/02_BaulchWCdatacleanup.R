#new Baulch data: sediment
#data clean up for SDNWA projecs

#------------------------------------------------------------------------
#load packages
library(readr) #csv
library(dplyr)
library(assertr) #checks for errors in data sets by restricting searches
library(lubridate) #for dates
library(sp) #for lat/long conversions
library(sf)
library(readxl) #excel
#------------------------------------------------------------------------
#load data

SDWS_2014 <- read_excel("data/KD St.Denis Data/SDWS - 2014 - Long Term Monitoring - All Data.xlsx")
SDWS_2015 <- read_excel("data/KD St.Denis Data/SDWS - 2015 - Long Term Monitoring - All Data.xlsx")
SDWS_2016 <- read_excel("data/KD St.Denis Data/SDWS - 2016 - Long Term Monitoring - All Data.xlsx")
SDWS_2017 <- read_excel("data/KD St.Denis Data/SDWS - 2017 - Long Term Monitoring - All Data.xlsx")
SDWS_2019 <- read_excel("data/KD St.Denis Data/SDWS - 2019 - Long Term Monitoring - All Data.xlsx")
SDWS_2020 <- read_excel("data/KD St.Denis Data/SDWS - 2020 - Long Term Monitoring - All Data.xlsx")
SDWS_2021 <- read_excel("data/KD St.Denis Data/SDWS - 2021 - Long Term Monitoring - All Data.xlsx")
SDWS_2022 <- read_excel("data/KD St.Denis Data/SDWS - 2022 - Long Term Monitoring - All Data.xlsx")

#------------------------------------------------------------------------
#dates are not all same format
#fix 2014, 2016

#2014 was not displaying dates correctly; manually removed 'Unknown' dates, and formatted as YYYY-MM-DD

SDWS_2016$Date <- as.Date(SDWS_2016$Date)
SDWS_2016$Date <- format(SDWS_2016$Date, "%Y-%m-%d")

#------------------------------------------------------------------------
#time: 2016, 2020; all others are NA but may still need reformat
# Add leading zeros to hour if necessary
SDWS_2016$Time <- ifelse(nchar(SDWS_2016$Time) == 3, paste0("0", SDWS_2016$Time), SDWS_2016$Time)

# Extract hour and minute parts and format as "HH:MM"
SDWS_2016$Time <- paste(substr(SDWS_2016$Time, 1, 2), ":", substr(SDWS_2016$Time, 3, 4), sep = "")

SDWS_2016$Time[SDWS_2016$Time == "NA:NA"] <- NA

# Convert datetime column to POSIXct object
SDWS_2020$Time <- as.POSIXct(SDWS_2020$Time)
# Extract only the time part
SDWS_2020$Time <- format(SDWS_2020$Time, "%H:%M")


#all other variables appear numeric/good
#------------------------------------------------------------------------
#join data;all files have same variables
# Full join on all variables
merged_SDWS <- merge(SDWS_2014, SDWS_2015, by = intersect(names(SDWS_2014), names(SDWS_2015)), all = TRUE)

#add 2016
merged_SDWS <- merge(merged_SDWS, SDWS_2016, by = intersect(names(merged_SDWS), names(SDWS_2016)), all = TRUE)

#add 2017
merged_SDWS <- merge(merged_SDWS, SDWS_2017, by = intersect(names(merged_SDWS), names(SDWS_2017)), all = TRUE)

#add 2019
merged_SDWS <- merge(merged_SDWS, SDWS_2019, by = intersect(names(merged_SDWS), names(SDWS_2019)), all = TRUE)

#add 2020
merged_SDWS <- merge(merged_SDWS, SDWS_2020, by = intersect(names(merged_SDWS), names(SDWS_2020)), all = TRUE)

#add 2021
merged_SDWS <- merge(merged_SDWS, SDWS_2021, by = intersect(names(merged_SDWS), names(SDWS_2021)), all = TRUE)

#add 2022
merged_SDWS <- merge(merged_SDWS, SDWS_2022, by = intersect(names(merged_SDWS), names(SDWS_2022)), all = TRUE)

#------------------------------------------------------------------------
#clean up pond names and dates

#remove time aspect from Date again
merged_SDWS$Date <- as.Date(merged_SDWS$Date)
merged_SDWS$Date <- format(merged_SDWS$Date, "%Y-%m-%d")

#remove spaces from pond names 
merged_SDWS$Pond <- gsub(" ", "", merged_SDWS$Pond)


#save it
write.csv(merged_SDWS, "merged_SDWS_clean.csv", row.names = FALSE)
