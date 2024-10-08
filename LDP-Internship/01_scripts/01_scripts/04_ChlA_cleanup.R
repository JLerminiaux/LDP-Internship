#new Baulch data: ChlA

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

ChlA_2014 <- read_excel("data/KD St.Denis Data/SDWS - 2014 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "ChlA")
ChlA_2015 <- read_excel("data/KD St.Denis Data/SDWS - 2015 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "ChlA")
ChlA_2016 <- read_excel("data/KD St.Denis Data/SDWS - 2016 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "ChlA")
ChlA_2017 <- read_excel("data/KD St.Denis Data/SDWS - 2017 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "ChlA")
ChlA_2019 <- read_excel("data/KD St.Denis Data/SDWS - 2019 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "ChlA")
ChlA_2020 <- read_excel("data/KD St.Denis Data/SDWS - 2020 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "ChlA")
ChlA_2021 <- read_excel("data/KD St.Denis Data/SDWS - 2021 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "ChlA")
ChlA_2022 <- read_excel("data/KD St.Denis Data/SDWS - 2022 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "ChlA")

#------------------------------------------------------------------------
#fix 2014 so all columns match
ChlA_2014 <- ChlA_2014 %>%
  rename(RunDate = `File Name`)
ChlA_2014$RunDate <- gsub(".*", "NA", ChlA_2014$RunDate)
ChlA_2014$RunDate <- as.Date(ChlA_2014$RunDate, format = "%Y%m%d")
ChlA_2014$`%Error` <- as.numeric(ChlA_2014$`%Error`)
ChlA_2014$`Rep #` <- as.numeric(ChlA_2014$`Rep #`)
ChlA_2014$`Average` <- as.numeric(ChlA_2014$`Average`)
ChlA_2014$Date <- as.Date(ChlA_2014$Date, format = "%Y%m%d")

#------------------------------------------------------------------------
#location names for 2017 and earlier need to be updated to pond names only
#also 2019 and 2021

ChlA_2014$Location <- sub("SDWS ", "", ChlA_2014$Location)

# Remove the space between number and letter in each column
ChlA_2014$Location[] <- lapply(ChlA_2014$Location, function(x) gsub(" ", "", x))
ChlA_2014$Location <- as.character(ChlA_2014$Location)
ChlA_2015$Location <- sub("SDWS ", "", ChlA_2015$Location)
ChlA_2015$Location <- sub("P", "", ChlA_2015$Location)
ChlA_2016$Location <- sub("SDWS P", "", ChlA_2016$Location)
ChlA_2016$Location <- sub("SDWS ", "", ChlA_2016$Location)
ChlA_2017$Location <- sub("SD", "", ChlA_2017$Location)
#2017 notes also need to be formatted as characters

ChlA_2017$Notes <- as.character(ChlA_2017$Notes)

#------------------------------------------------------------------------
#2019 & 2021 location name changes
ChlA_2019$Location[] <- lapply(ChlA_2019$Location, function(x) gsub(" ", "", x))
ChlA_2019$Location <- as.character(ChlA_2019$Location)

ChlA_2021$Location[] <- lapply(ChlA_2021$Location, function(x) gsub(" ", "", x))
ChlA_2021$Location <- as.character(ChlA_2021$Location)

#2022 includes replicate in location/ID, but also have replicate column, so unnecessary
ChlA_2022$Location <- gsub(".*P([0-9]+).*", "\\1", ChlA_2022$Location )
ChlA_2022$Location[ChlA_2022$Location == "SDWS P 1 - R1"] <- "1"
ChlA_2022$Location[ChlA_2022$Location == "01"] <- "1"

#------------------------------------------------------------------------
#date formats to change: 2015 (+ run date), 
ChlA_2015$Date <- as.Date(ChlA_2015$Date, format = "%B %d %Y")
ChlA_2015$Date <- as.Date(ChlA_2015$Date, format = "%Y-%m-%d")

ChlA_2015 <- ChlA_2015 %>%
  rename(RunDate = `RunDate (yyyymmdd)`)
ChlA_2015$RunDate <- as.Date(ChlA_2015$RunDate, format = "%Y%m%d")

#all the abs for 2016 have "e" numbers instead of fully written out like other datasets
ChlA_2016$`Abs @ 750` <- as.numeric(ChlA_2016$`Abs @ 750`)
ChlA_2016$`Abs @ 665` <- as.numeric(ChlA_2016$`Abs @ 665`)
ChlA_2016$`Abs @ 649` <- as.numeric(ChlA_2016$`Abs @ 649`)

ChlA_2016$`%Error` <- as.numeric(ChlA_2016$`%Error`) #format to be samae for merging

#------------------------------------------------------------------------
#make sure all dates are formatted as such
ChlA_2016$Date <- as.Date(ChlA_2016$Date, format = "%Y-%m-%d")
ChlA_2017$Date <- as.Date(ChlA_2017$Date, format = "%Y-%m-%d")
ChlA_2019$Date <- as.Date(ChlA_2019$Date, format = "%Y-%m-%d")
ChlA_2020$Date <- as.Date(ChlA_2020$Date, format = "%Y-%m-%d")
ChlA_2021$Date <- as.Date(ChlA_2021$Date, format = "%Y-%m-%d")
ChlA_2022$Date <- as.Date(ChlA_2022$Date, format = "%Y-%m-%d")

ChlA_2016$RunDate <- as.Date(ChlA_2016$RunDate, format = "%Y-%m-%d")
ChlA_2017$RunDate <- as.Date(ChlA_2017$RunDate, format = "%Y-%m-%d")
ChlA_2019$RunDate <- as.Date(ChlA_2019$RunDate, format = "%Y-%m-%d")
ChlA_2020$RunDate <- as.Date(ChlA_2020$RunDate, format = "%Y-%m-%d")
ChlA_2021$RunDate <- as.Date(ChlA_2021$RunDate, format = "%Y-%m-%d")
ChlA_2022$RunDate <- as.Date(ChlA_2022$RunDate, format = "%Y-%m-%d")
#------------------------------------------------------------------------

#2019-2022 reformatting to match format types for variables
#2019
ChlA_2019$`%Error` <- as.numeric(ChlA_2019$`%Error`)
ChlA_2019$`Average` <- as.numeric(ChlA_2019$`Average`)
ChlA_2019$Notes <- as.character(ChlA_2019$Notes)

#2020
ChlA_2020$`%Error` <- as.numeric(ChlA_2020$`%Error`)
ChlA_2020$`Average` <- as.numeric(ChlA_2020$`Average`)
ChlA_2020$Notes <- as.character(ChlA_2020$Notes)

#2021
ChlA_2021$`%Error` <- as.numeric(ChlA_2021$`%Error`)
ChlA_2021$`Average` <- as.numeric(ChlA_2021$`Average`)
ChlA_2021$Notes <- as.character(ChlA_2021$Notes)

#------------------------------------------------------------------------
#merge data together
merged_ChlA <- merge(ChlA_2014, ChlA_2015, by = intersect(names(ChlA_2014), names(ChlA_2015)), all = TRUE)

merged_ChlA <- merge(merged_ChlA, ChlA_2016, by = intersect(names(merged_ChlA), names(ChlA_2016)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2017, by = intersect(names(merged_ChlA), names(ChlA_2017)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2019, by = intersect(names(merged_ChlA), names(ChlA_2019)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2020, by = intersect(names(merged_ChlA), names(ChlA_2020)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2021, by = intersect(names(merged_ChlA), names(ChlA_2021)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2022, by = intersect(names(merged_ChlA), names(ChlA_2022)), all = TRUE)

#------------------------------------------------------------------------
#other small clean up
#2015 two still have P attached to name
#make sure all depth are spelled same/upper lower case same
#two samples from 2015 for location 60 have "?" Ask Katy about this

merged_ChlA$`Depth (m)`[merged_ChlA$`Depth (m)` == "surface"] <- "Surface"
merged_ChlA$`Depth (m)`[merged_ChlA$`Depth (m)` == "wetland"] <- "Wetland"
merged_ChlA$`Depth (m)`[merged_ChlA$`Depth (m)` == "NA"] <- NA

#------------------------------------------------------------------------
#save it
write.csv(merged_ChlA, "merged_ChlA_clean.csv", row.names = FALSE)

