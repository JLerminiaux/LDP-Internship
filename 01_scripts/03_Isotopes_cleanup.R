#new Baulch data: Isotopes (only 2014-2016)

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

Iso_2014 <- read_excel("data/KD St.Denis Data/SDWS - 2014 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "Isotopes")
Iso_2015 <- read_excel("data/KD St.Denis Data/SDWS - 2015 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "Isotopes")
Iso_2016 <- read_excel("data/KD St.Denis Data/SDWS - 2016 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "Isotopes")

#------------------------------------------------------------------------
# Remove the space between number and letter in each column

Iso_2014$Sample_ID <- sub("SDWS ", "", Iso_2014$Sample_ID)
Iso_2014$Sample_ID[] <- lapply(Iso_2014$Sample_ID, function(x) gsub(" ", "", x))
Iso_2014$Sample_ID <- as.character(Iso_2014$Sample_ID)


Iso_2015$Sample_ID <- sub("SDWS ", "", Iso_2015$Sample_ID)
Iso_2015$Sample_ID[] <- lapply(Iso_2015$Sample_ID, function(x) gsub(" ", "", x))
Iso_2015$Sample_ID <- as.character(Iso_2015$Sample_ID)

Iso_2016$Sample_ID <- sub("SDWS P", "", Iso_2016$Sample_ID)
Iso_2016$Sample_ID <- sub("SDNWA", "", Iso_2016$Sample_ID)
Iso_2016$Sample_ID[] <- lapply(Iso_2016$Sample_ID, function(x) gsub(" ", "", x))
Iso_2016$Sample_ID <- as.character(Iso_2016$Sample_ID)
Iso_2016$Sample_ID <- toupper(Iso_2016$Sample_ID)

#------------------------------------------------------------------------
#make sure dates are consistent

Iso_2014$`Collection Date` <- as.Date(Iso_2014$`Collection Date` , format = "%Y-%m-%d")
Iso_2015$`Collection Date` <- as.Date(Iso_2015$`Collection Date` , format = "%Y-%m-%d")
Iso_2016$`Collection Date` <- as.Date(Iso_2016$`Collection Date` , format = "%Y-%m-%d")

#------------------------------------------------------------------------
#merge data

merged_Iso <- merge(Iso_2014, Iso_2015, by = intersect(names(Iso_2014), names(Iso_2015)), all = TRUE)

merged_Iso <- merge(merged_Iso, Iso_2016, by = intersect(names(merged_Iso), names(Iso_2016)), all = TRUE)

#------------------------------------------------------------------------
#save it
write.csv(merged_Iso, "merged_Isotopes_clean.csv", row.names = FALSE)
