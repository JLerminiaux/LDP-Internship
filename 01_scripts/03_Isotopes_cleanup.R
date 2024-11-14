################################################################################
#
# Script: Baulch data - Isotopes
# Author: Jess Lerminiaux (adapted from Erika)
# Purpose: To clean up the SDNWA water quality data to be published on DataStream
#
################################################################################

library(readr) #csv
library(dplyr)
library(assertr) #checks for errors in data sets by restricting searches
library(lubridate) #for dates
library(sp) #for lat/long conversions
library(sf)
library(readxl) #excel
library(tidyverse)

#-------------------------------------------------------------------------------

#load data
Iso_2014 <- read_excel("00_rawdata/UofR data/SDWS - 2014 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "Isotopes")
Iso_2015 <- read_excel("00_rawdata/UofR data/SDWS - 2015 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "Isotopes")
Iso_2016 <- read_excel("00_rawdata/UofR data/SDWS - 2016 - Long Term Monitoring - All Data.xlsx", 
                        sheet = "Isotopes")
Iso_2017 <- read_excel("00_rawdata/UofR data/SDWS - 2017 - Long Term Monitoring - All Data.xlsx", 
                       sheet = "Isotpes")

#-------------------------------------------------------------------------------

# fix column names in 2017 df
names(Iso_2017)[2] <- "Sample_ID"

# remove extra (all-NA) column in 2017 df
Iso_2017 <- Iso_2017[, -7]

#-------------------------------------------------------------------------------

# remove the space between number and letter in each column
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

#-------------------------------------------------------------------------------

# make sure dates are consistent
Iso_2014$`Collection Date` <- as.Date(Iso_2014$`Collection Date` , format = "%Y-%m-%d")
Iso_2015$`Collection Date` <- as.Date(Iso_2015$`Collection Date` , format = "%Y-%m-%d")
Iso_2016$`Collection Date` <- as.Date(Iso_2016$`Collection Date` , format = "%Y-%m-%d")
Iso_2017$`Collection Date` <- as.Date(Iso_2017$`Collection Date` , format = "%Y-%m-%d")

#-------------------------------------------------------------------------------

# merge data
merged_Iso <- merge(Iso_2014, Iso_2015, by = intersect(names(Iso_2014), names(Iso_2015)), all = TRUE)
merged_Iso <- merge(merged_Iso, Iso_2016, by = intersect(names(merged_Iso), names(Iso_2016)), all = TRUE)
merged_Iso <- merge(merged_Iso, Iso_2017, by = intersect(names(merged_Iso), names(Iso_2017)), all = TRUE)

#-------------------------------------------------------------------------------

# write as tidy csv
write.csv(merged_Iso, "02_tidydata/merged_Isotopes_clean.csv", row.names = FALSE)
