################################################################################
#
# Script: ECCC Data Cleanup
# Author: Jess Lerminiaux
# Purpose: Cleaning and Quality Checks for ECCC Data (1993-1999)
#
################################################################################

library(readr) #csv
library(dplyr)
library(assertr) #checks for errors in data sets by restricting searches
library(lubridate) #for dates
library(sp) #for lat/long conversions
library(sf)
library(readxl) #excel
library(hms) # for fixing time formats

# read in ECCC master file
ECCC <- read.csv("00_rawdata/VijayDriveSDNWA/ECCC_Hydrolab_1993-1999.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 

# fix date format
ECCC <- ECCC %>% mutate(Date = lubridate::ymd(Date))

# fix time format 
ECCC$Time <- as_hms(ECCC$Time)



