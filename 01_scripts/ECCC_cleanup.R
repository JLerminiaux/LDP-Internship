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

# read in ECCC hydrolab master file
ECCC_hydrolab <- read.csv("00_rawdata/VijayDriveSDNWA/ECCC_Hydrolab_1993-1999.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 

# fix date format
ECCC_hydrolab <- ECCC_hydrolab %>% mutate(Date = lubridate::ymd(Date))

# fix time format 
ECCC_hydrolab$Time <- as_hms(ECCC_hydrolab$Time)

# make sure all depths are < 0 

# make sure there's no outliers in any of the WQ columns



