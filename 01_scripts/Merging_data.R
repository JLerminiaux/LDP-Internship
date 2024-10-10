################################################################################
#
# Script: LDP WQ Merging
# Author: Jess Lerminiaux
# Purpose: Merging St. Denis National Wildlife Area water quality data sets  
#          from the University of R and the University of Saskatchewan 
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

# read in data cleaned by Erika
chla <- read.csv("02_tidydata/merged_ChlA_clean.csv", na = c("", "na", "NA", "n/a", "N/A"))
isotopes <- read.csv("02_tidydata/merged_Isotopes_clean.csv", na = c("", "na", "NA", "n/a", "N/A"))
SDWS <- read.csv("02_tidydata/merged_SDWS_clean.csv", na = c("", "na", "NA", "n/a", "N/A"))
Baulch <- read.csv("02_tidydata/Baulch_WCdata_clean.csv", na = c("", "na", "NA", "n/a", "N/A"))

# fix date formats
chla <- chla %>% mutate(Date = lubridate::ymd(Date))
isotopes <- isotopes %>% mutate(Collection.Date = lubridate::ymd(Collection.Date))
SDWS <- SDWS %>% mutate(Date = lubridate::ymd(Date))
Baulch <- Baulch %>% mutate(sample_date = lubridate::ymd(sample_date))

# change column names to match SDWS
chla <- chla %>% rename(Pond = Location)
isotopes <- isotopes %>% rename(Date = Collection.Date, Pond = Sample_ID)
Baulch <- Baulch %>% rename(Date = sample_date, Pond = pond_id)


