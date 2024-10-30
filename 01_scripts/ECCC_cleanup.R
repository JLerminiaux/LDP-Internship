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
library(rstatix)
library(tidyverse)

###-----------------------------------------------------------------------------
#
# Hydrolab cleanup
#
###-----------------------------------------------------------------------------

# read in ECCC hydrolab raw data
ECCC_hydro_1993 <- read.csv("00_rawdata/VijayDriveSDNWA/Hydrolab/Hydrolab 1993.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_hydro_1994 <- read.csv("00_rawdata/VijayDriveSDNWA/Hydrolab/Hydrolab 1994.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_hydro_1995 <- read.csv("00_rawdata/VijayDriveSDNWA/Hydrolab/Hydrolab 1995.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_hydro_1996 <- read.csv("00_rawdata/VijayDriveSDNWA/Hydrolab/Hydrolab 1996.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_hydro_1997 <- read.csv("00_rawdata/VijayDriveSDNWA/Hydrolab/Hydrolab 1997.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_hydro_1998 <- read.csv("00_rawdata/VijayDriveSDNWA/Hydrolab/WEVS1998 Hydrolab data.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_hydro_1999 <- read.csv("00_rawdata/VijayDriveSDNWA/Hydrolab/WEVS1999 Hydrolab data.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 

#

### Hydrolab 1993 --------------------------------------------------------------

# rename columns - 1993
names(ECCC_hydro_1993)[1] <- "Pond"
names(ECCC_hydro_1993)[2] <- "Date"
names(ECCC_hydro_1993)[3] <- "Julianday"
names(ECCC_hydro_1993)[4] <- "Temp_degC"
names(ECCC_hydro_1993)[5] <- "DO_mg.L"
names(ECCC_hydro_1993)[6] <- "SPC_mS.cm"
names(ECCC_hydro_1993)[7] <- "pH_hydro"

# remove first few rows that have no data + all-NA rows/columns
ECCC_hydro_1993 <- ECCC_hydro_1993[-c(1:7), -c(8:256)]
ECCC_hydro_1993 <- print(ECCC_hydro_1993[rowSums(is.na(ECCC_hydro_1993)) != ncol(ECCC_hydro_1993), ])

# fix date format
ECCC_hydro_1993$Date <- lubridate::ymd(ECCC_hydro_1993$Date)

# make sure all other columns are numeric
ECCC_hydro_1993$Julianday <- as.numeric(ECCC_hydro_1993$Julianday)
ECCC_hydro_1993$Temp_degC <- as.numeric(ECCC_hydro_1993$Temp_degC)
ECCC_hydro_1993$DO_mg.L <- as.numeric(ECCC_hydro_1993$DO_mg.L)
ECCC_hydro_1993$SPC_mS.cm <- as.numeric(ECCC_hydro_1993$SPC_mS.cm)
ECCC_hydro_1993$pH_hydro <- as.numeric(ECCC_hydro_1993$pH_hydro)


### Hydrolab 1994 --------------------------------------------------------------

# rename columns - 1994
names(ECCC_hydro_1994)[1] <- "Pond"
names(ECCC_hydro_1994)[2] <- "Date"
names(ECCC_hydro_1994)[3] <- "Julianday"
names(ECCC_hydro_1994)[4] <- "Temp_degC"
names(ECCC_hydro_1994)[5] <- "DO_mg.L"
names(ECCC_hydro_1994)[6] <- "SPC_mS.cm"
names(ECCC_hydro_1994)[7] <- "pH_hydro"

# remove first few rows that have no data + all-NA rows/columns
ECCC_hydro_1994 <- ECCC_hydro_1994[-c(1:7), ]
ECCC_hydro_1994 <- print(ECCC_hydro_1994[rowSums(is.na(ECCC_hydro_1994)) != ncol(ECCC_hydro_1994), ])

# fix date format
ECCC_hydro_1994$Date <- lubridate::ymd(ECCC_hydro_1994$Date)

# make sure all other columns are numeric
ECCC_hydro_1994$Julianday <- as.numeric(ECCC_hydro_1994$Julianday)
ECCC_hydro_1994$Temp_degC <- as.numeric(ECCC_hydro_1994$Temp_degC)
ECCC_hydro_1994$DO_mg.L <- as.numeric(ECCC_hydro_1994$DO_mg.L)
ECCC_hydro_1994$SPC_mS.cm <- as.numeric(ECCC_hydro_1994$SPC_mS.cm)
ECCC_hydro_1994$pH_hydro <- as.numeric(ECCC_hydro_1994$pH_hydro)


### Hydrolab 1995 --------------------------------------------------------------

# rename columns - 1995
names(ECCC_hydro_1995)[1] <- "Pond"
names(ECCC_hydro_1995)[2] <- "Date"
names(ECCC_hydro_1995)[3] <- "Julianday"
names(ECCC_hydro_1995)[4] <- "Temp_degC"
names(ECCC_hydro_1995)[5] <- "DO_mg.L"
names(ECCC_hydro_1995)[6] <- "SPC_mS.cm"
names(ECCC_hydro_1995)[7] <- "pH_hydro"

# remove first few rows that have no data + all-NA rows/columns
ECCC_hydro_1995 <- ECCC_hydro_1995[-c(1:7), ]
ECCC_hydro_1995 <- print(ECCC_hydro_1995[rowSums(is.na(ECCC_hydro_1995)) != ncol(ECCC_hydro_1995), ])

# fix date format
ECCC_hydro_1995$Date <- lubridate::ymd(ECCC_hydro_1995$Date)

# make sure all other columns are numeric
ECCC_hydro_1995$Julianday <- as.numeric(ECCC_hydro_1995$Julianday)
ECCC_hydro_1995$Temp_degC <- as.numeric(ECCC_hydro_1995$Temp_degC)
ECCC_hydro_1995$DO_mg.L <- as.numeric(ECCC_hydro_1995$DO_mg.L)
ECCC_hydro_1995$SPC_mS.cm <- as.numeric(ECCC_hydro_1995$SPC_mS.cm)
ECCC_hydro_1995$pH_hydro <- as.numeric(ECCC_hydro_1995$pH_hydro)


### Hydrolab 1996 --------------------------------------------------------------

# rename columns - 1996
names(ECCC_hydro_1996)[1] <- "Pond"
names(ECCC_hydro_1996)[2] <- "Date"
names(ECCC_hydro_1996)[3] <- "Julianday"
names(ECCC_hydro_1996)[4] <- "Temp_degC"
names(ECCC_hydro_1996)[5] <- "DO_mg.L"
names(ECCC_hydro_1996)[6] <- "SPC_mS.cm"
names(ECCC_hydro_1996)[7] <- "pH_hydro"

# remove first few rows that have no data + all-NA rows/columns
ECCC_hydro_1996 <- ECCC_hydro_1996[-c(1:7), ]
ECCC_hydro_1996 <- print(ECCC_hydro_1996[rowSums(is.na(ECCC_hydro_1996)) != ncol(ECCC_hydro_1996), ])

# fix date format
ECCC_hydro_1996$Date <- lubridate::ymd(ECCC_hydro_1996$Date)

# make sure all other columns are numeric
ECCC_hydro_1996$Julianday <- as.numeric(ECCC_hydro_1996$Julianday)
ECCC_hydro_1996$Temp_degC <- as.numeric(ECCC_hydro_1996$Temp_degC)
ECCC_hydro_1996$DO_mg.L <- as.numeric(ECCC_hydro_1996$DO_mg.L)
ECCC_hydro_1996$SPC_mS.cm <- as.numeric(ECCC_hydro_1996$SPC_mS.cm)
ECCC_hydro_1996$pH_hydro <- as.numeric(ECCC_hydro_1996$pH_hydro)


### Hydrolab merging: 1993-1996 ------------------------------------------------

# merge 1993-1996 data 
ECCC_hydro_merged <- full_join(ECCC_hydro_1993, ECCC_hydro_1994)
ECCC_hydro_merged <- full_join(ECCC_hydro_merged, ECCC_hydro_1995)
ECCC_hydro_merged <- full_join(ECCC_hydro_merged, ECCC_hydro_1996)

# add Month column
ECCC_hydro_merged$Month <- month(ECCC_hydro_merged$Date)
ECCC_hydro_merged <- ECCC_hydro_merged %>% 
  relocate(Month, .after = Date)
ECCC_hydro_merged$Month <- gsub(5,"May",ECCC_hydro_merged$Month)
ECCC_hydro_merged$Month <- gsub(6,"June",ECCC_hydro_merged$Month)
ECCC_hydro_merged$Month <- gsub(7,"July",ECCC_hydro_merged$Month)
ECCC_hydro_merged$Month <- gsub(8,"August",ECCC_hydro_merged$Month)
ECCC_hydro_merged$Month <- gsub(9,"September",ECCC_hydro_merged$Month)
ECCC_hydro_merged$Month <- gsub(10,"October",ECCC_hydro_merged$Month)

# add Year column
ECCC_hydro_merged$Year <- year(ECCC_hydro_merged$Date)
ECCC_hydro_merged <- ECCC_hydro_merged %>% 
  relocate(Year, .after = Month)

### Hydrolab 1997 --------------------------------------------------------------

# rename columns - 1997
names(ECCC_hydro_1997)[1] <- "Pond"
names(ECCC_hydro_1997)[2] <- "Month"
names(ECCC_hydro_1997)[3] <- "Temp_degC"
names(ECCC_hydro_1997)[4] <- "pH_hydro"
names(ECCC_hydro_1997)[5] <- "SPC_mS.cm"
names(ECCC_hydro_1997)[6] <- "Sal_ppt"
names(ECCC_hydro_1997)[7] <- "DO_sat"
names(ECCC_hydro_1997)[8] <- "DO_mg.L"
names(ECCC_hydro_1997)[9] <- "Redox_mV"
names(ECCC_hydro_1997)[10] <- "Depth_m"
names(ECCC_hydro_1997)[11] <- "Time_hydro"

# remove rows that have no data + all-NA rows/columns
ECCC_hydro_1997 <- ECCC_hydro_1997[-c(1:5, 309:356), -c(12:13)]
ECCC_hydro_1997 <- print(ECCC_hydro_1997[rowSums(is.na(ECCC_hydro_1997)) != ncol(ECCC_hydro_1997), ])

# remove rows that were column names
ECCC_hydro_1997 <- ECCC_hydro_1997 %>% 
  filter(!Pond == "ID")

# fix time format 
ECCC_hydro_1997$Time_hydro <- sub("(\\d{2})(\\d{2})", "\\1:\\2", ECCC_hydro_1997$Time_hydro) # add colon between HH and MM

# make sure all other columns are numeric
ECCC_hydro_1997$Temp_degC <- as.numeric(ECCC_hydro_1997$Temp_degC)
ECCC_hydro_1997$pH_hydro <- as.numeric(ECCC_hydro_1997$pH_hydro)
ECCC_hydro_1997$SPC_mS.cm <- as.numeric(ECCC_hydro_1997$SPC_mS.cm)
ECCC_hydro_1997$Sal_ppt <- as.numeric(ECCC_hydro_1997$Sal_ppt)
ECCC_hydro_1997$DO_sat <- as.numeric(ECCC_hydro_1997$DO_sat)
ECCC_hydro_1997$DO_mg.L <- as.numeric(ECCC_hydro_1997$DO_mg.L)
ECCC_hydro_1997$Redox_mV <- as.numeric(ECCC_hydro_1997$Redox_mV)
ECCC_hydro_1997$Depth_m <- as.numeric(ECCC_hydro_1997$Depth_m)

# convert depth to negative values for Data Stream template
ECCC_hydro_1997$Depth_m <- ifelse(ECCC_hydro_1997$Depth_m > 0, -1*ECCC_hydro_1997$Depth_m, ECCC_hydro_1997$Depth_m)

# add Year column and put after Month column
ECCC_hydro_1997$Year <- 1997
ECCC_hydro_1997 <- ECCC_hydro_1997 %>% 
  relocate(Year, .after = Month)


### Hydrolab 1998 --------------------------------------------------------------

# rename columns - 1998
names(ECCC_hydro_1998)[1] <- "Pond"
names(ECCC_hydro_1998)[2] <- "Temp_degC"
names(ECCC_hydro_1998)[3] <- "pH_hydro"
names(ECCC_hydro_1998)[4] <- "SPC_mS.cm"
names(ECCC_hydro_1998)[5] <- "Sal_ppt"
names(ECCC_hydro_1998)[6] <- "DO_sat"
names(ECCC_hydro_1998)[7] <- "DO_mg.L"
names(ECCC_hydro_1998)[8] <- "Redox_mV"
names(ECCC_hydro_1998)[9] <- "Depth_m"

# remove rows that have no data + all-NA rows/columns
ECCC_hydro_1998 <- ECCC_hydro_1998[-c(1:2), -c(10:15)]
ECCC_hydro_1998 <- print(ECCC_hydro_1998[rowSums(is.na(ECCC_hydro_1998)) != ncol(ECCC_hydro_1998), ])

# make Month column
ECCC_hydro_1998 <- ECCC_hydro_1998 %>%
  mutate(
    Month = str_extract(Pond, "May|June|July|August|Septembar|October"), # extract month names from Pond column + put into Month column
    Pond = str_remove(Pond, "May|June|July|August|Septembar|October"), # remove month names from Pond column
    Pond = if_else(Pond == "", NA, Pond)) # replace month names with NA
ECCC_hydro_1998 <- ECCC_hydro_1998 %>% 
  relocate(Month, .after = Pond)
ECCC_hydro_1998$Month <- gsub("Septembar","September",ECCC_hydro_1998$Month)

# fill in the month if it's NA with the one directly above it
ECCC_hydro_1998 <- ECCC_hydro_1998 %>%
  fill(Month, .direction = "down")

# fill in the pond name if it's NA with the one directly above it
ECCC_hydro_1998 <- ECCC_hydro_1998 %>%
  fill(Pond, .direction = "down")

# remove rows that were column names
ECCC_hydro_1998 <- ECCC_hydro_1998 %>% 
  filter(!Pond == "Pond")
ECCC_hydro_1998 <- ECCC_hydro_1998 %>% 
  filter(!pH_hydro == "units")

# remove sample that was dry all months
ECCC_hydro_1998 <- ECCC_hydro_1998 %>% 
  filter(!Temp_degC == "Dry")

# make sure all other columns are numeric
ECCC_hydro_1998$Temp_degC <- as.numeric(ECCC_hydro_1998$Temp_degC)
ECCC_hydro_1998$pH_hydro <- as.numeric(ECCC_hydro_1998$pH_hydro)
ECCC_hydro_1998$SPC_mS.cm <- as.numeric(ECCC_hydro_1998$SPC_mS.cm)
ECCC_hydro_1998$Sal_ppt <- as.numeric(ECCC_hydro_1998$Sal_ppt)
ECCC_hydro_1998$DO_sat <- as.numeric(ECCC_hydro_1998$DO_sat)
ECCC_hydro_1998$DO_mg.L <- as.numeric(ECCC_hydro_1998$DO_mg.L)
ECCC_hydro_1998$Redox_mV <- as.numeric(ECCC_hydro_1998$Redox_mV)
ECCC_hydro_1998$Depth_m <- as.numeric(ECCC_hydro_1998$Depth_m)

# convert depth to negative values for Data Stream template
ECCC_hydro_1998$Depth_m <- ifelse(ECCC_hydro_1998$Depth_m > 0, -1*ECCC_hydro_1998$Depth_m, ECCC_hydro_1998$Depth_m)

# add Year column and put after Month column
ECCC_hydro_1998$Year <- 1998
ECCC_hydro_1998 <- ECCC_hydro_1998 %>% 
  relocate(Year, .after = Month)


### Hydrolab 1999 --------------------------------------------------------------

# rename columns - 1999
names(ECCC_hydro_1999)[1] <- "Pond"
names(ECCC_hydro_1999)[2] <- "Date"
names(ECCC_hydro_1999)[3] <- "Time_hydro"
names(ECCC_hydro_1999)[4] <- "Temp_degC"
names(ECCC_hydro_1999)[5] <- "pH_hydro"
names(ECCC_hydro_1999)[6] <- "SPC_mS.cm"
names(ECCC_hydro_1999)[7] <- "Sal_ppt"
names(ECCC_hydro_1999)[8] <- "DO_mg.L"
names(ECCC_hydro_1999)[9] <- "Redox_mV"
names(ECCC_hydro_1999)[10] <- "Depth_m"

# remove rows that have no data + all-NA rows/columns
ECCC_hydro_1999 <- ECCC_hydro_1999[-c(1:3), -c(11:14)]
ECCC_hydro_1999 <- print(ECCC_hydro_1999[rowSums(is.na(ECCC_hydro_1999)) != ncol(ECCC_hydro_1999), ])

# fill in the pond name if it's NA with the one directly above it
ECCC_hydro_1999 <- ECCC_hydro_1999 %>%
  fill(Pond, .direction = "down")

# remove rows that don't have any data 
ECCC_hydro_1999 <- ECCC_hydro_1999 %>% 
  filter(!is.na(Date))

# fix date format
ECCC_hydro_1999$Date <- str_pad(ECCC_hydro_1999$Date, width = 6, pad = "0") # add leading zero for months 1-9
ECCC_hydro_1999$Date <- as.Date(strptime(ECCC_hydro_1999$Date, format = "%m%d%y"))

# add Month column
ECCC_hydro_1999$Month <- month(ECCC_hydro_1999$Date)
ECCC_hydro_1999 <- ECCC_hydro_1999 %>% 
  relocate(Month, .after = Date)
ECCC_hydro_1999$Month <- gsub(4,"April",ECCC_hydro_1999$Month)
ECCC_hydro_1999$Month <- gsub(5,"May",ECCC_hydro_1999$Month)
ECCC_hydro_1999$Month <- gsub(6,"June",ECCC_hydro_1999$Month)
ECCC_hydro_1999$Month <- gsub(7,"July",ECCC_hydro_1999$Month)
ECCC_hydro_1999$Month <- gsub(8,"August",ECCC_hydro_1999$Month)
ECCC_hydro_1999$Month <- gsub(9,"September",ECCC_hydro_1999$Month)
ECCC_hydro_1999$Month <- gsub(10,"October",ECCC_hydro_1999$Month)

# add Year column and put after Month column
ECCC_hydro_1999$Year <- 1999
ECCC_hydro_1999 <- ECCC_hydro_1999 %>% 
  relocate(Year, .after = Month)

# fix time format
ECCC_hydro_1999$Time_hydro <- sub("(\\d{2})(\\d{2})(\\d{2})", "\\1:\\2:\\3", ECCC_hydro_1999$Time_hydro) # add colon between HH and MM and SS

# make sure all other columns are numeric
ECCC_hydro_1999$Temp_degC <- as.numeric(ECCC_hydro_1999$Temp_degC)
ECCC_hydro_1999$pH_hydro <- as.numeric(ECCC_hydro_1999$pH_hydro)
ECCC_hydro_1999$SPC_mS.cm <- as.numeric(ECCC_hydro_1999$SPC_mS.cm)
ECCC_hydro_1999$Sal_ppt <- as.numeric(ECCC_hydro_1999$Sal_ppt)
ECCC_hydro_1999$DO_mg.L <- as.numeric(ECCC_hydro_1999$DO_mg.L)
ECCC_hydro_1999$Redox_mV <- as.numeric(ECCC_hydro_1999$Redox_mV)
ECCC_hydro_1999$Depth_m <- as.numeric(ECCC_hydro_1999$Depth_m)

# convert depth to negative values for Data Stream template
ECCC_hydro_1999$Depth_m <- ifelse(ECCC_hydro_1999$Depth_m > 0, -1*ECCC_hydro_1999$Depth_m, ECCC_hydro_1999$Depth_m)


### Hydrolab merging: 1997-1999 ------------------------------------------------

# merge 1997-1999 data 
ECCC_hydro_merged2 <- full_join(ECCC_hydro_1997, ECCC_hydro_1998)
ECCC_hydro_merged2 <- full_join(ECCC_hydro_merged2, ECCC_hydro_1999)

### Hydrolab merging: all years ------------------------------------------------

# merge 1993-1999 data
ECCC_hydrolab <- full_join(ECCC_hydro_merged, ECCC_hydro_merged2)

ECCC_hydrolab$Pond <- gsub("Gursky","Gursky's", ECCC_hydrolab$Pond)
ECCC_hydrolab$Pond <- gsub("'s's","'s", ECCC_hydrolab$Pond)

### Checking for outliers-------------------------------------------------------

# visualize each variable to check for outliers
boxplot(ECCC_hydrolab$Temp_degC, ylab = "Temp (˚C)") # clearly one outlier here
boxplot(ECCC_hydrolab$DO_mg.L, ylab = "DO (mg/L)")
boxplot(ECCC_hydrolab$Depth_m, ylab = "Depth (m)")
boxplot(ECCC_hydrolab$SPC_mS.cm, ylab = "SPC (mS/cm)") # two outliers
boxplot(ECCC_hydrolab$pH, ylab = "pH") 
boxplot(ECCC_hydrolab$Redox_mV, ylab = "Redox (mV)")
boxplot(ECCC_hydrolab$DO_sat, ylab = "DO (%)")
boxplot(ECCC_hydrolab$Sal_ppt, ylab = "Salinity (ppt)")

# double check outliers in Temp_degC column 
temp_outlier <- ECCC_hydrolab %>% 
  identify_outliers(Temp_degC)
data.frame(temp_outlier)

# replace extreme temp outlier (6046.0 ˚C) with NA
ECCC_hydrolab <- ECCC_hydrolab %>%
  mutate(Temp_degC = ifelse(Temp_degC %in% temp_outlier$Temp_degC[temp_outlier$is.extreme == TRUE], NA, Temp_degC))

# double check for outliers in SPC_uS.cm column 
SPC_outlier <- ECCC_hydrolab %>% 
  identify_outliers(SPC_mS.cm)
data.frame(SPC_outlier) 
# two outliers with very high values - doesn't make sense given low salinity for same sample

# remove 2 SPC outliers above 150 mS/cm
ECCC_hydrolab$SPC_mS.cm <- ifelse(ECCC_hydrolab$SPC_mS.cm > 150, NA, ECCC_hydrolab$SPC_mS.cm)

### Export Hydrolab data -------------------------------------------------------

write.csv(ECCC_hydrolab,"02_tidydata/ECCC_hydrolab_clean.csv")

#

###-----------------------------------------------------------------------------
#
# Nutrients cleanup
#
###-----------------------------------------------------------------------------

# read in ECCC nutrient raw data
ECCC_nut_1993 <- read.csv("00_rawdata/VijayDriveSDNWA/Nutrient/Nutrient data for 1993.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_nut_1994 <- read.csv("00_rawdata/VijayDriveSDNWA/Nutrient/Nutrient data for 1994.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_nut_1995 <- read.csv("00_rawdata/VijayDriveSDNWA/Nutrient/Nutrient data for 1995.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 
ECCC_nut_1996 <- read.csv("00_rawdata/VijayDriveSDNWA/Nutrient/Nutrient data for 1996.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 

#

### Nutrient 1993 --------------------------------------------------------------

# rename columns - 1993
names(ECCC_nut_1993)[1] <- "Sample"
names(ECCC_nut_1993)[2] <- "Pond"
names(ECCC_nut_1993)[3] <- "Date"
names(ECCC_nut_1993)[4] <- "Julianday"
names(ECCC_nut_1993)[5] <- "Time"
names(ECCC_nut_1993)[6] <- "TDS_mg.L"
names(ECCC_nut_1993)[7] <- "Cond_uS.cm"
names(ECCC_nut_1993)[8] <- "TOC_mg.L"
names(ECCC_nut_1993)[9] <- "DOC_mg.L"
names(ECCC_nut_1993)[10] <- "HCO3_mg.L"
names(ECCC_nut_1993)[11] <- "CO3_mg.L"
names(ECCC_nut_1993)[12] <- "FreeCO2_mg.L"
names(ECCC_nut_1993)[13] <- "POC_mg.L"
names(ECCC_nut_1993)[14] <- "NO3NO2_mg.L"
names(ECCC_nut_1993)[15] <- "NH3_tot_mg.L"
names(ECCC_nut_1993)[16] <- "NH3_union_mg.L"
names(ECCC_nut_1993)[17] <- "TN_mg.L"
names(ECCC_nut_1993)[18] <- "DN_mg.L"
names(ECCC_nut_1993)[19] <- "PN_mg.L"
names(ECCC_nut_1993)[20] <- "OH_mg.L"
names(ECCC_nut_1993)[21] <- "F_diss_mg.L"
names(ECCC_nut_1993)[22] <- "Alk_tot_mg.L"
names(ECCC_nut_1993)[23] <- "Alk_p_mg.L"
names(ECCC_nut_1993)[24] <- "pH"
names(ECCC_nut_1993)[25] <- "Hard_tot_mg.L"
names(ECCC_nut_1993)[26] <- "Hard_nonCO3_mg.L"
names(ECCC_nut_1993)[27] <- "Na_diss_mg.L"
names(ECCC_nut_1993)[28] <- "Na_perc"
names(ECCC_nut_1993)[29] <- "Mg_diss_mg.L"
names(ECCC_nut_1993)[30] <- "SiO2_mg.L"
names(ECCC_nut_1993)[31] <- "P_diss_ortho_mg.L"
names(ECCC_nut_1993)[32] <- "P_tot_mg.L"
names(ECCC_nut_1993)[33] <- "P_diss_mg.L"
names(ECCC_nut_1993)[34] <- "P_part_mg.L"
names(ECCC_nut_1993)[35] <- "SO4_diss_mg.L"
names(ECCC_nut_1993)[36] <- "Cl_diss_mg.L"
names(ECCC_nut_1993)[37] <- "K_diss_mg.L"
names(ECCC_nut_1993)[38] <- "Ca_diss_mg.L"
names(ECCC_nut_1993)[39] <- "PCPN"
names(ECCC_nut_1993)[40] <- "PCPP"
names(ECCC_nut_1993)[41] <- "PNPP"
names(ECCC_nut_1993)[42] <- "Chla_mg.L"

# remove first few rows that have no data + all-NA rows/columns
ECCC_nut_1993 <- ECCC_nut_1993[-c(1:4, 150), -43]
ECCC_nut_1993 <- print(ECCC_nut_1993[rowSums(is.na(ECCC_nut_1993)) != ncol(ECCC_nut_1993), ])

# fix date format
ECCC_nut_1993$Date <- lubridate::dmy(ECCC_nut_1993$Date)

# fix time format 
ECCC_nut_1993$Time <- sub("(\\d{2})(\\d{2})", "\\1:\\2", ECCC_nut_1993$Time) # add colon between HH and MM

# fix NO3NO2 column
ECCC_nut_1993$NO3NO2_mg.L <- gsub("L0.010","0.001",ECCC_nut_1993$NO3NO2_mg.L) # make it near 0 bc it's below detection
ECCC_nut_1993$NO3NO2_mg.L <- gsub("L0.01","0.001",ECCC_nut_1993$NO3NO2_mg.L) # make it near 0 bc it's below detection

# fix F diss column
ECCC_nut_1993$F_diss_mg.L <- gsub("L.01","0.001",ECCC_nut_1993$F_diss_mg.L) # make it near 0 bc it's below detection
ECCC_nut_1993$F_diss_mg.L <- gsub("SusInt",NA,ECCC_nut_1993$F_diss_mg.L) 

# fix Na diss column
ECCC_nut_1993$Na_diss_mg.L <- gsub("L.4","0.4",ECCC_nut_1993$Na_diss_mg.L) 

# fix P diss ortho column
ECCC_nut_1993$P_diss_ortho_mg.L <- gsub("L0.002","0.001",ECCC_nut_1993$P_diss_ortho_mg.L) # make it near 0 bc it's below detection

# fix Cl diss column
ECCC_nut_1993$Cl_diss_mg.L <- gsub("????g33",NA,ECCC_nut_1993$Cl_diss_mg.L) 

# make sure all other columns are numeric
ECCC_nut_1993$Julianday <- as.numeric(ECCC_nut_1993$Julianday)
ECCC_nut_1993$TDS_mg.L <- as.numeric(ECCC_nut_1993$TDS_mg.L)
ECCC_nut_1993$Cond_uS.cm <- as.numeric(ECCC_nut_1993$Cond_uS.cm)
ECCC_nut_1993$TOC_mg.L <- as.numeric(ECCC_nut_1993$TOC_mg.L)
ECCC_nut_1993$DOC_mg.L <- as.numeric(ECCC_nut_1993$DOC_mg.L)
ECCC_nut_1993$HCO3_mg.L <- as.numeric(ECCC_nut_1993$HCO3_mg.L)
ECCC_nut_1993$CO3_mg.L <- as.numeric(ECCC_nut_1993$CO3_mg.L)
ECCC_nut_1993$FreeCO2_mg.L <- as.numeric(ECCC_nut_1993$FreeCO2_mg.L)
ECCC_nut_1993$POC_mg.L <- as.numeric(ECCC_nut_1993$POC_mg.L)
ECCC_nut_1993$NO3NO2_mg.L <- as.numeric(ECCC_nut_1993$NO3NO2_mg.L)
ECCC_nut_1993$NH3_tot_mg.L <- as.numeric(ECCC_nut_1993$NH3_tot_mg.L)
ECCC_nut_1993$NH3_union_mg.L <- as.numeric(ECCC_nut_1993$NH3_union_mg.L)
ECCC_nut_1993$TN_mg.L <- as.numeric(ECCC_nut_1993$TN_mg.L)
ECCC_nut_1993$DN_mg.L <- as.numeric(ECCC_nut_1993$DN_mg.L)
ECCC_nut_1993$PN_mg.L <- as.numeric(ECCC_nut_1993$PN_mg.L)
ECCC_nut_1993$OH_mg.L <- as.numeric(ECCC_nut_1993$OH_mg.L)
ECCC_nut_1993$F_diss_mg.L <- as.numeric(ECCC_nut_1993$F_diss_mg.L)
ECCC_nut_1993$Alk_tot_mg.L <- as.numeric(ECCC_nut_1993$Alk_tot_mg.L)
ECCC_nut_1993$Alk_p_mg.L <- as.numeric(ECCC_nut_1993$Alk_p_mg.L)
ECCC_nut_1993$pH <- as.numeric(ECCC_nut_1993$pH)
ECCC_nut_1993$Hard_tot_mg.L <- as.numeric(ECCC_nut_1993$Hard_tot_mg.L)
ECCC_nut_1993$Hard_nonCO3_mg.L <- as.numeric(ECCC_nut_1993$Hard_nonCO3_mg.L)
ECCC_nut_1993$Na_diss_mg.L <- as.numeric(ECCC_nut_1993$Na_diss_mg.L)
ECCC_nut_1993$Na_perc <- as.numeric(ECCC_nut_1993$Na_perc)
ECCC_nut_1993$Mg_diss_mg.L <- as.numeric(ECCC_nut_1993$Mg_diss_mg.L)
ECCC_nut_1993$SiO2_mg.L <- as.numeric(ECCC_nut_1993$SiO2_mg.L)
ECCC_nut_1993$P_diss_ortho_mg.L <- as.numeric(ECCC_nut_1993$P_diss_ortho_mg.L)
ECCC_nut_1993$P_tot_mg.L <- as.numeric(ECCC_nut_1993$P_tot_mg.L)
ECCC_nut_1993$P_diss_mg.L <- as.numeric(ECCC_nut_1993$P_diss_mg.L)
ECCC_nut_1993$P_part_mg.L <- as.numeric(ECCC_nut_1993$P_part_mg.L)
ECCC_nut_1993$SO4_diss_mg.L <- as.numeric(ECCC_nut_1993$SO4_diss_mg.L)
ECCC_nut_1993$Cl_diss_mg.L <- as.numeric(ECCC_nut_1993$Cl_diss_mg.L)
ECCC_nut_1993$K_diss_mg.L <- as.numeric(ECCC_nut_1993$K_diss_mg.L)
ECCC_nut_1993$Ca_diss_mg.L <- as.numeric(ECCC_nut_1993$Ca_diss_mg.L)
ECCC_nut_1993$PCPN <- as.numeric(ECCC_nut_1993$PCPN)
ECCC_nut_1993$PCPP <- as.numeric(ECCC_nut_1993$PCPP)
ECCC_nut_1993$PNPP <- as.numeric(ECCC_nut_1993$PNPP)
ECCC_nut_1993$Chla_mg.L <- as.numeric(ECCC_nut_1993$Chla_mg.L)

# add Month column
ECCC_nut_1993$Month <- month(ECCC_nut_1993$Date)
ECCC_nut_1993 <- ECCC_nut_1993 %>% 
  relocate(Month, .after = Date)
ECCC_nut_1993$Month <- gsub(4,"April",ECCC_nut_1993$Month)
ECCC_nut_1993$Month <- gsub(5,"May",ECCC_nut_1993$Month)
ECCC_nut_1993$Month <- gsub(6,"June",ECCC_nut_1993$Month)
ECCC_nut_1993$Month <- gsub(7,"July",ECCC_nut_1993$Month)
ECCC_nut_1993$Month <- gsub(8,"August",ECCC_nut_1993$Month)
ECCC_nut_1993$Month <- gsub(9,"September",ECCC_nut_1993$Month)
ECCC_nut_1993$Month <- gsub(10,"October",ECCC_nut_1993$Month)

# add Year column and put after Month column
ECCC_nut_1993$Year <- 1993
ECCC_nut_1993 <- ECCC_nut_1993 %>% 
  relocate(Year, .after = Month)


### Nutrient 1994 --------------------------------------------------------------

# rename columns - 1994
names(ECCC_nut_1994)[1] <- "Sample"
names(ECCC_nut_1994)[2] <- "Pond"
names(ECCC_nut_1994)[3] <- "Date"
names(ECCC_nut_1994)[4] <- "Julianday"
names(ECCC_nut_1994)[5] <- "Time"
names(ECCC_nut_1994)[6] <- "TDS_mg.L"
names(ECCC_nut_1994)[7] <- "Cond_uS.cm"
names(ECCC_nut_1994)[8] <- "TOC_mg.L"
names(ECCC_nut_1994)[9] <- "DOC_mg.L"
names(ECCC_nut_1994)[10] <- "HCO3_mg.L"
names(ECCC_nut_1994)[11] <- "CO3_mg.L"
names(ECCC_nut_1994)[12] <- "FreeCO2_mg.L"
names(ECCC_nut_1994)[13] <- "POC_mg.L"
names(ECCC_nut_1994)[14] <- "NO3NO2_mg.L"
names(ECCC_nut_1994)[15] <- "NH3_tot_mg.L"
names(ECCC_nut_1994)[16] <- "NH3_union_mg.L"
names(ECCC_nut_1994)[17] <- "TN_mg.L"
names(ECCC_nut_1994)[18] <- "DN_mg.L"
names(ECCC_nut_1994)[19] <- "PN_mg.L"
names(ECCC_nut_1994)[20] <- "OH_mg.L"
names(ECCC_nut_1994)[21] <- "F_diss_mg.L"
names(ECCC_nut_1994)[22] <- "Alk_tot_mg.L"
names(ECCC_nut_1994)[23] <- "Alk_p_mg.L"
names(ECCC_nut_1994)[24] <- "pH"
names(ECCC_nut_1994)[25] <- "Hard_tot_mg.L"
names(ECCC_nut_1994)[26] <- "Hard_nonCO3_mg.L"
names(ECCC_nut_1994)[27] <- "Na_diss_mg.L"
names(ECCC_nut_1994)[28] <- "Na_perc"
names(ECCC_nut_1994)[29] <- "Mg_diss_mg.L"
names(ECCC_nut_1994)[30] <- "SiO2_mg.L"
names(ECCC_nut_1994)[31] <- "P_diss_ortho_mg.L"
names(ECCC_nut_1994)[32] <- "P_tot_mg.L"
names(ECCC_nut_1994)[33] <- "P_diss_mg.L"
names(ECCC_nut_1994)[34] <- "P_part_mg.L"
names(ECCC_nut_1994)[35] <- "SO4_diss_mg.L"
names(ECCC_nut_1994)[36] <- "Cl_diss_mg.L"
names(ECCC_nut_1994)[37] <- "K_diss_mg.L"
names(ECCC_nut_1994)[38] <- "Ca_diss_mg.L"
names(ECCC_nut_1994)[39] <- "PCPN"
names(ECCC_nut_1994)[40] <- "PCPP"
names(ECCC_nut_1994)[41] <- "PNPP"
names(ECCC_nut_1994)[42] <- "Chla_mg.L"

# remove first few rows that have no data + all-NA rows/columns
ECCC_nut_1994 <- ECCC_nut_1994[-c(1:4, 166), -c(43:252)]
ECCC_nut_1994 <- print(ECCC_nut_1994[rowSums(is.na(ECCC_nut_1994)) != ncol(ECCC_nut_1994), ])

# fix date format
ECCC_nut_1994$Date <- lubridate::dmy(ECCC_nut_1994$Date)

# fix time format 
ECCC_nut_1994$Time <- sub("(\\d{2})(\\d{2})", "\\1:\\2", ECCC_nut_1994$Time) # add colon between HH and MM

# fix pH column (should not have pH of 0)
ECCC_nut_1994$pH <- gsub("0",NA,ECCC_nut_1994$pH)

# make sure all other columns are numeric
ECCC_nut_1994$Julianday <- as.numeric(ECCC_nut_1994$Julianday)
ECCC_nut_1994$TDS_mg.L <- as.numeric(ECCC_nut_1994$TDS_mg.L)
ECCC_nut_1994$Cond_uS.cm <- as.numeric(ECCC_nut_1994$Cond_uS.cm)
ECCC_nut_1994$TOC_mg.L <- as.numeric(ECCC_nut_1994$TOC_mg.L)
ECCC_nut_1994$DOC_mg.L <- as.numeric(ECCC_nut_1994$DOC_mg.L)
ECCC_nut_1994$HCO3_mg.L <- as.numeric(ECCC_nut_1994$HCO3_mg.L)
ECCC_nut_1994$CO3_mg.L <- as.numeric(ECCC_nut_1994$CO3_mg.L)
ECCC_nut_1994$FreeCO2_mg.L <- as.numeric(ECCC_nut_1994$FreeCO2_mg.L)
ECCC_nut_1994$POC_mg.L <- as.numeric(ECCC_nut_1994$POC_mg.L)
ECCC_nut_1994$NO3NO2_mg.L <- as.numeric(ECCC_nut_1994$NO3NO2_mg.L)
ECCC_nut_1994$NH3_tot_mg.L <- as.numeric(ECCC_nut_1994$NH3_tot_mg.L)
ECCC_nut_1994$NH3_union_mg.L <- as.numeric(ECCC_nut_1994$NH3_union_mg.L)
ECCC_nut_1994$TN_mg.L <- as.numeric(ECCC_nut_1994$TN_mg.L)
ECCC_nut_1994$DN_mg.L <- as.numeric(ECCC_nut_1994$DN_mg.L)
ECCC_nut_1994$PN_mg.L <- as.numeric(ECCC_nut_1994$PN_mg.L)
ECCC_nut_1994$OH_mg.L <- as.numeric(ECCC_nut_1994$OH_mg.L)
ECCC_nut_1994$F_diss_mg.L <- as.numeric(ECCC_nut_1994$F_diss_mg.L)
ECCC_nut_1994$Alk_tot_mg.L <- as.numeric(ECCC_nut_1994$Alk_tot_mg.L)
ECCC_nut_1994$Alk_p_mg.L <- as.numeric(ECCC_nut_1994$Alk_p_mg.L)
ECCC_nut_1994$pH <- as.numeric(ECCC_nut_1994$pH)
ECCC_nut_1994$Hard_tot_mg.L <- as.numeric(ECCC_nut_1994$Hard_tot_mg.L)
ECCC_nut_1994$Hard_nonCO3_mg.L <- as.numeric(ECCC_nut_1994$Hard_nonCO3_mg.L)
ECCC_nut_1994$Na_diss_mg.L <- as.numeric(ECCC_nut_1994$Na_diss_mg.L)
ECCC_nut_1994$Na_perc <- as.numeric(ECCC_nut_1994$Na_perc)
ECCC_nut_1994$Mg_diss_mg.L <- as.numeric(ECCC_nut_1994$Mg_diss_mg.L)
ECCC_nut_1994$SiO2_mg.L <- as.numeric(ECCC_nut_1994$SiO2_mg.L)
ECCC_nut_1994$P_diss_ortho_mg.L <- as.numeric(ECCC_nut_1994$P_diss_ortho_mg.L)
ECCC_nut_1994$P_tot_mg.L <- as.numeric(ECCC_nut_1994$P_tot_mg.L)
ECCC_nut_1994$P_diss_mg.L <- as.numeric(ECCC_nut_1994$P_diss_mg.L)
ECCC_nut_1994$P_part_mg.L <- as.numeric(ECCC_nut_1994$P_part_mg.L)
ECCC_nut_1994$SO4_diss_mg.L <- as.numeric(ECCC_nut_1994$SO4_diss_mg.L)
ECCC_nut_1994$Cl_diss_mg.L <- as.numeric(ECCC_nut_1994$Cl_diss_mg.L)
ECCC_nut_1994$K_diss_mg.L <- as.numeric(ECCC_nut_1994$K_diss_mg.L)
ECCC_nut_1994$Ca_diss_mg.L <- as.numeric(ECCC_nut_1994$Ca_diss_mg.L)
ECCC_nut_1994$PCPN <- as.numeric(ECCC_nut_1994$PCPN)
ECCC_nut_1994$PCPP <- as.numeric(ECCC_nut_1994$PCPP)
ECCC_nut_1994$PNPP <- as.numeric(ECCC_nut_1994$PNPP)
ECCC_nut_1994$Chla_mg.L <- as.numeric(ECCC_nut_1994$Chla_mg.L)

# add Month column
ECCC_nut_1994$Month <- month(ECCC_nut_1994$Date)
ECCC_nut_1994 <- ECCC_nut_1994 %>% 
  relocate(Month, .after = Date)
ECCC_nut_1994$Month <- gsub(4,"April",ECCC_nut_1994$Month)
ECCC_nut_1994$Month <- gsub(5,"May",ECCC_nut_1994$Month)
ECCC_nut_1994$Month <- gsub(6,"June",ECCC_nut_1994$Month)
ECCC_nut_1994$Month <- gsub(7,"July",ECCC_nut_1994$Month)
ECCC_nut_1994$Month <- gsub(8,"August",ECCC_nut_1994$Month)
ECCC_nut_1994$Month <- gsub(9,"September",ECCC_nut_1994$Month)
ECCC_nut_1994$Month <- gsub(10,"October",ECCC_nut_1994$Month)

# add Year column and put after Month column
ECCC_nut_1994$Year <- 1994
ECCC_nut_1994 <- ECCC_nut_1994 %>% 
  relocate(Year, .after = Month)


### Nutrient 1995 --------------------------------------------------------------

# rename columns - 1995
names(ECCC_nut_1995)[1] <- "Sample"
names(ECCC_nut_1995)[2] <- "Pond"
names(ECCC_nut_1995)[3] <- "Date"
names(ECCC_nut_1995)[4] <- "Julianday"
names(ECCC_nut_1995)[5] <- "Time"
names(ECCC_nut_1995)[6] <- "TDS_mg.L"
names(ECCC_nut_1995)[7] <- "Cond_uS.cm"
names(ECCC_nut_1995)[8] <- "TOC_mg.L"
names(ECCC_nut_1995)[9] <- "DOC_mg.L"
names(ECCC_nut_1995)[10] <- "HCO3_mg.L"
names(ECCC_nut_1995)[11] <- "CO3_mg.L"
names(ECCC_nut_1995)[12] <- "FreeCO2_mg.L"
names(ECCC_nut_1995)[13] <- "POC_mg.L"
names(ECCC_nut_1995)[14] <- "NO3NO2_mg.L"
names(ECCC_nut_1995)[15] <- "NH3_tot_mg.L"
names(ECCC_nut_1995)[16] <- "NH3_union_mg.L"
names(ECCC_nut_1995)[17] <- "TN_mg.L"
names(ECCC_nut_1995)[18] <- "DN_mg.L"
names(ECCC_nut_1995)[19] <- "PN_mg.L"
names(ECCC_nut_1995)[20] <- "OH_mg.L"
names(ECCC_nut_1995)[21] <- "F_diss_mg.L"
names(ECCC_nut_1995)[22] <- "Alk_tot_mg.L"
names(ECCC_nut_1995)[23] <- "Alk_p_mg.L"
names(ECCC_nut_1995)[24] <- "pH"
names(ECCC_nut_1995)[25] <- "Hard_tot_mg.L"
names(ECCC_nut_1995)[26] <- "Hard_nonCO3_mg.L"
names(ECCC_nut_1995)[27] <- "Na_diss_mg.L"
names(ECCC_nut_1995)[28] <- "Na_perc"
names(ECCC_nut_1995)[29] <- "Mg_diss_mg.L"
names(ECCC_nut_1995)[30] <- "SiO2_mg.L"
names(ECCC_nut_1995)[31] <- "P_diss_ortho_mg.L"
names(ECCC_nut_1995)[32] <- "P_tot_mg.L"
names(ECCC_nut_1995)[33] <- "P_diss_mg.L"
names(ECCC_nut_1995)[34] <- "P_part_mg.L"
names(ECCC_nut_1995)[35] <- "SO4_diss_mg.L"
names(ECCC_nut_1995)[36] <- "Cl_diss_mg.L"
names(ECCC_nut_1995)[37] <- "K_diss_mg.L"
names(ECCC_nut_1995)[38] <- "Ca_diss_mg.L"
names(ECCC_nut_1995)[39] <- "PCPN"
names(ECCC_nut_1995)[40] <- "PCPP"
names(ECCC_nut_1995)[41] <- "PNPP"
names(ECCC_nut_1995)[42] <- "Chla_mg.L"

# remove first few rows that have no data + all-NA rows/columns
ECCC_nut_1995 <- ECCC_nut_1995[-c(1:4, 68, 106), -c(43:65)]
ECCC_nut_1995 <- print(ECCC_nut_1995[rowSums(is.na(ECCC_nut_1995)) != ncol(ECCC_nut_1995), ])

# fix date format
ECCC_nut_1995$Date <- lubridate::dmy(ECCC_nut_1995$Date)

# fix time format 
ECCC_nut_1995$Time <- sub("(\\d{2})(\\d{2})", "\\1:\\2", ECCC_nut_1995$Time) # add colon between HH and MM

# fix NO3NO2 column
ECCC_nut_1995$NO3NO2_mg.L <- gsub("L0.01!","0.001",ECCC_nut_1995$NO3NO2_mg.L) # make it near 0 bc it's below detection

# fix F diss column
ECCC_nut_1995$F_diss_mg.L <- gsub("L.01","0.001",ECCC_nut_1995$F_diss_mg.L) # make it near 0 bc it's below detection
ECCC_nut_1995$F_diss_mg.L <- gsub("IN",NA,ECCC_nut_1995$F_diss_mg.L) 

# fix P diss ortho column
ECCC_nut_1995$P_diss_ortho_mg.L <- gsub("L0.002!","0.001",ECCC_nut_1995$P_diss_ortho_mg.L) # make it near 0 bc it's below detection

# fix K diss column
ECCC_nut_1995$K_diss_mg.L <- gsub("I56",NA,ECCC_nut_1995$K_diss_mg.L) 
ECCC_nut_1995$K_diss_mg.L <- gsub("G55",NA,ECCC_nut_1995$K_diss_mg.L)

# make sure all other columns are numeric
ECCC_nut_1995$Julianday <- as.numeric(ECCC_nut_1995$Julianday)
ECCC_nut_1995$TDS_mg.L <- as.numeric(ECCC_nut_1995$TDS_mg.L)
ECCC_nut_1995$Cond_uS.cm <- as.numeric(ECCC_nut_1995$Cond_uS.cm)
ECCC_nut_1995$TOC_mg.L <- as.numeric(ECCC_nut_1995$TOC_mg.L)
ECCC_nut_1995$DOC_mg.L <- as.numeric(ECCC_nut_1995$DOC_mg.L)
ECCC_nut_1995$HCO3_mg.L <- as.numeric(ECCC_nut_1995$HCO3_mg.L)
ECCC_nut_1995$CO3_mg.L <- as.numeric(ECCC_nut_1995$CO3_mg.L)
ECCC_nut_1995$FreeCO2_mg.L <- as.numeric(ECCC_nut_1995$FreeCO2_mg.L)
ECCC_nut_1995$POC_mg.L <- as.numeric(ECCC_nut_1995$POC_mg.L)
ECCC_nut_1995$NO3NO2_mg.L <- as.numeric(ECCC_nut_1995$NO3NO2_mg.L)
ECCC_nut_1995$NH3_tot_mg.L <- as.numeric(ECCC_nut_1995$NH3_tot_mg.L)
ECCC_nut_1995$NH3_union_mg.L <- as.numeric(ECCC_nut_1995$NH3_union_mg.L)
ECCC_nut_1995$TN_mg.L <- as.numeric(ECCC_nut_1995$TN_mg.L)
ECCC_nut_1995$DN_mg.L <- as.numeric(ECCC_nut_1995$DN_mg.L)
ECCC_nut_1995$PN_mg.L <- as.numeric(ECCC_nut_1995$PN_mg.L)
ECCC_nut_1995$OH_mg.L <- as.numeric(ECCC_nut_1995$OH_mg.L)
ECCC_nut_1995$F_diss_mg.L <- as.numeric(ECCC_nut_1995$F_diss_mg.L)
ECCC_nut_1995$Alk_tot_mg.L <- as.numeric(ECCC_nut_1995$Alk_tot_mg.L)
ECCC_nut_1995$Alk_p_mg.L <- as.numeric(ECCC_nut_1995$Alk_p_mg.L)
ECCC_nut_1995$pH <- as.numeric(ECCC_nut_1995$pH)
ECCC_nut_1995$Hard_tot_mg.L <- as.numeric(ECCC_nut_1995$Hard_tot_mg.L)
ECCC_nut_1995$Hard_nonCO3_mg.L <- as.numeric(ECCC_nut_1995$Hard_nonCO3_mg.L)
ECCC_nut_1995$Na_diss_mg.L <- as.numeric(ECCC_nut_1995$Na_diss_mg.L)
ECCC_nut_1995$Na_perc <- as.numeric(ECCC_nut_1995$Na_perc)
ECCC_nut_1995$Mg_diss_mg.L <- as.numeric(ECCC_nut_1995$Mg_diss_mg.L)
ECCC_nut_1995$SiO2_mg.L <- as.numeric(ECCC_nut_1995$SiO2_mg.L)
ECCC_nut_1995$P_diss_ortho_mg.L <- as.numeric(ECCC_nut_1995$P_diss_ortho_mg.L)
ECCC_nut_1995$P_tot_mg.L <- as.numeric(ECCC_nut_1995$P_tot_mg.L)
ECCC_nut_1995$P_diss_mg.L <- as.numeric(ECCC_nut_1995$P_diss_mg.L)
ECCC_nut_1995$P_part_mg.L <- as.numeric(ECCC_nut_1995$P_part_mg.L)
ECCC_nut_1995$SO4_diss_mg.L <- as.numeric(ECCC_nut_1995$SO4_diss_mg.L)
ECCC_nut_1995$Cl_diss_mg.L <- as.numeric(ECCC_nut_1995$Cl_diss_mg.L)
ECCC_nut_1995$K_diss_mg.L <- as.numeric(ECCC_nut_1995$K_diss_mg.L)
ECCC_nut_1995$Ca_diss_mg.L <- as.numeric(ECCC_nut_1995$Ca_diss_mg.L)
ECCC_nut_1995$PCPN <- as.numeric(ECCC_nut_1995$PCPN)
ECCC_nut_1995$PCPP <- as.numeric(ECCC_nut_1995$PCPP)
ECCC_nut_1995$PNPP <- as.numeric(ECCC_nut_1995$PNPP)
ECCC_nut_1995$Chla_mg.L <- as.numeric(ECCC_nut_1995$Chla_mg.L)

# add Month column
ECCC_nut_1995$Month <- month(ECCC_nut_1995$Date)
ECCC_nut_1995 <- ECCC_nut_1995 %>% 
  relocate(Month, .after = Date)
ECCC_nut_1995$Month <- gsub(4,"April",ECCC_nut_1995$Month)
ECCC_nut_1995$Month <- gsub(5,"May",ECCC_nut_1995$Month)
ECCC_nut_1995$Month <- gsub(6,"June",ECCC_nut_1995$Month)
ECCC_nut_1995$Month <- gsub(7,"July",ECCC_nut_1995$Month)
ECCC_nut_1995$Month <- gsub(8,"August",ECCC_nut_1995$Month)
ECCC_nut_1995$Month <- gsub(9,"September",ECCC_nut_1995$Month)
ECCC_nut_1995$Month <- gsub(10,"October",ECCC_nut_1995$Month)

# add Year column and put after Month column
ECCC_nut_1995$Year <- 1995
ECCC_nut_1995 <- ECCC_nut_1995 %>% 
  relocate(Year, .after = Month)


### Nutrient 1996 --------------------------------------------------------------

# rename columns - 1996
names(ECCC_nut_1996)[1] <- "Sample"
names(ECCC_nut_1996)[2] <- "Pond"
names(ECCC_nut_1996)[3] <- "Date"
names(ECCC_nut_1996)[4] <- "Julianday"
names(ECCC_nut_1996)[5] <- "Time"
names(ECCC_nut_1996)[6] <- "TDS_mg.L"
names(ECCC_nut_1996)[7] <- "Cond_uS.cm"
names(ECCC_nut_1996)[8] <- "TOC_mg.L"
names(ECCC_nut_1996)[9] <- "DOC_mg.L"
names(ECCC_nut_1996)[10] <- "HCO3_mg.L"
names(ECCC_nut_1996)[11] <- "CO3_mg.L"
names(ECCC_nut_1996)[12] <- "FreeCO2_mg.L"
names(ECCC_nut_1996)[13] <- "POC_mg.L"
names(ECCC_nut_1996)[14] <- "NO3NO2_mg.L"
names(ECCC_nut_1996)[15] <- "NH3_tot_mg.L"
names(ECCC_nut_1996)[16] <- "NH3_union_mg.L"
names(ECCC_nut_1996)[17] <- "TN_mg.L"
names(ECCC_nut_1996)[18] <- "DN_mg.L"
names(ECCC_nut_1996)[19] <- "PN_mg.L"
names(ECCC_nut_1996)[20] <- "OH_mg.L"
names(ECCC_nut_1996)[21] <- "F_diss_mg.L"
names(ECCC_nut_1996)[22] <- "Alk_tot_mg.L"
names(ECCC_nut_1996)[23] <- "Alk_p_mg.L"
names(ECCC_nut_1996)[24] <- "pH"
names(ECCC_nut_1996)[25] <- "Hard_tot_mg.L"
names(ECCC_nut_1996)[26] <- "Hard_nonCO3_mg.L"
names(ECCC_nut_1996)[27] <- "Na_diss_mg.L"
names(ECCC_nut_1996)[28] <- "Na_perc"
names(ECCC_nut_1996)[29] <- "Mg_diss_mg.L"
names(ECCC_nut_1996)[30] <- "SiO2_mg.L"
names(ECCC_nut_1996)[31] <- "P_diss_ortho_mg.L"
names(ECCC_nut_1996)[32] <- "P_tot_mg.L"
names(ECCC_nut_1996)[33] <- "P_diss_mg.L"
names(ECCC_nut_1996)[34] <- "P_part_mg.L"
names(ECCC_nut_1996)[35] <- "SO4_diss_mg.L"
names(ECCC_nut_1996)[36] <- "Cl_diss_mg.L"
names(ECCC_nut_1996)[37] <- "K_diss_mg.L"
names(ECCC_nut_1996)[38] <- "Ca_diss_mg.L"
names(ECCC_nut_1996)[39] <- "PCPN"
names(ECCC_nut_1996)[40] <- "PCPP"
names(ECCC_nut_1996)[41] <- "PNPP"
names(ECCC_nut_1996)[42] <- "Chla_mg.L"

# remove first few rows that have no data + all-NA rows/columns
ECCC_nut_1996 <- ECCC_nut_1996[-c(1:4, 107, 109:111), -c(43:56)]
ECCC_nut_1996 <- print(ECCC_nut_1996[rowSums(is.na(ECCC_nut_1996)) != ncol(ECCC_nut_1996), ])

# fix Gursky's pond ID
ECCC_nut_1996$Pond <- gsub("Gursky","Gursky's",ECCC_nut_1996$Pond)

# fix date format
ECCC_nut_1996$Date <- lubridate::dmy(ECCC_nut_1996$Date)

# fix time format 
ECCC_nut_1996$Time <- sub("(\\d{2})(\\d{2})", "\\1:\\2", ECCC_nut_1996$Time) # add colon between HH and MM

# fix NO3NO2 column
ECCC_nut_1996$NO3NO2_mg.L <- gsub("L0.010","0.001",ECCC_nut_1996$NO3NO2_mg.L) # make it near 0 bc it's below detection

# fix F diss column
ECCC_nut_1996$F_diss_mg.L <- gsub("SusInt",NA,ECCC_nut_1996$F_diss_mg.L) 
ECCC_nut_1996$F_diss_mg.L <- gsub("L.01","0.001",ECCC_nut_1996$F_diss_mg.L) # make it near 0 bc it's below detection

# fix PCPP column
ECCC_nut_1996$PCPP <- gsub("NC",NA,ECCC_nut_1996$PCPP) 

# fix PNPP column
ECCC_nut_1996$PNPP <- gsub("NC",NA,ECCC_nut_1996$PNPP) 

# make sure all other columns are numeric
ECCC_nut_1996$Julianday <- as.numeric(ECCC_nut_1996$Julianday)
ECCC_nut_1996$TDS_mg.L <- as.numeric(ECCC_nut_1996$TDS_mg.L)
ECCC_nut_1996$Cond_uS.cm <- as.numeric(ECCC_nut_1996$Cond_uS.cm)
ECCC_nut_1996$TOC_mg.L <- as.numeric(ECCC_nut_1996$TOC_mg.L)
ECCC_nut_1996$DOC_mg.L <- as.numeric(ECCC_nut_1996$DOC_mg.L)
ECCC_nut_1996$HCO3_mg.L <- as.numeric(ECCC_nut_1996$HCO3_mg.L)
ECCC_nut_1996$CO3_mg.L <- as.numeric(ECCC_nut_1996$CO3_mg.L)
ECCC_nut_1996$FreeCO2_mg.L <- as.numeric(ECCC_nut_1996$FreeCO2_mg.L)
ECCC_nut_1996$POC_mg.L <- as.numeric(ECCC_nut_1996$POC_mg.L)
ECCC_nut_1996$NO3NO2_mg.L <- as.numeric(ECCC_nut_1996$NO3NO2_mg.L)
ECCC_nut_1996$NH3_tot_mg.L <- as.numeric(ECCC_nut_1996$NH3_tot_mg.L)
ECCC_nut_1996$NH3_union_mg.L <- as.numeric(ECCC_nut_1996$NH3_union_mg.L)
ECCC_nut_1996$TN_mg.L <- as.numeric(ECCC_nut_1996$TN_mg.L)
ECCC_nut_1996$DN_mg.L <- as.numeric(ECCC_nut_1996$DN_mg.L)
ECCC_nut_1996$PN_mg.L <- as.numeric(ECCC_nut_1996$PN_mg.L)
ECCC_nut_1996$OH_mg.L <- as.numeric(ECCC_nut_1996$OH_mg.L)
ECCC_nut_1996$F_diss_mg.L <- as.numeric(ECCC_nut_1996$F_diss_mg.L)
ECCC_nut_1996$Alk_tot_mg.L <- as.numeric(ECCC_nut_1996$Alk_tot_mg.L)
ECCC_nut_1996$Alk_p_mg.L <- as.numeric(ECCC_nut_1996$Alk_p_mg.L)
ECCC_nut_1996$pH <- as.numeric(ECCC_nut_1996$pH)
ECCC_nut_1996$Hard_tot_mg.L <- as.numeric(ECCC_nut_1996$Hard_tot_mg.L)
ECCC_nut_1996$Hard_nonCO3_mg.L <- as.numeric(ECCC_nut_1996$Hard_nonCO3_mg.L)
ECCC_nut_1996$Na_diss_mg.L <- as.numeric(ECCC_nut_1996$Na_diss_mg.L)
ECCC_nut_1996$Na_perc <- as.numeric(ECCC_nut_1996$Na_perc)
ECCC_nut_1996$Mg_diss_mg.L <- as.numeric(ECCC_nut_1996$Mg_diss_mg.L)
ECCC_nut_1996$SiO2_mg.L <- as.numeric(ECCC_nut_1996$SiO2_mg.L)
ECCC_nut_1996$P_diss_ortho_mg.L <- as.numeric(ECCC_nut_1996$P_diss_ortho_mg.L)
ECCC_nut_1996$P_tot_mg.L <- as.numeric(ECCC_nut_1996$P_tot_mg.L)
ECCC_nut_1996$P_diss_mg.L <- as.numeric(ECCC_nut_1996$P_diss_mg.L)
ECCC_nut_1996$P_part_mg.L <- as.numeric(ECCC_nut_1996$P_part_mg.L)
ECCC_nut_1996$SO4_diss_mg.L <- as.numeric(ECCC_nut_1996$SO4_diss_mg.L)
ECCC_nut_1996$Cl_diss_mg.L <- as.numeric(ECCC_nut_1996$Cl_diss_mg.L)
ECCC_nut_1996$K_diss_mg.L <- as.numeric(ECCC_nut_1996$K_diss_mg.L)
ECCC_nut_1996$Ca_diss_mg.L <- as.numeric(ECCC_nut_1996$Ca_diss_mg.L)
ECCC_nut_1996$PCPN <- as.numeric(ECCC_nut_1996$PCPN)
ECCC_nut_1996$PCPP <- as.numeric(ECCC_nut_1996$PCPP)
ECCC_nut_1996$PNPP <- as.numeric(ECCC_nut_1996$PNPP)
ECCC_nut_1996$Chla_mg.L <- as.numeric(ECCC_nut_1996$Chla_mg.L)

# add Month column
ECCC_nut_1996$Month <- month(ECCC_nut_1996$Date)
ECCC_nut_1996 <- ECCC_nut_1996 %>% 
  relocate(Month, .after = Date)
ECCC_nut_1996$Month <- gsub(4,"April",ECCC_nut_1996$Month)
ECCC_nut_1996$Month <- gsub(5,"May",ECCC_nut_1996$Month)
ECCC_nut_1996$Month <- gsub(6,"June",ECCC_nut_1996$Month)
ECCC_nut_1996$Month <- gsub(7,"July",ECCC_nut_1996$Month)
ECCC_nut_1996$Month <- gsub(8,"August",ECCC_nut_1996$Month)
ECCC_nut_1996$Month <- gsub(9,"September",ECCC_nut_1996$Month)
ECCC_nut_1996$Month <- gsub(10,"October",ECCC_nut_1996$Month)

# add Year column and put after Month column
ECCC_nut_1996$Year <- 1996
ECCC_nut_1996 <- ECCC_nut_1996 %>% 
  relocate(Year, .after = Month)


### Nutrient merging: all years ------------------------------------------------

# merge 1993-1996 data 
ECCC_nut_merged <- full_join(ECCC_nut_1993, ECCC_nut_1994)
ECCC_nut_merged <- full_join(ECCC_nut_merged, ECCC_nut_1995)
ECCC_nut_merged <- full_join(ECCC_nut_merged, ECCC_nut_1996)

# remove quality check blank sample in 1994
ECCC_nut_merged <- ECCC_nut_merged %>% 
  filter(!Pond == "QC Blank")

### Checking for outliers ------------------------------------------------------

# visualize each variable to check for outliers
boxplot(ECCC_nut_merged$TDS_mg.L, ylab = "TDS (mg/L)") 
boxplot(ECCC_nut_merged$Cond_uS.cm, ylab = "Conductivity (uS/cm)")
boxplot(ECCC_nut_merged$TOC_mg.L, ylab = "TOC (mg/L)")
boxplot(ECCC_nut_merged$DOC_mg.L, ylab = "DOC (mg/L)")
boxplot(ECCC_nut_merged$HCO3_mg.L, ylab = "HCO3 (mg/L)")
boxplot(ECCC_nut_merged$CO3_mg.L, ylab = "CO3 (mg/L)")
boxplot(ECCC_nut_merged$FreeCO2_mg.L, ylab = "Free CO2 (mg/L)")
boxplot(ECCC_nut_merged$POC_mg.L, ylab = "POC (mg/L)")
boxplot(ECCC_nut_merged$NO3NO2_mg.L, ylab = "NO3NO2 (mg/L)")
boxplot(ECCC_nut_merged$NH3_tot_mg.L, ylab = "Total NH3 (mg/L)") # one clear outlier
boxplot(ECCC_nut_merged$NH3_union_mg.L, ylab = "Un-ion NH3 (mg/L)") 
boxplot(ECCC_nut_merged$TN_mg.L, ylab = "TN (mg/L)")
boxplot(ECCC_nut_merged$DN_mg.L, ylab = "DN (mg/L)")
boxplot(ECCC_nut_merged$PN_mg.L, ylab = "PN (mg/L)")
boxplot(ECCC_nut_merged$OH_mg.L, ylab = "OH (mg/L)")
boxplot(ECCC_nut_merged$F_diss_mg.L, ylab = "Dissolved F (mg/L)")
boxplot(ECCC_nut_merged$Alk_tot_mg.L, ylab = "Total alkalinity (mg/L)")
boxplot(ECCC_nut_merged$Alk_p_mg.L, ylab = "Particulate alkalinity (mg/L)")
boxplot(ECCC_nut_merged$pH, ylab = "pH")
boxplot(ECCC_nut_merged$Hard_tot_mg.L, ylab = "Total hardness (mg/L)")
boxplot(ECCC_nut_merged$Hard_nonCO3_mg.L, ylab = "Non-CO3 hardness (mg/L)")
boxplot(ECCC_nut_merged$Na_diss_mg.L, ylab = "Dissolved Na (mg/L)")
boxplot(ECCC_nut_merged$Na_perc, ylab = "Na (%)")
boxplot(ECCC_nut_merged$Mg_diss_mg.L, ylab = "Dissolved Mg (mg/L)")
boxplot(ECCC_nut_merged$SiO2_mg.L, ylab = "SiO2 (mg/L)")
boxplot(ECCC_nut_merged$P_diss_ortho_mg.L, ylab = "Dissolved ortho P (mg/L)")
boxplot(ECCC_nut_merged$P_tot_mg.L, ylab = "Total P (mg/L)")
boxplot(ECCC_nut_merged$P_diss_mg.L, ylab = "Dissolved P (mg/L)")
boxplot(ECCC_nut_merged$P_part_mg.L, ylab = "Particulate P (mg/L)")
boxplot(ECCC_nut_merged$SO4_diss_mg.L, ylab = "Dissolved SO4 (mg/L)")
boxplot(ECCC_nut_merged$Cl_diss_mg.L, ylab = "Dissolved Cl (mg/L)")
boxplot(ECCC_nut_merged$K_diss_mg.L, ylab = "Dissolved K (mg/L)")
boxplot(ECCC_nut_merged$Ca_diss_mg.L, ylab = "Dissolved Ca (mg/L)")
boxplot(ECCC_nut_merged$PCPN, ylab = "PC:PN")
boxplot(ECCC_nut_merged$PCPP, ylab = "PC:PP")
boxplot(ECCC_nut_merged$PNPP, ylab = "PN:PP")
boxplot(ECCC_nut_merged$Chla_mg.L, ylab = "Chl a (mg/L)")

# fix Cond and TDS columns - should not have value of 0 in one but not other
ECCC_nut_merged$Cond_uS.cm <- gsub(0,NA,ECCC_nut_merged$Cond_uS.cm)
ECCC_nut_merged$Cond_uS.cm <- as.numeric(ECCC_nut_merged$Cond_uS.cm)
ECCC_nut_merged$TDS_mg.L <- gsub(0,NA,ECCC_nut_merged$TDS_mg.L)
ECCC_nut_merged$TDS_mg.L <- as.numeric(ECCC_nut_merged$TDS_mg.L)


### Export Nutrient data -------------------------------------------------------

write.csv(ECCC_nut_merged,"02_tidydata/ECCC_nutrients_clean.csv")

#


###-----------------------------------------------------------------------------
#
# Water level cleanup
#
###-----------------------------------------------------------------------------

# read in ECCC water level raw data
ECCC_waterlevel <- read.csv("00_rawdata/VijayDriveSDNWA/Water level/ECCC_Waterlevel_1993-1996.csv", 
                            na = c("", "NA", "#N/A", "#VALUE!", "n/a",  "Not Measured", 
                                   "No Bar", "No bar", "Dry", "Stake snapped", "Not Sampled", "PEIV")) 

### Water level 1993-1996 ------------------------------------------------------

# combine Date and Year columns
ECCC_waterlevel$Date <- paste(ECCC_waterlevel$Date, ECCC_waterlevel$Year, sep="-")

# fix Date format
ECCC_waterlevel <- ECCC_waterlevel %>% mutate(Date = lubridate::dmy(Date))

# add Month column
ECCC_waterlevel$Month <- month(ECCC_waterlevel$Date)
ECCC_waterlevel <- ECCC_waterlevel %>% 
  relocate(Month, .after = Date)
ECCC_waterlevel$Month <- gsub(5,"May",ECCC_waterlevel$Month)
ECCC_waterlevel$Month <- gsub(6,"June",ECCC_waterlevel$Month)
ECCC_waterlevel$Month <- gsub(7,"July",ECCC_waterlevel$Month)
ECCC_waterlevel$Month <- gsub(8,"August",ECCC_waterlevel$Month)
ECCC_waterlevel$Month <- gsub(9,"September",ECCC_waterlevel$Month)
ECCC_waterlevel$Month <- gsub(10,"October",ECCC_waterlevel$Month)

# put Year column after Month column
ECCC_waterlevel <- ECCC_waterlevel %>% 
  relocate(Year, .after = Month)

# make water level column numeric 
ECCC_waterlevel$Water_level_m <- as.numeric(ECCC_waterlevel$Water_level_m)

# remove missing water level data
ECCC_waterlevel <- ECCC_waterlevel %>% 
  filter(!is.na(Water_level_m))

# look for any outliers 
boxplot(ECCC_waterlevel$Water_level_m, ylab = "Water level (m)") # looks good

### Export Water level data ----------------------------------------------------

write.csv(ECCC_waterlevel,"02_tidydata/ECCC_waterlevel_clean.csv")

#



###-----------------------------------------------------------------------------
#
# Merging all three ECCC data sets
#
###-----------------------------------------------------------------------------

# read in tidy data
ECCC_hydrolab_clean <- read.csv("02_tidydata/ECCC_hydrolab_clean.csv")
ECCC_nutrients_clean <- read.csv("02_tidydata/ECCC_nutrients_clean.csv")
ECCC_waterlevel_clean <- read.csv("02_tidydata/ECCC_waterlevel_clean.csv")

# merge hydrolab, nutrients, and water level df
ECCC_master <- full_join(ECCC_hydrolab_clean, ECCC_nutrients_clean)
ECCC_master <- full_join(ECCC_master, ECCC_waterlevel_clean)

# remove X and Sample columns (irrelevant)
ECCC_master <- ECCC_master %>% select(-c(X, Sample))

# write as new csv
write.csv(ECCC_master,"02_tidydata/merged_ECCC_clean.csv")


###-----------------------------------------------------------------------------
#
# Converting ECCC data to long format for Data Stream template
#
###-----------------------------------------------------------------------------

# read in merged tidy data
ECCC <- read.csv("02_tidydata/merged_ECCC_clean.csv")

# convert variables to long format
ECCC_long <- pivot_longer(ECCC, cols = c("Temp_degC", "DO_mg.L", "SPC_mS.cm", "pH_hydro", 
                          "Sal_ppt", "DO_sat", "Redox_mV", "Depth_m", "TDS_mg.L", "Cond_uS.cm", 
                          "TOC_mg.L", "DOC_mg.L", "HCO3_mg.L", "CO3_mg.L", "FreeCO2_mg.L", "POC_mg.L", 
                          "NO3NO2_mg.L", "NH3_tot_mg.L", "NH3_union_mg.L", "TN_mg.L", "DN_mg.L", "PN_mg.L", 
                          "OH_mg.L", "F_diss_mg.L", "Alk_tot_mg.L", "Alk_p_mg.L", "pH", "Hard_tot_mg.L", 
                          "Hard_nonCO3_mg.L", "Na_diss_mg.L", "Na_perc", "Mg_diss_mg.L", "SiO2_mg.L", 
                          "P_diss_ortho_mg.L", "P_tot_mg.L", "P_diss_mg.L", "P_part_mg.L", "SO4_diss_mg.L", 
                          "Cl_diss_mg.L", "K_diss_mg.L", "Ca_diss_mg.L", "PCPN", "PCPP", "PNPP", 
                          "Chla_mg.L", "Water_level_m"), names_to = "CharacteristicID", 
                          values_to = "ResultValue")

# write as new csv
write.csv(ECCC_long,"05_DataStream/ECCC_long.csv")





