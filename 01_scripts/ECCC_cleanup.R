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

###-----------------------------------------------------------------------------
#
# Hydrolab cleanup
#
###-----------------------------------------------------------------------------

# read in ECCC hydrolab master file
ECCC_hydrolab <- read.csv("00_rawdata/VijayDriveSDNWA/ECCC_Hydrolab_1993-1999.csv", na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 

# remove end columns with only NAs
ECCC_hydrolab <- print(ECCC_hydrolab[, colSums(is.na(ECCC_hydrolab)) != nrow(ECCC_hydrolab)]) 

# fix date format
ECCC_hydrolab <- ECCC_hydrolab %>% mutate(Date = lubridate::ymd(Date))

# fix time format 
ECCC_hydrolab$Time <- as_hms(ECCC_hydrolab$Time)

# make sure all depths are < 0 for DataStream
ECCC_hydrolab$Depth_m <- ifelse(ECCC_hydrolab$Depth_m > 0, -1*ECCC_hydrolab$Depth_m, ECCC_hydrolab$Depth_m)

# double check that there are no positive depth values
max(ECCC_hydrolab$Depth_m)

### Checking for outliers-------------------------------------------------------

# visualize each variable to check for outliers
boxplot(ECCC_hydrolab$Temp_degC, ylab = "Temp (˚C)") # clearly one outlier here
boxplot(ECCC_hydrolab$DO_mg.L, ylab = "DO (mg/L)")
boxplot(ECCC_hydrolab$Depth_m, ylab = "Depth (m)")
boxplot(ECCC_hydrolab$SPC_uS.cm, ylab = "SPC (uS/cm)") # might be two outliers?
boxplot(ECCC_hydrolab$pH, ylab = "pH") # need to make sure highest point < 14
boxplot(ECCC_hydrolab$Redox_mV, ylab = "Redox (mV)")
boxplot(ECCC_hydrolab$DO_perc, ylab = "DO (%)")
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
  identify_outliers(SPC_uS.cm)
options(max.print = 10000)
data.frame(SPC_outlier) # getting very different numbers between years
# seems as though earlier data used mS/cm not uS/cm even tho data sheet says uS/cm
# 1997 says mS/cm - need to convert those for sure

# check for outliers in pH column 
pH_outlier <- ECCC_hydrolab %>% 
  identify_outliers(pH)
data.frame(pH_outlier) # outliers don't exceed 14, should be fine to leave in 

### Fixing SPC units -----------------------------------------------------------

# multiply all mS/cm values in 1997 by 1000 to change to uS/cm
ECCC_hydrolab <- ECCC_hydrolab %>%
  mutate(SPC_uS.cm = ifelse(year(Date) == 1997, SPC_uS.cm*1000, SPC_uS.cm))

# check boxplot again
boxplot(ECCC_hydrolab$SPC_uS.cm, ylab = "SPC (uS/cm)") 

# still lots of values near zero in the earlier years
# data files say uS/cm though so not sure what to do

###-----------------------------------------------------------------------------
#
# Nutrients cleanup
#
###-----------------------------------------------------------------------------

# read in ECCC nutrient master file 
ECCC_nutrients <- read.csv("00_rawdata/VijayDriveSDNWA/ECCC_Nutrients_1993-1996.csv", 
                           na = c("", "NA", "#N/A", "#VALUE!", "n/a")) 

# remove end columns with only NAs
ECCC_nutrients <- print(ECCC_nutrients[, colSums(is.na(ECCC_nutrients)) != nrow(ECCC_nutrients)]) 

# fix date format
ECCC_nutrients <- ECCC_nutrients %>% mutate(Date = lubridate::dmy(Date))

# fix time format 
ECCC_nutrients$Time <- as_hms(ECCC_nutrients$Time)

# fix NO3NO2 column
ECCC_nutrients <- ECCC_nutrients %>% 
  ifelse(NO3NO2_mg.L == "L0.010" | NO3NO2_mg.L == "L0.01!", 0.001, NO3NO2_mg.L)


# make sure all fixed nutrient columns are numeric
ECCC_nutrients$NO3NO2_mg.L <- as.numeric(ECCC_nutrients$NO3NO2_mg.L)
ECCC_nutrients$F.diss_mg.L <- as.numeric(ECCC_nutrients$F.diss_mg.L)
ECCC_nutrients$Na.diss_mg.L <- as.numeric(ECCC_nutrients$Na.diss_mg.L)
ECCC_nutrients$P.diss.ortho_mg.L <- as.numeric(ECCC_nutrients$P.diss.ortho_mg.L)
ECCC_nutrients$Cl.diss_mg.L <- as.numeric(ECCC_nutrients$Cl.diss_mg.L)
ECCC_nutrients$K.diss_mg.L <- as.numeric(ECCC_nutrients$K.diss_mg.L)
ECCC_nutrients$PCPP <- as.numeric(ECCC_nutrients$PCPP)
ECCC_nutrients$PNPP <- as.numeric(ECCC_nutrients$PNPP)

# visualize each variable to check for outliers
boxplot(ECCC_nutrients$TDS_mg.L, ylab = "") 
boxplot(ECCC_nutrients$Cond_uS.cm, ylab = "")
boxplot(ECCC_nutrients$TOC_mg.L, ylab = "")
boxplot(ECCC_nutrients$DOC_mg.L, ylab = "")
boxplot(ECCC_nutrients$HCO3_mg.L, ylab = "")
boxplot(ECCC_nutrients$CO3_mg.L, ylab = "")
boxplot(ECCC_nutrients$freeCO2_mg.L, ylab = "")
boxplot(ECCC_nutrients$POC_mg.L, ylab = "")
boxplot(ECCC_nutrients$NO3NO2_mg.L, ylab = "")
boxplot(ECCC_nutrients$NH3.tot_mg.L, ylab = "") # one clear outlier
boxplot(ECCC_nutrients$NH3.un.ion_mg.L, ylab = "") # maybe a few outliers?
boxplot(ECCC_nutrients$TN_mg.L, ylab = "")
boxplot(ECCC_nutrients$DN_mg.L, ylab = "")
boxplot(ECCC_nutrients$PN_mg.L, ylab = "")
boxplot(ECCC_nutrients$OH_mg.L, ylab = "") # two outliers
boxplot(ECCC_nutrients$F.diss_mg.L, ylab = "")
boxplot(ECCC_nutrients$Alk.tot_mg.L, ylab = "")
boxplot(ECCC_nutrients$Alk.p_mg.L, ylab = "")
boxplot(ECCC_nutrients$pH, ylab = "")
boxplot(ECCC_nutrients$Hard.tot_mg.L, ylab = "")
boxplot(ECCC_nutrients$Hard.nonCO3_mg.L, ylab = "")
boxplot(ECCC_nutrients$Na.diss_mg.L, ylab = "")
boxplot(ECCC_nutrients$Na_perc, ylab = "")
boxplot(ECCC_nutrients$Mg.diss_mg.L, ylab = "")
boxplot(ECCC_nutrients$SiO2_mg.L, ylab = "")
boxplot(ECCC_nutrients$P.diss.ortho_mg.L, ylab = "")
boxplot(ECCC_nutrients$P.tot_mg.L, ylab = "")
boxplot(ECCC_nutrients$P.diss_mg.L, ylab = "")
boxplot(ECCC_nutrients$P.part_mg.L, ylab = "")
boxplot(ECCC_nutrients$SO4.diss_mg.L, ylab = "")
boxplot(ECCC_nutrients$Cl.diss_mg.L, ylab = "")
boxplot(ECCC_nutrients$K.diss_mg.L, ylab = "")
boxplot(ECCC_nutrients$Ca.diss_mg.L, ylab = "")
boxplot(ECCC_nutrients$PCPN, ylab = "")
boxplot(ECCC_nutrients$PCPP, ylab = "")
boxplot(ECCC_nutrients$PNPP, ylab = "")
boxplot(ECCC_nutrients$Chla_ug.L, ylab = "")

###-----------------------------------------------------------------------------
#
# Water level cleanup
#
###-----------------------------------------------------------------------------

# read in ECCC water level master file
ECCC_waterlevel <- read.csv("00_rawdata/VijayDriveSDNWA/ECCC_Waterlevel_1993-1996.csv", 
                            na = c("", "NA", "#N/A", "#VALUE!", "n/a",  "Not Measured", 
                                   "No Bar", "No bar", "Dry", "Stake snapped")) 

# remove end columns with only NAs
ECCC_waterlevel <- print(ECCC_waterlevel[, colSums(is.na(ECCC_waterlevel)) != nrow(ECCC_nutrients)]) 

# combine Date and Year columns
ECCC_waterlevel$Date <- paste(ECCC_waterlevel$Date, ECCC_waterlevel$Year, sep="-")

# fix Date format
ECCC_waterlevel <- ECCC_waterlevel %>% mutate(Date = lubridate::dmy(Date))

# make water level column numeric 
ECCC_waterlevel$Water_level_m <- as.numeric(ECCC_waterlevel$Water_level_m)

# look for any outliers 
boxplot(ECCC_waterlevel$Water_level_m, ylab = "Water level (m)") # looks good

###-----------------------------------------------------------------------------
#
# Merging all three ECCC data sets
#
###-----------------------------------------------------------------------------








