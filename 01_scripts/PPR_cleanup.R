################################################################################
#
# Script: PPR data cleaning and merging
# Author: Jess Lerminiaux
# Purpose: To clean up the PPR water quality data to be published on DataStream
# Date: Fall 2024
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
library(hms)


#-------------------------------------------------------------------------------
#
# SDWS_wq data cleanup
#
#-------------------------------------------------------------------------------

#load data
SDWS_2014 <- read_excel("00_rawdata/UofR data/SDWS - 2014 - Long Term Monitoring - All Data.xlsx")
SDWS_2015 <- read_excel("00_rawdata/UofR data/SDWS - 2015 - Long Term Monitoring - All Data.xlsx")
SDWS_2016 <- read_excel("00_rawdata/UofR data/SDWS - 2016 - Long Term Monitoring - All Data.xlsx")
SDWS_2017 <- read_excel("00_rawdata/UofR data/SDWS - 2017 - Long Term Monitoring - All Data.xlsx")
SDWS_2019 <- read_excel("00_rawdata/UofR data/SDWS - 2019 - Long Term Monitoring - All Data.xlsx")
SDWS_2020 <- read_excel("00_rawdata/UofR data/SDWS - 2020 - Long Term Monitoring - All Data.xlsx")
SDWS_2021 <- read_excel("00_rawdata/UofR data/SDWS - 2021 - Long Term Monitoring - All Data.xlsx")
SDWS_2022 <- read_excel("00_rawdata/UofR data/SDWS - 2022 - Long Term Monitoring - All Data.xlsx")

# fix 2016 date
SDWS_2016$Date <- as.Date(SDWS_2016$Date)
SDWS_2016$Date <- format(SDWS_2016$Date, "%Y-%m-%d")

# fix 2016 + 2020 time (add leading zeros to hour if necessary)
SDWS_2016$Time <- ifelse(nchar(SDWS_2016$Time) == 3, paste0("0", SDWS_2016$Time), SDWS_2016$Time)

# extract hour and minute parts and format as "HH:MM"
SDWS_2016$Time <- paste(substr(SDWS_2016$Time, 1, 2), ":", substr(SDWS_2016$Time, 3, 4), sep = "")
SDWS_2016$Time[SDWS_2016$Time == "NA:NA"] <- NA

# convert datetime column to POSIXct object
SDWS_2020$Time <- as.POSIXct(SDWS_2020$Time)
SDWS_2020$Time <- format(SDWS_2020$Time, "%H:%M") # extract only the time part

# full join on all variables
merged_SDWS <- merge(SDWS_2014, SDWS_2015, by = intersect(names(SDWS_2014), names(SDWS_2015)), all = TRUE)
merged_SDWS <- merge(merged_SDWS, SDWS_2016, by = intersect(names(merged_SDWS), names(SDWS_2016)), all = TRUE)
merged_SDWS <- merge(merged_SDWS, SDWS_2017, by = intersect(names(merged_SDWS), names(SDWS_2017)), all = TRUE)
merged_SDWS <- merge(merged_SDWS, SDWS_2019, by = intersect(names(merged_SDWS), names(SDWS_2019)), all = TRUE)
merged_SDWS <- merge(merged_SDWS, SDWS_2020, by = intersect(names(merged_SDWS), names(SDWS_2020)), all = TRUE)
merged_SDWS <- merge(merged_SDWS, SDWS_2021, by = intersect(names(merged_SDWS), names(SDWS_2021)), all = TRUE)
merged_SDWS <- merge(merged_SDWS, SDWS_2022, by = intersect(names(merged_SDWS), names(SDWS_2022)), all = TRUE)

# clean up pond names and dates, remove time aspect from Date again
merged_SDWS$Date <- as.Date(merged_SDWS$Date)
merged_SDWS$Date <- format(merged_SDWS$Date, "%Y-%m-%d")

# remove spaces and decimals from pond names 
merged_SDWS$Pond <- gsub(" ", "", merged_SDWS$Pond)
merged_SDWS$Pond <- sub("\\.0$", "", merged_SDWS$Pond)


# write as tidy csv
write.csv(merged_SDWS, "02_tidydata/merged_SDWS_clean.csv", row.names = FALSE)

# 


#-------------------------------------------------------------------------------
#
# SDWS_iso data cleanup
#
#-------------------------------------------------------------------------------

#load data
Iso_2014 <- read_excel("00_rawdata/UofR data/SDWS - 2014 - Long Term Monitoring - All Data.xlsx", sheet = "Isotopes")
Iso_2015 <- read_excel("00_rawdata/UofR data/SDWS - 2015 - Long Term Monitoring - All Data.xlsx", sheet = "Isotopes")
Iso_2016 <- read_excel("00_rawdata/UofR data/SDWS - 2016 - Long Term Monitoring - All Data.xlsx", sheet = "Isotopes")
Iso_2017 <- read_excel("00_rawdata/UofR data/SDWS - 2017 - Long Term Monitoring - All Data.xlsx", sheet = "Isotpes")

# fix column names in 2017 df
names(Iso_2017)[2] <- "Sample_ID"

# remove extra (all-NA) column in 2017 df
Iso_2017 <- Iso_2017[, -7]

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

# make sure dates are consistent
Iso_2014$`Collection Date` <- as.Date(Iso_2014$`Collection Date` , format = "%Y-%m-%d")
Iso_2015$`Collection Date` <- as.Date(Iso_2015$`Collection Date` , format = "%Y-%m-%d")
Iso_2016$`Collection Date` <- as.Date(Iso_2016$`Collection Date` , format = "%Y-%m-%d")
Iso_2017$`Collection Date` <- as.Date(Iso_2017$`Collection Date` , format = "%Y-%m-%d")

# merge data
merged_Iso <- merge(Iso_2014, Iso_2015, by = intersect(names(Iso_2014), names(Iso_2015)), all = TRUE)
merged_Iso <- merge(merged_Iso, Iso_2016, by = intersect(names(merged_Iso), names(Iso_2016)), all = TRUE)
merged_Iso <- merge(merged_Iso, Iso_2017, by = intersect(names(merged_Iso), names(Iso_2017)), all = TRUE)


# write as tidy csv
write.csv(merged_Iso, "02_tidydata/merged_Isotopes_clean.csv", row.names = FALSE)


#

#-------------------------------------------------------------------------------
#
# SDWS_chl data cleanup
#
#-------------------------------------------------------------------------------

#load data
ChlA_2014 <- read_excel("00_rawdata/UofR data/SDWS - 2014 - Long Term Monitoring - All Data.xlsx", sheet = "ChlA")
ChlA_2015 <- read_excel("00_rawdata/UofR data/SDWS - 2015 - Long Term Monitoring - All Data.xlsx", sheet = "ChlA")
ChlA_2016 <- read_excel("00_rawdata/UofR data/SDWS - 2016 - Long Term Monitoring - All Data.xlsx", sheet = "ChlA")
ChlA_2017 <- read_excel("00_rawdata/UofR data/SDWS - 2017 - Long Term Monitoring - All Data.xlsx", sheet = "ChlA")
ChlA_2019 <- read_excel("00_rawdata/UofR data/SDWS - 2019 - Long Term Monitoring - All Data.xlsx", sheet = "ChlA")
ChlA_2020 <- read_excel("00_rawdata/UofR data/SDWS - 2020 - Long Term Monitoring - All Data.xlsx", sheet = "ChlA")
ChlA_2021 <- read_excel("00_rawdata/UofR data/SDWS - 2021 - Long Term Monitoring - All Data.xlsx", sheet = "ChlA")
ChlA_2022 <- read_excel("00_rawdata/UofR data/SDWS - 2022 - Long Term Monitoring - All Data.xlsx", sheet = "ChlA")

# change column names so they all match
names(ChlA_2014)[14] <- "RunDate"
names(ChlA_2015)[14] <- "RunDate"

# fix pond names
ChlA_2014$Location <- sub("SDWS ", "", ChlA_2014$Location)
ChlA_2014$Location[] <- lapply(ChlA_2014$Location, function(x) gsub(" ", "", x))
ChlA_2014$Location <- as.character(ChlA_2014$Location)

ChlA_2015$Location <- sub("SDWS ", "", ChlA_2015$Location)
ChlA_2015$Location <- sub("P", "", ChlA_2015$Location)

ChlA_2016$Location <- sub("SDWS P", "", ChlA_2016$Location)
ChlA_2016$Location <- sub("SDWS ", "", ChlA_2016$Location)

ChlA_2017$Location <- sub("SD", "", ChlA_2017$Location)

ChlA_2019$Location[] <- lapply(ChlA_2019$Location, function(x) gsub(" ", "", x))
ChlA_2019$Location <- sub("\\.0$", "", ChlA_2019$Location)

ChlA_2020$Location <- sub("\\.0$", "", ChlA_2020$Location)

ChlA_2021$Location[] <- lapply(ChlA_2021$Location, function(x) gsub(" ", "", x))
ChlA_2021$Location <- sub("\\.0$", "", ChlA_2021$Location)

# 2022 includes replicate in location/ID, but also have replicate column, so unnecessary
ChlA_2022$Location <- gsub(".*P([0-9]+).*", "\\1", ChlA_2022$Location )
ChlA_2022$Location[ChlA_2022$Location == "SDWS P 1 - R1"] <- "1"
ChlA_2022$Location[ChlA_2022$Location == "01"] <- "1"

# date formats to change: 2014, 2015 
ChlA_2014$RunDate <- as.Date(ChlA_2014$RunDate, format = "%Y%m%d")

ChlA_2015$Date <- as.Date(ChlA_2015$Date, format = "%B %d %Y")
ChlA_2015$Date <- as.Date(ChlA_2015$Date, format = "%Y-%m-%d")
ChlA_2015$RunDate <- as.Date(ChlA_2015$RunDate, format = "%Y%m%d")

# convert Rep # to numeric
ChlA_2014$`Rep #` <- as.numeric(ChlA_2014$`Rep #`) 

# convert Abs columns to numeric
ChlA_2016$`Abs @ 750` <- as.numeric(ChlA_2016$`Abs @ 750`) 
ChlA_2016$`Abs @ 665` <- as.numeric(ChlA_2016$`Abs @ 665`) 
ChlA_2016$`Abs @ 649` <- as.numeric(ChlA_2016$`Abs @ 649`) 

# convert Chl A column to numeric
ChlA_2016$`Chl  A (µg/L)` <- as.numeric(ChlA_2016$`Chl  A (µg/L)`) 

# convert Average to numeric
ChlA_2014$`Average` <- as.numeric(ChlA_2014$`Average`) 
ChlA_2019$`Average` <- as.numeric(ChlA_2019$`Average`) 
ChlA_2020$`Average` <- as.numeric(ChlA_2020$`Average`) 
ChlA_2021$`Average` <- as.numeric(ChlA_2021$`Average`) 

# convert %Error to numeric for merging
ChlA_2014$`%Error` <- as.numeric(ChlA_2014$`%Error`) 
ChlA_2016$`%Error` <- as.numeric(ChlA_2016$`%Error`) 
ChlA_2019$`%Error` <- as.numeric(ChlA_2019$`%Error`) 
ChlA_2020$`%Error` <- as.numeric(ChlA_2020$`%Error`) 
ChlA_2021$`%Error` <- as.numeric(ChlA_2021$`%Error`) 

# convert Notes to characters
ChlA_2017$Notes <- as.character(ChlA_2017$Notes)
ChlA_2019$Notes <- as.character(ChlA_2019$Notes)
ChlA_2020$Notes <- as.character(ChlA_2020$Notes)
ChlA_2021$Notes <- as.character(ChlA_2021$Notes)

# make sure all dates are formatted as such
ChlA_2014$Date <- as.Date(ChlA_2014$Date, format = "%Y-%m-%d")
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

# merge data together
merged_ChlA <- merge(ChlA_2014, ChlA_2015, by = intersect(names(ChlA_2014), names(ChlA_2015)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2016, by = intersect(names(merged_ChlA), names(ChlA_2016)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2017, by = intersect(names(merged_ChlA), names(ChlA_2017)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2019, by = intersect(names(merged_ChlA), names(ChlA_2019)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2020, by = intersect(names(merged_ChlA), names(ChlA_2020)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2021, by = intersect(names(merged_ChlA), names(ChlA_2021)), all = TRUE)
merged_ChlA <- merge(merged_ChlA, ChlA_2022, by = intersect(names(merged_ChlA), names(ChlA_2022)), all = TRUE)

# make sure all depth are spelled same/upper lower case same
merged_ChlA$`Depth (m)`[merged_ChlA$`Depth (m)` == "surface"] <- "Surface"
merged_ChlA$`Depth (m)`[merged_ChlA$`Depth (m)` == "wetland"] <- "Wetland"
merged_ChlA$`Depth (m)`[merged_ChlA$`Depth (m)` == "NA"] <- NA


# write as tidy csv
write.csv(merged_ChlA, "02_tidydata/merged_ChlA_clean.csv", row.names = FALSE)

#




#-------------------------------------------------------------------------------
#
# Read in all other data
#
#-------------------------------------------------------------------------------

# read in updated raw PPR data from Britt
Abs_PPR_2024 <- read_excel("00_rawdata/UofR data/All_Data_PPR_2006_to_2024 MASTER FILE.xlsx", 
                           sheet = "Absorbance",
                           na = c("", "na", "NA", "n/a", "N/A", " "))
Data_PPR_2024 <- read_excel("00_rawdata/UofR data/All_Data_PPR_2006_to_2024 MASTER FILE.xlsx", 
                            sheet = "Data",
                            na = c("", "na", "NA", "n/a", "N/A", " "))
Field_PPR_2024 <- read_excel("00_rawdata/UofR data/All_Data_PPR_2006_to_2024 MASTER FILE.xlsx", 
                             sheet = "Field Stats",
                             na = c("", "na", "NA", "n/a", "N/A", " "))

# read in old PPR data from 2006
Data_2006 <- read_excel("00_rawdata/UofR data/Data PPR 2006 COMPLETE April 2009.xlsx", 
                        sheet = "data",
                        na = c("", "na", "NA", "n/a", "N/A"))

# read in Baulch data that I cleaned 
SDWS_chla <- read.csv("02_tidydata/merged_ChlA_clean.csv", na = c("", "na", "NA", "n/a", "N/A"))
SDWS_iso <- read.csv("02_tidydata/merged_Isotopes_clean.csv", na = c("", "na", "NA", "n/a", "N/A"))
SDWS_wq <- read.csv("02_tidydata/merged_SDWS_clean.csv", na = c("", "na", "NA", "n/a", "N/A"))

# read in more raw data for DataStream
Baulch <- read_excel("00_rawdata/UofR data/SDWS 2015-2022 Baulch.xlsx")
THG_data <- read.csv("00_rawdata/UofR data/DMA-80 THG Data_Cleandata.csv")
Sed_moisture <- read_excel("00_rawdata/UofR data/(Logan Edited) Moisture-Content-of-Sediment.xlsx")
Robbins <- read.csv("00_rawdata/UofR data/L.Robbins Lab Data 2021-2023 - SW Sed PW chem data.xlsx - Sediment bulk TM [clean].csv")
MeHg2021 <- read_excel("00_rawdata/UofR data/MeHg_2021_ID_Samples_results.xlsx")
TICTOC <- read_excel("00_rawdata/UofR data/TICTOC Results May 30, 2023.xlsx")

#-------------------------------------------------------------------------------

### PPR Absorbance df (Abs_PPR_2024)

#-------------------------------------------------------------------------------

# select St. Denis region
Abs_PPR_2024 <- Abs_PPR_2024 %>% 
  filter(Region == "SDNWA")

# remove any columns with all-NAs
Abs_PPR_2024 <- Abs_PPR_2024 %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(Abs_PPR_2024)[3] <- "Analyzing_date"
names(Abs_PPR_2024)[4] <- "Date"
names(Abs_PPR_2024)[5] <- "250_nm"
names(Abs_PPR_2024)[6] <- "254_nm"
names(Abs_PPR_2024)[7] <- "280_nm"
names(Abs_PPR_2024)[8] <- "350_nm"
names(Abs_PPR_2024)[9] <- "365_nm"
names(Abs_PPR_2024)[10] <- "370_nm"

# put pond 97 together 98
Abs_PPR_2024$Pond[Abs_PPR_2024$Pond == "97"] <- "97/98"
Abs_PPR_2024$Pond[Abs_PPR_2024$Pond == "125 N"] <- "125N"
Abs_PPR_2024$Pond[Abs_PPR_2024$Pond == "125 S"] <- "125S"

# fix date format
Abs_PPR_2024$Date <- as.numeric(Abs_PPR_2024$Date)
Abs_PPR_2024$Date <- as.Date(Abs_PPR_2024$Date, format = "%Y-%m-%d", origin = "1899-12-30")
Abs_PPR_2024$Analyzing_date <- as.numeric(Abs_PPR_2024$Analyzing_date)
Abs_PPR_2024$Analyzing_date <- as.Date(Abs_PPR_2024$Analyzing_date, format = "%Y-%m-%d", origin = "1899-12-30")

# only keep 1.2 um filter results (will get duplicates on DS otherwise)
Abs_PPR_2024 <- Abs_PPR_2024 %>% 
  filter(!Filter == 0.45)

#-------------------------------------------------------------------------------

### PPR Data df (Data_PPR_2024)

#-------------------------------------------------------------------------------

# select St. Denis region
Data_PPR_2024 <- Data_PPR_2024 %>% 
  filter(Region == "SDNWA")

# remove any columns with all-NAs
Data_PPR_2024 <- Data_PPR_2024 %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(Data_PPR_2024)[1] <- "Pond"
names(Data_PPR_2024)[3] <- "Date"
names(Data_PPR_2024)[5] <- "Fe_mg.L"
names(Data_PPR_2024)[6] <- "SO4_mg.L"
names(Data_PPR_2024)[7] <- "DOC_mg.L"
names(Data_PPR_2024)[8] <- "DOC_mg.L_0.45"
names(Data_PPR_2024)[9] <- "Abs254nm_1.2"
names(Data_PPR_2024)[10] <- "Abs254nm_0.45"
names(Data_PPR_2024)[11] <- "SUVA"
names(Data_PPR_2024)[12] <- "Bottle_PCO2"
names(Data_PPR_2024)[13] <- "Wholewater_THg_ng.L"
names(Data_PPR_2024)[14] <- "Wholewater_MeHg_ng.L"
names(Data_PPR_2024)[15] <- "MeHg_std.err"
names(Data_PPR_2024)[16] <- "MeHg_perc"
names(Data_PPR_2024)[17] <- "CO2_umol.L"
names(Data_PPR_2024)[18] <- "CH4_umol.L"
names(Data_PPR_2024)[19] <- "DIC_mg.L"
names(Data_PPR_2024)[20] <- "DIC_mg.L_0.45"
names(Data_PPR_2024)[21] <- "TDC_mg.L"
names(Data_PPR_2024)[22] <- "Chla"

# fix date format
Data_PPR_2024$Date <- as.Date(Data_PPR_2024$Date)

# fix pond IDs
Data_PPR_2024$Pond[Data_PPR_2024$Pond == "125 N"] <- "125N"
Data_PPR_2024$Pond[Data_PPR_2024$Pond == "97"] <- "97/98"

# make new column to put "Dry" or "NO YSI" conditions for pH
Data_PPR_2024$Comments <- ifelse(grepl("^[0-9.]+$", Data_PPR_2024$pH), NA, Data_PPR_2024$pH)

# remove columns that are duplicated from Abs_PPR_2024 df
Data_PPR_2024 <- Data_PPR_2024 %>% 
  select(-c("Abs254nm_1.2", "Abs254nm_0.45"))

# merge DOC and DIC columns with different filter size
Data_PPR_2024 <- Data_PPR_2024 %>%
  mutate(DOC_mg.L = ifelse(!is.na(DOC_mg.L) & !is.na(DOC_mg.L_0.45),
      (DOC_mg.L + DOC_mg.L_0.45) / 2, # calculate mean if both columns have values
      coalesce(DOC_mg.L, DOC_mg.L_0.45))) # select non-NA value from the two columns
Data_PPR_2024 <- Data_PPR_2024 %>%
  mutate(DIC_mg.L = ifelse(!is.na(DIC_mg.L) & !is.na(DIC_mg.L_0.45),
      (DIC_mg.L + DIC_mg.L_0.45) / 2, # calculate mean if both columns have values
      coalesce(DIC_mg.L, DIC_mg.L_0.45))) # select non-NA value from the two columns

# remove unwanted columns
Data_PPR_2024 <- Data_PPR_2024 %>% 
  select(-c("DOC_mg.L_0.45", "DIC_mg.L_0.45"))


#-------------------------------------------------------------------------------

### PPR Field data df (Field_PPR_2024)

#-------------------------------------------------------------------------------

# select St. Denis region
Field_PPR_2024 <- Field_PPR_2024 %>% 
  filter(Region == "SDNWA")

# remove any columns with all-NAs
Field_PPR_2024 <- Field_PPR_2024 %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(Field_PPR_2024)[1] <- "Pond"
names(Field_PPR_2024)[3] <- "Date"
names(Field_PPR_2024)[4] <- "Temp_degC"
names(Field_PPR_2024)[5] <- "Cond_uS.cm"
names(Field_PPR_2024)[6] <- "DO_perc"
names(Field_PPR_2024)[7] <- "DO_mg.L"
names(Field_PPR_2024)[9] <- "Secchi_cm"
names(Field_PPR_2024)[10] <- "Depth_m"
names(Field_PPR_2024)[11] <- "Secchi_on.ground"
names(Field_PPR_2024)[12] <- "ORP"
names(Field_PPR_2024)[13] <- "Airtemp_C"
names(Field_PPR_2024)[14] <- "Cloud"
names(Field_PPR_2024)[16] <- "Comments"

# fix date format
Field_PPR_2024$Date <- as.Date(Field_PPR_2024$Date)

# put Pond 97 with 98
Field_PPR_2024$Pond[Field_PPR_2024$Pond == "97"] <- "97/98"

# Replace all white-space-only cells with NA
Field_PPR_2024 <- Field_PPR_2024 %>%
  mutate(across(c(4:16), ~ ifelse(str_squish(.) == "", NA, .)))

# manually entered vegetation notes into Comments column because there was only two
# remove veg column 
Field_PPR_2024 <- Field_PPR_2024 %>% 
  select(!Vegetation)

# fix Cloud column
Field_PPR_2024$Cloud[Field_PPR_2024$Cloud == "?"] <- NA
Field_PPR_2024$Cloud <- as.numeric(Field_PPR_2024$Cloud)

# put Secchi_on.ground column into Comments column if not NA
Field_PPR_2024 <- Field_PPR_2024 %>%
  mutate(Comments = ifelse(!is.na(Secchi_on.ground),
      ifelse(is.na(Comments), 
        paste("Secchi on ground? ", Secchi_on.ground), 
        paste(Comments, paste("Secchi on ground?", Secchi_on.ground), sep = "; ")),
      Comments))

# remove Secchi_on.ground column
Field_PPR_2024 <- Field_PPR_2024 %>% 
  select(!Secchi_on.ground)


#-------------------------------------------------------------------------------

### Merge PPR datasets so far

#-------------------------------------------------------------------------------

PPR_merged <- full_join(Abs_PPR_2024, Data_PPR_2024)
PPR_merged <- full_join(PPR_merged, Field_PPR_2024)


#-------------------------------------------------------------------------------

### 2006 Data df, extra PPR data from 2006-2009 (Data_2006)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
Data_2006 <- Data_2006 %>% 
  select(where(~ !all(is.na(.))))

# remove columns that contain duplicate info
Data_2006 <- Data_2006 %>% 
  select(-c('Date stamp', '...22'))

# there's a bunch of column with values in it but no column name
# not sure what to do, but I will be removing them for now 
Data_2006 <- Data_2006 %>% 
  select(-c("...16", "...41", "...42", "...43", "...44", "...45", "...51", "...52"))

# I also removed ABS columns for now because I have no idea what it is or its units
Data_2006 <- Data_2006 %>% 
  select(-c('ABS', 'ABS /m')) # asked Britt - she hasn't replied

# select St. Denis region
Data_2006 <- Data_2006 %>% 
  filter(Region == "SDNWA")

# fix date format
Data_2006$Date <- as.Date(Data_2006$Date)

# move Date column to after Region
Data_2006 <- Data_2006 %>% 
  relocate(Date, .after = Region)

# rename columns
names(Data_2006)[1] <- "Pond"
names(Data_2006)[4] <- "Temp_degC"
names(Data_2006)[5] <- "Cond_uS.cm" # really in mS/cm - have to change later
names(Data_2006)[6] <- "DO_mg.L"
names(Data_2006)[8] <- "Wholewater_THg_ng.L"
names(Data_2006)[9] <- "Wholewater_MeHg_ng.L"
names(Data_2006)[10] <- "MeHg_std.err"
names(Data_2006)[11] <- "MeHg_perc"
names(Data_2006)[12] <- "SO4_mg.L"
names(Data_2006)[13] <- "DOC_mg.L"
names(Data_2006)[14] <- "DIC_umol.L" 
names(Data_2006)[15] <- "TDC_mg.L"
names(Data_2006)[16] <- "SUVA" 

# clean up pond names
Data_2006$Pond <- sub("Pond ", "", Data_2006$Pond)
Data_2006$Pond [] <- lapply(Data_2006$Pond , function(x) gsub(" ", "", x))
Data_2006$Pond  <- as.character(Data_2006$Pond )

# put Pond 97 with 98
Data_2006$Pond[Data_2006$Pond == "97"] <- "97/98"

# update units for conductivity and DIC
Data_2006$Cond_uS.cm <- Data_2006$Cond_uS.cm*1000
Data_2006$DIC_umol.L <- Data_2006$DIC_umol.L*0.01201
Data_2006 <- Data_2006 %>% 
  rename(DIC_mg.L = DIC_umol.L) # rename to match unit conversion

# change some columns to chr or numeric for merging
Data_2006$pH <- as.character(Data_2006$pH)
Data_2006$SO4_mg.L <- as.character(Data_2006$SO4_mg.L)
Data_2006$MeHg_perc <- as.numeric(Data_2006$MeHg_perc)
Data_2006$Temp_degC <- as.character(Data_2006$Temp_degC)
Data_2006$Cond_uS.cm <- as.character(Data_2006$Cond_uS.cm)
Data_2006$DO_mg.L <- as.character(Data_2006$DO_mg.L)

#

#-------------------------------------------------------------------------------

### Merge PPR datasets with 2006 data

#-------------------------------------------------------------------------------

# merge
PPR_merged <- full_join(PPR_merged, Data_2006)

# move comments from CO2, CH4, and YSI variable columns into Comments column
PPR_merged$Comments <- ifelse(grepl("^[0-9.]+$", PPR_merged$CO2_umol.L), NA, PPR_merged$CO2_umol.L)
PPR_merged$Comments <- ifelse(grepl("^[0-9.]+$", PPR_merged$Temp_degC), NA, PPR_merged$Temp_degC)

# make columns numeric
PPR_merged$pH <- as.numeric(PPR_merged$pH)
PPR_merged$SO4_mg.L <- as.numeric(PPR_merged$SO4_mg.L)
PPR_merged$CO2_umol.L <- as.numeric(PPR_merged$CO2_umol.L)
PPR_merged$CH4_umol.L <- as.numeric(PPR_merged$CH4_umol.L)
PPR_merged$Temp_degC <- as.numeric(PPR_merged$Temp_degC)
PPR_merged$Cond_uS.cm <- as.numeric(PPR_merged$Cond_uS.cm)
PPR_merged$DO_perc <- as.numeric(PPR_merged$DO_perc)
PPR_merged$DO_mg.L <- as.numeric(PPR_merged$DO_mg.L)
PPR_merged$Secchi_cm <- as.numeric(PPR_merged$Secchi_cm)
PPR_merged$ORP <- as.numeric(PPR_merged$ORP)
PPR_merged$Airtemp_C <- as.numeric(PPR_merged$Airtemp_C)

# deal with duplicates
PPR_merged <- PPR_merged %>%
  group_by(Pond, Date) %>%
  summarize(
    across(
      .cols = everything(),
      .fns = ~ ifelse(all(is.na(.)), NA, 
                      ifelse(is.numeric(.), mean(., na.rm = TRUE), 
                             paste(na.omit(unique(.)), collapse = "; "))),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# fix pond IDs
PPR_merged$Pond[PPR_merged$Pond == "125 N"] <- "125N"
PPR_merged$Pond[PPR_merged$Pond == "125 S"] <- "125S"

# remove sample with no date
PPR_merged <- PPR_merged %>% 
  filter(!is.na(Date))

# write as new csv
write.csv(PPR_merged,"02_tidydata/PPR_merged_2006-2024.csv")

#

#-------------------------------------------------------------------------------

### Convert PPR_merged to long format for DataStream template

#-------------------------------------------------------------------------------

# change all variables to chr
PPR_merged$"250_nm" <- as.character(PPR_merged$"250_nm")
PPR_merged$"254_nm" <- as.character(PPR_merged$"254_nm")
PPR_merged$"280_nm" <- as.character(PPR_merged$"280_nm")
PPR_merged$"350_nm" <- as.character(PPR_merged$"350_nm")
PPR_merged$"365_nm" <- as.character(PPR_merged$"365_nm")
PPR_merged$"370_nm" <- as.character(PPR_merged$"370_nm")
PPR_merged$DOC_mg.L <- as.character(PPR_merged$DOC_mg.L)
PPR_merged$SUVA <- as.character(PPR_merged$SUVA)
PPR_merged$Wholewater_THg_ng.L <- as.character(PPR_merged$Wholewater_THg_ng.L)
PPR_merged$Wholewater_MeHg_ng.L <- as.character(PPR_merged$Wholewater_MeHg_ng.L)
PPR_merged$MeHg_perc <- as.character(PPR_merged$MeHg_perc)
PPR_merged$DIC_mg.L <- as.character(PPR_merged$DIC_mg.L)
PPR_merged$TDC_mg.L <- as.character(PPR_merged$TDC_mg.L)
PPR_merged$Chla <- as.character(PPR_merged$Chla)
PPR_merged$Depth_m <- as.character(PPR_merged$Depth_m)
PPR_merged$Cloud <- as.character(PPR_merged$Cloud)
PPR_merged$pH <- as.character(PPR_merged$pH)
PPR_merged$SO4_mg.L <- as.character(PPR_merged$SO4_mg.L)
PPR_merged$CO2_umol.L <- as.character(PPR_merged$CO2_umol.L)
PPR_merged$CH4_umol.L <- as.character(PPR_merged$CH4_umol.L)
PPR_merged$Temp_degC <- as.character(PPR_merged$Temp_degC)
PPR_merged$Cond_uS.cm <- as.character(PPR_merged$Cond_uS.cm)
PPR_merged$DO_perc <- as.character(PPR_merged$DO_perc)
PPR_merged$DO_mg.L <- as.character(PPR_merged$DO_mg.L)
PPR_merged$Secchi_cm <- as.character(PPR_merged$Secchi_cm)
PPR_merged$ORP <- as.character(PPR_merged$ORP)
PPR_merged$Airtemp_C <- as.character(PPR_merged$Airtemp_C)

# convert to long format - leave in all variables already on DataStream
# will update as new file so will overwrite what's already published
PPR_merged_long <- pivot_longer(PPR_merged, 
                                       cols = c("250_nm", "254_nm", "280_nm", "350_nm", 
                                                "365_nm", "370_nm", "pH", "Fe_mg.L", 
                                                "SO4_mg.L", "DOC_mg.L", "SUVA",
                                                "Wholewater_THg_ng.L", 
                                                "Wholewater_MeHg_ng.L", "MeHg_perc", 
                                                "CO2_umol.L", "CH4_umol.L", "DIC_mg.L", 
                                                "TDC_mg.L", "Chla", "Temp_degC",
                                                "Cond_uS.cm", "DO_perc", "DO_mg.L", "Secchi_cm", 
                                                "ORP", "Airtemp_C", "Cloud"),
                                       names_to = "CharacteristicID", 
                                       values_to = "ResultValue")

# export file to be copied to Data Stream
write.csv(PPR_merged_long,"05_DataStream/PPR_merged_long_2006-2024.csv")


#


#-------------------------------------------------------------------------------

### SDWS chlorophyll df (SDWS_chla)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
SDWS_chla <- SDWS_chla %>% 
  select(where(~ !all(is.na(.))))

# remove Tube column
SDWS_chla <- SDWS_chla[, -1]

# rename columns
names(SDWS_chla)[1] <- "Pond"
names(SDWS_chla)[3] <- "Depth_m"
names(SDWS_chla)[4] <- "mL_filtered"
names(SDWS_chla)[5] <- "L_filtered"
names(SDWS_chla)[6] <- "Rep"
names(SDWS_chla)[7] <- "Abs_750nm"
names(SDWS_chla)[8] <- "Abs_665nm"
names(SDWS_chla)[9] <- "Abs_649nm"
names(SDWS_chla)[10] <- "Chla_ug.L"
names(SDWS_chla)[11] <- "Avg_Chla_ug.L" 
names(SDWS_chla)[12] <- "Error"
names(SDWS_chla)[14] <- "Comments"

# fix pond names
SDWS_chla$Pond <- gsub("\\.0$","",SDWS_chla$Pond)
SDWS_chla$Pond <- gsub("\\?$","",SDWS_chla$Pond)
SDWS_chla$Pond <- gsub("1255","125S",SDWS_chla$Pond)
SDWS_chla$Pond[SDWS_chla$Pond == "125"] <- "125S"
SDWS_chla$Pond[SDWS_chla$Pond == "159"] <- "15" # no such thing as pond 159 - typo

# fix date errors to match with SDWS_wq file
SDWS_chla$Date <- gsub("2022-05-18","2022-05-17", SDWS_chla$Date)
SDWS_chla <- SDWS_chla %>%
  mutate(Date = case_when(Pond == "39" & Date == "2016-07-18" ~ "2016-07-19", TRUE ~ Date))
SDWS_chla <- SDWS_chla %>%
  mutate(Date = case_when(Pond == "109" & Date == "2021-07-15" ~ "2021-07-16", TRUE ~ Date))
SDWS_chla <- SDWS_chla %>%
  mutate(Date = case_when(Pond == "5340" & Date == "2016-07-18" ~ "2016-07-19", TRUE ~ Date))
SDWS_chla <- SDWS_chla %>%
  mutate(Date = case_when(Pond == "67" & Date == "2016-07-18" ~ "2016-07-19", TRUE ~ Date))
SDWS_chla <- SDWS_chla %>%
  mutate(Date = case_when(Pond == "50" & Date == "2016-07-18" ~ "2016-07-19", TRUE ~ Date))
SDWS_chla <- SDWS_chla %>%
  mutate(Date = case_when(Pond == "40" & Date == "2016-07-18" ~ "2016-07-19", TRUE ~ Date))
SDWS_chla <- SDWS_chla %>%
  mutate(Date = case_when(Pond == "60" & Date == "2016-07-18" ~ "2016-07-19", TRUE ~ Date))
SDWS_chla <- SDWS_chla %>%
  mutate(Date = case_when(Pond == "63" & Date == "2016-07-18" ~ "2016-07-19", TRUE ~ Date))

# fix date format
SDWS_chla$Date <- as.Date(SDWS_chla$Date)
SDWS_chla$RunDate <- as.Date(SDWS_chla$RunDate)

# if there were multiple reps done, keep Avg_Chla_ug.L, otherwise keep Chla_ug.L
SDWS_chla <- SDWS_chla %>% 
  mutate(Chla_ug.L = ifelse(is.na(Avg_Chla_ug.L), Chla_ug.L, Avg_Chla_ug.L))

# remove duplicate rows
SDWS_chla <- SDWS_chla %>% 
  filter(!Rep == 2)

# only keep relevant columns to merge with other SDWS df
SDWS_chla <- SDWS_chla %>% 
  select(c(Pond, Date, Chla_ug.L, Error, RunDate, Comments))

# look for duplicates
duplicated(SDWS_chla$Chla_ug.L) 
SDWS_chla[duplicated(SDWS_chla$Chla_ug.L) | duplicated(SDWS_chla$Chla_ug.L, fromLast = TRUE), ]
duplicated(SDWS_chla)

# add column to know which df this came from
SDWS_chla$DataFile <- "SDWS_chla"

#

#-------------------------------------------------------------------------------

### SDWS isotopes df (SDWS_iso)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
SDWS_iso <- SDWS_iso %>% 
  select(where(~ !all(is.na(.))))

# remove Analysis_ID column
SDWS_iso <- SDWS_iso[, -1]

# rename columns
names(SDWS_iso)[1] <- "Pond"
names(SDWS_iso)[2] <- "Date"
names(SDWS_iso)[3] <- "Delta2H"
names(SDWS_iso)[4] <- "Delta18O"
names(SDWS_iso)[5] <- "Comments"

# fix pond names
SDWS_iso$Pond <- gsub("SDNWA ","",SDWS_iso$Pond)
SDWS_iso$Pond <- gsub("a","A",SDWS_iso$Pond)
SDWS_iso$Pond <- gsub("\\?$","",SDWS_iso$Pond)

# fix date errors to match with SDWS_wq
SDWS_iso <- SDWS_iso %>%
  mutate(Date = case_when(Pond == "10" & Date == "2014-08-21" ~ "2014-08-25", TRUE ~ Date))


# fix date format
SDWS_iso$Date <- as.Date(SDWS_iso$Date)

# look for duplicates
duplicated(SDWS_iso) # all good

# add column to know which df this came from
SDWS_iso$DataFile <- "SDWS_iso"

# matched up with SDWS_wq dataset, they are the same except for 12 rows with
# weird pond IDs that don't match the SDWS map  

#

#-------------------------------------------------------------------------------

### SDWS water quality df (SDWS_wq)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
SDWS_wq <- SDWS_wq %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(SDWS_wq)[4] <- "Temp_degC"
names(SDWS_wq)[6] <- "SPC_uS.cm"
names(SDWS_wq)[7] <- "DO_mg.L"
names(SDWS_wq)[8] <- "SRP_mg.L"
names(SDWS_wq)[9] <- "TDP_mg.L"
names(SDWS_wq)[10] <- "TP_mg.L"
names(SDWS_wq)[11] <- "NH3_mg.L"
names(SDWS_wq)[12] <- "Urea_mg.L"
names(SDWS_wq)[13] <- "NO3_mg.L"
names(SDWS_wq)[14] <- "TDN_mg.L"
names(SDWS_wq)[15] <- "TN_mg.L"
names(SDWS_wq)[16] <- "SO4_mg.L"
names(SDWS_wq)[17] <- "Delta2H"
names(SDWS_wq)[18] <- "Delta18O"
names(SDWS_wq)[19] <- "Chla_ug.L"
names(SDWS_wq)[20] <- "Alk_mg.L"
names(SDWS_wq)[21] <- "Comments"

# fix date format
SDWS_wq$Date <- as.Date(SDWS_wq$Date)

# add column to know which df this came from
SDWS_wq$DataFile <- "SDWS_wq"


#

#-------------------------------------------------------------------------------

### Merge SDWS datasets together

#-------------------------------------------------------------------------------

# merge
SDWS_merged <- full_join(SDWS_wq, SDWS_chla) # isotope data already in wq df

# deal with duplicates
SDWS_merged <- SDWS_merged %>%
  group_by(Pond, Date) %>%
  summarize(
    across(
      .cols = everything(),
      .fns = ~ ifelse(all(is.na(.)), NA, 
                      ifelse(is.numeric(.), mean(., na.rm = TRUE), 
                             paste(na.omit(unique(.)), collapse = "; "))),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# look for any more duplicates 
duplicated(SDWS_merged)
dups <- SDWS_merged[duplicated(SDWS_merged$Chla_ug.L) | duplicated(SDWS_merged$Chla_ug.L, fromLast = TRUE), ]

# remove rows with no date
SDWS_merged <- SDWS_merged %>% 
  filter(!is.na(Date))

# write as new csv
write.csv(SDWS_merged,"02_tidydata/SDWS_merged_2014-2022.csv")


#

#-------------------------------------------------------------------------------

### Baulch df (Baulch)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
Baulch <- Baulch %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(Baulch)[4] <- "Temp_degC"
names(Baulch)[6] <- "Cond_uS.cm"
names(Baulch)[7] <- "DO_mg.L"
names(Baulch)[8] <- "TRP_mg.L"
names(Baulch)[9] <- "SRP_mg.L"
names(Baulch)[10] <- "TDP_mg.L"
names(Baulch)[11] <- "TP_mg.L"
names(Baulch)[12] <- "NH3_mg.L"
names(Baulch)[13] <- "Urea_mg.L"
names(Baulch)[14] <- "NO3_mg.L"
names(Baulch)[15] <- "TN_mg.L"
names(Baulch)[16] <- "SO4_mg.L"
names(Baulch)[17] <- "Chla_ug.L"
names(Baulch)[18] <- "Alk_mg.L"
names(Baulch)[19] <- "TDN_mg.L"
# will use the method detection limits in DataStream template 

# remove row with no data in it
Baulch <- Baulch[Baulch$Pond != "Pond", ]

# fix pond IDs
Baulch <- Baulch[!is.na(Baulch$Pond), ]
Baulch$Pond[Baulch$Pond == "125s"] <- "125S"
Baulch$Pond[Baulch$Pond == "139a"] <- "139A"
Baulch$Pond[Baulch$Pond == "27a"] <- "27A" 
Baulch$Pond <- gsub("\\.0$","",Baulch$Pond)

# fix date format
Baulch$Date <- as.numeric(Baulch$Date)
Baulch$Date <- as.Date(Baulch$Date, format = "%Y-%m-%d", origin = "1899-12-30")

# fix date errors to match with SDWS_merged
Baulch$Date <- as.character(Baulch$Date)
Baulch$Date <- gsub("2016-07-18","2016-07-19", Baulch$Date)
Baulch <- Baulch %>%
  mutate(Date = case_when(Pond == "1" & Date == "2020-09-11" ~ "2020-09-09", TRUE ~ Date))
Baulch <- Baulch %>%
  mutate(Date = case_when(Pond == "60" & Date == "2022-05-17" ~ "2022-05-11", TRUE ~ Date))
Baulch$Date <- as.Date(Baulch$Date)

# remove rows with no date
Baulch <- Baulch %>% 
  filter(!is.na(Date))

# fix time format
Baulch$Time <- as.numeric(Baulch$Time)
Baulch$Time <- as_hms(Baulch$Time * 86400) # 86400 is the number of seconds in a day
Baulch$Time <- as.character(Baulch$Time) # for merging

# add column to know which df this came from
Baulch$DataFile <- "Baulch"

# write as new csv
write.csv(Baulch,"02_tidydata/Baulch_2015-2022.csv")


#

#-------------------------------------------------------------------------------

### Merge Baulch with SDWS_merged

#-------------------------------------------------------------------------------

# convert all variables to chr for merging 
SDWS_merged$Temp_degC <- as.character(SDWS_merged$Temp_degC)
SDWS_merged$pH <- as.character(SDWS_merged$pH)
SDWS_merged$SPC_uS.cm <- as.character(SDWS_merged$SPC_uS.cm)
SDWS_merged$DO_mg.L <- as.character(SDWS_merged$DO_mg.L)
SDWS_merged$SRP_mg.L <- as.character(SDWS_merged$SRP_mg.L)
SDWS_merged$TDP_mg.L <- as.character(SDWS_merged$TDP_mg.L)
SDWS_merged$TP_mg.L <- as.character(SDWS_merged$TP_mg.L)
SDWS_merged$NH3_mg.L <- as.character(SDWS_merged$NH3_mg.L)
SDWS_merged$Urea_mg.L <- as.character(SDWS_merged$Urea_mg.L)
SDWS_merged$NO3_mg.L <- as.character(SDWS_merged$NO3_mg.L)
SDWS_merged$TDN_mg.L <- as.character(SDWS_merged$TDN_mg.L)
SDWS_merged$TN_mg.L <- as.character(SDWS_merged$TN_mg.L)
SDWS_merged$SO4_mg.L <- as.character(SDWS_merged$SO4_mg.L)
SDWS_merged$Chla_ug.L <- as.character(SDWS_merged$Chla_ug.L)
SDWS_merged$Alk_mg.L <- as.character(SDWS_merged$Alk_mg.L)

# merge 
SDWS_Baulch_merged <- full_join(SDWS_merged, Baulch)

# move DO comment from DO column to Comments column
SDWS_Baulch_merged <- SDWS_Baulch_merged %>% 
  mutate(Comments = ifelse(!is.na(as.numeric(DO_mg.L)), Comments, 
                           ifelse(is.na(Comments), DO_mg.L, paste(Comments, DO_mg.L, sep = "; "))))

# convert columns to numeric
SDWS_Baulch_merged$Temp_degC <- as.numeric(SDWS_Baulch_merged$Temp_degC)
SDWS_Baulch_merged$pH <- as.numeric(SDWS_Baulch_merged$pH)
SDWS_Baulch_merged$Cond_uS.cm <- as.numeric(SDWS_Baulch_merged$Cond_uS.cm)
SDWS_Baulch_merged$DO_mg.L <- as.numeric(SDWS_Baulch_merged$DO_mg.L)
SDWS_Baulch_merged$SRP_mg.L <- as.numeric(SDWS_Baulch_merged$SRP_mg.L)
SDWS_Baulch_merged$TDP_mg.L <- as.numeric(SDWS_Baulch_merged$TDP_mg.L)
SDWS_Baulch_merged$TP_mg.L <- as.numeric(SDWS_Baulch_merged$TP_mg.L)
SDWS_Baulch_merged$NH3_mg.L <- as.numeric(SDWS_Baulch_merged$NH3_mg.L)
SDWS_Baulch_merged$Urea_mg.L <- as.numeric(SDWS_Baulch_merged$Urea_mg.L)
SDWS_Baulch_merged$NO3_mg.L <- as.numeric(SDWS_Baulch_merged$NO3_mg.L)
SDWS_Baulch_merged$TDN_mg.L <- as.numeric(SDWS_Baulch_merged$TDN_mg.L)
SDWS_Baulch_merged$TN_mg.L <- as.numeric(SDWS_Baulch_merged$TN_mg.L)
SDWS_Baulch_merged$SO4_mg.L <- as.numeric(SDWS_Baulch_merged$SO4_mg.L)
SDWS_Baulch_merged$Chla_ug.L <- as.numeric(SDWS_Baulch_merged$Chla_ug.L)
SDWS_Baulch_merged$Alk_mg.L <- as.numeric(SDWS_Baulch_merged$Alk_mg.L)
SDWS_Baulch_merged$TRP_mg.L <- as.numeric(SDWS_Baulch_merged$TRP_mg.L)

# standardize numeric precision (e.g., 3 decimal places)
numeric_columns <- SDWS_Baulch_merged %>%
  select(where(is.numeric)) %>%
  colnames()
SDWS_Baulch_merged <- SDWS_Baulch_merged %>%
  mutate(across(all_of(numeric_columns), ~ round(.x, 3)))

# deal with duplicates
SDWS_Baulch_merged <- SDWS_Baulch_merged %>%
  group_by(Pond, Date) %>%
  summarize(
    across(
      .cols = everything(),
      .fns = ~ ifelse(all(is.na(.)), NA, 
                      ifelse(is.numeric(.), mean(., na.rm = TRUE), 
                             paste(na.omit(unique(.)), collapse = "; "))),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# still duplicates - dates are off by one day
# match rows with DataFile containing "SDWS_wq"
SDWS_Baulch_merged <- SDWS_Baulch_merged %>%
  arrange(Pond, Date) %>% 
  # Create a helper column to group dates within 1 day
  group_by(Pond) %>%
  mutate(Date_group = cumsum(c(0, diff(as.Date(Date)) > 1))) %>%
  ungroup() %>%
  # Summarize by Pond and Date_group
  group_by(Pond, Date_group) %>%
  summarize(
    # Match Date with rows where DataFile contains "SDWS_wq"
    Date = ifelse(
      any(grepl("SDWS_wq", DataFile, ignore.case = TRUE)),
      # Select the Date corresponding to the first match
      Date[grep("SDWS_wq", DataFile, ignore.case = TRUE)[1]],
      min(Date) # If no match, use the earliest date
    ),
    across(
      .cols = setdiff(names(SDWS_Baulch_merged), c("Pond", "Date_group", "Date", "DataFile")),
      .fns = ~ ifelse(all(is.na(.)), NA, 
                      ifelse(is.numeric(.), mean(., na.rm = TRUE), 
                             paste(na.omit(unique(.)), collapse = "; "))),
      .names = "{.col}"
    ),
    DataFile = paste(unique(DataFile), collapse = "; "),
    .groups = "drop"
  ) %>%
  select(-Date_group) # Remove helper column

# fix date column
SDWS_Baulch_merged$Date <- as.Date(SDWS_Baulch_merged$Date, origin = "1970-01-01") # For Unix Epoch
SDWS_Baulch_merged$RunDate <- as.Date(SDWS_Baulch_merged$RunDate)

# fix SPC column
SDWS_Baulch_merged$SPC_uS.cm <- gsub("1.42; 1.501","1.46",SDWS_Baulch_merged$SPC_uS.cm) # average it
SDWS_Baulch_merged$SPC_uS.cm <- as.numeric(SDWS_Baulch_merged$SPC_uS.cm)
SDWS_Baulch_merged <- SDWS_Baulch_merged %>% 
  mutate(SPC_uS.cm = ifelse(SPC_uS.cm < 90, SPC_uS.cm*1000, SPC_uS.cm)) # convert to proper units

# SPC and Cond column have identical values - combine them
SDWS_Baulch_merged <- SDWS_Baulch_merged %>% 
  mutate(Cond_uS.cm = coalesce(SPC_uS.cm, Cond_uS.cm)) %>% 
  relocate(Cond_uS.cm, .after = pH)
SDWS_Baulch_merged <- SDWS_Baulch_merged %>% 
  select(!SPC_uS.cm)

# fix pond ID
SDWS_Baulch_merged$Pond[SDWS_Baulch_merged$Pond == "97"] <- "97/98"

# write as new csv
write.csv(SDWS_Baulch_merged,"02_tidydata/SDWS_Baulch_merged_2014-2022.csv")


#

#-------------------------------------------------------------------------------

### Convert SDWS_Baulch_merged to long format for DataStream template

#-------------------------------------------------------------------------------

# convert to long format - leave out temp, DO + pH bc already on DataStream?
SDWS_Baulch_merged_long <- pivot_longer(SDWS_Baulch_merged, 
                                 cols = c("Delta2H", "Delta18O", "Temp_degC", 
                                          "pH", "Cond_uS.cm", "DO_mg.L", "SRP_mg.L", 
                                          "TDP_mg.L", "TP_mg.L", "NH3_mg.L", 
                                          "Urea_mg.L", "NO3_mg.L", "TDN_mg.L",
                                          "TN_mg.L", "SO4_mg.L", "Chla_ug.L",
                                          "Alk_mg.L", "TRP_mg.L"),
                                 names_to = "CharacteristicID", 
                                 values_to = "ResultValue")

# export file to be copied to Data Stream
write.csv(SDWS_Baulch_merged_long,"05_DataStream/SDWS_Baulch_merged_long_2014-2022.csv")


#



#-------------------------------------------------------------------------------

### THG df (THG_data)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
THG_data <- THG_data %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(THG_data)[1] <- "Pond"
names(THG_data)[4] <- "Analyzing_date"
names(THG_data)[5] <- "Date"
names(THG_data)[9] <- "THg_ug.kg"
# will use method names and other details in raw file for DataStream template

# fix date formats
THG_data$Date <- as.Date(THG_data$Date)
THG_data$Analyzing_date <- as.Date(THG_data$Analyzing_date)

# fix pond ID
THG_data$Pond[THG_data$Pond == "97"] <- "97/98"

# remove m from site column
THG_data$Site <- gsub("m","",THG_data$Site)
# not sure what "Off" means.. Off shore? I asked Britt but she hasn't replied 

# change sites to negative for DS template
THG_data <- THG_data %>%
  mutate(Site = ifelse(Site != "Off", -as.numeric(Site), Site))

# combine replicates into one mean THg value
THG_data <- THG_data %>%
  group_by(Pond, Site, Date) %>%
  mutate(mean_THg_ug.kg = ifelse(!is.na(Replicate), mean(THg_ug.kg, na.rm = TRUE), 
      THg_ug.kg)) %>%
  ungroup()

# remove duplicate rows
THG_data <- THG_data %>%
  filter(!(Replicate %in% c(2, 3)))

# remove unwanted columns
THG_data <- THG_data %>%
  select(c(Pond, Site, Analyzing_date, Date, mean_THg_ug.kg))

# rename THg column
names(THG_data)[5] <- "THg_ug.kg"

# write as new csv
write.csv(THG_data,"02_tidydata/THG_data_2021.csv")


#-------------------------------------------------------------------------------

### Convert THG_data to long format for DataStream template

#-------------------------------------------------------------------------------

# convert to long format - leave out temp, DO + pH bc already on DataStream?
THG_data_long <- pivot_longer(THG_data, cols = c("THg_ug.kg"),
                                        names_to = "CharacteristicID", 
                                        values_to = "ResultValue")

# export file to be copied to Data Stream
write.csv(THG_data_long,"05_DataStream/THG_data_long_2021.csv")


#




# export file to be copied to Data Stream
write.csv(THG_data,"05_DataStream/THG_data_long_2021.csv")



#-------------------------------------------------------------------------------

### Logan Edited Moisture content of sediment df (Sed_moisture)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
Sed_moisture <- Sed_moisture %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(Sed_moisture)[4] <- "Depth_m"
names(Sed_moisture)[5] <- "WetWeight_g"
names(Sed_moisture)[6] <- "DryWeight_g"
names(Sed_moisture)[7] <- "LOIbefore_g"
names(Sed_moisture)[8] <- "LOIafter_g"

# fix date format
Sed_moisture$Date <- as.Date(Sed_moisture$Date)
Sed_moisture <- Sed_moisture %>% 
  relocate(Date, .after = Pond)

# fix pond ID
Sed_moisture$Pond <- as.character(Sed_moisture$Pond)
Sed_moisture$Pond[Sed_moisture$Pond == "97"] <- "97/98"

# fix depth
Sed_moisture$Depth_m <- gsub("M","",Sed_moisture$Depth_m)

#-------------------------------------------------------------------------------

### Leslie Robbins Lab df (Robbins)

#-------------------------------------------------------------------------------

## manually got rid of dupicates in excel because it wasn't working in R

# remove any columns with all-NAs
Robbins <- Robbins %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(Robbins)[1] <- "Depth_cm"
names(Robbins)[2] <- "Pond"
names(Robbins)[3] <- "Be_ppm"
names(Robbins)[4] <- "B_ppm"
names(Robbins)[5] <- "Al_ppm"
names(Robbins)[6] <- "Li_ppm"
names(Robbins)[7] <- "Ga_ppm"
names(Robbins)[8] <- "Na_ppm"
names(Robbins)[9] <- "Mg_ppm"
names(Robbins)[10] <- "K_ppm"
names(Robbins)[11] <- "Ti_ppm"
names(Robbins)[12] <- "Si_ppm"
names(Robbins)[13] <- "Ca_ppm"
names(Robbins)[14] <- "Fe_ppm"
names(Robbins)[15] <- "V_ppm"
names(Robbins)[16] <- "Cr_ppm"
names(Robbins)[17] <- "Mn_ppm"
names(Robbins)[18] <- "Co_ppm"
names(Robbins)[19] <- "Ni_ppm"
names(Robbins)[20] <- "Cu_ppm"
names(Robbins)[21] <- "Zn_ppm"
names(Robbins)[22] <- "Rb_ppm"
names(Robbins)[23] <- "Sr_ppm"
names(Robbins)[24] <- "P_ppm"
names(Robbins)[25] <- "S_ppm"
names(Robbins)[26] <- "As_ppm"
names(Robbins)[27] <- "Se_ppm"
names(Robbins)[28] <- "Mo_ppm"
names(Robbins)[29] <- "Cd_ppm"
names(Robbins)[30] <- "Cs_ppm"
names(Robbins)[31] <- "Ba_ppm"
names(Robbins)[32] <- "Ce_ppm"
names(Robbins)[33] <- "Pb_ppm"
names(Robbins)[34] <- "Th_ppm"
names(Robbins)[35] <- "U_ppm"

# make Date column (file said all samples were collected on Jul 28, 2023)
Robbins$Date <- "2023-07-28" 
Robbins <- Robbins %>% 
  relocate(Date, .after = Pond)

# fix date format
Robbins$Date <- as.Date(Robbins$Date)

# remove "cm" from values in depth column
Robbins$Depth_cm <- gsub("cm","",Robbins$Depth_cm)

# add column for depth to input into DataStream
Robbins$Depth_cm_sum <- sub("-.*", "", Robbins$Depth_cm)
Robbins$Depth_cm_sum <- as.numeric(Robbins$Depth_cm_sum)
Robbins$Depth_cm_sum <- -1*(Robbins$Depth_cm_sum)

# write as new csv
write.csv(Robbins,"02_tidydata/Robbins_2023.csv")


#

#-------------------------------------------------------------------------------

### Convert Robbins to long format for DataStream template

#-------------------------------------------------------------------------------


# convert to long format
Robbins_long <- pivot_longer(Robbins,
                             cols = c("Be_ppm", "B_ppm", "Al_ppm", "Li_ppm", 
                                          "Ga_ppm", "Na_ppm", "Mg_ppm", "K_ppm", 
                                          "Ti_ppm", "Si_ppm", "Ca_ppm", 
                                          "Fe_ppm", "V_ppm", "Cr_ppm",
                                          "Mn_ppm", "Co_ppm", "Ni_ppm", 
                                          "Cu_ppm", "Zn_ppm", "Rb_ppm",
                                          "Sr_ppm", "P_ppm", "S_ppm", 
                                          "As_ppm", "Se_ppm", "Mo_ppm",
                                          "Cd_ppm", "Cs_ppm", "Ba_ppm",
                                          "Ce_ppm", "Pb_ppm", "Th_ppm", "U_ppm"),
                             names_to = "CharacteristicID", 
                             values_to = "ResultValue")

# export file to be copied to Data Stream
write.csv(Robbins_long,"05_DataStream/Robbins_long_2023.csv")


 #

#-------------------------------------------------------------------------------

### MeHg 2021 df (MeHg2021)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
MeHg2021 <- MeHg2021 %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(MeHg2021)[4] <- "Pond"
names(MeHg2021)[6] <- "Date"
names(MeHg2021)[7] <- "MeHg_ppt"

# select only columns we will be usimg
MeHg2021 <- MeHg2021 %>% 
  select(c(Pond, Date, MeHg_ppt))

# fix date format
MeHg2021$Date <- as.Date(MeHg2021$Date)

# fix pond ID
MeHg2021$Pond <- as.character(MeHg2021$Pond)
MeHg2021$Pond[MeHg2021$Pond == "97"] <- "97/98"

# after looking more closely at the data, MeHg is included in PPR_merged 


#-------------------------------------------------------------------------------

### TIC TOC df (TICTOC)

#-------------------------------------------------------------------------------

# remove any columns with all-NAs
TICTOC <- TICTOC %>% 
  select(where(~ !all(is.na(.))))

# rename columns
names(TICTOC)[2] <- "TIC_ppm"
names(TICTOC)[3] <- "TOC_ppm"
names(TICTOC)[4] <- "TIC_rerun"
names(TICTOC)[5] <- "TOC_rerun"

# make TIC_rerun numeric
TICTOC$TIC_rerun <- as.numeric(TICTOC$TIC_rerun)

# move rerun values into TIC_ppm and TOC_ppm columns
TICTOC <- TICTOC %>% 
  mutate(TOC_ppm = ifelse(!is.na(TOC_rerun), TOC_rerun, TOC_ppm), 
         TIC_ppm = ifelse(!is.na(TIC_rerun), TIC_rerun, TIC_ppm))

# remove columns we don't need/have duplicate info
TICTOC <- TICTOC %>% 
  select(c(Sample, TIC_ppm, TOC_ppm))

# no date or pond ID - need to match the Sample IDs with UniqueID df
UniqueID <- read.csv("00_rawdata/UofR data/UniqueID_PPR_2006_to_Present.csv")

# select only columns we want 
UniqueID <- UniqueID %>% 
  select(c(sample_id, pond_id, sample_date))

# rename columns to keep consistent names
names(UniqueID)[1] <- "Sample"
names(UniqueID)[2] <- "Pond"
names(UniqueID)[3] <- "Date"

# change sample to numeric to match with TICTOC df
UniqueID$Sample <- as.numeric(UniqueID$Sample)

# do inner join to only keep matching IDs
TICTOC <- inner_join(TICTOC, UniqueID, by = "Sample")

# reorder columns
TICTOC <- TICTOC %>% 
  relocate(Pond, .after = Sample) %>% 
  relocate(Date, .after = Pond)

# fix date format
TICTOC$Date <- lubridate::dmy(TICTOC$Date)

# average values with same ID and date
TICTOC <- TICTOC %>% 
  group_by(Pond, Date) %>%
  summarize(
    TIC_ppm = mean(TIC_ppm, na.rm = TRUE), 
    TOC_ppm = mean(TOC_ppm, na.rm = TRUE),
    .groups = "drop"
  )

# write as new csv
write.csv(TICTOC,"02_tidydata/TICTOC_2021-2023.csv")

#

#-------------------------------------------------------------------------------

### Convert TICTOC to long format for DataStream template

#-------------------------------------------------------------------------------

# convert to long format
TICTOC_long <- pivot_longer(TICTOC,
                             cols = c("TIC_ppm", "TOC_ppm"),
                             names_to = "CharacteristicID", 
                             values_to = "ResultValue")

# export file to be copied to Data Stream
write.csv(TICTOC_long,"05_DataStream/TICTOC_long_2021-2023.csv")


#


#-------------------------------------------------------------------------------

### SO4 df (SO4_CHmod and SO4)

#-------------------------------------------------------------------------------

# are these df the same? need to find what's different about them
SO4_CHmod <- read_excel("00_rawdata/UofR data/SO4_2021_2022 CH mod.xlsx")
SO4 <- read_excel("00_rawdata/UofR data/SO4_2021_2022.xlsx")

# Compare dimensions
dim(SO4_CHmod) == dim(SO4) # same

# Compare column names
all(names(SO4_CHmod) == names(SO4)) # same

# Rows in SO4_CHmod not in SO4
setdiff(SO4_CHmod, SO4) 

# Rows in SO4 not in SO4_CHmod
setdiff(SO4, SO4_CHmod)

# 5 rows are different - every value is the same but SO4_CHmod says 2021-06-09
# and SO4 says 2022-06-09 ... the correct date is 2022-06-09

# we can just use SO4 df and ignore SO4_CHmod
rm(SO4_CHmod)

# rename columns
names(SO4)[1] <- "Pond"
names(SO4)[2] <- "Region"
names(SO4)[3] <- "Date"
names(SO4)[4] <- "Temp_degC"
names(SO4)[5] <- "Cond_uS.cm" # column says mS/cm but values are in uS/cm
names(SO4)[6] <- "DO_perc"
names(SO4)[7] <- "DO_mg.L"
names(SO4)[8] <- "Secchi_cm"
names(SO4)[9] <- "Depth_m"
names(SO4)[10] <- "Secchi_on.ground"
names(SO4)[11] <- "ORP"
names(SO4)[12] <- "Airtemp_C"
names(SO4)[13] <- "Cloud"
names(SO4)[15] <- "Comments"

# fix pond IDs
SO4$Pond <- as.character(SO4$Pond)
SO4$Pond[SO4$Pond == "97"] <- "97/98"

# after looking more closely at the data, SO4 is included in PPR_merged 

#

#-------------------------------------------------------------------------------
