################################################################################
#
# Script: PPR data cleaning and merging
# Author: Jess Lerminiaux (adapted from Erika)
# Purpose: To clean up the PPR water quality data to be published on DataStream
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

# read in raw data to connect to PPR
Baulch <- read_excel("00_rawdata/UofR data/SDWS 2015-2022 Baulch.xlsx")
THG_data <- read.csv("00_rawdata/UofR data/DMA-80 THG Data_Cleandata.csv")
Sed_moisture <- read_excel("00_rawdata/UofR data/(Logan Edited) Moisture-Content-of-Sediment.xlsx")
Robbins <- read.csv("00_rawdata/UofR data/L.Robbins Lab Data 2021-2023 - SW Sed PW chem data.xlsx - Sediment bulk TM [clean].csv")
MeHg2021 <- read_excel("00_rawdata/UofR data/MeHg_2021_ID_Samples_results.xlsx")
SO4_CHmod <- read_excel("00_rawdata/UofR data/SO4_2021_2022 CH mod.xlsx")
SO4 <- read_excel("00_rawdata/UofR data/SO4_2021_2022.xlsx")
TICTOC <- read_excel("00_rawdata/UofR data/TICTOC Results May 30, 2023.xlsx")

# figure out whether these are already included in the master file or if I need
# to merge them with PPR_merged for DataStream

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

#

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
  select(-c('ABS', 'ABS /m')) # ask Britt?

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

# make sure no columns were accidentally deleted during merging 
all_columns <- unique(c(
  colnames(Abs_PPR_2024),
  colnames(Data_PPR_2024),
  colnames(Field_PPR_2024),
  colnames(Data_2006))) # combine column names from all datasets

# Compare with the merged dataset
missing_columns <- setdiff(all_columns, colnames(PPR_merged))
extra_columns <- setdiff(colnames(PPR_merged), all_columns)

if (length(missing_columns) > 0) {
  warning(paste("Missing columns:", paste(missing_columns, collapse = ", ")))
} else if (length(extra_columns) > 0) {
  warning(paste("Extra columns detected:", paste(extra_columns, collapse = ", ")))
} else {
  print("All expected columns are present.")
}

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
PPR_merged$DOC_mg.L_0.45 <- as.character(PPR_merged$DOC_mg.L_0.45)
PPR_merged$Abs254nm_1.2 <- as.character(PPR_merged$Abs254nm_1.2)
PPR_merged$Abs254nm_0.45 <- as.character(PPR_merged$Abs254nm_0.45)
PPR_merged$SUVA <- as.character(PPR_merged$SUVA)
PPR_merged$Wholewater_THg_ng.L <- as.character(PPR_merged$Wholewater_THg_ng.L)
PPR_merged$Wholewater_MeHg_ng.L <- as.character(PPR_merged$Wholewater_MeHg_ng.L)
PPR_merged$MeHg_perc <- as.character(PPR_merged$MeHg_perc)
PPR_merged$DIC_mg.L <- as.character(PPR_merged$DIC_mg.L)
PPR_merged$DIC_mg.L_0.45 <- as.character(PPR_merged$DIC_mg.L_0.45)
PPR_merged$TDC_mg.L <- as.character(PPR_merged$TDC_mg.L)
PPR_merged$Chla <- as.character(PPR_merged$Chla)
PPR_merged$Depth_m <- as.character(PPR_merged$Depth_m)
PPR_merged$Cloud <- as.character(PPR_merged$Cloud)

# convert to long format - leave out temp, DO, DO_perc, + Cond bc already on DataStream
PPR_merged_long <- pivot_longer(PPR_merged, 
                                       cols = c("250_nm", "254_nm", "280_nm", "350_nm", 
                                                "365_nm", "370_nm", "pH", "Fe_mg.L", 
                                                "SO4_mg.L", "DOC_mg.L", "DOC_mg.L_0.45", 
                                                "Abs254nm_1.2", "Abs254nm_0.45", "SUVA",
                                                "Wholewater_THg_ng.L", 
                                                "Wholewater_MeHg_ng.L", "MeHg_perc", 
                                                "CO2_umol.L", "CH4_umol.L", "DIC_mg.L", 
                                                "DIC_mg.L_0.45", "TDC_mg.L", "Chla", 
                                                "Secchi_cm", "Depth_m", "ORP", 
                                                "Airtemp_C", "Cloud"),
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
names(SDWS_chla)[11] <- "Avg_Chla_ug.L" # use this column bc it's mean of all reps
names(SDWS_chla)[12] <- "Error"
names(SDWS_chla)[14] <- "Comments"

# fix pond names
SDWS_chla$Pond <- gsub("\\.0$","",SDWS_chla$Pond)
SDWS_chla$Pond <- gsub("\\?$","",SDWS_chla$Pond)

# fix date format
SDWS_chla$Date <- as.Date(SDWS_chla$Date)

# only keep mean chl data bc it takes into account all reps + DS doesn't like duplicates
SDWS_chla_sum <- SDWS_chla %>% 
  filter(!is.na(Avg_Chla_ug.L))

# only keep relevant columns to merge with other SDWS df
SDWS_chla_sum <- SDWS_chla_sum %>% 
  select(c(Pond, Date, Avg_Chla_ug.L, Error, RunDate, Comments))

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

# fix date format
SDWS_iso$Date <- as.Date(SDWS_iso$Date)

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


#

#-------------------------------------------------------------------------------

### Merge SDWS datasets together

#-------------------------------------------------------------------------------

# merge
SDWS_merged <- full_join(SDWS_chla_sum, SDWS_iso)
SDWS_merged <- full_join(SDWS_merged, SDWS_wq)

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

# fix time format
Baulch$Time <- as.numeric(Baulch$Time)
Baulch$Time <- as_hms(Baulch$Time * 86400) # 86400 is the number of seconds in a day
Baulch$Time <- as.character(Baulch$Time) # for merging


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

# select columns we will need/be using
THG_data <- THG_data %>% 
  select(c(Pond, Analyzing_date, Date, THg_ug.kg))

# fix date formats
THG_data$Date <- as.Date(THG_data$Date)
THG_data$Analyzing_date <- as.Date(THG_data$Analyzing_date)

# fix pond ID
THG_data$Pond <- as.character(THG_data$Pond)
THG_data$Pond[THG_data$Pond == "97"] <- "97/98"


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

# make Date column (all samples were collected on Jul 28, 2023)
Robbins$Date <- "2023-07-28" 
Robbins <- Robbins %>% 
  relocate(Date, .after = Pond)

# fix date format
Robbins$Date <- as.Date(Robbins$Date)

# remove samples that were below detection for all elements (8 of them)
Robbins <- Robbins %>% 
  filter(!Si_ppm == "BDL")

# remove "cm" from values in depth column
Robbins$Depth_cm <- gsub("cm","",Robbins$Depth_cm)

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

#-------------------------------------------------------------------------------

### SO4 df (SO4_CHmod and SO4)

#-------------------------------------------------------------------------------

# are these df the same? need to find what's different about them

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

#-------------------------------------------------------------------------------

### Try merging df together

















#-------------------------------------------------------------------------------
