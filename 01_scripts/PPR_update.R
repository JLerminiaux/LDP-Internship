################################################################################
#
# Script: LDP WQ Merging
# Author: Jess Lerminiaux
# Purpose: Updating PPR water quality data that's already on DataStream with 
#           new 2023/2024 data 
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
library(waldo) # for comparing df
library(tidyverse)

# read in updated raw data from Britt
Field_PPR_2024 <- read_excel("00_rawdata/UofR data/All_Data_PPR_2006_to_2024 MASTER FILE.xlsx", 
                          sheet = "Field Stats",
                          na = c("", "na", "NA", "n/a", "N/A"))

#

### Updating published SDNWA data in DataStream --------------------------------

# select specific columns from field stats data
DS_df <- Field_PPR_2024 %>% 
  select(c("Site", "Region", "Date Sampled", "water temp C", "Conductivity (uS/cm)", 
           "DO (%)", "DO (mg/L)", "Depth (m)"))

# filter for the dates not included in DataStream already
DS_df <- DS_df %>% 
  filter(`Date Sampled` > "2023-08-15") %>% 
  filter(Region == "SDNWA")

# fix pond names
DS_df$Site[DS_df$Site == "125 N"] <- "125N"
DS_df$Site[DS_df$Site == "125 S"] <- "125S"
DS_df$Site[DS_df$Site == "97"] <- "97/98"

# rename columns in DS_df
names(DS_df)[1] <- "Pond"
names(DS_df)[2] <- "Region"
names(DS_df)[3] <- "Date"
names(DS_df)[4] <- "Temp_degC"
names(DS_df)[5] <- "Cond_uS.cm"
names(DS_df)[6] <- "DO_perc"
names(DS_df)[7] <- "DO_mg.L"
names(DS_df)[8] <- "Depth_m"

# convert depth to negative values for Data Stream template
DS_df$Depth_m <- as.numeric(DS_df$Depth_m)
DS_df$Depth_m <- ifelse(DS_df$Depth_m > 0, -1*DS_df$Depth_m, DS_df$Depth_m)

# convert Field Stats to long format be added to current DataStream file
DS_df_long <- pivot_longer(DS_df, cols = c("Temp_degC", "Cond_uS.cm", "DO_perc", "DO_mg.L"),
                           names_to = "CharacteristicID", values_to = "ResultValue")

# make sure results column is numeric
DS_df_long$ResultValue <- as.numeric(DS_df_long$ResultValue)

# write as new csv
write.csv(DS_df_long,"05_DataStream/PPR_update_long.csv")

#-------------------------------------------------------------------------------



# rename columns in SDNWA
names(SDNWA)[1] <- "Pond"
names(SDNWA)[3] <- "Date"
names(SDNWA)[4] <- "pH"
names(SDNWA)[5] <- "Fe_mg.L"
names(SDNWA)[6] <- "SO4_mg.L"
names(SDNWA)[7] <- "DOC_mg.L"
names(SDNWA)[8] <- "DOC_mg.L_0.45"
names(SDNWA)[9] <- "Abs254nm_1.2"
names(SDNWA)[10] <- "Abs254nm_0.45"
names(SDNWA)[11] <- "SUVA"
names(SDNWA)[12] <- "Bottle_PCO2"
names(SDNWA)[13] <- "Wholewater_THg_ng.L"
names(SDNWA)[14] <- "Wholewater_MeHg_ng.L"
names(SDNWA)[15] <- "MeHg_std.err"
names(SDNWA)[16] <- "MeHg_perc"
names(SDNWA)[17] <- "CO2_umol.L"
names(SDNWA)[18] <- "CH4_umol.L"
names(SDNWA)[19] <- "DIC_mg.L"
names(SDNWA)[20] <- "DIC_mg.L_0.45"
names(SDNWA)[21] <- "TDC_mg.L"
names(SDNWA)[22] <- "Chla"
names(SDNWA)[23] <- "DataSource"
names(SDNWA)[24] <- "Temp_C"
names(SDNWA)[25] <- "Cond_uS.cm"
names(SDNWA)[26] <- "DO"
names(SDNWA)[27] <- "DO_mg.L"
names(SDNWA)[28] <- "Secchi_cm"
names(SDNWA)[29] <- "Secchi_on.ground"
names(SDNWA)[30] <- "ORP"
names(SDNWA)[31] <- "Airtemp_C"
names(SDNWA)[32] <- "Cloud"
names(SDNWA)[33] <- "Vegetation"
names(SDNWA)[34] <- "Analysis_date"
names(SDNWA)[35] <- "Abs250nm_0.45"
names(SDNWA)[36] <- "Abs280nm_0.45"
names(SDNWA)[37] <- "Abs350nm_0.45"
names(SDNWA)[38] <- "Abs365nm_0.45"
names(SDNWA)[39] <- "Abs370nm_0.45"
names(SDNWA)[40] <- "Abs250nm_1.2"
names(SDNWA)[41] <- "Abs280nm_1.2"
names(SDNWA)[42] <- "Abs350nm_1.2"
names(SDNWA)[43] <- "Abs365nm_1.2"
names(SDNWA)[44] <- "Abs370nm_1.2"
names(SDNWA)[45] <- "Time"
names(SDNWA)[46] <- "TRP_mg.L"
names(SDNWA)[47] <- "SRP_mg.L"
names(SDNWA)[48] <- "TDP_mg.L"
names(SDNWA)[49] <- "TP_mg.L"
names(SDNWA)[50] <- "NH3_mg.L"
names(SDNWA)[51] <- "Urea_mg.L"
names(SDNWA)[52] <- "NO3_mg.L"
names(SDNWA)[53] <- "TN_mg.L"
names(SDNWA)[54] <- "SO4_mg.L"
names(SDNWA)[55] <- "Alk_mg.L"
names(SDNWA)[56] <- "TDN_mg.L"
names(SDNWA)[57] <- "Site"
names(SDNWA)[58] <- "Replicate"
names(SDNWA)[59] <- "Weight_g"
names(SDNWA)[60] <- "Height"
names(SDNWA)[61] <- "Hg_ng"
names(SDNWA)[62] <- "Concentration_ug.kg"
names(SDNWA)[63] <- "Lat"
names(SDNWA)[64] <- "Long"
names(SDNWA)[65] <- "Class"
names(SDNWA)[66] <- "Other_observations"
names(SDNWA)[67] <- "Comments"

# rename columns in SDWS
names(SDWS)[6] <- "SPC_uS.cm"
names(SDWS)[7] <- "DO_mg.L"
names(SDWS)[8] <- "SRP_mg.L"
names(SDWS)[9] <- "TDP_mg.L"
names(SDWS)[10] <- "TP_mg.L"
names(SDWS)[11] <- "NH3_mg.L"
names(SDWS)[12] <- "Urea_mg.L"
names(SDWS)[13] <- "NO3_mg.L"
names(SDWS)[14] <- "TDN_mg.L"
names(SDWS)[15] <- "TN_mg.L"
names(SDWS)[16] <- "SO4_mg.L"
names(SDWS)[17] <- "Delta2H"
names(SDWS)[18] <- "Delta18O"
names(SDWS)[19] <- "Chla_ug.L"
names(SDWS)[20] <- "Alk_mg.L"
names(SDWS)[21] <- "Notes"






