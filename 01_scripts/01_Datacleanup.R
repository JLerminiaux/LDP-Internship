#data clean up for SDNWA projects

#------------------------------------------------------------------------
#load packages
library(readr) #csv
library(dplyr)
library(assertr) #checks for errors in data sets by restricting searches
library(lubridate) #for dates
library(sp) #for lat/long conversions
library(sf)
library(readxl) #excel - I converted all files except Data_2006 to csv for this, last minute addition
#------------------------------------------------------------------------

#load data
#for all data, manually added a column, "Data Source" for each dataset - university source
#PPR
Data_PPR <- read_csv("data/Data_PPR_2006_to_Present for LDP.csv") %>%
  select(-'...29', -'Sample ID', -'dissolved MeHg (ng/L)', -`Particulate MeHg (ng/L)`,
         -`Particulate THg (ng/L)`, -'Dissolved THg (ng/L)', -'Dissolved MeHg (ng/L)') #empty columns
Field_Stats_PPR <- read_csv("data/Field_Stats_PPR_2006_to_Present.csv")
Absorbence_PPR <- read_csv("data/Absorbence_PPR_2006_to_Present.csv")
Data_2006 <- read_excel("data/Data PPR 2006 COMPLETE April 2009.xls", sheet = "data")

#to connect to PPR
THG_data <- read_csv("data/DMA-80 THG Data_Cleandata.csv")
Baulch <- read_csv("data/SDWS 2015-2022 Baulch.csv")
LesserWetlands <- read_csv("data/LesserWetlands.csv") #UofR ponds
MajorWetlands <- read_csv("data/MajorWetlands.csv") #UofR ponds
SPonds <- read_csv("data/SDNWA_Pond locations - Lat and Long.csv") #UofS ponds (Baulch)

#------------------------------------------------------------------------
#Absorbence

Absorbence_PPR <- Absorbence_PPR %>%
  rename(pond_id  = Pond) %>%
  rename(sample_date = `Collection Date`) %>%
  rename(analyzing_date = 'Analysing date') 

#pond_id 97 put together with 97/98
Absorbence_PPR$pond_id[Absorbence_PPR$pond_id == "97"] <- "97/98"


Absorbence_PPR$pond_id <- as.character(Absorbence_PPR$pond_id)


#for Abs nm data, formatted it manually to be like Data_PPR data (filter included in columns)

#------------------------------------------------------------------------
#Data_PPR

Data_PPR <- Data_PPR %>%
  rename(pond_id = Site) %>%
  rename(sample_date = `Date sampled`) %>%
  rename(`Abs254nm (1.2 filter)` = Abs254nm) %>%
  rename(`ChlA` = `Chl a`) %>% 
  rename('Fe (mg/L)'  = `CHECK UNITS Fe (mg/L)`)

#pond_id 125N has two formats
#pond_id 97 put together with 97/98
Data_PPR$pond_id[Data_PPR$pond_id == "125 N"] <- "125N"
Data_PPR$pond_id[Data_PPR$pond_id == "97"] <- "97/98"

#manually created new column "Other Observation" where "Dry" condition from ph is now (to match Field_Stats)
Data_PPR$pH <- as.numeric(Data_PPR$pH)
#YSI/Dry = NA for ph
#several numeric variables need to be formatted from characters
Data_PPR$pH <- as.numeric(Data_PPR$pH)
Data_PPR$`SO4 (mg/L)` <- as.numeric(gsub(",", "", Data_PPR$`SO4 (mg/L)`))
Data_PPR$`%MeHg` <- as.numeric(Data_PPR$`%MeHg`)
Data_PPR$`CO2 (umol/L)` <- as.numeric(Data_PPR$`CO2 (umol/L)`)
Data_PPR$`CH4 (umol/L)`<- as.numeric(Data_PPR$`CH4 (umol/L)`)
#NAs introduced for character observations such as "No Sample", that's okay

#------------------------------------------------------------------------
#Field PPR

Field_Stats_PPR <- Field_Stats_PPR %>%
  rename(pond_id  = Site) %>%
  rename(sample_date = `Date Sampled`)

#pond_id 97 put together with 97/98
Field_Stats_PPR$pond_id[Field_Stats_PPR$pond_id == "97"] <- "97/98"

#had trouble changing the names for SO4_data$`Conductivity (mS/cm)` and Field_Stats_PPR$`Conductivity (uS/cm)`,
##did manually instead to SO4_data$Conductivity_mS_cm and Field_Stats_PPR$Conductivity_uS_cm

#convert characters to numbers
Field_Stats_PPR$Conductivity_uS_cm <- as.numeric(Field_Stats_PPR$Conductivity_uS_cm)
Field_Stats_PPR$`water temp C` <- as.numeric(Field_Stats_PPR$`water temp C`)
Field_Stats_PPR$pH <- as.numeric(Field_Stats_PPR$pH)
Field_Stats_PPR$`Secchi (cm)` <- as.numeric(Field_Stats_PPR$`Secchi (cm)`)
Field_Stats_PPR$`Air temp C` <- as.numeric(Field_Stats_PPR$`Air temp C`)
Field_Stats_PPR$`Cloud %` <- as.numeric(Field_Stats_PPR$`Cloud %`)
#leaving ORP, and both DOs as character since some values are ranges

#------------------------------------------------------------------------
#Data_2006; extra PPR data from 2006-2009

#filter relevant data
Data_2006 <- Data_2006 %>%
  filter(Region == "SDNWA") %>%
  select(c(1:26), -c(17:22), -c(14:15)) %>%
  select(-'Date stamp', -'%dissolved THg', -'TDC') %>%
  rename(SUVA = "...16")


#clean up names to match other PPR data

Data_2006 <- Data_2006  %>%
  rename(pond_id = Site) %>%
  rename(sample_date = `Date`) %>%
  rename('water temp C' = `Temp. (°C)`) %>%
  rename('Whole Water THg (ng/L)'  = `whole water THg (ng/L)`) %>%
  rename('Whole Water MeHg (ng/L)' = `whole water MeHg (ng/L)`) %>%
  rename('MeHg Std Err' = `MeHg std err`) %>%
  rename('%MeHg' = `%MeHg...11`) %>%
  rename('DOC (mg/L) (0.45 Filter)' = 'DOC (mg/L)')


#format variables same
Data_2006$sample_date <- as.character(Data_2006$sample_date)
Data_2006$`DO (mg/L)` <- as.character(Data_2006$`DO (mg/L)`)
Data_2006$SUVA <- as.double(Data_2006$SUVA)


#clean up pond names
Data_2006$pond_id <- sub("Pond ", "", Data_2006$pond_id)
Data_2006$pond_id [] <- lapply(Data_2006$pond_id , function(x) gsub(" ", "", x))
Data_2006$pond_id  <- as.character(Data_2006$pond_id )

#update names and units for Conductivity (mS/cm) and DIC (mg/L) 

Data_2006$`Conductivity (mS/cm)` <-  Data_2006$`Conductivity (mS/cm)` * 1000
# Convert μmol/L to mg/L
Data_2006$`DIC (umol/L) (from pCO2)` <- Data_2006$`DIC (umol/L) (from pCO2)` * 0.01201

#rename to match
Data_2006 <- Data_2006  %>%
  rename(`Conductivity_uS_cm` = `Conductivity (mS/cm)`) %>%
  rename(`DIC (mg/L)` = `DIC (umol/L) (from pCO2)`)

Data_2006$pond_id[Data_2006$pond_id == "97"] <- "97/98" 

#remove duplicates found in Data PPR
#1, 1, 109, 118, 125N* (pH = 8.49), 2, 2, 97*-97/98 (pH = 8.23)

indices_to_remove <- c(1, 2, 5, 8, 10, 14, 15, 24)

# Remove observations at these indices
Data_2006 <- Data_2006[-indices_to_remove, ]

#------------------------------------------------------------------------
#Baulch

Baulch <- Baulch %>%
  rename(pond_id  = Pond) %>%
  rename(sample_date = Date) %>%
  rename(Conductivity_uS_cm = `Cond (uS/cm)`) %>%
  rename('water temp C' = Temp) %>%
  rename(ChlA = `Chl A`) #to match field data

Baulch <- Baulch[Baulch$pond_id != "Pond", ]
Baulch<- Baulch[!is.na(Baulch$pond_id), ]
Baulch$pond_id[Baulch$pond_id == "125s"] <- "125S"
Baulch$pond_id[Baulch$pond_id == "139a"] <- "139A"
Baulch$pond_id[Baulch$pond_id == "27a"] <- "27A" 


#convert characters to numbers

Baulch$`water temp C` <- as.numeric(Baulch$`water temp C`) 
Baulch$pH <- as.numeric(Baulch$pH)
Baulch$Conductivity_uS_cm <- as.numeric(Baulch$Conductivity_uS_cm)
Baulch$`TRP MDL = .00567` <- as.numeric(Baulch$`TRP MDL = .00567`)
Baulch$`SRP MDL =.00567` <- as.numeric(Baulch$`SRP MDL =.00567`)
Baulch$`TDP MDL = .0041` <- as.numeric(Baulch$`TDP MDL = .0041`)
Baulch$`TP MDL = 0.0041` <- as.numeric(Baulch$`TP MDL = 0.0041`)
Baulch$`NH3 MDL = .021` <- as.numeric(Baulch$`NH3 MDL = .021`)
Baulch$`UREA MDL = .0202` <- as.numeric(Baulch$`UREA MDL = .0202`)
Baulch$`NO3 MDL = .0073` <- as.numeric(Baulch$`NO3 MDL = .0073`)
Baulch$`TN MDL = .0447` <- as.numeric(Baulch$`TN MDL = .0447`)
Baulch$`SO4 MDL = 1.015` <- as.numeric(Baulch$`SO4 MDL = 1.015`)
Baulch$ChlA <- as.numeric(Baulch$ChlA)
Baulch$`Alk MDL = 3.16` <- as.numeric(Baulch$`Alk MDL = 3.16`)
Baulch$`TDN MDL = 0.0447` <- as.numeric(Baulch$`TDN MDL = 0.0447`)
#leave DO as characters to match Field Data
#***DO should not have ranges!


#------------------------------------------------------------------------
#THG

THG_data <- THG_data %>%
  rename(pond_id  = 'Pond #') %>%
  rename(sample_date = `Sample Date`) %>%
  rename(analyzing_date = 'Analyzing Date') 

THG_data$pond_id <- as.character(THG_data$pond_id)
THG_data$pond_id[THG_data$pond_id == "97"] <- "97/98"

#------------------------------------------------------------------------
#SPonds - update columns to match others

SPonds <- SPonds %>%
  rename(pond_id = WETLAND_NO) %>%
  rename(Latitude = x) %>% 
  rename(Longitude = y) 

#------------------------------------------------------------------------
#Wetlands in NAD 83 UTM Zone 13N
#include DataSource for these?
#need to deal with 97/98 - they are together in all data sets except this
#Y and X are not Latitude and Longitude

LesserWetlands <- LesserWetlands %>%
  rename(pond_id = POND_NUMBE) %>%
  rename(Latitude = Y) %>% #northing
  rename(Longitude = X) #easting

LesserWetlands$pond_id <- toupper(LesserWetlands$pond_id) #capitalize all pond_id letters


MajorWetlands <- MajorWetlands %>%
  rename(pond_id = WETLAND_NO) %>%
  rename(Latitude = POINT_Y) %>% #northing
  rename(Longitude = POINT_X) #easting

MajorWetlands$pond_id <- toupper(MajorWetlands$pond_id) #capitalize all pond_id letters


#Major
maj_df <- data.frame(Longitude=c(MajorWetlands$Longitude),
                 Latitude=c(MajorWetlands$Latitude))

#Convert the data frame to SpatialPointsDataFrame
coordinates (maj_df) = ~Longitude + Latitude

#Assign a projection to it
proj4string(maj_df) <- CRS("+proj=utm +zone=13")

#Projection transformation
maj_longlats <- spTransform(maj_df, CRS("+proj=longlat")) 

#Convert it to data frame
maj_longlats.df <- as.data.frame(maj_longlats)

maj_longlats.df$ID <- seq(1, 117)

Major <- full_join(maj_longlats.df, MajorWetlands, by = c(
  "ID"))  %>%
  select(coords.x1, coords.x2, pond_id, CLASS, ID, OID_) %>%
  rename(Latitude = coords.x1) %>%
  rename(Longitude = coords.x2)


#Lesser
less_df <- data.frame(Longitude=c(LesserWetlands$Longitude),
                 Latitude=c(LesserWetlands$Latitude))
                 
#Convert the data frame to SpatialPointsDataFrame
coordinates(less_df) = ~Longitude + Latitude
                 
#Assign a projection to it
proj4string(less_df) <- CRS("+proj=utm +zone=13")
                 
#Projection transformation
less_longlats <- spTransform(less_df, CRS("+proj=longlat")) 
                 
#Convert it to data frame
less_longlats.df <- as.data.frame(less_longlats)
                 
less_longlats.df$ID <- seq(1, 99)
                 
Lesser <- full_join(less_longlats.df, LesserWetlands, by = c(
                   "ID")) %>%
  select(coords.x1, coords.x2, pond_id, ID, OID_) %>%
  rename(Latitude = coords.x1) %>%
  rename(Longitude = coords.x2) 
          
#------------------------------------------------------------------------
#Convert all dates to ISO standards, date-time format
#Baulch and THG in correct format already

#Sampling data
Data_PPR$sample_date <- format(dmy(Data_PPR$sample_date), "%Y-%m-%d")
Field_Stats_PPR$sample_date <- format(dmy(Field_Stats_PPR$sample_date), "%Y-%m-%d")
Absorbence_PPR$sample_date <- format(dmy(Absorbence_PPR$sample_date), "%Y-%m-%d")
THG_data$sample_date <- as.character(THG_data$sample_date)


#Analyzing data
Absorbence_PPR$analyzing_date <- format(dmy(Absorbence_PPR$analyzing_date), "%Y-%m-%d")
THG_data$analyzing_date <- as.character(THG_data$analyzing_date)


#------------------------------------------------------------------------
#first join PPR tables: field, data, absorbence
#add column for pond aliases and make sure region/site variable is included

merged_Field_Data <- left_join(Data_PPR, Field_Stats_PPR, by = c(
  "pond_id", "sample_date", "pH", "Region", "Other Observation", "DataSource")) 

merged_FDA <- left_join(merged_Field_Data, Absorbence_PPR, by = c(
  "pond_id", "sample_date", "Region", "Abs254nm (0.45 filter)", "Abs254nm (1.2 filter)", "DataSource"))

#add 2006-2009 data 
#merge with rest of PPR Data

merged_FDA_total <- full_join(merged_FDA, Data_2006, by = c(
  "pond_id", "sample_date", "Region", "water temp C", "pH", "DO (mg/L)", "SUVA",
  "Whole Water THg (ng/L)", "Whole Water MeHg (ng/L)", "MeHg Std Err", "%MeHg", 
  "SO4 (mg/L)", "DOC (mg/L) (0.45 Filter)",  "Conductivity_uS_cm", "DIC (mg/L)"
)) 
#conductivity and DIC should be able to be apart of this as well depending on units confirmed

merged_FDA_total$DataSource[is.na(merged_FDA_total$DataSource)] <- "U of R"

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#join PPR to Baulch 
merged_FDA_Baulch <- full_join(merged_FDA_total, Baulch, by = c(
  "pond_id", "sample_date", "pH", "Conductivity_uS_cm", "DO (mg/L)", "ChlA", "water temp C", "DataSource"))

#------------------------------------------------------------------------
#join PPR/Baulch to THG

SDNWA_data <- full_join(merged_FDA_Baulch, THG_data, by = c(
  "pond_id", "sample_date", "analyzing_date", "DataSource"))

#------------------------------------------------------------------------
#add class and lat/long (join Major/Lesser Wetlands to each other, then SPonds, then SDNWA)

Wetland_data <- full_join(Major, Lesser, by = c(
  "pond_id", "Longitude", "Latitude", "OID_", "ID"))

Wetlandpond_data <- full_join(Wetland_data, SPonds, by = c(
  "pond_id", "Longitude", "Latitude", "CLASS"))


SDNWA_Wetlanddata <- full_join(SDNWA_data, Wetland_data, by = c(
  "pond_id"))

#------------------------------------------------------------------------
#clean up

SDNWA_Wetlanddata$Region[is.na(SDNWA_Wetlanddata$Region)] <- "SDNWA"

#remove observations without data sources

SDNWA_Wetlanddata <- SDNWA_Wetlanddata %>%
  filter(DataSource != "NA")

#move observations and comments to end, remove unnecessary/redundant variables
SDNWA_Wetlanddata <- SDNWA_Wetlanddata %>%
  select(-Comments, -'Other Observation', everything(), 'Other Observation',  Comments) %>%
  select(-ID, -OID_)

#update lat/longs that were not included in data files:
#97/98: using 97 lat/long -> -106.078079	52.209151 

SDNWA_Wetlanddata$Longitude[SDNWA_Wetlanddata$pond_id == "97/98"] <-52.209151  
SDNWA_Wetlanddata$Latitude[SDNWA_Wetlanddata$pond_id == "97/98"] <- -106.078079

#125 -> -106.080895	52.212426

SDNWA_Wetlanddata$Longitude[SDNWA_Wetlanddata$pond_id == "125"] <- 52.212426
SDNWA_Wetlanddata$Latitude[SDNWA_Wetlanddata$pond_id == "125"] <- -106.080895


#W140 = 140 -> -106.0851225	52.21303487

SDNWA_Wetlanddata$Longitude[SDNWA_Wetlanddata$pond_id == "125"] <- 52.21303487
SDNWA_Wetlanddata$Latitude[SDNWA_Wetlanddata$pond_id == "125"] <- -106.0851225

#still don't have data for 5340  lat/long

SDNWA_Wetlanddata[is.na(SDNWA_Wetlanddata)] <- NA

#save it
write.csv(SDNWA_Wetlanddata, "SDNWA_data_clean.csv", row.names = FALSE)

#Region data is missing for some ponds/observations, manually made them all SDNWA, 
#just odd that in some data files region is listed as "Pond"
