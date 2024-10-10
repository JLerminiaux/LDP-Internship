#adding more Baulch data to SDNWA_Wetlanddata

#------------------------------------------------------------------------
#load packages
library(readr) #csv
library(dplyr)
library(assertr) #checks for errors in data sets by restricting searches
library(lubridate) #for dates
library(sp) #for lat/long conversions
library(sf)
#------------------------------------------------------------------------
#load data
SDNWA <- read_csv("clean data/SDNWA_data_clean.csv") #water quality+
Isotopes <- read_csv("clean data/merged_Isotopes_clean.csv") #isotopes
ChlA<- read_csv("clean data/merged_ChlA_clean.csv") #ChlA
WC <- read_csv("clean data/merged_SDWS_clean.csv") #new Baulch water quality

#------------------------------------------------------------------------
#fix variable names
#need to rename: DO (mg/L), TRP, SRP, TDP, TP, NH3, UREA, NO3, TDN, TN, Sulfate, 
#ChlA (ug/L), Alkalinity to match SDNWA


#I personally like the "SRP_mg_L" format better, might be worth switching all of these? Ask Hall lab***
WC <- WC %>%
  rename(pond_id  = Pond) %>%
  rename(sample_date = Date) %>%
  rename('water temp C' = Temp_C) %>%
  rename(Conductivity_uS_cm = SpCond_uS_cm) %>% #the range of this variable seems extreme. 0.001 - 1000s, **ask
  rename(`SRP MDL =.00567` = SRP_mg_L) %>%
  rename(`TDP MDL = .0041` = TDP_mg_L) %>%
  rename(`TP MDL = 0.0041` = TP_mg_L) %>%
  rename(`NH3 MDL = .021` = NH3_mg_L) %>%
  rename(`UREA MDL = .0202` = UREA_mg_L) %>%
  rename(`NO3 MDL = .0073` = NO3_mg_L) %>%
  rename(`TN MDL = .0447` = TN_mg_L) %>%
  rename(`SO4 MDL = 1.015` = Sulfate_mg_L) %>%
  rename(`Alk MDL = 3.16` = Alkalinity_mg_L) %>%
  rename(`TDN MDL = 0.0447`  = ) %>%
  rename(ChlA = Chla_ug_L) %>%
  rename(`DO (mg/L)` = DO_mg_L)

  
Isotopes <- Isotopes  %>%
  rename(pond_id  = Sample_ID) %>%
  rename(sample_date = `Collection Date`) %>%
  select(-Analysis_ID) 

ChlA <- ChlA  %>%
  rename(pond_id  = Location) %>%
  rename(sample_date = Date) %>%
  rename(analyzing_date = RunDate) %>%
  rename('ChlA' = `Chl  A (µg/L)`)

#------------------------------------------------------------------------
#merge

WCIso <- full_join(WC, Isotopes, by = c("pond_id", "sample_date", "Notes"))
WC_Iso_Ch <- full_join(WCIso, ChlA, by = c("pond_id", "sample_date", "ChlA", "Notes"))


#------------------------------------------------------------------------
#save it
write.csv(WC_Iso_Ch, "Baulch_WCdata_clean.csv", row.names = FALSE)

#data is ready to join with other SDNWA, join?


#------------------------------------------------------------------------
#merge to other SDNWA wetland data?
 
