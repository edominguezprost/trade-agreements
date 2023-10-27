# Working with Trade Agreements


# Loading Packages
library(tidyverse)
library(dataverse)
library(dplyr)
library(haven)

# Loading large and original dataset
eia_dataset <- read_dta("eia_14.dta")

# We understand EIA values
unique(eia_dataset$EIA) 

# We will work from year 2000, so we reduce the dataset
df <- eia_dataset %>%
  filter(year>=1999)

# Adding 2018, 2019, 2020, 2021 (using 2017 as framework)
df_2017 <- df %>%
  filter(year==2017)
# 2018
df_2018 <- df_2017 %>%
  mutate(year=2018)
# 2019
df_2019 <- df_2017 %>%
  mutate(year=2019)
# 2020
df_2020 <- df_2017 %>%
  mutate(year=2020)
#2021
df_2021 <- df_2017 %>%
  mutate(year=2021)

# Appending 2018, 2019, 2020 and 2021 the general dataset
df <- bind_rows(df, df_2018, df_2019, df_2020, df_2021)

# Adding Relevant Variables by groups of countries (one considering exporters ISOEX and one considering importers ISOIM)

# EFTA
df <- df %>%
    mutate(efta_x=case_when(ISOEX%in%c("CHE", "ISL", "NOR") ~ 1,
                          TRUE ~ 0),
           efta_m=case_when(ISOIM%in%c("CHE", "ISL", "NOR") ~ 1,
                          TRUE ~ 0))

# European Union (including GBR)
df <- df %>%
  mutate(eu_x=case_when(ISOEX%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "GBR", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER") ~ 1,
                          TRUE ~ 0),
         eu_m=case_when(ISOIM%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "GBR", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER") ~ 1,
                        TRUE ~ 0))

# Replace GBR 2020 (Brexit)
df <- df %>%
  mutate(eu_x=replace(eu_x, year>=2020 & ISOEX=="GBR", 0),
         eu_m=replace(eu_m, year>2020 & ISOIM=="GBR", 0))
      
         
# CPTPP
df <- df %>%
  mutate(cptpp_x=case_when(ISOEX%in%c("AUS", "BRN", "CAN", "CHL", "JPN", "MYS", "MEX", "NZL", "PER", "SGP", "VNM") ~ 1,
                        TRUE ~ 0),
         cptpp_m=case_when(ISOIM%in%c("AUS", "BRN", "CAN", "CHL", "JPN", "MYS", "MEX", "NZL", "PER", "SGP", "VNM") ~ 1,
                        TRUE ~ 0))

# RCEP
df <- df %>%
  mutate(rcep_x=case_when(ISOEX%in%c("AUS", "BRN", "KHM", "CHN", "IDN", "JPN", "KOR", "LAO", "MYS", "MMR", "NZL", "PHL", "SGP", "THA", "VNM") ~ 1,
                           TRUE ~ 0),
         rcep_m=case_when(ISOIM%in%c("AUS", "BRN", "KHM", "CHN", "IDN", "JPN", "KOR", "LAO", "MYS", "MMR", "NZL", "PHL", "SGP", "THA", "VNM") ~ 1,
                           TRUE ~ 0))

# AfCFTA (52 out of 54, missing South Sudan, Sahrawi Arab Democratic Republic)
df <- df %>%
  mutate(afcfta_x=case_when(ISOEX%in%c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD", "COM", "COD", "COG", "DJI", "EGY", "GNQ", "GAB", "GMB", "GHA", "GIN", "GNB", "CIV", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SDN", "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE", "ETH", "SWZ") ~ 1, 
                          TRUE ~ 0),
         afcfta_m=case_when(ISOIM%in%c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD", "COM", "COD", "COG", "DJI", "EGY", "GNQ", "GAB", "GMB", "GHA", "GIN", "GNB", "CIV", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SDN", "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE", "ETH", "SWZ") ~ 1,
                          TRUE ~ 0))


# Western Africa
df <- df %>%
  mutate(wafrica_x=case_when(ISOEX%in%c("BEN","BFA","CPV", "GMB", "GHA", "GIN", "GNB", "CIV", "LBR", "MLI","MRT","NER","NGA","SEN","SLE","TGO") ~ 1,
                            TRUE ~ 0),
         wafrica_m=case_when(ISOIM%in%c("BEN","BFA","CPV", "GMB", "GHA", "GIN", "GNB", "CIV", "LBR", "MLI","MRT","NER","NGA","SEN","SLE","TGO") ~ 1,
                                                        TRUE ~ 0))


# ASEAN
df <- df %>%
  mutate(asean_x=case_when(ISOEX%in%c("BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "VNM") ~ 1,
                          TRUE ~ 0),
         asean_m=case_when(ISOIM%in%c("BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "VNM") ~ 1,
                          TRUE ~ 0))

# MERCOSUR
df <- df %>%
  mutate(mercosur_x=case_when(ISOEX%in%c("ARG", "BRA", "URY", "PRY") ~ 1,
                           TRUE ~ 0),
         mercosur_m=case_when(ISOIM%in%c("ARG", "BRA", "URY", "PRY") ~ 1,
                           TRUE ~ 0))

#GCC
df <- df %>%
  mutate(gcc_x=case_when(ISOEX%in%c("BHR", "KWT", "OMN", "QAT", "SAU", "ARE") ~ 1,
                         TRUE ~ 0),
         gcc_m=case_when(ISOIM%in%c("BHR", "KWT", "OMN", "QAT", "SAU", "ARE") ~ 1,
                         TRUE ~ 0))

###############################################################################
###############################################################################

# Adding new Agreements

# 2017 

# EU and Peru, Colombia and Ecuador (ECU) (accession of Ecuador)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2017 & eu_x==1 & ISOIM=="ECU", 3),
         EIA=replace(EIA, year>=2017 & eu_m==1 & ISOEX=="ECU", 3))

# Peru and Honduras (already included in the dataset)

# Canada Ukraine
df <- df %>%
  mutate(EIA=replace(EIA, year>=2017 & ISOEX=="CAN" & ISOIM=="UKR", 3),
         EIA=replace(EIA, year>=2017 & ISOIM=="CAN" & ISOEX=="UKR", 3))

# EFTA-Georgia (Liechestain not in dataset) check year for Switzerlandqw
df <- df %>%
  mutate(EIA=replace(EIA, year>=2017 & ISOEX=="CHE" & ISOIM=="GEO", 3),
         EIA=replace(EIA, year>=2018 & ISOIM=="CHE" & ISOEX=="GEO", 3),
         EIA=replace(EIA, year>=2017 & ISOEX=="ISL" & ISOIM=="GEO", 3),
         EIA=replace(EIA, year>=2017 & ISOIM=="ISL" & ISOEX=="GEO", 3),
         EIA=replace(EIA, year>=2017 & ISOEX=="NOR" & ISOIM=="GEO", 3),
         EIA=replace(EIA, year>=2017 & ISOIM=="NOR" & ISOEX=="GEO", 3))

# Mercosur - Egypt (signed in 2010 but entered in force in 2017, we keep 2)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2017 & ISOEX=="ARG" & ISOIM=="EGY", 2),
         EIA=replace(EIA, year>=2017 & ISOEX=="BRA" & ISOIM=="EGY", 2),
         EIA=replace(EIA, year>=2017 & ISOEX=="PRY" & ISOIM=="EGY", 2),
         EIA=replace(EIA, year>=2017 & ISOEX=="URY" & ISOIM=="EGY", 2),
         EIA=replace(EIA, year>=2017 & ISOIM=="ARG" & ISOIM=="EGY", 2),
         EIA=replace(EIA, year>=2017 & ISOIM=="BRA" & ISOIM=="EGY", 2),
         EIA=replace(EIA, year>=2017 & ISOIM=="PRY" & ISOEX=="EGY", 2),
         EIA=replace(EIA, year>=2017 & ISOIM=="URY" & ISOEX=="EGY", 2),
         EIA=replace(EIA, year<2017 & ISOEX=="ARG" & ISOIM=="EGY", 0),
         EIA=replace(EIA, year<2017 & ISOEX=="BRA" & ISOIM=="EGY", 0),
         EIA=replace(EIA, year<2017 & ISOEX=="PRY" & ISOIM=="EGY", 0),
         EIA=replace(EIA, year<2017 & ISOEX=="URY" & ISOIM=="EGY", 0),
         EIA=replace(EIA, year<2017 & ISOIM=="ARG" & ISOIM=="EGY", 0),
         EIA=replace(EIA, year<2017 & ISOIM=="BRA" & ISOIM=="EGY", 0),
         EIA=replace(EIA, year<2017 & ISOIM=="PRY" & ISOEX=="EGY", 0),
         EIA=replace(EIA, year<2017 & ISOIM=="URY" & ISOEX=="EGY", 0))


# EU and Canada (already included)

# Turkey (TUR) and Singapore (SGP)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2017 & ISOEX=="TUR" & ISOIM=="SGP", 3),
         EIA=replace(EIA, year>=2017 & ISOIM=="TUR" & ISOEX=="SGP", 3))


# Ecuador (ECU) and El Salvador (SLV) - Enabling Clause (then 2), better access for El Salvador than Ecuador
df <- df %>%
  mutate(EIA=replace(EIA, year>=2017 & ISOEX=="ECU" & ISOIM=="SLV", 2),
         EIA=replace(EIA, year>=2017 & ISOIM=="ECU" & ISOEX=="SLV", 2))

###############################################################################

# 2018

# Hong Kong and Macao - 2018
df <- df %>%
  mutate(EIA=replace(EIA, year>=2018 & ISOEX=="HKG" & ISOIM=="MAC", 3),
         EIA=replace(EIA, year>=2018 & ISOIM=="HKG" & ISOEX=="MAC", 3))

# China CHN and Georgia (GEO)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2018 & ISOEX=="CHN" & ISOIM=="GEO", 3),
         EIA=replace(EIA, year>=2018 & ISOIM=="CHN" & ISOEX=="GEO", 3))

# CPTPP (2019 for Australia, Canada, Japan, Mexico, New Zealand, Singapore; Peru 2021; rest not yet)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & cptpp_x==1 & ISOIM=="AUS", 3),
         EIA=replace(EIA, year>=2019 & cptpp_x==1 & ISOIM=="CAN", 3),
         EIA=replace(EIA, year>=2019 & cptpp_x==1 & ISOIM=="JPN", 3),
         EIA=replace(EIA, year>=2019 & cptpp_x==1 & ISOIM=="MEX", 3),
         EIA=replace(EIA, year>=2019 & cptpp_x==1 & ISOIM=="NZL", 3),
         EIA=replace(EIA, year>=2019 & cptpp_x==1 & ISOIM=="SGP", 3),
         EIA=replace(EIA, year>=2021 & cptpp_x==1 & ISOIM=="PER", 3))


# EFTA and Phillipines (PHL) - Check if 2 or 3
df <- df %>%
  mutate(EIA=replace(EIA, year>=2018 & ISOEX=="CHE" & ISOIM=="PHL", 3),
         EIA=replace(EIA, year>=2018 & ISOIM=="CHE" & ISOEX=="PHL", 3),
         EIA=replace(EIA, year>=2018 & ISOEX=="ISL" & ISOIM=="PHL", 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="ISL" & ISOEX=="PHL", 3),
         EIA=replace(EIA, year>=2018 & ISOEX=="NOR" & ISOIM=="PHL", 3),
         EIA=replace(EIA, year>=2018 & ISOIM=="NOR" & ISOEX=="PHL", 3))

###############################################################################

# 2019

# Samoa and EU (accession of samoa)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & eu_x==1 & ISOIM=="WSM", 3),
         EIA=replace(EIA, year>=2019 & eu_m==1 & ISOEX=="WSM", 3))
         
# EU and Japan
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & eu_x==1 & ISOIM=="JPN", 3),
         EIA=replace(EIA, year>=2019 & eu_m==1 & ISOEX=="JPN", 3))

# EU - Eastern and Southern Africa States - Accession of Comoros (COM)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & eu_x==1 & ISOIM=="COM", 3),
         EIA=replace(EIA, year>=2019 & eu_m==1 & ISOEX=="COM", 3))

# Hong Kong Georgia
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & ISOEX=="HKG" & ISOIM=="GEO", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="HKG" & ISOEX=="GEO", 3))

# ASEAN and Hong Kong (different dates)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & ISOIM=="HKG" & asean_x==1, 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="LAO" & ISOEX=="HKG", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="MMR" & ISOEX=="HKG", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="SGP" & ISOEX=="HKG", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="THA" & ISOEX=="HKG", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="VNM" & ISOEX=="HKG", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="MYS" & ISOEX=="HKG", 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="BRN" & ISOEX=="HKG", 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="PHL" & ISOEX=="HKG", 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="IDN" & ISOEX=="HKG", 3))


# Chile Indonesia
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & ISOEX=="CHL" & ISOIM=="IDN", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="CHL" & ISOEX=="IDN", 3))

# Turkey - Kosovo (not included since Kosovo is not in the dataset)

# Costa Rica; El Salvador; Honduras; Korea, Republic of; Nicaragua; Panama
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & ISOIM=="KOR" & ISOEX=="CRI", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="KOR" & ISOEX=="SLV", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="KOR" & ISOEX=="HND", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="KOR" & ISOEX=="NIC", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="KOR" & ISOEX=="PAN", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="CRI" & ISOEX=="KOR", 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="SLV" & ISOEX=="KOR", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="HND" & ISOEX=="KOR", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="NIC" & ISOEX=="KOR", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="PAN" & ISOEX=="KOR", 3))

# Iran IRN and Eurasian Economic Union (Armenia ARM, Belarus BLR, Kazakhstan KAZ, Kyrgyzstan KGZ, Russia RUS)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & ISOEX=="ARM" & ISOIM=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOEX=="BLR" & ISOIM=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOEX=="KAZ" & ISOIM=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOEX=="KGZ" & ISOIM=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOEX=="RUS" & ISOIM=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="ARM" & ISOEX=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="BLR" & ISOEX=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="KAZ" & ISOEX=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="KGZ" & ISOEX=="IRN", 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="RUS" & ISOEX=="IRN", 3))

# European Union and Singapore
df <- df %>%
  mutate(EIA=replace(EIA, year>=2019 & ISOEX=="SGP" & eu_m==1, 3),
         EIA=replace(EIA, year>=2019 & ISOIM=="SGP" & eu_x==1, 3))


###############################################################################

# 2020

# Australia Hong Kong
df <- df %>%
  mutate(EIA=replace(EIA, year>=2020 & ISOEX=="AUS" & ISOIM=="HKG", 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="AUS" & ISOEX=="HGK", 3))

# Peru Australia
df <- df %>%
  mutate(EIA=replace(EIA, year>=2020 & ISOEX=="AUS" & ISOIM=="PER", 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="AUS" & ISOEX=="PER", 3))

# EU - Pacific States - Accession of Solomon Islands (SLB)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2020 & ISOEX=="SLB" & eu_m==1, 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="SLB" & eu_x==1, 3))

# United States-Mexico-Canada Agreement (USMCA/CUSMA/T-MEC) ALREADY INCLUDED BECAUSE OF NAFTA

# Indonesia Australia 
df <- df %>%
  mutate(EIA=replace(EIA, year>=2020 & ISOEX=="AUS" & ISOIM=="IDN", 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="AUS" & ISOEX=="IDN", 3))

# Vietnam and EU
df <- df %>%
  mutate(EIA=replace(EIA, year>=2020 & ISOEX=="VNM" & eu_m==1, 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="VNM" & eu_x==1, 3))

# EFTA ECUADOR

df <- df %>%
  mutate(EIA=replace(EIA, year>=2020 & ISOEX=="ECU" & efta_m==1, 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="ECU" & efta_x==1, 3))

###############################################################################

# 2021 (to be completed with UK new agreements - does not add additionality)

# Brexit (UK and EU is 3 now)
df <- df %>%
  mutate(EIA=replace(EIA, year>=2020 & ISOEX=="GBR" & eu_m==1, 3),
         EIA=replace(EIA, year>=2020 & ISOIM=="GBR" & eu_x==1, 3))

# UK Turkey
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="TUR", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="TUR", 3))

# UK VIETMAN
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="VNM", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="VNM", 3))

# UK CAMEROON
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="CMR", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="CMR", 3))

# UK MOLDOVA
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="MDA", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="MDA", 3))

# SAMOA
df <- df %>%
mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="WSM", 3),
       EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="WSM", 3))

# SOLOMON
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="SLB", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="SLB", 3))

# UK CANADA
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="CAN", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="CAN", 3))

# SINGAPORE
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="SGP", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="SGP", 3))

# NORWAY
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="NOR", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="NOR", 3))

# ICELAND
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="ISL", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="ISL", 3))

# KENYA
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="KEN", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="KEN", 3))

# UK EGYPT 
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="GBR" & ISOIM=="EGY", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="GBR" & ISOEX=="EGY", 3))

# CHINA MAURITIUS
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="CHN" & ISOIM=="MUS", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="CHN" & ISOEX=="MUS", 3))

# UKRAINE ISRALE

df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & ISOEX=="UKR" & ISOIM=="ISR", 3),
         EIA=replace(EIA, year>=2021 & ISOIM=="UKR" & ISOEX=="ISR", 3))

# EFTA INDONESIA
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & efta_x==1 & ISOIM=="IDN", 3),
         EIA=replace(EIA, year>=2021 & efta_m==1 & ISOEX=="IDN", 3))

# EFTA Turkey
df <- df %>%
  mutate(EIA=replace(EIA, year>=2021 & efta_x==1 & ISOIM=="TUR", 3),
         EIA=replace(EIA, year>=2021 & efta_m==1 & ISOEX=="TUR", 3))

##################################################################################################################################
##################################################################################################################################

# Adding potential PTAs

# We add a new column for potential Agreements (high probability)
df <- df %>%
  mutate(Potential_PTA_high=EIA)

# RCEP
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & rcep_x==1 & rcep_m==1, 3))

# AfCFTA
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & afcfta_x==1 & afcfta_m==1, 3))

#CPTPP
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & cptpp_x==1 & cptpp_m==1, 3))


# Canada - MERCOSUR
df <- df %>%
mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="CAN" & ISOEX=="ARG", 3),
       Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="CAN" & ISOEX=="BRA", 3),
       Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="CAN" & ISOEX=="URY", 3),
       Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="CAN" & ISOEX=="PRY", 3),
       Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="CAN" & ISOIM=="ARG", 3),
       Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="CAN" & ISOIM=="BRA", 3),
       Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="CAN" & ISOIM=="URY", 3),
       Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="CAN" & ISOIM=="PRY", 3))


# China Norway
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="CHN" & ISOIM=="NOR", 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="CHN" & ISOEX=="NOR", 3))

# EFTA MERCOSUR
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="ARG" & efta_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="ARG" & efta_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="BRA" & efta_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="BRA" & efta_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="PRY" & efta_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="PRY" & efta_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="URY" & efta_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="URY" & efta_x==1, 3))

# EFTA - Vietnam
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="VNM" & efta_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="VNM" & efta_x==1, 3))

# Eastern African Community (Burundi, Kenya, Rwanda, Tanzania, and Uganda) AND EEU
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="BDI" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="BDI" & eu_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="KEN" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="KEN" & eu_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="RWA" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="RWA" & eu_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="TZA" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="TZA" & eu_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="UGA" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="UGA" & eu_x==1, 3))


# EU - Indonesia
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="IDN" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="IDN" & eu_x==1, 3))


# EU - Phillipines
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="PHL" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="PHL" & eu_x==1, 3))

# EU - Tunisia
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="TUN" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="TUN" & eu_x==1, 3))

# EU - West Africa EPA 
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & wafrica_m==1 & eu_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & wafrica_x==1 & eu_m==1, 3))


# KOREA MEXICO
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="MEX" & ISOIM=="KOR", 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="MEX" & ISOEX=="KOR", 3))

# moldova china
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="CHN" & ISOIM=="MDA", 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="CHN" & ISOEX=="MDA", 3))


# EFTA - Central America - Accession of Guatemala
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="GTM" & efta_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="GTM" & efta_x==1, 3))


# Future Agreements that were not reported to WTO (Own Source)

# United Arab Emirates India
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="ARE" & ISOIM=="IND", 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="ARE" & ISOEX=="IND", 3))

# European Union Mercosur
df <- df %>%
  mutate(Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="ARG" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="ARG" & eu_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="BRA" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="BRA" & eu_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="PRY" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="PRY" & eu_x==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOEX=="URY" & eu_m==1, 3),
         Potential_PTA_high=replace(Potential_PTA_high, year>=2019 & ISOIM=="URY" & eu_x==1, 3))



# We add a new column for potential Agreements (all of them)
df <- df %>%
  mutate(Potential_PTA=EIA)

# RCEP
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & rcep_x==1 & rcep_m==1, 3))

# AfCFTA
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & afcfta_x==1 & afcfta_m==1, 3))

#CPTPP
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & cptpp_x==1 & cptpp_m==1, 3))

# Australia GCC
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="AUS" & ISOIM=="BHR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="AUS" & ISOIM=="KWT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="AUS" & ISOIM=="OMN", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="AUS" & ISOIM=="QAT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="AUS" & ISOIM=="SAU", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="AUS" & ISOIM=="ARE", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="AUS" & ISOEX=="BHR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="AUS" & ISOEX=="KWT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="AUS" & ISOEX=="OMN", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="AUS" & ISOEX=="QAT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="AUS" & ISOEX=="SAU", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="AUS" & ISOEX=="ARE", 3))

# Canada and CARICOM (Antigua & Barbuda, the Bahamas, Barbados, Belize, the Commonwealth of Dominica, Grenada, Guyana, Jamaica, Haiti, Montserrat, Saint Kitts & Nevis, Saint Lucia, Saint Vincent & the Grenadines, Suriname, and Trinidad & Tobago)
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="ATG", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="BHS", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="BRB", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="BLZ", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="DMA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="GRD", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="GUY", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="JAM", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="HTI", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="KNA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="LCA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="VCT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="SUR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="TTO", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="ATG", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="BHS", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="BRB", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="BLZ", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="DMA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="GRD", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="GUY", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="JAM", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="HTI", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="KNA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="LCA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="VCT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="SUR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="TTO", 3))

# Canada Dominican Republic
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="DOM", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="DOM", 3))

#  Canada - El Salvador - Guatemala - Honduras - Nicaragua
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="HND", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="SLV", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="NIC", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="GTM", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="HND", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="SLV", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="NIC", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="GTM", 3))

# Canada - MERCOSUR
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="ARG", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="BRA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="URY", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="PRY", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="ARG", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="BRA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="URY", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="PRY", 3))

# Canada - Singapore
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CAN" & ISOIM=="SGP", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CAN" & ISOEX=="SGP", 3))

# China Norway
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CHN" & ISOIM=="NOR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CHN" & ISOEX=="NOR", 3))

# EFTA INDIA
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="IND" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="IND" & efta_x==1, 3))

# EFTA MERCOSUR
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="ARG" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="ARG" & efta_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="BRA" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="BRA" & efta_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="PRY" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="PRY" & efta_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="URY" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="URY" & efta_x==1, 3))

# EFTA - Russian Federation / Belarus / Kazakhstan
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="BLR" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="BLR" & efta_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="KAZ" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="KAZ" & efta_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="RUS" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="RUS" & efta_x==1, 3))

# EFTA - Vietnam
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="VNM" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="VNM" & efta_x==1, 3))

# Eastern African Community (Burundi, Kenya, Rwanda, Tanzania, and Uganda) AND EEU
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="BDI" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="BDI" & eu_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="KEN" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="KEN" & eu_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="RWA" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="RWA" & eu_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="TZA" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="TZA" & eu_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="UGA" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="UGA" & eu_x==1, 3))

# EU and India
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="IND" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="IND" & eu_x==1, 3))

# EU - Indonesia
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="IDN" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="IDN" & eu_x==1, 3))

# EU - Malaysia
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="MYS" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="MYS" & eu_x==1, 3))

# EU - Morocco
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="MAR" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="MAR" & eu_x==1, 3))

# EU - Phillipines
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="PHL" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="PHL" & eu_x==1, 3))

# EU - Thailand
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="THA" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="THA" & eu_x==1, 3))

# EU - Tunisia
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="TUN" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="TUN" & eu_x==1, 3))

# EU - West Africa EPA 
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & wafrica_m==1 & eu_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & wafrica_x==1 & eu_m==1, 3))

# India - SACU (Botswana, Lesotho, Namibia, South Africa, and Swaziland)
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="IND" & ISOIM=="BWA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="IND" & ISOEX=="BWA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="IND" & ISOIM=="LSO", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="IND" & ISOEX=="LSO", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="IND" & ISOIM=="NAM", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="IND" & ISOEX=="NAM", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="IND" & ISOIM=="ZAF", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="IND" & ISOEX=="ZAF", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="IND" & ISOIM=="SWZ", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="IND" & ISOEX=="SWZ", 3))

# Japan - Gulf Cooperation Council (GCC)

df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="JPN" & ISOIM=="BHR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="JPN" & ISOIM=="KWT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="JPN" & ISOIM=="OMN", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="JPN" & ISOIM=="QAT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="JPN" & ISOIM=="SAU", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="JPN" & ISOIM=="ARE", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="JPN" & ISOEX=="BHR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="JPN" & ISOEX=="KWT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="JPN" & ISOEX=="OMN", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="JPN" & ISOEX=="QAT", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="JPN" & ISOEX=="SAU", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="JPN" & ISOEX=="ARE", 3))


# Japan Korea
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="JPN" & ISOIM=="KOR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="JPN" & ISOEX=="KOR", 3))

# KOREA MEXICO
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="MEX" & ISOIM=="KOR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="MEX" & ISOEX=="KOR", 3))

# moldova china
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="CHN" & ISOIM=="MDA", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="CHN" & ISOEX=="MDA", 3))

# Ukraine Singapore
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="UKR" & ISOIM=="SGP", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="UKR" & ISOEX=="SGP", 3))

# Ukraine Turkey
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="UKR" & ISOIM=="TUR", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="UKR" & ISOEX=="TUR", 3))

# EFTA - Central America - Accession of Guatemala
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="GTM" & efta_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="GTM" & efta_x==1, 3))

# United Arab Emirates India
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="ARE" & ISOIM=="IND", 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="ARE" & ISOEX=="IND", 3))

# European Union Mercosur
df <- df %>%
  mutate(Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="ARG" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="ARG" & eu_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="BRA" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="BRA" & eu_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="PRY" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="PRY" & eu_x==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOEX=="URY" & eu_m==1, 3),
         Potential_PTA=replace(Potential_PTA, year>=2019 & ISOIM=="URY" & eu_x==1, 3))

# We save results
write.csv(df, "df_agreements.csv")















































  
