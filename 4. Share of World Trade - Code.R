# New Agreements: share of world trade #


# Instructions: for each agreement we check exports and imports.
# To see trade for different years, just change the year in "filter". 

library(tidyverse)
library(dataverse)
library(dplyr)
library(haven)
library(ggplot2)
library(reshape2)
library(ggrepel)

# Loading large and original dataset
df_final <- read_csv("df_final.csv")



# Understanding trade covered by new agreements
# Adding new Agreements

# 2011

# India Japan
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="JPN" & ISOIM=="IND") | (ISOIM=="JPN" & ISOEX=="IND"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Peru Korea
df_final %>%
  filter(year==2020 & ((ISOEX=="PER" & ISOIM=="KOR") | (ISOIM=="PER" & ISOEX=="KOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

df_final %>%
  group_by(PTA) %>%
  filter(year==2012 & ((ISOEX=="PER" & ISOIM=="KOR") | (ISOIM=="PER" & ISOEX=="KOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Peru Mexico
df_final %>%
  filter(year==2020 & ((ISOEX=="PER" & ISOIM=="MEX") | (ISOIM=="PER" & ISOEX=="MEX"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

df_final %>%
  group_by(PTA) %>%
  filter(year==2014 & ((ISOEX=="PER" & ISOIM=="MEX") | (ISOIM=="PER" & ISOEX=="MEX"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Peru Panama
df_final %>%
  filter(year==2020 & ((ISOEX=="PER" & ISOIM=="PAN") | (ISOIM=="PER" & ISOEX=="PAN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

df_final %>%
  group_by(PTA) %>%
  filter(year==2012 & ((ISOEX=="PER" & ISOIM=="PAN") | (ISOIM=="PER" & ISOEX=="PAN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Peru Costa Rica
df_final %>%
  group_by(PTA) %>%
  filter(year==2011 & ((ISOEX=="PER" & ISOIM=="CRI") | (ISOIM=="PER" & ISOEX=="CRI"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA Hong Kong
df_final %>%
  group_by(PTA) %>%
  filter(year==2011 & ((efta_x==1 & ISOIM=="HKG") | (efta_m==1 & ISOEX=="HKG"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Turkey Mauritus
df_final %>%
  filter(year==2020 & ((ISOEX=="MUS" & ISOIM=="TUR") | (ISOIM=="MUS" & ISOEX=="TUR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# El Salvador Cuba
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="SLV" & ISOIM=="CUB") | (ISOIM=="SLV" & ISOEX=="CUB"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# CISFTA

df_final <- df_final %>%
  mutate(cisfta_x=case_when(ISOEX%in%c("RUS", "UKR", "BLR", "UZB", "MDA", "ARM", "KGZ", "KAZ", "TJK") ~ 1,
                           TRUE ~ 0),
         cisfta_m=case_when(ISOIM%in%c("RUS", "UKR", "BLR", "UZB", "MDA", "ARM", "KGZ", "KAZ", "TJK") ~ 1,
                           TRUE ~ 0))

df_final %>%
  group_by(PTA) %>%
  filter(year==2011 & cisfta_x==1 & cisfta_m==1) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Chile Vietnam (not in dataset)
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="CHL" & ISOIM=="VNM") | (ISOIM=="CHL" & ISOEX=="VNM"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Mexico and Central America
df_final <- df_final %>%
  mutate(centralamerica_x=case_when(ISOEX%in%c("CRI", "SLV", "HND", "NIC", "GTM") ~ 1,
                            TRUE ~ 0),
         centralamerica_m=case_when(ISOIM%in%c("CRI", "SLV", "HND", "NIC", "GTM") ~ 1,
                            TRUE ~ 0))

df_final %>%
  group_by(PTA) %>%
  filter(year==2019 & ((centralamerica_x==1 & ISOIM=="MEX") | (centralamerica_m==1 & ISOEX=="MEX"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Croatia EU

df_final %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="HRV") | (eu_m==1 & ISOEX=="HRV"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# 2012

# Malaysia Australia
df_final %>%
  group_by(PTA) %>%
  filter(year==2011 & ((ISOEX=="AUS" & ISOIM=="MYS") | (ISOEX=="AUS" & ISOIM=="MYS"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU Colombia Peru

df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((eu_x==1 & (ISOIM=="COL" | ISOIM=="PER")) | (eu_m==1 & (ISOEX=="COL"| ISOIM=="PER")))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Korea - Turkey
df_final %>%
  group_by(PTA) %>%
  filter(year==2012 & ((ISOEX=="KOR" & ISOIM=="TUR") | (ISOIM=="KOR" & ISOEX=="TUR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Hong Kong - Chile
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="HKG" & ISOIM=="CHL") | (ISOIM=="HKG" & ISOEX=="CHL"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

#KOREA COLOMBIA
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="KOR" & ISOIM=="COL") | (ISOIM=="KOR" & ISOEX=="COL"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# ICELAND CHINA
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="ISL" & ISOIM=="CHN") | (ISOIM=="ISL" & ISOEX=="CHN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# EFTA BOSNIA
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((efta_x==1 & ISOIM=="BIH") | (efta_m==1 & ISOEX=="BIH"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Switzerland China
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="CHE" & ISOIM=="CHN") | (ISOIM=="CHE" & ISOEX=="CHN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# New Zealand Taiwan 
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="TWN" & ISOIM=="NZL") | (ISOIM=="TWN" & ISOEX=="NZL"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Chile Thailand
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="CHL" & ISOIM=="THA") | (ISOIM=="CHL" & ISOEX=="THA"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Singapore Taiwan 
df_final %>%
  group_by(PTA) %>%
  filter(year==2019 & ((ISOEX=="TWN" & ISOIM=="SGP") | (ISOIM=="TWN" & ISOEX=="SGP"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU Central America

df_final <- df_final %>%
  mutate(centralamerica_6_x=case_when(ISOEX%in%c("CRI", "SLV", "HND", "NIC", "GTM", "PAN") ~ 1,
                                    TRUE ~ 0),
         centralamerica_6_m=case_when(ISOIM%in%c("CRI", "SLV", "HND", "NIC", "GTM", "PAN") ~ 1,
                                    TRUE ~ 0))

df_final %>%
  filter(year==2020 & ((eu_x==1 & centralamerica_6_m==1) | (eu_m==1 & centralamerica_6_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Canada Honduras
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="CAN" & ISOIM=="HND") | (ISOIM=="CAN" & ISOEX=="HND"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

############################################################

# 2014

# Turkey Malaysia
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="TUR" & ISOIM=="MYS") | (ISOIM=="TUR" & ISOEX=="MYS"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# korea Australia
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOIM=="KOR" & ISOEX=="AUS"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# JAPAN australia
df_final %>% 
  group_by(PTA) %>%
  filter(year==2014 & ((ISOEX=="AUS" & ISOIM=="JPN") | (ISOIM=="AUS" & ISOEX=="JPN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# Mexico Panama
df_final %>% 
  group_by(PTA) %>%
  filter(year==2014 & ((ISOEX=="MEX" & ISOIM=="PAN") | (ISOIM=="MEX" & ISOEX=="PAN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Eurasian Economic Union (EAEU) 
df_final <- df_final %>%
  mutate(eaeu_x=case_when(ISOEX%in%c("RUS", "BLR", "KAZ") ~ 1,
                            TRUE ~ 0),
         eaeu_m=case_when(ISOIM%in%c("RUS", "BLR", "KAZ") ~ 1,
                            TRUE ~ 0))
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((eaeu_x==1 & eaeu_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# Canada Korea

df_final %>% 
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="KOR" & ISOIM=="CAN") | (ISOIM=="KOR" & ISOEX=="CAN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

#EU GEORGIA
df_final %>% 
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="GEO" & eu_m==1) | (ISOIM=="GEO" & eu_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

#EU moldova
df_final %>% 
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="MDA" & eu_m==1) | (ISOIM=="MDA" & eu_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Turkey moldova
df_final %>% 
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="MDA" & ISOIM=="TUR") | (ISOIM=="MDA" & ISOEX=="TUR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


############################################################
# 2015
# Korea New Zealand
df_final %>% 
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="KOR" & ISOIM=="NZL") | (ISOIM=="KOR" & ISOEX=="NZL"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Korea VIETNAm
df_final %>% 
  group_by(PTA) %>%
  filter(year==2013 & ((ISOEX=="KOR" & ISOIM=="VNM") | (ISOIM=="KOR" & ISOEX=="VNM"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# CHINA KOREA
df_final %>% 
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="KOR" & ISOIM=="CHN") | (ISOIM=="KOR" & ISOEX=="CHN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Australia China
df_final %>% 
  group_by(PTA) %>%
  filter(year==2020 & (ISOEX=="AUS" & ISOIM=="CHN")) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
# Turkey Singapore
df_final %>% 
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="TUR" & ISOIM=="SGP") | (ISOIM=="TUR" & ISOEX=="SGP"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

############################################################
# 2016

#EU SADC
df_final <- df_final %>%
  mutate(sadc_x=case_when(ISOEX%in%c("ZAF", "BWA", "NAM", "MOZ", "LSO") ~ 1,
                          TRUE ~ 0),
         sadc_m=case_when(ISOIM%in%c("ZAF", "BWA", "NAM", "MOZ", "LSO") ~ 1,
                          TRUE ~ 0))

df_final %>% 
  filter(year==2020 & ((eu_x==1 & sadc_m==1) | (eu_m==1 & sadc_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# EFTA Georgia
df_final %>% 
  filter(year==2020 & ((efta_x==1 & ISOIM=="GEO") | (efta_m==1 & ISOEX=="GEO"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU CANADA
df_final %>%
  group_by(PTA) %>%
  filter(year==2015 & ((eu_x==1 & ISOIM=="CAN") | (eu_m==1 & ISOEX=="CAN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EAEU - VIETNAM
df_final %>%
  group_by(PTA) %>%
  filter(year==2015 & ((eaeu_x==1 & ISOIM=="VNM") | (eaeu_m==1 & ISOEX=="VNM"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# EFTA Phillipines
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((efta_x==1 & ISOIM=="PHL") | (efta_m==1 & ISOEX=="PHL"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Canada Ukraine
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="CAN" & ISOIM=="UKR") | (ISOIM=="CAN" & ISOEX=="UKR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


############################################################
# 2017 


# EU and Peru, Colombia and Ecuador (ECU) (accession of Ecuador)
df_final %>%
 filter(year==2020 & ((eu_x==1 & ISOIM=="ECU") | (eu_m==1 & ISOEX=="ECU"))) %>%
 summarise(total=sum(export, na.rm=TRUE)/1000000000) 

df_final %>%
  group_by(PTA) %>%
  filter(year==2011 & ((eu_x==1 & ISOIM=="ECU") | (eu_m==1 & ISOEX=="ECU"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# CHILE INDONESIA
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="CHL" & ISOIM=="IDN") | (ISOEX=="CHL" & ISOIM=="IDN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# Peru and Honduras (included)
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="HND" & ISOIM=="PER") | (ISOEX=="PER" & ISOIM=="HND"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Peru and Australia
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="AUS" & ISOIM=="PER") | (ISOEX=="PER" & ISOIM=="AUS"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# Canada Ukraine
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="CAN" & ISOIM=="UKR") | (ISOEX=="UKR" & ISOIM=="CAN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA-Georgia (Liechestain not in dataset) check year for Switzerlandqw
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((efta_x==1 & ISOIM=="GEO") | (efta_m==1 & ISOEX=="GEO"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Mercosur - Egypt (signed in 2010 but entered in force in 2017, we keep 2)

df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((mercosur_x==1 & ISOIM=="EGY") | (mercosur_m==1 & ISOEX=="EGY"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# EU and Canada (already included)
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((eu_x==1 & ISOIM=="CAN") | (eu_m==1 & ISOEX=="CAN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# Turkey (TUR) and Singapore (SGP)
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="TUR" & ISOIM=="SGP") | (ISOEX=="SGP" & ISOIM=="TUR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Hong Kong and Macao - 2018
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="HKG" & ISOIM=="MAC") | (ISOEX=="MAC" & ISOIM=="HKG"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Ecuador (ECU) and El Salvador (SLV) - Enabling Clause (then 2), better access for El Salvador than Ecuador
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="SLV" & ISOIM=="ECU") | (ISOEX=="ECU" & ISOIM=="SLV"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# 2018
# China CHN and Georgia (GEO)
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="CHN" & ISOIM=="GEO") | (ISOEX=="GEO" & ISOIM=="CHN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# CPTPP (2019 for Australia, Canada, Japan, Mexico, New Zealand, Singapore; Peru 2021; rest not yet)
df_final %>%
  filter(year==2020 & (cptpp_x==1 & cptpp_m==1)) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA and Phillipines (PHL) - Check if 2 or 3
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((efta_x==1 & ISOIM=="PHL") | (efta_m==1 & ISOEX=="PHL"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA Ecuador
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((efta_x==1 & ISOIM=="ECU") | (efta_m==1 & ISOEX=="ECU"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000)

# EFTA TURKEY
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((efta_x==1 & ISOIM=="TUR") | (efta_m==1 & ISOEX=="TUR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# KOREA CENTRAL AMERICA
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((centralamerica_6_x==1 & ISOIM=="KOR") | (central_america_6_m==1 & ISOEX=="PHL"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 



# 2019
# Samoa and EU (accession of samoa)
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((eu_x==1 & ISOIM=="WSM") | (eu_m==1 & ISOEX=="WSM"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# EU and Japan
df_final %>%
  group_by(PTA) %>%
  filter(year==2018 & ((eu_x==1 & ISOIM=="JPN") | (eu_m==1 & ISOEX=="JPN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# EU - Eastern and Southern Africa States - Accession of Comoros (COM)

df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((eu_x==1 & ISOIM=="COM") | (eu_m==1 & ISOEX=="COM"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# Hong Kong Georgia
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="HKG" & ISOIM=="GEO") | (ISOEX=="GEO" & ISOIM=="HKG"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# ASEAN and Hong Kong (different dates)
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((asean_x==1 & ISOIM=="HKG") | (asean_m==1 & ISOEX=="HKG"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# Chile Indonesia
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((ISOEX=="CHL" & ISOIM=="IDN") | (ISOEX=="IDN" & ISOIM=="CHL"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Turkey - Kosovo (not included since Kosovo is not in the list)

# Costa Rica; El Salvador; Honduras; Korea, Republic of; Nicaragua; Panama
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="KOR" & ISOIM=="CRI") | (ISOEX=="CRI" & ISOIM=="KOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="KOR" & ISOIM=="SLV") | (ISOEX=="SLV" & ISOIM=="KOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="KOR" & ISOIM=="HND") | (ISOEX=="HND" & ISOIM=="KOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="KOR" & ISOIM=="NIC") | (ISOEX=="NIC" & ISOIM=="KOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="KOR" & ISOIM=="PAN") | (ISOEX=="PAN" & ISOIM=="KOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 



# Iran IRN and Eurasian Economic Union (Armenia ARM, Belarus BLR, Kazakhstan KAZ, Kyrgyzstan KGZ, Russia RUS)

df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="IRN" & ISOIM=="ARM") | (ISOEX=="ARM" & ISOIM=="IRN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  filter(year==2019 & ((ISOEX=="IRN" & ISOIM=="BLR") | (ISOEX=="BLR" & ISOIM=="IRN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  filter(year==2019 & ((ISOEX=="IRN" & ISOIM=="KAZ") | (ISOEX=="KAZ" & ISOIM=="IRN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  filter(year==2019 & ((ISOEX=="IRN" & ISOIM=="KGZ") | (ISOEX=="KGZ" & ISOIM=="IRN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  filter(year==2019 & ((ISOEX=="IRN" & ISOIM=="RUS") | (ISOEX=="RUS" & ISOIM=="IRN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 



# European Union and Singapore
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="SGP") | (eu_m==1 & ISOEX=="SGP"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# 2020

# Australia Hong Kong
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="HKG" & ISOIM=="AUS") | (ISOEX=="AUS" & ISOIM=="HKG"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Peru Australia
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="PER" & ISOIM=="AUS") | (ISOEX=="AUS" & ISOIM=="PER"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU - Pacific States - Accession of Solomon Islands (SLB)
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((eu_x==1 & ISOIM=="SLB") | (eu_m==1 & ISOEX=="SLB"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# United States-Mexico-Canada Agreement (USMCA/CUSMA/T-MEC) ALREADY INCLUDED BECAUSE OF NAFTA
df_final %>%
  filter(year==2019 & ((ISOEX=="USA" & ISOIM=="CAN") | (ISOEX=="CAN" & ISOIM=="USA"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  filter(year==2019 & ((ISOEX=="USA" & ISOIM=="MEX") | (ISOEX=="MEX" & ISOIM=="USA"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
df_final %>%
  filter(year==2019 & ((ISOEX=="MEX" & ISOIM=="CAN") | (ISOEX=="CAN" & ISOIM=="MEX"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Indonesia Australia 
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((ISOEX=="IDN" & ISOIM=="AUS") | (ISOEX=="AUS" & ISOIM=="IDN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA Indonesia
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((efta_x==1 & ISOIM=="IDN") | (efta_m==1 & ISOEX=="IDN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


# Vietnam and EU
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="VNM") | (eu_m==1 & ISOEX=="VNM"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA ECUADOR
df_final %>%
  group_by(PTA) %>%
  filter(year==2016 & ((efta_x==1 & ISOIM=="ECU") | (efta_m==1 & ISOEX=="ECU"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU UK
df_final %>%
  group_by(PTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOEX=="GBR") | (eu_m==1 & ISOEX=="GBR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


