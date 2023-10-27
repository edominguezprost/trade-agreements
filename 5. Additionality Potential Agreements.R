# Additionallity New Agreements

# The following code is to estimate Additionality in Table 
# As for Table 1, to see different years we need to change the filter

# Loading Packages
library(tidyverse)
library(dataverse)
library(dplyr)
library(haven)
library(ggplot2)
library(reshape2)
library(ggrepel)

# Loading large and original dataset
df_final <- read_csv("df_final.csv")

df_final <- df_final %>%
  mutate(FTA=case_when(EIA==0 ~ "No Agreement",
                             EIA==1 ~ "NRPTA-FPTA",
                             EIA==2 ~ "NRPTA-FPTA",
                             EIA==3 ~ "FTA or CU-CM-EU", 
                             EIA%in%c(4,5,6) ~ "FTA or CU-CM-EU",
                             TRUE ~ "No Agreement"))


# TPP
df_final <- df_final %>%
  mutate(tpp_x=case_when(ISOEX%in%c("AUS", "BRN", "CAN", "CHL", "JPN", "MYS", "MEX", "NZL", "PER", "SGP", "VNM", "USA") ~ 1,
                           TRUE ~ 0),
         tpp_m=case_when(ISOIM%in%c("AUS", "BRN", "CAN", "CHL", "JPN", "MYS", "MEX", "NZL", "PER", "SGP", "VNM", "USA") ~ 1,
                           TRUE ~ 0))

# by country
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & (usmca_x==1 & usmca_m==1)) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

#CPTPP
df_final %>%
  filter(year==2017 & (tpp_x==1 & tpp_m==1)) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

cptpp <- df_final %>%
  filter(year==2017 & (cptpp_x==1 & cptpp_m==1)) %>%
  select(ISOEX, ISOIM, Exporter, Importer, export, import, EIA, FTA)


cptpp_x <- df_final %>%
  group_by(ISOEX, Exporter,  FTA) %>%
  filter(year==2019 & (cptpp_x==1 & cptpp_m==1)) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

cptpp_x %>%
  ggplot(aes(y=Exporter, x=total, fill=FTA)) +
  geom_bar(position="fill", stat="identity") +
  theme(legend.position="bottom")

cptpp_m <- df_final %>%
  group_by(ISOIM, Importer,  FTA) %>%
  filter(year==2017 & (cptpp_x==1 & cptpp_m==1)) %>%
  summarise(total=sum(import, na.rm=TRUE)/1000000000) 

cptpp_m %>%
  ggplot(aes(y=Importer, x=total, fill=FTA)) +
  geom_bar(position="fill", stat="identity") +
  theme(legend.position="bottom")


#RCEP
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & (rcep_x==1 & rcep_m==1)) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

#AfCFTA
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & (afcfta_x==1 & afcfta_m==1)) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU India
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="IND") | (eu_m==1 & ISOEX=="IND"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU MERCOSUR
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eu_x==1 & mercosur_m==1) | (eu_m==1 & mercosur_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

#  Japan GCC
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((gcc_x==1 & ISOIM=="JPN") | (gcc_m==1 & ISOEX=="JPN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU Wafrica
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eu_x==1 & wafrica_m==1) | (eu_m==1 & wafrica_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# INDIA UAE
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="ARE" & ISOIM=="IND") | (ISOEX=="IND" & ISOIM=="ARE"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU MALAYSIA
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="MYS") | (ISOEX=="MYS" & eu_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU THAILAND
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="THA") | (ISOEX=="THA" & eu_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU INDONESIA
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="IDN") | (ISOEX=="IDN" & eu_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU TUNISIA
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="TUN") | (ISOEX=="TUN" & eu_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA INDIA
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((efta_x==1 & ISOIM=="IND") | (ISOEX=="IND" & efta_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU PHL
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eu_x==1 & ISOIM=="PHL") | (ISOEX=="PHL" & eu_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# KOREA MEXICO
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="KOR" & ISOIM=="MEX") | (ISOEX=="MEX" & ISOIM=="KOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# INDIA SACU
df_final <- df_final %>%
  mutate(sacu_x=case_when(ISOEX%in%c("BWA", "LSO","NAM","ZAF","SWZ") ~ 1,
                          TRUE ~ 0),
         sacu_m=case_when(ISOIM%in%c("BWA", "LSO","NAM","ZAF","SWZ") ~ 1,
                          TRUE ~ 0))

df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="IND" & sacu_m==1) | (ISOIM=="IND" & sacu_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA MCS
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((efta_x==1 & mercosur_m==1) | (efta_m==1 & mercosur_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# China Norway
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="CHN" & ISOIM=="NOR") | (ISOIM=="CHN" & ISOEX=="NOR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# AUSTRALIA GCC
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="AUS" & gcc_m==1) | (ISOIM=="AUS" & gcc_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# Canada MCS
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="CAN" & mercosur_m==1) | (ISOIM=="CAN" & mercosur_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EU EAC
df_final <- df_final %>%
  mutate(eac_x=case_when(ISOEX%in%c("BDI", "KEN", "RWA", "UGA", "TZA") ~ 1,
                          TRUE ~ 0),
         eac_m=case_when(ISOIM%in%c("BDI", "KEN", "RWA", "UGA", "TZA") ~ 1,
                          TRUE ~ 0))

df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((eac_x==1 & eu_m==1) | (eac_m==1 & eu_x==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# UKRAine turkey
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="UKR" & ISOIM=="TUR") | (ISOIM=="UKR" & ISOEX=="TUR"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA VIETNAM
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((efta_x==1 & ISOIM=="VNM") | (ISOEX=="VNM" & efta_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 
# CANADA SGP
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="CAN" & ISOIM=="SGP") | (ISOIM=="CAN" & ISOEX=="SGP"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# CANADA DR
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="CAN" & ISOIM=="DOM") | (ISOIM=="CAN" & ISOEX=="DOM"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# UKR SGP
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="UKR" & ISOIM=="SGP") | (ISOIM=="UKR" & ISOEX=="SGP"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# MOLDOVA CHINA
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((ISOEX=="MDA" & ISOIM=="CHN") | (ISOIM=="MDA" & ISOEX=="CHN"))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 

# EFTA GUATEMALA
df_final %>%
  group_by(FTA) %>%
  filter(year==2020 & ((efta_x==1 & ISOIM=="GTM") | (ISOEX=="GTM" & efta_m==1))) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000) 


