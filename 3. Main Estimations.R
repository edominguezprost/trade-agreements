# Loading Packages
library(tidyverse)
library(dataverse)
library(dplyr)
library(haven)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(readr)

# Due to problems with Arial Font, I am using this code, but might not be necessary in other computers
library(showtext)
font_add(family = "Arial", regular = "Arial.ttf") ## here is the path to the font to add.
showtext.auto()


# Load
# To create the outputs we need to work with df_final

df_final <- read_csv("df_final.csv")

theme <- theme_set(theme_minimal())

df_final %>%
  group_by(year) %>%
  count()

# We check we are working in period 1999-2021
df_final <- df_final %>%
  filter(year>1999 & year<2022)

# We create variable PTA for categories according to EIA
df_final <- df_final %>%
  mutate(PTA=case_when(EIA==0 ~ "0. No Agreement",
                       EIA==1 ~ "1. NR-PTA",
                       EIA==2 ~ "2. PFTA",
                       EIA==3 ~ "3. FTA", 
                       EIA%in%c(4,5,6) ~ "4. CU-CM-EU",
                       TRUE ~ "0. No Agreement"),
         PTA_potential=case_when(Potential_PTA==0 ~ "0. No Agreement",
                                 Potential_PTA==1 ~ "1. NR-PTA",
                                 Potential_PTA==2 ~ "2. PFTA",
                                 Potential_PTA==3 ~ "3. FTA",
                                 Potential_PTA%in%c(4,5,6) ~ "4. CU-CM-EU",
                                 TRUE ~ "0. No Agreement"))

# We create dummy (text)
df_final <- df_final %>%
  mutate(PTA_dummy=case_when(EIA==0 ~ "0. No Agreement",
                       EIA==1 ~ "1. PTA",
                       EIA==2 ~ "1. PTA",
                       EIA==3 ~ "1. PTA", 
                       EIA%in%c(4,5,6) ~ "1. PTA",
                       TRUE ~ "0. No Agreement"),
         PTA_potential_dummy=case_when(Potential_PTA==0 ~ "0. No Agreement",
                                       Potential_PTA==1 ~ "1. PTA",
                                       Potential_PTA==2 ~ "1. PTA",
                                       Potential_PTA==3 ~ "1. PTA", 
                                       Potential_PTA%in%c(4,5,6) ~ "1. PTA",
                                       TRUE ~ "0. No Agreement"))
# We create dummy (string)
df_final <- df_final %>%
  mutate(PTA_dummy_n=case_when(EIA==0 ~ 0,
                             EIA==1 ~ 0,
                             EIA==2 ~ 1,
                             EIA==3 ~ 1, 
                             EIA%in%c(4,5,6) ~ 1,
                             TRUE ~ 0),
         PTA_potential_dummy_n=case_when(Potential_PTA==0 ~ 0,
                                       Potential_PTA==1 ~ 1,
                                       Potential_PTA==2 ~ 1,
                                       Potential_PTA==3 ~ 1, 
                                       Potential_PTA%in%c(4,5,6) ~ 1,
                                       TRUE ~ 0))


#################################################################################

# Average Number of partners with PTA #
avg <- df_final %>%
  filter(year==2020) %>%
  group_by(ISOEX, Exporter, year) %>%
  summarise(total=sum(PTA_dummy_n),
            expo=sum(export, na.rm=TRUE)/1000000000)

weighted.mean(avg$total, avg$expo)
mean(avg$total)

# On average, countries have some type of PTA with 55 partners (59 weighted average)

#################################################################################

# We create variable for big players
df_final <- df_final %>%
  mutate(group_exporter=case_when(ISOEX%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER") ~ "EU-27",
                                  ISOEX=="CHN" ~ "China",
                                  ISOEX=="USA" ~ "USA",
                                  ISOEX=="IND" ~ "India",
                                  ISOEX=="JPN" ~ "Japan",
                                  ISOEX=="BRA" ~ "Brazil",
                                  TRUE ~ "Rest"))

# We create another variable but putting Brazil in Rest
df_final <- df_final %>%
  mutate(group_exporter_reduced=case_when(ISOEX%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER") ~ "EU-27",
                                  ISOEX=="CHN" ~ "China",
                                  ISOEX=="USA" ~ "USA",
                                  ISOEX=="IND" ~ "India",
                                  ISOEX=="JPN" ~ "Japan",
                                  TRUE ~ "Rest"))

                                
############################        TABLE 1     ################################

# Table 1 is constructed using data from WTO and Trade Additionality (See File 4 and 5)

############################        TABLE 2     ################################

# By running these codes we can get the numbers to prepare Table 2 

# 2000
df_final %>%
  filter(year==2000) %>%
  group_by(PTA) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000)

# PTA: 3,477
# Total Exports 2000 (dataset): 6,117
# Total Exports 2000 (comtrade): 6,357 

# 2010
df_final %>%
  filter(year==2010) %>%
  group_by(PTA) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000)

# PTA: 9241
# Total Exports 2010 (dataset): 14,351 
# Total Exports 2010 (comtrade): 14,978 

# 2020
df_final %>%
  filter(year==2020) %>%
  group_by(PTA_dummy) %>%
  summarise(total=sum(export, na.rm=TRUE)/1000000000)

# PTA: 10,808
# Total Exports 2010 (dataset): 16,399 
# Total Exports 2010 (comtrade): 16,963


############################        TABLE 3     ################################

# Table prepared with Hofmann, C., Osnago, A., & Ruta, M. (2017). Horizontal depth: a new database on the content of preferential trade agreements. World Bank Policy Research Working Paper, (7981).

############################        TABLE 4     ################################

# This file could be use to prepare table 4
country_evolution <- df_final %>%
  filter(year==2000 | year==2010 | year==2020) %>%
  group_by(year, group_exporter, PTA) %>%
  summarise(total_x=sum(export, na.rm=TRUE)/1000000000)

write_csv(country_evolution, "country_evolution.csv")

############################        TABLE 6     ################################

# The following codes are useful to create a new dataframe. If we order by total_x we will get the top 10 bilateral relationships with no agreement

df_final_bilateral <- df_final %>%
  filter(year==2020) %>%
  mutate(group_ex=Exporter,
         group_im=Importer)

df_final_bilateral <- df_final_bilateral %>%
  mutate(group_ex=replace(group_ex, ISOEX%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER"), "UE-27"),
         group_im=replace(group_im, ISOIM%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER"), "UE-27"))

df_final_bilateral <- df_final_bilateral %>%
  filter(PTA=="0. No Agreement" | PTA=="1. NR-PTA") %>%
  group_by(group_ex, group_im, PTA) %>%
  summarise(total_x=sum(export, na.rm=TRUE)/1000000000,
            total_m=sum(import, na.rm=TRUE)/1000000000,
            total=total_x+total_m)

# Info about MFN tariffs can be checked in World Bank

#########################################################

# Estimations about specific countries can be estiamted using this dataset. 
# Estimations about additionality are specified in file "4. Share of World Trade"

# Save 
write_csv(df_final, "df_final.csv")



# EStimating the EU-27

eu <- df_final

eu <- eu %>%
  mutate(eu_importer=case_when(ISOIM%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER") ~ "EU-27",
                                          TRUE ~ ISOIM),
         eu_exporter=case_when(ISOEX%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER") ~ "EU-27",
                               TRUE ~ ISOEX))

eu <- eu %>%
  filter(year==2000 | year==2010 | year==2020) %>%
  group_by(eu_exporter, eu_importer, year, PTA_dummy) %>%
  summarise(total_x=sum(export, na.rm=TRUE)/1000000000)  

eu <- eu %>%
  filter(eu_exporter=="EU-27" & eu_importer!="EU-27")

eu_table <- eu

write_csv(eu_table, "eu_table.csv")

# EU reciprocal
df_final <- read_csv("df_final.csv")

# We check we are working in period 1999-2021
df_final <- df_final %>%
  filter(year>1999 & year<2022)

# We create variable PTA for categories according to EIA
# We create variable PTA for categories according to EIA
df_final <- df_final %>%
  mutate(PTA=case_when(EIA==0 ~ "0. No Agreement",
                       EIA==1 ~ "1. NR-PTA",
                       EIA==2 ~ "2. PFTA",
                       EIA==3 ~ "3. FTA", 
                       EIA%in%c(4,5,6) ~ "4. CU-CM-EU",
                       TRUE ~ "0. No Agreement"),
         PTA_potential=case_when(Potential_PTA==0 ~ "0. No Agreement",
                                 Potential_PTA==1 ~ "1. NR-PTA",
                                 Potential_PTA==2 ~ "2. PFTA",
                                 Potential_PTA==3 ~ "3. FTA",
                                 Potential_PTA%in%c(4,5,6) ~ "4. CU-CM-EU",
                                 TRUE ~ "0. No Agreement"))

# We create dummy (text)
df_final <- df_final %>%
  mutate(PTA_dummy=case_when(EIA==0 ~ "0. No Agreement",
                             EIA==1 ~ "0. No Agreement",
                             EIA==2 ~ "1. PTA",
                             EIA==3 ~ "1. PTA", 
                             EIA%in%c(4,5,6) ~ "1. PTA",
                             TRUE ~ "0. No Agreement"))


eu <- df_final

eu <- eu %>%
  mutate(eu_importer=case_when(ISOIM%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER") ~ "EU-27",
                               TRUE ~ ISOIM),
         eu_exporter=case_when(ISOEX%in%c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVN", "SVK", "ESP", "SWER") ~ "EU-27",
                               TRUE ~ ISOEX))

eu <- eu %>%
  filter(year==2000 | year==2010 | year==2020) %>%
  group_by(eu_exporter, eu_importer, year, PTA_dummy) %>%
  summarise(total_x=sum(export, na.rm=TRUE)/1000000000)  

eu <- eu %>%
  filter(eu_exporter=="EU-27" & eu_importer!="EU-27")

eu_table_2 <- eu

write_csv(eu_table_2, "eu_table_2.csv")


# Japan China

japan_china <- df_final %>%
  filter(ISOEX=="JPN" | ISOEX=="CHN")

japan_china <- japan_china %>%
  filter(year==2020) %>%
  group_by(PTA_dummy, year, ISOEX, rcep_m) %>%
  summarise(total_x=sum(export, na.rm=TRUE)/1000000000)

write_csv(japan_china, "japan_china.csv")
