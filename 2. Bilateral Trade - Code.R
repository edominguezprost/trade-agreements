# Loading Packages

library(tidyverse)
library(dataverse)
library(dplyr)
library(haven)
library(readr)

##Downloading Trade data from the dataverse and processing the three-year average

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

#---------------------------------------------------------------------------------------------

trade_2000 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2000.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2000 <- trade_2000 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2000, "trade_2000.csv")
trade_2000 <- read_csv("trade_2000.csv")

trade_2001 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2001.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2001 <- trade_2001 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2001, "trade_2001.csv")
trade_2001 <- read_csv("trade_2001.csv")



trade_2002 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2002.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2002 <- trade_2002 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2002, "trade_2002.csv")
trade_2002 <- read_csv("trade_2002.csv")


trade_2003 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2003.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2003 <- trade_2003 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2003, "trade_2003.csv")
trade_2003 <- read_csv("trade_2003.csv")



trade_2004 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2004.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2004 <- trade_2004 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2004, "trade_2004.csv")
trade_2004 <- read_csv("trade_2004.csv")



trade_2005 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2005.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2005 <- trade_2005 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2005, "trade_2005.csv")
trade_2005 <- read_csv("trade_2005.csv")


trade_2006 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2006.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2006 <- trade_2006 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2006, "trade_2006.csv")
trade_2006 <- read_csv("trade_2006.csv")


trade_2007 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2007.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2007 <- trade_2007 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2007, "trade_2007.csv")
trade_2007 <- read_csv("trade_2007.csv")


trade_2008 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2008.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2008 <- trade_2008 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2008, "trade_2008.csv")
trade_2008 <- read_csv("trade_2008.csv")

trade_2009 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2009.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2009 <- trade_2009 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2009, "trade_2009.csv")
trade_2009 <- read_csv("trade_2009.csv")

trade_2010 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2010.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2010 <- trade_2010 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2010, "trade_2010.csv")
trade_2010 <- read_csv("trade_2010.csv")

trade_2011 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2011.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2011 <- trade_2011 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2011, "trade_2011.csv")
trade_2011 <- read_csv("trade_2011.csv")

trade_2012 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2012.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2012 <- trade_2012 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2012, "trade_2012.csv")
trade_2012 <- read_csv("trade_2012.csv")


trade_2013 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2013.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2013 <- trade_2013 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2013, "trade_2013.csv")
trade_2013 <- read_csv("trade_2013.csv")

trade_2014 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2014.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2014 <- trade_2014 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2014, "trade_2014.csv")
trade_2014 <- read_csv("trade_2014.csv")

trade_2015 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2015.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2015 <- trade_2015 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2015, "trade_2015.csv")
trade_2015 <- read_csv("trade_2015.csv")

trade_2016 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2016.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2016 <- trade_2016 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2016, "trade_2016.csv")
trade_2016 <- read_csv("trade_2016.csv")


trade_2017 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2017.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2017 <- trade_2017 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2017, "trade_2017.csv")
trade_2017 <- read_csv("trade_2017.csv")

trade_2018 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2018.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2018 <- trade_2018 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2018, "trade_2018.csv")
trade_2018 <- read_csv("trade_2018.csv")

trade_2019 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2019.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2019 <- trade_2019 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2019, "trade_2019.csv")
trade_2019 <- read_csv("trade_2019.csv")

trade_2020 <- rbind(get_dataframe_by_name(
  "country_partner_hsproduct4digit_year_2020.dta", "10.7910/DVN/T4CHWJ", .f=haven::read_dta)) %>%
  group_by(location_id, partner_id, location_code, partner_code, hs_product_code, product_id)

trade_2020 <- trade_2020 %>%
  group_by(location_id, partner_id, year, location_code, partner_code) %>%
  summarise(export=sum(export_value, na.rm=TRUE),
            import=sum(import_value, na.rm=TRUE))

write_csv(trade_2020, "trade_2020.csv")
trade_2020 <- read_csv("trade_2020.csv")

#---------------------------------------------------------------------------------------------

# 2021
trade_2021 <- trade_2020 %>%
  mutate(year=2021)

trade <- bind_rows(trade_2000, trade_2001, trade_2002, trade_2003, trade_2004, trade_2005, trade_2006, trade_2007, trade_2008, 
                   trade_2009, trade_2010, trade_2011, trade_2012, trade_2013, trade_2014, trade_2015, trade_2016, trade_2017, 
                   trade_2018, trade_2019, trade_2020, trade_2021)


# Changing Romania (code is not well written)
trade <- trade %>%
  mutate(location_code=replace(location_code, location_code=="ROU", "ROM"),
         partner_code=replace(partner_code, partner_code=="ROU", "ROM"))

      
write_csv(trade, "trade_allyears.csv")

#---------------------------------------------------------------------------------------------
# We create the main dataset
df <- read_csv("df_agreements.csv")
df_final <- left_join(df, trade, by = c("ISOEX"="location_code", "ISOIM"="partner_code", "year"))

# Saving File

write_csv(df_final, "df_final.csv")


