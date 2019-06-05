
library(ggplot2)
library(skimr)
library(tidyverse)
library(janitor)
library(reshape2)
library(lubridate)


#load data
iowa_data <- read_csv("data/iowa_data.csv") %>%
  clean_names() %>%
  select(date, hourly_dew_point_temperature, hourly_dry_bulb_temperature,
         hourly_wet_bulb_temperature) %>%
  separate(date, into = c("day", "time"), " ")%>%
  filter(time == "12:53:00")

#filter data for appropriate timeframe
iowa_data1617 <- filter(iowa_data, day >= "2016-12-01" & day <= "2017-03-15")

# load data pt.2
iowa_data_2 <- read_csv("data/iowa_data2.csv")%>%
  clean_names()%>%
  select(date, hourly_dew_point_temperature, hourly_dry_bulb_temperature,
       hourly_wet_bulb_temperature) %>%
  separate(date, into = c("day", "time"), " ")%>%
  filter(time == "12:53:00")

# Filter data by year
iowa_data1516 <- filter(iowa_data_2, day >= "2015-12-01" & day <= "2016-03-15")
iowa_data1415 <- filter(iowa_data_2, day >= "2014-12-01" & day <= "2015-03-15")
iowa_data1314 <- filter(iowa_data_2, day >= "2013-12-01" & day <= "2014-03-15")
iowa_data1213 <- filter(iowa_data_2, day >= "2012-12-01" & day <= "2013-03-15")
iowa_data1112 <- filter(iowa_data_2, day >= "2011-12-01" & day <= "2012-03-15")
iowa_data1011 <- filter(iowa_data_2, day >= "2010-12-01" & day <= "2011-03-15")
iowa_data0910 <- filter(iowa_data_2, day >= "2009-12-01" & day <= "2010-03-15")
iowa_data0809 <- filter(iowa_data_2, day >= "2008-12-01" & day <= "2009-03-15")
iowa_data0708 <- filter(iowa_data_2, day >= "2007-12-01" & day <= "2008-03-15")
iowa_data0607 <- filter(iowa_data_2, day >= "2006-12-01" & day <= "2007-03-15")

# bind data together
iowa141516 <- rbind(iowa_data1415, iowa_data1516)
iowa121314 <- rbind(iowa_data1314, iowa_data1213)
iowa101112 <- rbind(iowa_data1112, iowa_data1011)
iowa100809 <- rbind(iowa_data0809, iowa_data0910)
iowa070806 <- rbind(iowa_data0708, iowa_data0607)

#bind data 
iowateen <- rbind(iowa_data1617, iowa141516)
iowatween <- rbind(iowa121314, iowa101112)
iowachild <- rbind(iowa100809, iowa070806)
iowalots <- rbind(iowateen, iowatween)

#filter out NA
iowa_data_combo <- rbind(iowalots, iowachild) %>%
  filter(!is.na(hourly_dry_bulb_temperature))%>%
  filter(!is.na(hourly_wet_bulb_temperature))%>%
  select(-time)
  
#get unique daily values
iowa_data_combo <- iowa_data_combo %>%
  mutate(avg_dry = hourly_dry_bulb_temperature,
         avg_wet = hourly_wet_bulb_temperature)%>%
  select(day,avg_dry,avg_wet)%>%
  unique()

# Meteorlogical Winter Data Prep

#prep Meterorlogical 2017
iowa_m_data1617 <- filter(iowa_data_combo, day >= "2016-12-15" & day <= "2017-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2017")

#prep Meterorlogical 2016
iowa_m_data1516 <- filter(iowa_data_combo, day >= "2015-12-15" & day <= "2016-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2016")

#prep Meterorlogical 2015
iowa_m_data1415 <- filter(iowa_data_combo, day >= "2014-12-15" & day <= "2015-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2015")

#prep Meterorlogical 2014
iowa_m_data1314 <- filter(iowa_data_combo, day >= "2013-12-15" & day <= "2014-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2014")

#prep Meterorlogical 2013
iowa_m_data1213 <- filter(iowa_data_combo, day >= "2012-12-15" & day <= "2013-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2013")

#prep Meterorlogical 2012
iowa_m_data1112 <- filter(iowa_data_combo, day >= "2011-12-15" & day <= "2012-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2012")

#prep Meterorlogical 2011
iowa_m_data1011 <- filter(iowa_data_combo, day >= "2010-12-15" & day <= "2011-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2011")

#prep Meterorlogical 2010
iowa_m_data0910 <- filter(iowa_data_combo, day >= "2009-12-15" & day <= "2010-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2010")

#prep Meterorlogical 2009
iowa_m_data0809 <- filter(iowa_data_combo, day >= "2008-12-15" & day <= "2009-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2009")

#prep Meterorlogical 2008
iowa_m_data0708 <- filter(iowa_data_combo, day >= "2007-12-15" & day <= "2008-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2008")

#prep Meterorlogical 2007
iowa_m_data0607 <- filter(iowa_data_combo, day >= "2006-12-15" & day <= "2007-03-15")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2007")

#bind data together
iowam161718 <- rbind(iowa_m_data1718, iowa_m_data1617)
iowam141516 <- rbind(iowa_m_data1516, iowa_m_data1415)
iowam121314 <- rbind(iowa_m_data1314, iowa_m_data1213)
iowam101112 <- rbind(iowa_m_data1112, iowa_m_data1011)
iowam100809 <- rbind(iowa_m_data0910, iowa_m_data0809)
iowam070806 <- rbind(iowa_m_data0708, iowa_m_data0607)

iowamteen <- rbind(iowam161718, iowam141516)
iowamtween <- rbind(iowam121314, iowam101112)
iowamchild <- rbind(iowam100809, iowam070806)
iowamlots <- rbind(iowamteen, iowamtween)


iowa_m_winter <-rbind(iowamlots,iowamchild)

# create west nile data
west_nile <- data.frame(c(12, 37,14,15,44,31,9,9,5,6,30))

# add west nile data
iowa_m_west_nile <- cbind(iowa_m_winter, west_nile)

# mutate names  
names(iowa_m_west_nile)[4] <- "westnile"
names(iowa_m_west_nile)[2] <- "avg_dry_year"
names(iowa_m_west_nile)[3] <- "avg_wet_year"


#Traditional Winter Data Prep 

#Traditional Winter 2017
iowa_t_data1617 <- filter(iowa_data_combo, day >= "2016-12-01" & day <= "2017-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2017")

#Traditional Winter 2016
iowa_t_data1516 <- filter(iowa_data_combo, day >= "2015-12-01" & day <= "2016-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2016")

#Traditional Winter 2015
iowa_t_data1415 <- filter(iowa_data_combo, day >= "2014-12-01" & day <= "2015-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2015")

#Traditional Winter 2014
iowa_t_data1314 <- filter(iowa_data_combo, day >= "2013-12-01" & day <= "2014-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2014")

#Traditional Winter 2013
iowa_t_data1213 <- filter(iowa_data_combo, day >= "2012-12-01" & day <= "2013-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2013")

#Traditional Winter 2012
iowa_t_data1112 <- filter(iowa_data_combo, day >= "2011-12-01" & day <= "2012-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2012")

#Traditional Winter 2011
iowa_t_data1011 <- filter(iowa_data_combo, day >= "2010-12-01" & day <= "2011-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2011")

#Traditional Winter 2010
iowa_t_data0910 <- filter(iowa_data_combo, day >= "2009-12-01" & day <= "2010-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2010")

#Traditional Winter 2009
iowa_t_data0809 <- filter(iowa_data_combo, day >= "2008-12-01" & day <= "2009-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2009")

#Traditional Winter 2008
iowa_t_data0708 <- filter(iowa_data_combo, day >= "2007-12-01" & day <= "2008-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2008")

#Traditional Winter 2007
iowa_t_data0607 <- filter(iowa_data_combo, day >= "2006-12-01" & day <= "2007-03-01")%>%
  separate(day, into = c("year", "day"), "-")%>%
  separate(day, into = c("month", "day"), "-")%>%
  select(-day)%>%
  group_by(month)%>%
  mutate(avg_dry_month = mean(avg_dry),
         avg_wet_month = mean(avg_wet))%>%
  select(-c(avg_wet, avg_dry))%>%
  unique()%>%
  adorn_totals("row")%>%
  mutate(avg_dry_month = ifelse(year == "Total", avg_dry_month/4, avg_dry_month ))%>%
  mutate(avg_wet_month = ifelse(year == "Total", avg_wet_month/4, avg_wet_month ))%>%
  filter(year == "Total")%>%
  select(-month)%>%
  mutate(year = "2007")

#Bind data together
iowat141516 <- rbind(iowa_t_data1516, iowa_t_data1415)
iowat121314 <- rbind(iowa_t_data1314, iowa_t_data1213)
iowat101112 <- rbind(iowa_t_data1112, iowa_t_data1011)
iowat100809 <- rbind(iowa_t_data0910, iowa_t_data0809)
iowat070806 <- rbind(iowa_t_data0708, iowa_t_data0607)

iowatteen <- rbind(iowa_t_data1617, iowat141516)
iowattween <- rbind(iowat121314, iowat101112)
iowatchild <- rbind(iowat100809, iowat070806)
iowatlots <- rbind(iowatteen, iowattween)


iowa_t_winter <-rbind(iowatlots,iowatchild)

#create west nile dataframe
west_nile <- data.frame(c(12, 37,14,15,44,31,9,9,5,6,30))

#Bind westnile to database
iowa_t_west_nile <- cbind(iowa_t_winter, west_nile)

#mutate names
names(iowa_t_west_nile)[4] <- "westnile"
names(iowa_t_west_nile)[2] <- "avg_dry_year"
names(iowa_t_west_nile)[3] <- "avg_wet_year"


