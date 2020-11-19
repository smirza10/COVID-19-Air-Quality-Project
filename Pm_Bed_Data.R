library(dplyr)
library(stringr)
library(RCurl)
library(curl)
library(httr)

date_of_study = "06-29-2020"

# Import exposure PM2.5 data
county_pm = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_pm25.csv"))
county_pm_subset = subset(county_pm, fips == 47051 & is.na(fips) == F | fips == 8101 & is.na(fips) == F)
#Importing hospital bed data and creating a subset with the counties I am using
hospitals = read.csv(curl("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
hospitals_subset = subset(hospitals, COUNTYFIPS == "47051" & is.na(COUNTYFIPS) == F | COUNTYFIPS == "53033" & is.na(COUNTYFIPS) == F | COUNTYFIPS == "48439" & is.na(COUNTYFIPS) == F | COUNTYFIPS == "18163" & is.na(COUNTYFIPS) == F | COUNTYFIPS == "17143" & is.na(COUNTYFIPS) == F | COUNTYFIPS == "20177" & is.na(COUNTYFIPS) == F | COUNTYFIPS == "08101" & is.na(COUNTYFIPS) == F)
hospitals_subset$BEDS[hospitals_subset$BEDS < 0] = NA

county_hospitals_aggregated = hospitals_subset %>%
  group_by(COUNTYFIPS) %>%
  summarise(beds = sum(BEDS, na.rm=TRUE))
county_hospitals_aggregated$COUNTYFIPS = str_pad(county_hospitals_aggregated$COUNTYFIPS, 5, pad = "0") # again since this was created from a subset...

county_hospitals_aggregated 


