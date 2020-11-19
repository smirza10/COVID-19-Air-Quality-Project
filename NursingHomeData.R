library(RSocrata)
library(dplyr)
library(stringr)
library(RCurl)
library(curl)
library(httr)


df <- read.socrata("https://data.cms.gov/resource/s2uc-8wxp.json")


county_nh_subset = subset(df, county == 'Pueblo' & is.na(county) == F)
county_nh_subset = subset(county_nh_subset, provider_state == 'CO' & is.na(provider_state) == F)
county_nh_subset = subset(county_nh_subset, week_ending == '2020-09-06' & is.na(week_ending) == F)
county_nh_subset[c("residents_total_covid_19")][is.na(county_nh_subset[c("residents_total_covid_19")])] <- 0
county_nh_subset[c("residents_total_confirmed")][is.na(county_nh_subset[c("residents_total_confirmed")])] <- 0
cases <- as.numeric(county_nh_subset$residents_total_confirmed)
sum(cases)
deaths <- as.numeric(county_nh_subset$residents_total_covid_19)
sum(deaths)


