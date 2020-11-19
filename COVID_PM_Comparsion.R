library(dplyr)
library(stringr)
library(RCurl)
library(curl)
library(httr)
library(ggplot2)
library(tidyverse)
library(ggpmisc)
date_of_study = "06-29-2020"

# Import exposure PM2.5 data
county_pm = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_pm25.csv"))
county_pm_subset = subset(county_pm, fips == 17031 & is.na(fips) == F | fips == 8069 & is.na(fips) == F)

#Finding PM2.5 for all counties
Chi_PM = subset(county_pm, fips == 17031 & is.na(fips) == F)
Chi_avg=mean(Chi_PM$pm25)
LA_PM = subset(county_pm, fips == 06037 & is.na(fips) == F)
LA_avg =mean(LA_PM$pm25)
Kane_PM = subset(county_pm, fips == 17089 & is.na(fips) == F)
Kane_avg =mean(Kane_PM$pm25)
Gal_PM = subset(county_pm, fips == 48167 & is.na(fips) == F)
Gal_avg =mean(Gal_PM$pm25)
Peo_PM = subset(county_pm, fips == 17143 & is.na(fips) == F)
Peo_avg =mean(Peo_PM$pm25)
Shw_PM = subset(county_pm, fips == 20177 & is.na(fips) == F)
Shw_avg =mean(Shw_PM$pm25)
San_PM = subset(county_pm, fips == 17167 & is.na(fips) == F)
San_avg =mean(San_PM$pm25)
Sm_PM = subset(county_pm, fips == 48423 & is.na(fips) == F)
Sm_avg =mean(Sm_PM$pm25)
Har_PM= subset(county_pm, fips == 48201 & is.na(fips) == F)
Har_avg=mean(Har_PM$pm25)
MD_PM = subset(county_pm, fips == 12086 & is.na(fips) == F)
MD_avg=mean(MD_PM$pm25)
Mar_PM = subset(county_pm, fips == 04013 & is.na(fips) == F)
Mar_avg=mean(Mar_PM$pm25)
Laf_PM = subset(county_pm, fips == 22055  & is.na(fips) == F)
Laf_avg=mean(Laf_PM$pm25)
Gui_PM = subset(county_pm, fips == 37081 & is.na(fips) == F)
Gui_avg=mean(Gui_PM$pm25)
Den_PM = subset(county_pm, fips == 48121 & is.na(fips) == F)
Den_avg=mean(Den_PM$pm25)
Ect_PM = subset(county_pm, fips == 48135  & is.na(fips) == F)
Ect_avg=mean(Ect_PM$pm25)
Lac_PM = subset(county_pm, fips == 42069  & is.na(fips) == F)
Lac_avg=mean(Lac_PM$pm25)
Bra_PM = subset(county_pm, fips == 48041 & is.na(fips) == F)
Bra_avg=mean(Bra_PM$pm25)
Or_PM=subset(county_pm, fips == 06059 & is.na(fips) == F)
Or_avg=mean(Or_PM$pm25)
Ut_PM=subset(county_pm, fips == 49049  & is.na(fips) == F)
Ut_avg=mean(Ut_PM$pm25)
Ala_PM = subset(county_pm, fips == 12001  & is.na(fips) == F)
Ala_avg=mean(Ala_PM$pm25)
Bro_PM = subset(county_pm, fips == 12011  & is.na(fips) == F)
Bro_avg=mean(Bro_PM$pm25)
Ven_PM = subset(county_pm, fips == 06111  & is.na(fips) == F)
Ven_avg=mean(Ven_PM$pm25)
Pue_PM = subset(county_pm, fips == 08101  & is.na(fips) == F)
Pue_avg=mean(Pue_PM$pm25)
Way_PM = subset(county_pm, fips == 26163  & is.na(fips) == F)
Way_avg=mean(Way_PM$pm25)
Tar_PM = subset(county_pm, fips == 48439  & is.na(fips) == F)
Tar_avg=mean(Tar_PM$pm25)
SD_PM = subset(county_pm, fips == 06073  & is.na(fips) == F)
SD_avg=mean(SD_PM$pm25)
Van_PM = subset(county_pm, fips == 18163  & is.na(fips) == F)
Van_avg=mean(Van_PM$pm25)
Stj_PM = subset(county_pm, fips == 18141  & is.na(fips) == F)
Stj_avg=mean(Stj_PM$pm25)
Dog_PM = subset(county_pm, fips == 20045  & is.na(fips) == F)
Dog_avg=mean(Dog_PM$pm25)
We_PM = subset(county_pm, fips == 08123  & is.na(fips) == F)
We_avg=mean(We_PM$pm25)
Brza_PM = subset(county_pm, fips == 48039  & is.na(fips) == F)
Brza_avg=mean(Brza_PM$pm25)
Nh_PM = subset(county_pm, fips == 37129  & is.na(fips) == F)
Nh_avg=mean(Nh_PM$pm25)
Olm_PM = subset(county_pm, fips == 27109  & is.na(fips) == F)
Olm_avg=mean(Olm_PM$pm25)
Dal_PM = subset(county_pm, fips == 48113 & is.na(fips) == F)
Dal_avg=mean(Dal_PM$pm25)
Pb_PM = subset(county_pm, fips == 12099 & is.na(fips) == F)
Pb_avg=mean(Pb_PM$pm25)
Rut_PM = subset(county_pm, fips == 47051  & is.na(fips) == F)
Rut_avg=mean(Rut_PM$pm25)

#Creating Dataframes
#Creating comparisons for cook Similarity Score KNN K = 7
cook_Knn=data.frame(County= c('Cook', 'Harris', 'Dallas', 'Miami-Dade', 'Orange', 'Broward', 'Tarrant', 'Maricopa'),
                PM25_AVG= c(Chi_avg, Har_avg, Dal_avg, MD_avg, Or_avg, Bro_avg, Tar_avg, Mar_avg),
                Mortality_Rate = c(3.7, 1.82, 1.39, 1.83, 2.16, 1.74, 1.47, 2.33),
                Comparison = c('Cook'))

cook_Knn_Excess=data.frame(County= c('Cook', 'Harris', 'Dallas', 'Miami-Dade', 'Orange', 'Broward', 'Tarrant', 'Maricopa'),
                    PM25_AVG= c(Chi_avg, Har_avg, Dal_avg, MD_avg, Or_avg, Bro_avg, Tar_avg, Mar_avg),
                    Mortality_Rate = c(2.64, 1.54, 1.03, 1.61, 1.64, 1.54, .97, 2.13),
                    Comparison = c('Cook'))

#Creating comparisons for cook Similarity Score < 2.5
cook_2.5= data.frame(County= c('Cook', 'Harris', 'Dallas', 'Miami-Dade', 'Orange', 'Broward', 'Tarrant'),
                     PM25_AVG= c(Chi_avg, Har_avg, Dal_avg, MD_avg, Or_avg, Bro_avg, Tar_avg),
                     Mortality_Rate = c(3.7, 1.82, 1.39, 1.83, 2.16, 1.74, 1.47),
                     Comparison = c('Cook'))

cook_2.5_Excess=data.frame(County= c('Cook', 'Harris', 'Dallas', 'Miami-Dade', 'Orange', 'Broward', 'Tarrant'),
                           PM25_AVG= c(Chi_avg, Har_avg, Dal_avg, MD_avg, Or_avg, Bro_avg, Tar_avg),
                           Mortality_Rate = c(2.64, 1.54, 1.03, 1.61, 1.64, 1.54, .97),
                           Comparison = c('Cook'))

#Creating comparisons for cook Similarity Score < 3.0
cook_3= data.frame(County= c('Cook', 'Harris', 'Dallas', 'Miami-Dade', 'Orange', 'Broward', 'Tarrant', 'Maricopa', 'San Diego', 'Palm Beach'),
                     PM25_AVG= c(Chi_avg, Har_avg, Dal_avg, MD_avg, Or_avg, Bro_avg, Tar_avg, Mar_avg, SD_avg, Pb_avg),
                     Mortality_Rate = c(3.7, 1.82, 1.39, 1.83, 2.16, 1.74, 1.47, 2.33, 1.69, 2.81),
                     Comparison = c('Cook'))

cook_3_Excess= data.frame(County= c('Cook', 'Harris', 'Dallas', 'Miami-Dade', 'Orange', 'Broward', 'Tarrant', 'Maricopa', 'San Diego', 'Palm Beach'),
                     PM25_AVG= c(Chi_avg, Har_avg, Dal_avg, MD_avg, Or_avg, Bro_avg, Tar_avg, Mar_avg, SD_avg, Pb_avg),
                     Mortality_Rate = c(2.64, 1.54, 1.03, 1.61, 1.64, 1.54, .97, 2.13, 1.49, 2.20),
                     Comparison = c('Cook'))

#Creating comparisons for Kane Similarity Score Knn
Kane_Knn=data.frame(County= c('Kane', 'Galveston', 'Denton', 'Guilford', 'Ventura', 'Rutherford', 'Lafayette', 'Utah'),
                  PM25_AVG= c(Kane_avg, Gal_avg, Den_avg, Gui_avg, Ven_avg, Rut_avg, Laf_avg, Ut_avg),
                  Mortality_Rate = c(2.57, 1.34, 1.28, 2.16, 1.15, .96, 1.39, .34),
                  Comparison = c('Kane')) 

Kane_Knn_Excess=data.frame(County= c('Kane', 'Galveston', 'Denton', 'Guilford', 'Ventura', 'Rutherford', 'Lafayette', 'Utah'),
                    PM25_AVG= c(Kane_avg, Gal_avg, Den_avg, Gui_avg, Ven_avg, Rut_avg, Laf_avg, Ut_avg),
                    Mortality_Rate = c(1.57, .94, .92, 1.63, .96, .83, .83, .33),
                    Comparison = c('Kane')) 

#Creating comparisons for Kane Similarity Score < 2.5
Kane_2.5=data.frame(County= c('Kane', 'Galveston', 'Denton', 'Guilford', 'Ventura', 'Rutherford'),
                    PM25_AVG= c(Kane_avg, Gal_avg, Den_avg, Gui_avg, Ven_avg, Rut_avg),
                    Mortality_Rate = c(2.57, 1.34, 1.28, 2.16, 1.15, .96),
                    Comparison = c('Kane'))

Kane_2.5_Excess=data.frame(County= c('Kane', 'Galveston', 'Denton', 'Guilford', 'Ventura', 'Rutherford'),
                    PM25_AVG= c(Kane_avg, Gal_avg, Den_avg, Gui_avg, Ven_avg, Rut_avg),
                    Mortality_Rate = c(1.57, .94, .92, 1.63, .96, .83),
                    Comparison = c('Kane'))

#Creating comparisons for Kane Similarity Score < 3
Kane_3=data.frame(County= c('Kane', 'Galveston', 'Denton', 'Guilford', 'Ventura', 'Rutherford', 'Lafayette', 'Utah', 'Brazoria', 'St. Joseph'),
                PM25_AVG= c(Kane_avg, Gal_avg, Den_avg, Gui_avg, Ven_avg, Rut_avg, Laf_avg, Ut_avg, Brza_avg, Stj_avg),
                Mortality_Rate = c(2.57, 1.34, 1.28, 2.16, 1.15, .96, 1.39, .34, 1.43, 1.64),
                Comparison = c('Kane'))

Kane_3_Excess=data.frame(County= c('Kane', 'Galveston', 'Denton', 'Guilford', 'Ventura', 'Rutherford', 'Lafayette', 'Utah', 'Brazoria', 'St. Joseph'),
                  PM25_AVG= c(Kane_avg, Gal_avg, Den_avg, Gui_avg, Ven_avg, Rut_avg, Laf_avg, Ut_avg, Brza_avg, Stj_avg),
                  Mortality_Rate = c(1.57, .94, .92, 1.63, .96, .83, .83, .33, 1.19, 1.18),
                  Comparison = c('Kane'))

#Creating comparisons for Peoria Similarity KNN
Peoria_Knn=data.frame(County= c('Peoria', 'Sangamon', 'Smith', 'Shawnee', 'Lackawanna', 'Ector', 'Douglas', 'Pueblo'),
                      PM25_AVG= c(Peo_avg, San_avg, Sm_avg, Shw_avg, Lac_avg, Ect_avg, Dog_avg, Pue_avg),
                      Mortality_Rate = c(1.49, 1.96, 2.65, 1.16, 9.0, 2.0, .48, 3.46),
                      Comparison = c('Peoria'))

Peoria_Knn_Excess =data.frame(County= c('Peoria', 'Sangamon', 'Smith', 'Shawnee', 'Lackawanna', 'Ector', 'Douglas', 'Pueblo'),
                      PM25_AVG= c(Peo_avg, San_avg, Sm_avg, Shw_avg, Lac_avg, Ect_avg, Dog_avg, Pue_avg),
                      Mortality_Rate = c(.89, 1.87, 2.04, .76, 5.81, 1.42, .05, 2.88),
                      Comparison = c('Peoria'))

#Creating comparisons for Peoria Similarity Score < 2.5
Peoria_2.5=data.frame(County= c('Peoria', 'Sangamon', 'Smith', 'Shawnee', 'Lackawanna', 'Ector', 'Alachua', 'Pueblo',
                              'Vanderburgh', 'Douglas', 'St. Joseph', 'Weld'),
                    PM25_AVG= c(Peo_avg, San_avg, Sm_avg, Shw_avg, Lac_avg, Ect_avg, Ala_avg, Pue_avg, Van_avg, Dog_avg, Stj_avg,
                                We_avg ),
                    Mortality_Rate = c(1.49, 1.96, 2.65, 1.16, 9.0, 2.0, .61, 3.46, .89, .48, 1.64, 3.28),
                    Comparison = c('Peoria'))
Peoria_2.5_Excess=data.frame(County= c('Peoria', 'Sangamon', 'Smith', 'Shawnee', 'Lackawanna', 'Ector', 'Alachua', 'Pueblo',
                                'Vanderburgh', 'Douglas', 'St. Joseph', 'Weld'),
                      PM25_AVG= c(Peo_avg, San_avg, Sm_avg, Shw_avg, Lac_avg, Ect_avg, Ala_avg, Pue_avg, Van_avg, Dog_avg, Stj_avg,
                                  We_avg),
                      Mortality_Rate = c(.89, 1.87, 2.04, .76, 5.81, 1.42, .26, 2.88, .72, .5, 1.18, 2.53),
                      Comparison = c('Peoria'))

#Creating comparisons for Peoria Similarity Score < 3
Peoria_3=data.frame(County= c('Peoria', 'Sangamon', 'Smith', 'Shawnee', 'Lackawanna', 'Ector', 'Alachua', 'Pueblo',
                            'Vanderburgh', 'Douglas', 'St. Joseph', 'Weld', 'Brazos', 'Lafayette', 'Guilford', 'Rutherford'),
                  PM25_AVG= c(Peo_avg, San_avg, Sm_avg, Shw_avg, Lac_avg, Ect_avg, Ala_avg, Pue_avg, Van_avg, Dog_avg, Stj_avg,
                              We_avg, Bra_avg, Laf_avg, Gui_avg, Rut_avg),
                  Mortality_Rate = c(1.49, 1.96, 2.65, 1.16, 9.0, 2.0, .61, 3.46, .89, .48, 1.64, 3.28, 1.02, 1.39, 2.16, .96),
                  Comparison = c('Peoria'))

Peoria_3_Excess=data.frame(County= c('Peoria', 'Sangamon', 'Smith', 'Shawnee', 'Lackawanna', 'Ector', 'Alachua', 'Pueblo',
                              'Vanderburgh', 'Douglas', 'St. Joseph', 'Weld', 'Brazos', 'Lafayette', 'Guilford', 'Rutherford'),
                    PM25_AVG= c(Peo_avg, San_avg, Sm_avg, Shw_avg, Lac_avg, Ect_avg, Ala_avg, Pue_avg, Van_avg, Dog_avg, Stj_avg,
                                We_avg, Bra_avg, Laf_avg, Gui_avg, Rut_avg),
                    Mortality_Rate = c(.89, 1.87, 2.04, .76, 5.81, 1.42, .26, 2.88, .72, .5, 1.18, 2.53, .95, .83, 1.63, .83),
                    Comparison = c('Peoria'))

#GGPlots
#Cook Covid/Pm counties Comparison plot Sim Score Knn
ggplot(data = cook_Knn, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = cook_Knn, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), size = 5) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  geom_text(aes(label=County),hjust=0, vjust=0) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for counties comparable to Cook (KNN=7) ") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )
#Cook Covid/Pm counties Comparison plot Sim Score Knn Excess
ggplot(data = cook_Knn_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = cook_Knn_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs  Excess Mortality rate for Cook (KNN=7)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#Cook Covid/Pm counties Comparison plot Sim Score < 2.5
ggplot(data = cook_2.5, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = cook_2.5, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for counties comparable to Cook (Similarity Score < 2.5)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = cook_2.5_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = cook_2.5_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Excess Mortality rate for Cook (Similarity Score < 2.5)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#Cook Covid/Pm counties Comparison plot Sim Score <3
ggplot(data = cook_3, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = cook_3, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), size = 5) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for counties comparable to Cook (Similarity Score < 3)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = cook_3_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = cook_3_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Excess Mortality rate for counties comparable to Cook (Similarity Score < 3)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#Kane Covid/Pm counties Comparison plot Sim Score KNN
ggplot(data = Kane_Knn, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Kane_Knn, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 17, size = 5) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  geom_text(aes(label=County),hjust=0, vjust=0) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  ggtitle("Pm vs Mortality rate for counties comparable to Kane (KNN = 7)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )


ggplot(data = Kane_Knn_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Kane_Knn_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 2, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  ggtitle("Pm vs  Excess Mortality rate for counties comparable to Kane (KNN = 7)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )


#Kane Covid/Pm counties Comparison plot Sim Score <2.5
ggplot(data = Kane_2.5, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Kane_2.5, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 2, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  ggtitle("Pm vs Mortality rate for counties comparable to Kane (Similarity Score < 2.5)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = Kane_2.5_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Kane_2.5, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 2, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  ggtitle("Pm vs Excess Mortality rate for  Kane (Similarity Score < 2.5)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#Kane Covid/Pm counties Comparison plot Sim Score < 3.0
ggplot(data = Kane_3, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Kane_3, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 2, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  ggtitle("Pm vs Mortality rate for counties comparable to Kane (Similarity Score < 3)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = Kane_3_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Kane_3_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 2, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  ggtitle("Pm vs Excess Mortality rate for Kane (Similarity Score < 3)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#Peoria COVID/PM counties comparison plot Sim Score KNN
ggplot(data = Peoria_Knn, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Peoria_Knn, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 18, size = 5) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  geom_text(aes(label=County),hjust=0, vjust=0) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for counties comparable to Peoria and Sangamon (KNN = 7)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = Peoria_Knn_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Peoria_Knn_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 22, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  geom_text(aes(label=County),hjust=0, vjust=0)
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs  Excess Mortality rate for Peoria and Sangamon (KNN = 7)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#Peoria COVID/PM counties comparison plot Sim Score < 2.5
ggplot(data = Peoria_2.5, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Peoria_2.5, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 0, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for counties comparable to Peoria and Sangamon (Similarity Score < 2.5)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = Peoria_2.5_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Peoria_2.5_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 0, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Excess Mortality rate for Peoria and Sangamon (Similarity < 2.5)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#Peoria COVID/PM counties comparison plot Sim Score < 3
ggplot(data = Peoria_3, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Peoria_3, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 0, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for counties comparable to Peoria and Sangamon (Similarity Score < 3)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = Peoria_3_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = Peoria_3_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County)), shape = 0, size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Excess Mortality rate for Peoria and Sangamon (Similarity Score < 3)") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#Combining all counties into one dataframe
all_Knn = rbind(cook_Knn, Kane_Knn, Peoria_Knn)
all_2.5 = rbind(cook_2.5, Kane_2.5, Peoria_2.5)
all_3 = rbind(cook_3, Kane_3, Peoria_3)

all_Knn_Excess = rbind(cook_Knn_Excess, Kane_Knn_Excess, Peoria_Knn_Excess)
all_2.5_Excess = rbind(cook_2.5_Excess, Kane_2.5_Excess, Peoria_2.5_Excess)
all_3_Excess = rbind(cook_3_Excess, Kane_3_Excess, Peoria_3_Excess)


#GGPlot for all counties with KNN
ggplot(data = all_Knn, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = all_Knn, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County), shape = factor(Comparison)), size = 5) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for all counties ") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' ) +
  theme_classic()

ggplot(data = all_Knn_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = all_Knn_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County), shape = factor(Comparison)), size = 3) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Excess Mortality rate for all counties ") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

#GGPlot for all counties Sim Score < 2.5
ggplot(data = all_2.5, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = all_2.5, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County), shape = factor(Comparison)), size = 3) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for all counties ") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = all_2.5_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = all_2.5_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County), shape = factor(Comparison)), size = 3) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Excess Mortality rate for all counties ") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )
 
#GGPlot for all counties Sim Score < 3 
ggplot(data = all_3, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = all_3, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County), shape = factor(Comparison)), size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Mortality rate for all counties ") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )

ggplot(data = all_3_Excess, aes(x=PM25_AVG, y = Mortality_Rate)) + 
  geom_point(data = all_3_Excess, mapping= aes(x=PM25_AVG, y = Mortality_Rate, color = factor(County), shape = factor(Comparison)), size = 4) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("Pm vs Excess Mortality rate for all counties ") +
  labs(y= 'Mortality Rate (%)', x = '2000-2016 pm2.5 average(ug/m3)' )






