#
# Program: groupproject_data cleaning and preprocessing.R
#
# Purpose: 
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 16 March 2019
#        
# ------------------------------------------------------.
#
# Set locale to English language and preparation work
#install.packages("tsoutliers")
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
#library(tsoutliers)
library(dplyr)
library(lubridate)
library(zoo)
library("readxl")
options(digits=3)
setwd("C:/Users/Zheng Chaoyang/Desktop/forecastingproject")

# output file for graphs
# pdf("Independent variable visualization")

## loading daily demand data
aaa <- read.csv("data/hrl_load_metered.csv")
DF <- data.frame(datetime = aaa$datetime_beginning_ept, MW = aaa$mw)
DF2 <- distinct(DF,datetime, .keep_all = TRUE)
DF2$datetime <- as.POSIXct(DF2$datetime,format = "%m/%d/%Y %I:%M:%S %p",tz="GMT")

# Treating sunlight saving
ts <- seq.POSIXt(as.POSIXct("1/1/2005 0:00:00", tz="GMT",format = "%m/%d/%Y %H:%M:%S"), as.POSIXct("12/31/2018 23:00:00", tz="GMT",format = "%m/%d/%Y %H:%M:%S"), by="hour")
df_full <- data.frame(datetime=ts)
df_with_missing_times <- full_join(df_full,DF2)
mw_nomissing <- na.locf(df_with_missing_times$MW)
PJM_hourlydemand_ready <- data.frame(daytimes_ready = df_with_missing_times$datetime,demand_ready = mw_nomissing) 

# Aggregate to daily demand
PJM_hourlydemand_ready$daytimes_ready <- as.Date(PJM_hourlydemand_ready$daytimes_ready, format ="%m/%d/%Y")
demand_daily_origin <- aggregate(demand_ready ~ daytimes_ready, PJM_hourlydemand_ready, 
                                 FUN = sum)


# PJM daily eletricity demand data cleaning

# Treatment of 2011 Dip 
for( i in 2494:2498)
demand_daily_origin$demand_ready[i] <- (demand_daily_origin$demand_ready[i-7]
                                     + demand_daily_origin$demand_ready[i+7])/2
# Treatment of 2012 Dip (Hurricane Sandy)
for( i in 2859:2865)
demand_daily_origin$demand_ready[i] <- (demand_daily_origin$demand_ready[i-7]
                                          + demand_daily_origin$demand_ready[i+7])/2
# Treatment of 2012 Dip (Hurricane Sandy)
for( i in 2859:2865)
  demand_daily_origin$demand_ready[i] <- (demand_daily_origin$demand_ready[i-7]
                                          + demand_daily_origin$demand_ready[i+7])/2

demand_daily.ts <- timeSeries(demand_daily_origin$demand_ready,demand_daily_origin$daytimes_ready,
                           format="%Y-%m-%d") #To convert the class of daily demand data from data frame to timeseries



## loading daily Temperature data
teterboro_weather_origin <- read.csv("data/weather/Teterboro Airport.csv")
teterboro_weather_origin$TAVG <- as.numeric(as.character(teterboro_weather_origin$TAVG ))

# Check the existence of NA value 
#length(teterboro_weather_origin$TAVG[is.na(teterboro_weather_origin$TAVG)==T])

# Replacing NA data by copying the data of yesterday
o <- na.locf(teterboro_weather_origin$TAVG)
teterboro_weather_nomissing <- data.frame(date_ready = teterboro_weather_origin$DATE,TVAG_ready = o) 
# length(teterboro_weather_nomissing[is.na(teterboro_weather_nomissing$TAVG)==T])
teterboro_weather_nomissing$TVAG_ready[1660]  <-80
teterboro_weather_nomissing$TVAG_ready[2501]  <-51
teterboro_weather_nomissing$TVAG_ready[2502]  <-51
teterboro_weather_nomissing$TVAG_ready[3196]  <-67.5
teterboro_weather_nomissing$TVAG_ready[4575]  <-74.5
teterboro_weather_nomissing$TVAG_ready[4576]  <-81.5
teterboro_weather_nomissing$TVAG_ready[4577]  <-82.5
teterboro_weather_nomissing$TVAG_ready[4578]  <-82.5

# Converting to time series data
teterboro_TAVG_ts<- timeSeries(teterboro_weather_nomissing$TVAG_ready,as.Date(teterboro_weather_nomissing$date_ready),
                                  format="%Y-%m-%d")

# Creating HDD and CDD
teterboro_weather_withHDDCDD<-data.frame(date_ready = teterboro_weather_nomissing$date_ready,
                                         TVAG_ready = teterboro_weather_nomissing$TVAG_ready,
                                         Hdd = 50-teterboro_weather_nomissing$TVAG_ready,
                                         Cdd = teterboro_weather_nomissing$TVAG_ready - 65 )
FUN.HDD<-function(Hdd){
  HDD<-max(Hdd,0)
  return(HDD)
}
teterboro_weather_withHDDCDD$Hdd <- apply(teterboro_weather_withHDDCDD[3],1,FUN.HDD)

FUN.CDD<-function(Cdd){
  CDD<-max(Cdd,0)
  return(CDD)
}
teterboro_weather_withHDDCDD$Cdd<- apply(teterboro_weather_withHDDCDD[4],1,FUN.CDD)

#Treating with winchill

bbb <- read.csv("data/weather/Windchill.csv")

# Check the existence of NA value 
#length(bbb$Windchill[is.na(bbb$Windchill)==T])
colnames(bbb)[1] <- "date_ready"

as.numeric(bbb$date_ready)

bbb$date_ready <- as.Date(bbb$date_ready, format = "%Y/%m/%d")
teterboro_weather_withHDDCDD$date_ready <-  as.Date(teterboro_weather_withHDDCDD$date_ready
                                                    , format = "%Y/%m/%d")

teterboro_weather_withHDDCDD_windchill <-left_join(teterboro_weather_withHDDCDD,bbb)

# Treating with humidity data
ccc <- read.csv("data/weather/Humidity_Teterboro.csv")
ccc$Date <-as.Date(ccc$Date, format="%Y/%m/%d")

# Delete the N.A value "999
ccc<-ccc[!(ccc$RHx > 100),]


humidity_daily <- aggregate(RHx ~ Date, ccc, 
                            FUN = mean)

humidity <- data.frame(date_ready =humidity_daily$Date, Relative_humidity = humidity_daily$RHx )

teterboro_weather_withHDDCDD_windchill_RHX <-left_join(teterboro_weather_withHDDCDD_windchill,
                                                       humidity)
# Adding weekday and weekend dummy variables

teterboro_weather_withHDDCDD_windchill_RHX$dayofweek <- substr(weekdays(strptime(teterboro_weather_withHDDCDD_windchill_RHX$date_ready
                                      ,"%Y-%m-%d")),1,3)
teterboro_weather_withHDDCDD_windchill_RHX$weekday <- 
  ifelse(teterboro_weather_withHDDCDD_windchill_RHX$dayofweek == "Sun",0,ifelse(
    teterboro_weather_withHDDCDD_windchill_RHX$dayofweek == "Sat",0,1
  ))
  
teterboro_weather_withHDDCDD_windchill_RHX$weekend <- 
  ifelse(teterboro_weather_withHDDCDD_windchill_RHX$weekday == 1,0,1)

# Adding holiday impactS 

ddd<- read_excel("data/Holiday_datarevised_1.xlsx",skip=1,col_names = TRUE)

ddd$DATE<- as.Date(ddd$DATE)

# Check if holiday present in 2.29

ddd[[2]][substr(as.character(ddd[[1]]),6,10)=="02-29"]
# Out put£º"2008-02-29" "2012-02-29" "2016-02-29"= ¡°0 0 0¡±
# So we can remove 2.29
ddd<-ddd[!(substr(as.character(ddd[[1]]),6,10)=="02-29"),]

ddd.df <- data.frame(date = ddd$DATE, holiday = ddd$Holiday)
# Treating with holiday and moving holdaiy dummy varaibles

#for (i in c(1:length(ddd.df$date))){
#  if(ddd.df$holiday[i] == 1 & ddd.df$holiday[i+365]== 1){
#    ddd.df$fixedholiday[i] = 1
#    }else{ddd.df$fixedholiday[i] = 0}}

colnames(ddd.df)[1] <- "date_ready"

teterboro_weather_withHDDCDD_windchill_RHX<-left_join(teterboro_weather_withHDDCDD_windchill_RHX,
                                                        ddd.df)

# Full dataset
colnames(demand_daily_origin)[1]<- "date_ready"

Full.dataset<- left_join(teterboro_weather_withHDDCDD_windchill_RHX,
                      demand_daily_origin)
# Remove 2.29 for a whole dataset
Full.dataset<-Full.dataset[!(substr(as.character(Full.dataset[[1]]),6,10)=="02-29"),]

class(Full.dataset)
class(Full.dataset$date_ready)

# Recall, here we are tying to fix the problem of the 
# negative sign of "humidity" and "windchill" in the liner regression.

# Creating "really_cold" and "really_hot" variables to replace the "HDD" and "CDD" in this case.
Full.dataset$really_cold <- ifelse(Full.dataset$Hdd >10,Full.dataset$Hdd,0 )
  
Full.dataset$really_hot <- ifelse(Full.dataset$Cdd >10,Full.dataset$Cdd,0 )



# Creating "felt_humilidity" to replace "relative humidity"

for (i in c(1:length(Full.dataset$date_ready))){
  if(Full.dataset$really_hot[i] > 0 ){
    Full.dataset$felt_humilidity[i] = Full.dataset$really_hot[i] * Full.dataset$Relative_humidity[i]
  }else{Full.dataset$felt_humilidity[i] = 0}}

# Calculating the real_Windchill

# Length(teterboro_weather_origin$Average.Daily.Wind.Speed[is.na(teterboro_weather_origin$Average.Daily.Wind.Speed)==T])

teterboro_weather_origin$Average.Daily.Wind.Speed <-na.locf(teterboro_weather_origin$Average.Daily.Wind.Speed )
teterboro_weather_origin$DATE <- date(teterboro_weather_origin$DATE)
teterboro_weather_origin<-teterboro_weather_origin[!(substr(as.character(teterboro_weather_origin$DATE),6,10)=="02-29"),]

Full.dataset$wind_speed <-teterboro_weather_origin$Average.Daily.Wind.Speed
Full.dataset$real_Windchill <- sqrt(Full.dataset$wind_speed) * Full.dataset$really_cold

class(Full.dataset)
class(Full.dataset$date_ready)

#write.csv(Full.dataset, file = "full_dataset.csv" )
