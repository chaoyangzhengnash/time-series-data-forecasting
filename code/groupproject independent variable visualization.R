#
# Program: groupproject independent variable visualization.R
#
# Purpose: To figure out the relationship between wather data and PJM daily electricity demand data 
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 21 February 2019
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
demand_daily <- timeSeries(demand_daily_origin$demand_ready,demand_daily_origin$daytimes_ready,
                           format="%Y-%m-%d") #To convert the class of daily demand data from data frame to timeseries

## loading daily Temperature data

teterboro_weather_origin <- read.csv("data/weather/Teterboro Airport.csv")
sussex_weather_origin <- read.csv("data/weather/weather_data_sussex.csv")

# Delecting na values
#teterboro_weather_origin$TAVG<-na.omit(teterboro_weather_origin$TAVG)
#sussex_weather_origin$TAVG<-na.omit(sussex_weather_origin$TAVG)

# Replacing NA data by copying the data of yesterday
o <- na.locf(teterboro_weather_origin$TAVG)
p<- na.locf(sussex_weather_origin$TAVG)

teterboro_weather_nomissing <- data.frame(date_ready = teterboro_weather_origin$DATE,TVAG_ready = o) 
sussex_weather_nomissing <- data.frame(date_ready = sussex_weather_origin$DATE,TVAG_ready = p) 


# Converting to time series data
teterboro_TAVG_daily<- timeSeries(teterboro_weather_nomissing$TVAG_ready,as.Date(teterboro_weather_nomissing$date_ready),
                             format="%Y-%m-%d")
sussex_TAVG_daily<- timeSeries(sussex_weather_nomissing$TVAG_ready,as.Date(sussex_weather_nomissing$date_ready),
                             format="%Y-%m-%d")


# plot TAVG
teterboro_plot <- series(window(teterboro_TAVG_daily,
                                start=timeDate("2005-01-01",format="%Y-%m-%d"),
                                end=timeDate("2018-12-31",format="%Y-%m-%d")))
sussex_plot <- series(window(sussex_TAVG_daily,
                             start=timeDate("2005-01-01",format="%Y-%m-%d"),
                             end=timeDate("2018-12-31",format="%Y-%m-%d")))
plot(main = "Trend of average temperature in teterboro and sussex from 2005 to 2018",teterboro_plot,axes=F,
     lty=1,type="l",
     ylab="Average temperature(In Fahrenheit)",xlab="")
lines(sussex_plot,lty=3,lwd=1.8)
axis(1,at=seq(1,365*15,by=365),
     labels=c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
legend(100, 20,legend=c("Teterboro 2015","Sussex 2015"),lty=c(1,3))

# To explore if there is a temperature effect on the demand
# We show daily demand versus daily daily temperature.
T_TAVG_plot <- series(window(teterboro_TAVG_daily,
                                start=timeDate("2014-01-01",format="%Y-%m-%d"),
                                end=timeDate("2016-12-31",format="%Y-%m-%d")))


E_demand_plot <- series(window(demand_daily,
                        start=timeDate("2014-01-01",format="%Y-%m-%d"),
                        end=timeDate("2016-12-29",format="%Y-%m-%d")))

plot(main = "PJM:daily demand versus  daily temperature from 2014 to 2016",T_TAVG_plot,E_demand_plot,
     ylab="daily electricity demand in PJM (MW/day)",
     xlab="Average Daily Temperature (Fahrenheit)",col=4:6,pch=23)

legend(40,6000,col=4:6,legend=2014:2016,pch=23)

# Ploting humidity data
ccc <- read.csv("data/weather/Humidity_Teterboro.csv")

ccc$Date <-as.Date(ccc$Date, format="%Y/%m/%d")

humidity_daily <- aggregate(RHx ~ Date, ccc, 
                                 FUN = mean)

humidity_daily_ts <- as.timeSeries(humidity_daily$RHx,humidity_daily$Date)

E_2demand_plot <- series(window(demand_daily,
                               start=timeDate("2014-01-01",format="%Y-%m-%d"),
                               end=timeDate("2016-12-31",format="%Y-%m-%d")))

humidity_plot <- series(window(humidity_daily_ts,
                                start=timeDate("2014-01-01",format="%Y-%m-%d"),
                                end=timeDate("2016-12-31",format="%Y-%m-%d")))
plot(main = "PJM:Daily average humidity versusdaily electricity demand from 2014 to 2016",humidity_plot,E_2demand_plot,
     ylab="daily electricity demand in PJM (MW/day)",
     xlab="Daily average relative humidity",col=4:6,pch=23)
legend(200,6000,col=4:6,legend=2014:2016,pch=23)


# Ploting windchill data
bbb <- read.csv("data/weather/Windchill.csv")
windchill_ts <- as.timeSeries(bbb$Windchill,as.Date(bbb$DATE, format="%Y/%m/%d"))
windchill_plot <- series(window(windchill_ts,
                                start=timeDate("2014-01-01",format="%Y-%m-%d"),
                                end=timeDate("2016-12-31",format="%Y-%m-%d")))

plot(main = "PJM:Wind chill versusdaily electricity demand from 2014 to 2016",windchill_plot,E_demand_plot,
     ylab="daily electricity demand in PJM (MW/day)",
     xlab="Windchill",col=4:6,pch=23)
legend(40,6000,col=4:6,legend=2014:2016,pch=23)

# Plotting HDD and CDD
to.dem.daily <- data.frame(date = Full.dataset$date_ready, Demand = Full.dataset$demand_ready)
to.wea.date <- Full.dataset$date_ready
to.wea.HDD  <- as.numeric(Full.dataset$Hdd)
to.wea.CDD  <- as.numeric(Full.dataset$Cdd)
HDDt <-  window(timeSeries(to.wea.HDD,to.wea.date,format="%Y-%m-%d"), start="2005-01-01",end = "2010-12-31")
CDDt <-  window(timeSeries(to.wea.CDD,to.wea.date,format="%Y-%m-%d"), start="2005-01-01",end = "2010-12-31")

HDD_plot <- series(HDDt)
Yt <- window(timeSeries(to.dem.daily[[2]],to.dem.daily[[1]],
                        format="%Y-%m-%d"), start="2005-01-01",end = "2010-12-31")
plot(main = "",HDD_plot,Yt,
     ylab="daily electricity demand in PJM (MW/day)",
     xlab="HDD",col=4:9,pch=23)
legend(20,8000,col=4:9,legend=2005:2010,pch=23)





#dev.off(dev.cur())