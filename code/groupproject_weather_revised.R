#
# Program: groupproject_weather_revised.R
#
# Purpose: Processing and ploting weather data.
#
# Description: Compare the TAVG trend in teterboro and sussex IN 2015.
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 18 February 2019
#        
# ------------------------------------------------------
#
# Set locale to English language and preparation work
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(rgr)
options(digits=3)
setwd("C:/Users/Zheng Chaoyang/Desktop/forecastingproject")

pdf("Compare the trend of average temperature 
    in teterboro and sussex from 2005 to 2018.pdf")


# Loading weather dataset
teterboro_weather_origin <- read.csv("data/weather/Teterboro Airport.csv")
sussex_weather_origin <- read.csv("data/weather/weather_data_sussex.csv")

# Delecting na values
teterboro_weather_origin$TAVG<-na.omit(teterboro_weather_origin$TAVG)
sussex_weather_origin$TAVG<-na.omit(sussex_weather_origin$TAVG)

# Converting to time series data
DF <- data.frame(datetime = teterboro_weather_origin$DATE, TAVG = teterboro_weather_origin$TAVG)
teterboro_TAVG_daily<- timeSeries(DF$TAVG,as.Date(DF$datetime),
                               format="%Y-%m-%d")
DF2 <- data.frame(datetime = sussex_weather_origin$DATE, TAVG = sussex_weather_origin$TAVG)
sussex_TAVG_daily<- timeSeries(DF2$TAVG,as.Date(DF2$datetime),
                               format="%Y-%m-%d")
# plot 
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

dev.off(dev.cur())




