#
# Program: groupproject_hourlydemand.R
#
# Purpose: Show one week(hourly demand) in March 2015 only
# Description: Find the daily seasonality
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 18 February 2019
# Set locale to English language and preparation work
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
options(digits=3)
setwd("C:/Users/Zheng Chaoyang/Desktop/forecastingproject")
aaa <- read.csv("data/hrl_load_metered.csv")

#pdf("Show one week(hourly demand) in March 2015 only.pdf")


# Data processing
DF <- data.frame(datetime = aaa$datetime_beginning_utc, MW = aaa$mw)
DF$datetime <- as.POSIXct(DF$datetime, format = "%m/%d/%Y %I:%M:%S %p")
DF$datetime <- as.numeric(DF$datetime)
demand_hourly<- as.timeSeries(DF$MW,DF$datetime,format ="%m/%d/%Y %H")
#demand_hourly

# Plot
# Show one week(daily demand) in March 2015 only



plot(window(demand_hourly,start=timeDate("2015-03-01 00",format="%Y-%m-%d %H"),
            end=timeDate("2015-03-07 23",format="%Y-%m-%d %H")),ylab="PJM hourly demand demand (in MW)",
     xlab="Sunday 2015-03-01 to Saturday 2015-03-07",main = "Show one week(hourly demand) in March 2015 only")

dev.off(dev.cur())








