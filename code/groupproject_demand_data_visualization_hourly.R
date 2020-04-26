#
# Program: groupproject_demanddata_visualization_hourly.R
#
# Purpose: To figure out the pattern of PJM hourly demand data (exploratpry analysis)
#
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 21 February 2019
#        
# ------------------------------------------------------.
#
# Set locale to English language and preparation work
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(dplyr)
#library(lubridate)
library(zoo)
options(digits=3)
setwd("C:/Users/Zheng Chaoyang/Desktop/forecastingproject")

# output file for graphs
# pdf("groupproject_visualization_revised.pdf")

# Data loading
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

demand_hourly <- timeSeries(PJM_hourlydemand_ready$demand_ready,PJM_hourlydemand_ready$daytimes_ready,
                            
                            format="%Y-%m-%d-%H") #To convert the class of daily demand data from data frame to timeseries

# Graph hourly demand for 4 weeks in 2015 (2nd week in January (winter)), 
# last week in April (spring),last week of July (summer), 
# last week of October (fall))

week1 <- series(window(demand_hourly,
                       start=timeDate("2015-01-11-00",format="%Y-%m-%d"),
                       end=timeDate("2015-01-25-00",format="%Y-%m-%d")))
week2 <- series(window(demand_hourly,
                       start=timeDate("2015-04-19-00",format="%Y-%m-%d"),
                       end=timeDate("2015-05-04-00",format="%Y-%m-%d")))
week3 <- series(window(demand_hourly,
                       start=timeDate("2015-07-19-00",format="%Y-%m-%d"),
                       end=timeDate("2015-07-25-23",format="%Y-%m-%d")))
week4 <- series(window(demand_hourly,
                       start=timeDate("2010-10-25-00",format="%Y-%m-%d"),
                       end=timeDate("2010-10-31-23",format="%Y-%m-%d")))
plot(main = "Graph 4 weeks in 2015",week2,axes=F,
     lty=1,type="l",ylim=c(min(week1,week2,week3,week4),max(week1,week2,week3,week4)),
     ylab="PJM hourly demand (in MW/hour)",xlab="")
lines(week2,lty=26,lwd=5)
lines(week3,lty=11,lwd=2)
lines(week4,lty=9,lwd=3)

axis(1,at=seq(1,7*24,by=24),
     labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
legend(90, 400,legend=c("2nd week in January", "last week in April","last week of July", 
                        "last week of October"),lty=c(1,26,11,9))

