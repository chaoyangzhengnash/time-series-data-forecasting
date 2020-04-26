#
# Program: groupproject_visualization.R
#
# Purpose: To figure out the pattern of PJM daily demand data (exploratpry analysis)
#
# Description: We aggregate our hourly data for PJM
#              demand to obtain a daily demand.  We then
#              visualize these data to do exploratpry analysis
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 16 February 2019
#        
# ------------------------------------------------------.
#
# Set locale to English language and preparation work
#install.packages("tsoutliers")
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(tsoutliers)
options(digits=3)
setwd("C:/Users/Zheng Chaoyang/Desktop/forecastingproject")

# output file for graphs
# pdf("groupproject_visualization.pdf")

# Aggregate our hourly data for PJM electricity hourly demand to obtain a daily demand
aaa <- read.csv("data/hrl_load_metered.csv")
aaa$datetime_beginning_utc <- as.Date(aaa$datetime_beginning_utc, "%m/%d/%Y")
demand_daily_origin <- aggregate(mw ~ datetime_beginning_utc, aaa, 
                          FUN = sum)
demand_daily <- timeSeries(demand_daily_origin$mw,demand_daily_origin$datetime_beginning_utc,
                           format="%Y-%m-%d") #To convert the class of daily demand data from data frame to timeseries

## Visualization 
# Plot all 

plot(demand_daily,xlab=" ", ylab="Daily demand(in MW/h)",type="l",main ="From 2005.1.1- 2018.12.31")

# Apply stl function, not finished, I wll try to fix it latter

#demand_daily_ts <- ts(demand_daily,frequency = 365,start=c(2005-01-01))
#stl(demand_daily_ts,"periodic")
#demand_daily_ts_2 <- ts(demand_daily, frequency=365, start=c(2005-01-01))
#plot(stl(demand_daily_ts_,"periodic"))
#ddd <- as.ts(window(demand_daily,start=timeDate("2005-01-01",format="%Y-%m-%d"),
#       end=timeDate("2005-12-31",format="%Y-%m-%d")),frequency = 8)
#stl(ddd,"periodic")
#plot(stl(demand_daily_ts_2,"periodic"),main="stl function")



# Show 2005 only
plot(main = "Show 2005 only", window(demand_daily,start=timeDate("2005-01-01",format="%Y-%m-%d"),
            end=timeDate("2005-12-31",format="%Y-%m-%d")),
     ylab="PJM daily demand (in MW)",
     xlab="2005-01-01 to 2005-12-31", at="pretty",main = "Show 2005 only")

# Show 2010 only
plot(window(demand_daily,start=timeDate("2010-01-01",format="%Y-%m-%d"),
            end=timeDate("2010-12-31",format="%Y-%m-%d")),
     ylab="PJM daily demand (in MW)",
     xlab="2010-01-01 to 2010-12-31", at="pretty",main = "Show 2010 only")

# Show 2015 only
plot(window(demand_daily,start=timeDate("2015-01-01",format="%Y-%m-%d"),
            end=timeDate("2015-12-31",format="%Y-%m-%d")),
     ylab="PJM daily demand (in MW)",
     xlab="2015-01-01 to 2015-12-31", at="pretty",main = "Show 2015 only")

# Show 2018 only
plot(window(demand_daily,start=timeDate("2018-01-01",format="%Y-%m-%d"),
            end=timeDate("2018-12-31",format="%Y-%m-%d")),
     ylab="PJM daily demand (in MW)",
     xlab="2018-01-01 to 2018-12-31", at="pretty",main = "Show 2018 only")

# FInd the outliers:try to see what happened in 2011, 2011. We dont have data for 2019

# Show 2011 only
plot(window(demand_daily,start=timeDate("2011-01-01",format="%Y-%m-%d"),
            end=timeDate("2011-12-31",format="%Y-%m-%d")),
     ylab="PJM daily demand (in MW)",
     xlab="2011-01-01 to 2011-12-31", at="pretty",main = "Show 2011 only")

# Show Oct &Nov 2011 only
plot(window(demand_daily,start=timeDate("2011-10-01",format="%Y-%m-%d"),
            end=timeDate("2011-11-30",format="%Y-%m-%d")),
     ylab="PJM daily demand demand (in MW)",
     xlab="Sunday 2011-10-01 to Saturday 2011-11-30",
     at="pretty",main = " The dip approxiametly
     between 2011-10-25 and 2011-11-5 ")


# Show 2012 only
plot(window(demand_daily,start=timeDate("2012-01-01",format="%Y-%m-%d"),
            end=timeDate("2012-12-31",format="%Y-%m-%d")),
     ylab="PJM daily demand (in MW)",
     xlab="2012-01-01 to 2012-12-31", at="pretty",main = "Show 2012 only")

# Show Oct &Nov 2012 only
plot(window(demand_daily,start=timeDate("2012-10-01",format="%Y-%m-%d"),
            end=timeDate("2012-11-30",format="%Y-%m-%d")),
     ylab="PJM daily demand demand (in MW)",
     xlab="Sunday 2012-10-01 to Saturday 2012-11-30",
     at="pretty",main = " The dip approxiametly between 2012-11-28 and 2012-12-13 ")


# Show one week in January 2010 only
plot(window(demand_daily,start=timeDate("2010-01-03",format="%Y-%m-%d"),
            end=timeDate("2010-01-10",format="%Y-%m-%d")),
     ylab="PJM daily demand demand (in MW)",
     xlab="Sunday 2010-01-03 to Saturday 2010-01-09",
     at="pretty",main = "Show one week in January 2010 only")

# Show two different weeks in January 2010
week1 <- series(window(demand_daily,
                       start=timeDate("2010-01-03",format="%Y-%m-%d"),
                       end=timeDate("2010-01-09",format="%Y-%m-%d")))
week2 <- series(window(demand_daily,
                       start=timeDate("2010-01-10",format="%Y-%m-%d"),
                       end=timeDate("2010-01-16",format="%Y-%m-%d")))
plot(main = "Show two different weeks in January 2010",week1,axes=F,
     lty=1,type="l",ylim=c(min(week1,week2),max(week1,week2)),
     ylab="PJM daily demand (in MW)",xlab="")
lines(week2,lty=3,lwd=1.8)
axis(1,at=seq(1,7,by=1),
     labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
legend(1, 3601,legend=c("Jan 3-9, 2010","Jan 10-16, 2010"),lty=c(1,3))


# Show one week in January 2010 and one week in July 2010
week3 <- series(window(demand_daily,
                       start=timeDate("2010-07-18",format="%Y-%m-%d"),
                       end=timeDate("2010-07-24",format="%Y-%m-%d")))
plot(main = "Show one week in January 2010 and one week in July 2010",week1,axes=F,
     lty=1,type="l",ylim=c(min(week1,week3),max(week1,week3)),
     ylab="PJM daily demand (in MW)",xlab="")
lines(week3,lty=3,lwd=1.8)
axis(1,at=seq(1,7,by=1),
     labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
legend(1, 5299,legend=c("Jan 3-9, 2010","Jul 18-24, 2010"),lty=c(1,3))


# Show the Same month in differernt years to see if there is a trend
month2005 <- series(window(demand_daily,
                       start=timeDate("2005-01-01",format="%Y-%m-%d"),
                       end=timeDate("2005-01-31",format="%Y-%m-%d")))
month2006 <- series(window(demand_daily,
                       start=timeDate("2006-01-01",format="%Y-%m-%d"),
                       end=timeDate("2006-01-31",format="%Y-%m-%d")))

plot(main = "Show the Same month in differernt years",month2005,axes=F,
     lty=1,type="l",ylim=c(min(month2005,month2006),max(month2005,month2006)),
     ylab="PJM daily demand (in MW)",xlab="")
lines(month2006,lty=3,lwd=1.8)
axis(1,at=seq(1,31,by=5),
     labels=c("Jan 1","Jan 6","Jan 11","Jan 16","Jan 21","Jan 26","Jan31"))
legend(10, 2750,legend=c("Jan 2005","Jan 2006"),lty=c(1,3))

# First week in March, hourly 2010
install.packages("lubridate")
library(lubridate)


bbb <- read.csv("data/hrl_load_metered.csv")



bbb$datetime_beginning_utc <-parse_date_time(bbb$datetime_beginning_utc, '%Y:%m:%d:%I:%M:%S %p')

bbb$datetime_beginning_utc <- as.Date(bbb$datetime_beginning_utc, "%Y:%m:%d:%H:%I:%M:%S:%P")



demand_hourly_ts <- timeSeries(demand_hourly$mw,demand_hourly$datetime_beginning_utc,
                           format="%Y-%m-%d") #To convert the class of daily demand data from data frame to timeseries



# Find those outliers 
# tso(demand_daily)


dev.off(dev.cur())


