#
# Program: groupproject_naive.R
#
# Purpose: Computing some naive forecasting methods.
#          An evaluation is also provided. Finally, we
#          also plot and compare the result of each naive method.
#         
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 21 February 2019
#        
# ------------------------------------------------------
#
# Set locale to English language and preparation work
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(dplyr)
library(zoo)
options(digits=3)
setwd("C:/Users/Zheng Chaoyang/Desktop/forecastingproject")

# output file for graphs
# pdf("groupproject_naive.pdf")

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

# Aggregate to daily demand
PJM_hourlydemand_ready$daytimes_ready <- as.Date(PJM_hourlydemand_ready$daytimes_ready, format ="%m/%d/%Y")
demand_daily<- aggregate(demand_ready ~ daytimes_ready, PJM_hourlydemand_ready, 
                                 FUN = sum)


## Mesure the performance of each naive methods generally
# Computing (1) no-change model forecast
naive1    <- demand_daily[[2]][-length(demand_daily[[2]])]
observed1 <- demand_daily[[2]][-1]

# Computing bias and MAPE
bias1 <- mean(naive1-observed1)
pbias1 <- mean((naive1-observed1)/observed1)*100
mape1 <- mean(abs((naive1-observed1)/observed1))*100

# Remove 29 February
demand_daily2 <- list()
demand_daily2[[1]] <-
  demand_daily[[1]][substr(as.character(demand_daily[[1]]),6,10)!="02-29"]
demand_daily2[[2]] <- 
  demand_daily[[2]][substr(as.character(demand_daily[[1]]),6,10)!="02-29"]

a<- window(demand_daily2, start="2015-05-01",end = "2015-05-31")
#write.table(demand_daily2)

# Computing (2) no-change yearly seasonal model forecast
# (We thus have no forecast for the first year of observed data.)
naive2    <- demand_daily2[[2]][-
                               ((length(demand_daily2[[2]])-364):length(demand_daily2[[2]]))]
observed2 <- demand_daily2[[2]][-(1:365)]

# Computing bias and MAPE
bias2 <- mean(naive2-observed2)
pbias2 <- mean((naive2-observed2)/observed2)*100
mape2 <- mean(abs((naive2-observed2)/observed2))*100


# Comparing bias and MAPE of (1) over the same period
bias1.a <- mean(naive1[-(1:364)]-observed1[-(1:364)])
pbias1.a <- mean(
  (naive1[-(1:364)]-observed1[-(1:364)])/observed1[-(1:364)])*100
mape1.a <- mean(abs((naive1[-(1:364)]-observed1[-(1:364)])/
                      observed1[-(1:364)]))*100

# Computing (3) rolling three-day window
daily.ts <- timeSeries(demand_daily[[2]],demand_daily[[1]])
naive3 <- rollMean(daily.ts,3,align="right",na.pad=T)[-(1:2)]
naive3 <- naive3[-length(naive3)]
observed3 <- demand_daily[[2]][-(1:3)]

# Computing bias and MAPE
bias3 <- mean(naive3-observed3)
pbias3 <- mean((naive3-observed3)/observed3)*100
mape3 <- mean(abs((naive3-observed3)/observed3))*100

cat("Results for 2005-2018\n")
cat("         no-change model: bias=",bias1.a,
    "%bias=",pbias1.a,"mape=",mape1.a,"\n")
cat("seasonal no-change model: bias=",bias2,
    "%bias=",pbias2,"mape=",mape2,"\n")
cat("rolling three-day window: bias=",bias3,
    "%bias=",pbias3,"mape=",mape3,"\n\n")


# Plotting observed and three different forecasts for may 2015

obs  <- window(demand_dailyplot,start="2015-05-01",end = "2015-05-31")

for1 <- window(timeSeries(naive1,demand_daily[[1]][-1]),
               start="2015-05-01",end = "2015-05-31")
for2 <- window(timeSeries(naive2,demand_daily2[[1]][-(1:365)]),
               start="2015-05-01",end = "2015-05-31")
for3 <- window(timeSeries(naive3,demand_daily[[1]][-(1:3)]),
               start="2015-05-01",end = "2015-05-31")

ybounds <- c(min(obs,for1,for2,for3),max(obs,for1,for2,for3))

# obs & for1
plot(obs,ylim=ybounds,ylab="Daily demand in PJM (in MW/h)")
lines(for1,col="blue")
legend("topright",
       c("observed","no-change forecast"),
       col=c("black","blue"),lty=1)

# obs & for2
plot(obs,ylim=ybounds,ylab="Daily demand in PJM (in MW/h)")
lines(for2,col="orange")
legend("topleft",
       c("observed","yearly seasonal no-change forecast"),
       col=c("black","orange"),lty=1)

# obs & for3
plot(obs,ylim=ybounds,ylab="Daily demand in Ontario (in MW/h)")
lines(for3,col="red")
legend("topright",
       c("observed","rolling 3-day forecast"),
       col=c("black","red"),lty=1)

# All three
plot(obs,ylim=ybounds,ylab="Daily demand in PJM (in MW/h)")
lines(for1,col="blue")
lines(for2,col="orange")
lines(for3,col="red")
legend("top",
       c("observed","no-change forecast",
         "seasonal no-change forecast","rolling 3-day forecast"),
       col=c("black","blue","orange","red"),lty=1)


# Compute the performance of 3 naive methods in the validation dataset(2014.1.1-2016.12.31)  
n1 <- length(naive1)
y2014_2016 <- (n1-5*364):(n1-2*364)
bias1.1 <- mean(naive1[y2014_2016]-observed1[y2014_2016])
pbias1.1 <- mean((naive1[y2014_2016]-observed1[y2014_2016])/observed1[y2014_2016])*100
mape1.1 <- mean(abs((naive1[y2014_2016]-observed1[y2014_2016])/observed1[y2014_2016]))*100

n2 <- length(naive2)
y2014_2016 <- (n2-5*364):(n2-2*364)
bias2.1 <- mean(naive2[y2014_2016]-observed2[y2014_2016])
pbias2.1 <- mean((naive2[y2014_2016]-observed2[y2014_2016])/observed2[y2014_2016])*100
mape2.1 <- mean(abs((naive2[y2014_2016]-observed2[y2014_2016])/observed2[y2014_2016]))*100

n3 <- length(naive3)
y2014_2016 <- (n3-5*364):(n3-2*364)
bias3.1 <- mean(naive3[y2014_2016]-observed3[y2014_2016])
pbias3.1 <- mean((naive3[y2014_2016]-observed3[y2014_2016])/observed3[y2014_2016])*100
mape3.1 <- mean(abs((naive3[y2014_2016]-observed3[y2014_2016])/observed3[y2014_2016]))*100

cat("Performance of 3 naive methods in the validation dataset(2014.1.1-2016.12.31)\n")
cat("         no-change model: bias=",bias1.1,
    "%bias=",pbias1.1,"mape=",mape1.1,"\n")
cat("seasonal no-change model: bias=",bias2.1,
    "%bias=",pbias2.1,"mape=",mape2.1,"\n")
cat("rolling three-day window: bias=",bias3.1,
    "%bias=",pbias3.1,"mape=",mape3.1,"\n")


# For data visiualization work in validation dataset  .
demand_dailyplot <- timeSeries(demand_daily$demand_ready,demand_daily$daytimes_ready,
                               format="%Y-%m-%d") #To convert the class of daily demand data from data frame to timeseries

# Plotting observed and three different forecasts for 
# 2014-1-1 to 2016-12-31.
obs  <- window(demand_dailyplot,start="2014-01-01",end = "2016-12-31")

for1 <- window(timeSeries(naive1,demand_daily[[1]][-1]),
               start="2014-01-01",end = "2016-12-31")
for2 <- window(timeSeries(naive2,demand_daily2[[1]][-(1:365)]),
               start="2014-01-01",end = "2016-12-31")
for3 <- window(timeSeries(naive3,demand_daily[[1]][-(1:3)]),
               start="2014-01-01",end = "2016-12-31")

ybounds <- c(min(obs,for1,for2,for3),max(obs,for1,for2,for3))

# obs & for1
plot(obs,ylim=ybounds,ylab="Daily demand in PJM (in MW/h)")
lines(for1,col="blue")
legend("topright",
       c("observed","no-change forecast"),
       col=c("black","blue"),lty=1)

# obs & for2
plot(obs,ylim=ybounds,ylab="Daily demand in PJM (in MW/h)")
lines(for2,col="orange")
legend("topleft",
       c("observed","yearly seasonal no-change forecast"),
       col=c("black","orange"),lty=1)

# obs & for3
plot(obs,ylim=ybounds,ylab="Daily demand in Ontario (in MW/h)")
lines(for3,col="red")
legend("topright",
       c("observed","rolling 3-day forecast"),
       col=c("black","red"),lty=1)

# All three
plot(obs,ylim=ybounds,ylab="Daily demand in PJM (in MW/h)")
lines(for1,col="blue")
lines(for2,col="orange")
lines(for3,col="red")
legend("top",
       c("observed","no-change forecast",
         "seasonal no-change forecast","rolling 3-day forecast"),
       col=c("black","blue","orange","red"),lty=1)

#dev.off(dev.cur())




