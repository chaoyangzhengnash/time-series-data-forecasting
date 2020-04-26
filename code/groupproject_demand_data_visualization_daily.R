#
# Program: groupproject_visualization_revised.R
#
# Purpose: To figure out the pattern of PJM daily demand data (exploratpry analysis)
#
# Description: After soloving the sunlight saving issues,
#              We aggregate our hourly data for PJM
#              demand to obtain a daily demand.  We then
#              visualize these data to do exploratpry analysis
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
library(zoo)
options(digits=3)
setwd("C:/Users/Zheng Chaoyang/Desktop/forecastingproject")

# output file for graphs
# pdf("groupproject_demand_data_visualization_daily.pdf")

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
demand_daily_origin <- aggregate(demand_ready ~ daytimes_ready, PJM_hourlydemand_ready, 
                                 FUN = sum)
demand_daily <- timeSeries(demand_daily_origin$demand_ready,demand_daily_origin$daytimes_ready,
                           format="%Y-%m-%d") #To convert the class of daily demand data from data frame to timeseries



# Visualization 
# Plot all 
plot(demand_daily,xlab=" ", ylab="Daily demand(in MW/D)",type="l",main ="PJM daily demand from 2005.1.1- 2018.12.31")


#dev.off(dev.cur())

