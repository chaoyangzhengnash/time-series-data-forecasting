#
# Program: groupproject_comprehensive model.R
#
# Purpose: 
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 7 April 2019
#        
# ------------------------------------------------------.
#
# Set locale to English language and preparation work
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(forecast)
options(digits=3)
library(zoo)
library(astsa)
library(forecast)
setwd("C:/Users/Zheng Chaoyang/Desktop/part_2/data")

# Deciding the cutting period of validation dataset,
# so that we can compare the different models' performance 
# in the differernt perodids of validation dataset
# Cutting criteria: the residual plot to see the model's 
# fitted condition in the training dataset.
# The code of plotting the residual plot of each model is at the 
# end of each model code respectively.

# After looking at the residual plot of each model, we found that 
# All models performed not very well during summer, and some of 
# them are performe really weill in time period other than summer,
# therefore we firstly test each model's performance in the summer of validation 
# dataset(May ¡ªAugust)

# Data loading
full_dataset <- read.csv("full_dataset.csv")
full_dataset$date_ready<- as.Date(full_dataset$date_ready)
# Get the observed value of eletricity demand in summer of 2015 and 2016
Td2015 <-as.numeric(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                    start = "2015-05-01", end ="2015-09-30"))
Td2016 <-as.numeric( window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                    start = "2016-05-01", end ="2016-09-30"))
Td.summer <- append(Td2015,Td2016)

# Get the observed value of eletricity demand in other period of 2015 and 2016
Td2015.strat <- as.numeric(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                                  start = "2015-01-01", end ="2015-04-30"))
Td2015.2016 <- as.numeric(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                                 start = "2015-10-01", end ="2016-04-30"))
Td2016.end <- as.numeric(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                                start = "2016-10-01", end ="2016-12-31"))
Td.other <- append(Td2015.strat,Td2015.2016,Td2016.end)

#TBATS
### To make life easier, we directly load the result from the file "forecast.conclude"

dfforecast.conclude<- read.csv("SARIMAforecast_conclude.csv")
dfforecast.conclude$date <- date(dfforecast.conclude$date)
# Forecast demand in summer 
TBATS2015 <-as.numeric(window(timeSeries(dfforecast.conclude$forecast, dfforecast.conclude$date),
                           start = "2015-05-01", end ="2015-09-30"))
TBATS2016 <-as.numeric( window(timeSeries(dfforecast.conclude$forecast, dfforecast.conclude$date),
                            start = "2016-05-01", end ="2016-09-30"))
TBATS.sumemr <- append(TBATS2015,TBATS2016)

# Forecast demand in other periods
TBATS2015.start <- as.numeric(window(timeSeries(dfforecast.conclude$forecast, dfforecast.conclude$date),
                                     start = "2015-01-01", end ="2015-04-30"))
TBATS2015.2016 <- as.numeric(window(timeSeries(dfforecast.conclude$forecast, dfforecast.conclude$date),
                                    start = "2015-10-01", end ="2016-04-30"))
TBATS.2016.end <- as.numeric(window(timeSeries(dfforecast.conclude$forecast, dfforecast.conclude$date),
                                 start = "2016-10-01", end ="2016-12-31"))
TBATS.other <- append(TBATS2015.start,TBATS2015.2016,TBATS.2016.end)


# Computing bias and MAPE
bias.TBATS.s <- mean(TBATS.sumemr-Td.summer)
pbias.TBATS.s <- mean((TBATS.sumemr-Td.summer)/Td.summer)*100
mape.TBATS.s <- mean(abs((TBATS.sumemr-Td.summer)/Td.summer))*100

bias.TBATS.other <- mean(TBATS.other-Td.other)
pbias.TBATS.other <- mean((TBATS.other-Td.other)/Td.other)*100
mape.TBATS.other <- mean(abs((TBATS.other-Td.other)/Td.other))*100


# Result
# bias.TBATS.s: -39.6
# pbias.TBATS.s: 0.0414
# mape.TBATS.s: 7.99
# bias.TBATS.other:30.7
# pbias.TBATS.other:0.962
# mape.TBATS.other:3.96

# Regression 
# To make life easier, we should firstly run the code in 
# groupproject_regression to get the result, here we assume that 
# we already did that

regression.result <- predict(fit,n.ahead=1,newxreg=reg.v)

reg2015 <- regression.result$pred[121:273]
reg2016 <- regression.result$pred[486:636]
reg.sumemr <- append(reg2015,reg2016)

reg2015.start <- regression.result$pred[3:120]
reg2015.2016 <- regression.result$pred[274:485]
reg2016.end  <- regression.result$pred[637:731]
reg.other <- append(reg2015.start,reg2015.2016,reg2016.end)

# Computing bias and MAPE
bias.reg.s <- mean(reg.sumemr-Td.summer)
pbias.reg.s <- mean((reg.sumemr-Td.summer)/Td.summer)*100
mape.reg.s <- mean(abs((reg.sumemr-Td.summer)/Td.summer))*100

Td.other.R  <- Td.other[-c(1, 2)]

bias.reg.o<- mean(reg.other-Td.other.R)
pbias.reg.o <- mean((reg.other-Td.other.R)/Td.other)*100
mape.reg.o <- mean(abs((reg.other-Td.other.R)/Td.other))*100

# Result
#bias.reg.s:-20
#pbias.reg.s: 0.00278
#mape.reg.s: 3.68

#bias.reg.o:156
#pbias.reg.o: 4.46
# mape.reg.o: 5.39

# SARIMA
# To make life easier, we should firstly run the code in 
# groupproject_ARIMA&SARIMA to get the result, here we assume that 
# we already did that


sarimaforecast.conclude$date <- as.Date(sarimaforecast.conclude$date)
SARIMA2015 <-as.numeric(window(timeSeries(sarimaforecast.conclude$forecast, sarimaforecast.conclude$date),
                              start = "2015-05-01", end ="2015-09-30"))
SARIMA2016 <-as.numeric( window(timeSeries(sarimaforecast.conclude$forecast, sarimaforecast.conclude$date),
                               start = "2016-05-01", end ="2016-09-30"))
SARIMA.sumemr <- append(SARIMA2015,SARIMA2016)

SARIMA.start <- as.numeric(window(timeSeries(sarimaforecast.conclude$forecast, sarimaforecast.conclude$date),
                                     start = "2015-01-01", end ="2015-04-30"))
SARIMA2015.2016 <- as.numeric(window(timeSeries(sarimaforecast.conclude$forecast, sarimaforecast.conclude$date),
                                    start = "2015-10-01", end ="2016-04-30"))
SARIMA.end <- as.numeric(window(timeSeries(sarimaforecast.conclude$forecast, sarimaforecast.conclude$date),
                                    start = "2016-10-01", end ="2016-12-31"))
SARIMA.other <-  append(SARIMA.start,SARIMA2015.2016,SARIMA.end)

# Computing bias and MAPE
bias.SARIMA.s <- mean(SARIMA.sumemr-Td.summer)
pbias.SARIMA.s <- mean((SARIMA.sumemr-Td.summer)/Td.summer)*100
mape.SARIMA.s <- mean(abs((SARIMA.sumemr-Td.summer)/Td.summer))*100
# bias.SARIMA.s: 12.2
# pbias.SARIMA.s : 0.845
# mape.SARIMA.s:  7.49

bias.SARIMA.o<- mean(SARIMA.other-Td.other)
pbias.SARIMA.o <- mean((SARIMA.other-Td.other)/Td.other)*100
mape.SARIMA.o <- mean(abs((SARIMA.other-Td.other)/Td.other))*100

#> bias.SARIMA.o
#[1]  6.54
#> pbias.SARIMA.o
#[1] 0.248
#> mape.SARIMA.o
#[1] 3.6

# ARX
# To make life easier, we should firstly run the code in 
# groupproject_ARX to get the result, here we assume that 
# we already did that
arx.date <- as.character(arx.date)
arx.result <- as.numeric(arx.result)
arx.date <- arx.date[-1]
arx.conclude <- data.frame( date = arx.date, forecast = arx.result)
arx.conclude$date <- date(arx.conclude$date)

arx2015 <-as.numeric(window(timeSeries(arx.conclude$forecast, arx.conclude$date),
                              start = "2015-05-01", end ="2015-09-30"))
arx2016 <-as.numeric(window(timeSeries(arx.conclude$forecast, arx.conclude$date),
                               start = "2016-05-01", end ="2016-09-30"))
arx.sumemr <- append(arx2015,arx2016)

arx2015.start<-as.numeric(window(timeSeries(arx.conclude$forecast, arx.conclude$date),
                                 start = "2015-01-02", end ="2015-04-30"))
arx2015.2016<-as.numeric(window(timeSeries(arx.conclude$forecast, arx.conclude$date),
                                start = "2015-10-01", end ="2016-04-30"))
arx2016.end<-as.numeric(window(timeSeries(arx.conclude$forecast, arx.conclude$date),
                               start = "2016-10-01", end ="2016-12-31"))
arx.other <- append(arx2015.start,arx2015.2016,arx2016.end)

Td.other.ARX  <- Td.other[-1]


# Computing bias and MAPE
bias.arx.s <- mean(arx.sumemr-Td.summer)
pbias.arx.s <- mean((arx.sumemr-Td.summer)/Td.summer)*100
mape.arx.s <- mean(abs((arx.sumemr-Td.summer)/Td.summer))*100
# bias.arx.s: -0.0838
# pbias.arx.s: 0.217
# mape.arx.s:3.26
bias.arx.o <- mean(arx.other-Td.other.ARX)
pbias.arx.o <- mean((arx.other-Td.other.ARX)/Td.other.ARX)*100
mape.arx.o <- mean(abs((arx.other-Td.other.ARX)/Td.other.ARX))*100

#bias.arx.o: 117
#pbias.arx.o: 3.31
#mape.arx.o: 4.23

# Based on the result above , we decided to use the 
# ARX model to fit the summer period , and TBATS to fit the winter period 

#get the observed demand in both summer and winter in the validation dataset
Td2017 <-as.numeric(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                           start = "2017-05-01", end ="2017-09-30"))
Td2018 <-as.numeric( window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                            start = "2018-05-01", end ="2018-09-30"))
Tdtest.summer <- append(Td2017,Td2018)

# Get the observed value of eletricity demand in other period of 2017 and 2018
Td2017.strat <- as.numeric(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                                  start = "2017-01-01", end ="2017-04-30"))
Td2017.2018 <- as.numeric(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                                 start = "2017-10-01", end ="2018-04-30"))
Td2018.end <- as.numeric(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                                start = "2018-10-01", end ="2018-12-31"))
Tdtest.other <- append(Td2017.strat,Td2017.2018,Td2018.end)
# ARX 
arx2017 <-as.numeric(window(timeSeries(arxtest.conclude$forecast, arxtest.conclude$date),
                            start = "2017-05-01", end ="2017-09-30"))
arx2018 <-as.numeric(window(timeSeries(arxtest.conclude$forecast, arxtest.conclude$date),
                            start = "2018-05-01", end ="2018-09-30"))
arxtest.sumemr <- append(arx2017,arx2018)
# Calculate the performane in test data
test.bias.arx.s <- mean(arxtest.sumemr-Tdtest.summer)
test.pbias.arx.s <- mean((arxtest.sumemr-Tdtest.summer)/Tdtest.summer)*100
test.mape.arx.s <- mean(abs((arxtest.sumemr-Tdtest.summer)/Tdtest.summer))*100

# For tbats in winter(other periods)

test.conclude<- read.csv("SARIMAforecast_conclude.csv")
test.conclude$date <- as.Date(test.conclude$date)

TBATS2017.starttest <- as.numeric(window(timeSeries(test.conclude$forecast, test.conclude$date),
                                     start = "2017-01-01", end ="2017-04-30"))
TBATS2017.2018test <- as.numeric(window(timeSeries(test.conclude$forecast, test.conclude$date),
                                    start = "2017-10-01", end ="2018-04-30"))
TBATS.2018.endtest <- as.numeric(window(timeSeries(test.conclude$forecast, test.conclude$date),
                                    start = "2018-10-01", end ="2018-12-31"))
TBATS.othertest <- append(TBATS2017.starttest,TBATS2017.2018test,TBATS.2018.endtest)


Tdtest.other

# Calculate the performane in test data
test.bias. <- mean(TBATS.othertest-Tdtest.other)
test.pbias. <- mean((TBATS.othertest-Tdtest.other)/Tdtest.other)*100
test.mape. <- mean(abs((TBATS.othertest-Tdtest.other)/Tdtest.other))*100
#test.bias.
#[1] 8.65
#> test.pbias.
#[1] 0.493
#> test.mape.
#[1] 4.74
#> 

#> test.bias.
#[1] 80.1
#> test.pbias.
#[1] 2.97
#> test.mape.
#[1] 11.2


# Plot 
Td20172018ts<- window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                                  start = "2017-01-01", end ="2018-12-31")
plot(Td20172018ts,ylim = c(1000,8000) )
tsTBATS2017.starttest <- window(timeSeries(test.conclude$forecast, test.conclude$date),
                                         start = "2017-01-01", end ="2017-04-30")
tsTBATS2017.2018test <- window(timeSeries(test.conclude$forecast, test.conclude$date),
                                        start = "2017-10-01", end ="2018-04-30")
tsTBATS.2018.endtest <- window(timeSeries(test.conclude$forecast, test.conclude$date),
                                        start = "2018-10-01", end ="2018-12-31")

tsarx2017 <-window(timeSeries(arxtest.conclude$forecast, arxtest.conclude$date),
                            start = "2017-05-01", end ="2017-09-30")
tsarx2018 <-window(timeSeries(arxtest.conclude$forecast, arxtest.conclude$date),
                            start = "2018-05-01", end ="2018-09-30")

lines(tsTBATS2017.starttest,col = "red")
lines(tsTBATS2017.2018test,col = "red")
lines(tsTBATS.2018.endtest,col = "red")
lines(tsarx2017,col = "blue")
lines(tsarx2018,col = "blue")



legend("bottomright",legend=c("SARIMA forecast","ARX forecast","obs"),lty=c(1,1),
       col=c("red","blue","black"))
