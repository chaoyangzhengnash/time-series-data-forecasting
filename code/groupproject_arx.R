#
# Program: groupproject_arx.R
#
# Purpose: 
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 6 April 2019
#        
# ------------------------------------------------------.
#
# Set locale to English language and preparation work
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(dplyr)
library(lubridate)
library(zoo)
library("readxl")
library(forecast)
options(digits=3)
library(dynlm)
#library(Hmisc)
setwd("C:/Users/Zheng Chaoyang/Desktop/part_2/data")

Full.dataset <- read.csv("full_dataset.CSV")
Full.dataset$date_ready<-as.Date(Full.dataset$date_ready)

# Creating electricity demand as dependent variable(Yt)
to.dem.daily <- data.frame(date = Full.dataset$date_ready, Demand = Full.dataset$demand_ready)
Yt <- window(timeSeries(to.dem.daily[[2]],to.dem.daily[[1]],
                        format="%Y-%m-%d"), start="2011-01-01",end = "2014-12-31")
# Creating explanatory variables

to.wea.date <- Full.dataset$date_ready
to.wea.temp <- as.numeric(Full.dataset$TVAG_ready)
to.wea.HDD  <- as.numeric(Full.dataset$Hdd)
to.wea.CDD  <- as.numeric(Full.dataset$Cdd)
to.wea.Rhum <- as.numeric(Full.dataset$felt_humilidity)
to.wea.WINDCHILL <- as.numeric(Full.dataset$real_Windchill)
to.other.WEEKDAY <- as.numeric(Full.dataset$weekday)
to.other.WEEKEND <- as.numeric(Full.dataset$weekend)
to.other.HOLIDAY <- as.numeric(Full.dataset$holiday)

# Create time series for mean temperature, heating degree days 
# and cooling degree days variables.
Tt   <- window(timeSeries(to.wea.temp,to.wea.date,format="%Y-%m-%d"), start="2011-01-01",end = "2014-12-31")
HDDt <-  window(timeSeries(to.wea.HDD,to.wea.date,format="%Y-%m-%d"), start="2011-01-01",end = "2014-12-31")
CDDt <-  window(timeSeries(to.wea.CDD,to.wea.date,format="%Y-%m-%d"), start="2011-01-01",end = "2014-12-31")
Rhumt <-  window(timeSeries(to.wea.Rhum,to.wea.date,format="%Y-%m-%d"), "2011-01-01",end = "2014-12-31")
WINDCHILLt<- window(timeSeries(to.wea.WINDCHILL,to.wea.date,format="%Y-%m-%d"), start="2011-01-01",end = "2014-12-31")
WEEKENDt <-  window(timeSeries(to.other.WEEKEND,to.wea.date,format="%Y-%m-%d"), start="2011-01-01",end = "2014-12-31")
HOLIDAYt <-  window(timeSeries(to.other.HOLIDAY,to.wea.date,format="%Y-%m-%d"), start="2011-01-01",end = "2014-12-31")
detach("package:dplyr")

# Try to fit an ARX model(Introducing the lag 1 value of PJM electricity demand as our new independent variable) 
data = cbind(Yt,Tt,HDDt,CDDt,Rhumt,WINDCHILLt,WEEKENDt,HOLIDAYt)
arxmodel<-dynlm(Yt~Tt+HDDt+CDDt+Rhumt+WINDCHILLt+WEEKENDt+HOLIDAYt+L(Yt,1),data=data)


# Forecast by using obseved value as the value of all explanatory variable
Tt.v   <- window(timeSeries(to.wea.temp,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
HDDt.v <-  window(timeSeries(to.wea.HDD,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
CDDt.v <-  window(timeSeries(to.wea.CDD,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
Rhumt.v <-  window(timeSeries(to.wea.Rhum,to.wea.date,format="%Y-%m-%d"), start = "2015-01-01",end = "2016-12-31")
WINDCHILLt.v<- window(timeSeries(to.wea.WINDCHILL,to.wea.date,format="%Y-%m-%d"), start = "2015-01-01",end = "2016-12-31")
WEEKENDt.v <-  window(timeSeries(to.other.WEEKEND,to.wea.date,format="%Y-%m-%d"), start = "2015-01-01",end = "2016-12-31")
HOLIDAYt.v <-  window(timeSeries(to.other.HOLIDAY,to.wea.date,format="%Y-%m-%d"), start = "2015-01-01",end = "2016-12-31")

# Bind time series data in validation dataset to get new data


# Using the obseved lag 1 value of electricity demand as the value of explanatory varibale for forecast
Yt.v <- lag(window(timeSeries(to.dem.daily[[2]],to.dem.daily[[1]],
                        format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31"), k = 1)
# Writing a loop to get the forecast result, 
# as the predict function seems doesn't work in this case 
Refdate.t<- data.frame(date = Full.dataset$date_ready[c(3650:4377)])
arx.date<-list()
arx.result<-list()
for (i in 1: 728){
  arx.result[i] <- as.numeric( arxmodel$coefficients[1] + arxmodel$coefficients[2] *Tt.v[i]
                                   + arxmodel$coefficients[3] * HDDt.v[i] + arxmodel$coefficients[4] * CDDt.v[i] 
                                   + arxmodel$coefficients[5] * Rhumt.v[i] + arxmodel$coefficients[6] * WINDCHILLt.v[i] 
                                   + arxmodel$coefficients[7] * WEEKENDt.v[i] + arxmodel$coefficients[8] * HOLIDAYt.v[i] 
                                   + arxmodel$coefficients[9] * Yt.v[i] )
  arx.date[i]<- as.character( Refdate.t$date[i])
  i = i+1
}



# Computing model performance in validation data
arx.result<- arx.result[-1]
# Computer the se 
arx.result.se <- arx.result - mean(arx.result)
arxse.conlcude <-data.frame(date = Refdate.t$date[-1], se = arx.result.se)

Taylordemand.n <- as.numeric(arx.result)
truedemand.n<- as.numeric(Full.dataset$demand_ready[c(3651:4377)])

# Computing bias and MAPE
bias.R <- mean(Taylordemand.n-truedemand.n)
pbias.R <- mean((Taylordemand.n-truedemand.n)/truedemand.n)*100
mape.R <- mean(abs((Taylordemand.n-truedemand.n)/truedemand.n))*100

# PLot 

taylorplot <- data.frame(date = Full.dataset$date_ready[c(3651:4377)],demand = Taylordemand.n)
taylorplot.ts <- timeSeries(taylorplot$demand,taylorplot$date)
truedemandplot <- data.frame(date = Full.dataset$date_ready[c(3651:4377)],demand = truedemand.n)
truedemandplot.ts<- timeSeries(truedemandplot$demand,truedemandplot$date)

plot(taylorplot.ts, ylab = "PJM daily demand", main = "ARx result",col = "red",ylim = c(2000,8000))
lines(truedemandplot.ts)
legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))

# PLot to get arx performance in only 2015 may
taylorplot <- data.frame(date = Full.dataset$date_ready[c(3651:4377)],demand = Taylordemand.n)
taylorplot.ts <- window(timeSeries(taylorplot$demand,taylorplot$date),start="2015-05-01",end = "2015-05-31")
truedemandplot <- data.frame(date = Full.dataset$date_ready[c(3651:4377)],demand = truedemand.n)
truedemandplot.ts<- window(timeSeries(truedemandplot$demand,truedemandplot$date),start="2015-05-01",end = "2015-05-31")

plot(taylorplot.ts, ylab = "PJM daily demand", main = "ARx result",col = "red",ylim = c(2000,8000))
lines(truedemandplot.ts)
legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))



# To get the residual plot of ARX in the training dataset

in.sample <- window(timeSeries(Full.dataset$demand_ready, Full.dataset$date_ready), 
                    start = "2011-01-01", end ="2014-12-30")
demand.train <- in.sample$TS.1
arx.fitted <- as.numeric(arxmodel$fitted.values) 

Refdate.R<- data.frame(date = Full.dataset$date_ready[c(2191:3648)])
Refdate.R$date <- as.Date(Refdate.R$date)

rua <- data.frame(residual = demand.train- arx.fitted, date = Refdate.R$date )

plot(timeSeries(rua$residual,rua$date), ylab = "residual", main =  "Residual plot of ARX in the training dataset")

# To go more detailed information on that, let's pick up 2013 and 2014 only 

in.sample <- window(timeSeries(Full.dataset$demand_ready, Full.dataset$date_ready), 
                    start = "2013-01-01", end ="2014-12-30")
demand.train <- in.sample$TS.1

duang <- as.numeric(arxmodel$fitted.value[(364*2+2):(364*4+2)])

Refdate.R<- data.frame(date = Full.dataset$date_ready[c(2920:3648)])
Refdate.R$date <- as.Date(Refdate.R$date)

rua <- data.frame(residual = demand.train- duang, date = Refdate.R$date )

plot(timeSeries(rua$residual,rua$date), ylab = "residual", main =  "Residual plot of ARX bwtween 2013 and 2014")

# To get the performance in test data set in general 

# Forecast by using obseved value as the value of all explanatory variable
Tt.t   <- window(timeSeries(to.wea.temp,to.wea.date,format="%Y-%m-%d"), start="2017-01-01",end = "2018-12-31")
HDDt.t <-  window(timeSeries(to.wea.HDD,to.wea.date,format="%Y-%m-%d"), start="2017-01-01",end = "2018-12-31")
CDDt.t <-  window(timeSeries(to.wea.CDD,to.wea.date,format="%Y-%m-%d"), start="2017-01-01",end = "2018-12-31")
Rhumt.t <-  window(timeSeries(to.wea.Rhum,to.wea.date,format="%Y-%m-%d"), start="2017-01-01",end = "2018-12-31")
WINDCHILLt.t<- window(timeSeries(to.wea.WINDCHILL,to.wea.date,format="%Y-%m-%d"), start="2017-01-01",end = "2018-12-31")
WEEKENDt.t <-  window(timeSeries(to.other.WEEKEND,to.wea.date,format="%Y-%m-%d"), start="2017-01-01",end = "2018-12-31")
HOLIDAYt.t <-  window(timeSeries(to.other.HOLIDAY,to.wea.date,format="%Y-%m-%d"), start="2017-01-01",end = "2018-12-31")

# Using the obseved lag 1 value of electricity demand as the value of explanatory varibale for forecast
Yt.t <- lag(window(timeSeries(to.dem.daily[[2]],to.dem.daily[[1]],
                              format="%Y-%m-%d"), start="2017-01-01",end = "2018-12-31"), k = 1)


# Writing a loop to get the forecast result in the test dataset, 
# as the predict function seems doesn't work in this case 
Refdate.t<- data.frame(date = Full.dataset$date_ready[c(4378:5105)])
arx.date<-list()
arx.result<-list()
for (i in 1: 728){
  arx.result[i] <- as.numeric( arxmodel$coefficients[1] + arxmodel$coefficients[2] *Tt.t[i]
                               + arxmodel$coefficients[3] * HDDt.t[i] + arxmodel$coefficients[4] * CDDt.t[i] 
                               + arxmodel$coefficients[5] * Rhumt.t[i] + arxmodel$coefficients[6] * WINDCHILLt.t[i] 
                               + arxmodel$coefficients[7] * WEEKENDt.t[i] + arxmodel$coefficients[8] * HOLIDAYt.t[i] 
                               + arxmodel$coefficients[9] * Yt.t[i] )
  arx.date[i]<- as.character( Refdate.t$date[i])
  i = i+1
}

arx.result <- as.numeric( arx.result)
arx.date <- as.character(arx.date)
arxtest.conclude <- data.frame(date=arx.date, forecast = arx.result)
arxtest.conclude$date <- date(arxtest.conclude$date)
