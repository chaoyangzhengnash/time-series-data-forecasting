#
# Program: groupproject_regression.R
#
# Purpose: 
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 19 March 2019
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
# Run liner regression model 
mreg <- lm(Yt ~  HDDt+
              CDDt + lag(HDDt,1) + lag(HDDt,2) + lag(CDDt,1) + lag(CDDt,2) + 
              Rhumt + WINDCHILLt + factor(WEEKENDt) +
             factor(HOLIDAYt),
            x=T, y=T)

print(summary(mreg))


######
# We notice that the real humidity and windchill have negative signs¡­¡­
plot(x= WINDCHILLt,y = Yt)
plot(x= Rhumt,y = Yt)

test1.reg <- lm(Yt~Rhumt+WINDCHILLt)
summary(test1.reg)


test2.reg <- lm(Yt~Rhumt+WINDCHILLt+CDDt)
summary(test2.reg)

test3.reg <- lm(Yt~Rhumt+WINDCHILLt+HDDt)
summary(test3.reg)


######
######


# Plot of diagnostic graph
par(mfrow=c(2,2))
plot(mreg)

# Durbin-Watson test for autocorrelated residuals
library(lmtest)
print(dwtest(mreg))

# Can also calculate DW statistic for different lags
# library(car)
# print(durbinWatsonTest(mreg,max.lag=5))

# lm without Windchill 
mreg2 <- lm(Yt ~  HDDt+
             CDDt + lag(HDDt,1) + lag(HDDt,2) + lag(CDDt,1) + lag(CDDt,2) + 
             Rhumt + factor(WEEKENDt) +
             factor(HOLIDAYt),
           x=T, y=T)

print(summary(mreg2))
# Plot of diagnostic graph
par(mfrow=c(2,2))
plot(mreg2)
# Durbin-Watson test for autocorrelated residuals
library(lmtest)
print(dwtest(mreg2))
# Can also calculate DW statistic for different lags
#library(car)
#print(durbinWatsonTest(mreg2,max.lag=5))


# Fit auto arma error

fit <- auto.arima(Yt, xreg=cbind(HDDt,CDDt,WINDCHILLt,
                                 HOLIDAYt,WEEKENDt,Rhumt,
                                 lag(HDDt,1),lag(CDDt,1),
                                 lag(HDDt,2),lag(CDDt,2)
                                 )) 

print(fit)
acf(residuals(fit)[-(1:2)],
    main="With proper error structure (using auto.arima)")
print(fit)
print(summary(fit))
# AIC=19176   AICc=19177   BIC=19261


# Fit arma error, using order=c(5,1,0)

fit.arima <- arima(Yt, xreg=cbind(HDDt,CDDt,WINDCHILLt,
                                  HOLIDAYt,WEEKENDt,
                                  lag(HDDt,1),lag(CDDt,1),
                                  lag(HDDt,2),lag(CDDt,2)
), order=c(3,0,0))
print(fit.arima)
acf(residuals(fit.arima)[-(1:2)],
    main="With proper error structure (using order=c(5,1,0))")

plot(fit.arima)

# Using sarima 
library(astsa)
adjreg <- sarima(Yt, 5,1,0, xreg=cbind(HDDt,CDDt,WINDCHILLt,
                                       HOLIDAYt,WEEKENDt,
                                       lag(HDDt,1),lag(CDDt,1),
                                       lag(HDDt,2),lag(CDDt,2)))
detach("package:astsa")


# auto.arma model without windchill
library(astsa)
fit.arima2 <- arima(Yt, xreg=cbind(HDDt,CDDt,
                                  HOLIDAYt,WEEKENDt,
                                  lag(HDDt,1),lag(CDDt,1),
                                  lag(HDDt,2),lag(CDDt,2)
), order=c(5,1,0))
print(summary(fit.arima2))
# aic = 19225
acf(residuals(fit.arima2)[-(1:2)],
    main="With proper error structure (using order=c(5,1,0))")


# Using sarima 
library(astsa)
adjreg2 <- sarima(Yt, 5,1,0, xreg=cbind(HDDt,CDDt,
                                       HOLIDAYt,WEEKENDt,
                                       lag(HDDt,1),lag(CDDt,1),
                                       lag(HDDt,2),lag(CDDt,2)))
detach("package:astsa")

# For Forecast 
# We decide to use the model fit : with all variables and proper error structure
# Use the ture value for all explanatory variables (in the validation dataset: 2015-2016)
HDDv.ts <-  window(timeSeries(to.wea.HDD,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
CDDv.ts <-  window(timeSeries(to.wea.CDD,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
Rhumv.ts <-  window(timeSeries(to.wea.Rhum,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
WINDCHILLv.ts<- window(timeSeries(to.wea.WINDCHILL,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
WEEKENDv.ts <-  window(timeSeries(to.other.WEEKEND,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
HOLIDAYv.ts <-  window(timeSeries(to.other.HOLIDAY,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")

HDDv<- as.numeric(HDDv.ts$TS.1)
CDDv<- as.numeric(CDDv.ts$TS.1)
HDDv.lag1<- as.numeric(lag(HDDv.ts,1))
HDDv.lag2<- as.numeric(lag(HDDv.ts,2))
CDDv.lag1<- as.numeric(lag(CDDv.ts,1))
CDDv.lag2<- as.numeric(lag(CDDv.ts,2))
Rhumv<- as.numeric(Rhumv.ts$TS.1)
WINDCHILLv<- as.numeric(WINDCHILLv.ts$TS.1)
WEEKENDv<- as.numeric(WEEKENDv.ts$TS.1)
HOLIDAYv<- as.numeric(HOLIDAYv.ts$TS.1)

reg.v <- cbind(HDDv,CDDv,WINDCHILLv,HOLIDAYv,WEEKENDv,Rhumv,HDDv.lag1,CDDv.lag1,HDDv.lag2,CDDv.lag2)

# Caculating the model's performance in validation dataset

pred.V <- predict(fit,n.ahead=1,newxreg=reg.v)

pred.Vlist <-list (as.numeric(pred.V$pred))

to.demand <- as.numeric(Full.dataset$demand_ready)
DEMAND.v <-  window(timeSeries(to.demand,to.wea.date,format="%Y-%m-%d"), start="2015-01-01",end = "2016-12-31")
truedemand.vlist<- list(as.numeric(DEMAND.v$TS.1))

# Delete the first 2 days both in pred.V and truedemand.v, as there
# are NA values in the first 2 days.

pred.Vlist<- pred.Vlist[[1]][-c(1,2)]
truedemand.vlist<- truedemand.vlist[[1]][-c(1,2)]

# Computing bias and MAPE
bias.R <- mean(pred.Vlist-truedemand.vlist)
pbias.R <- mean((pred.Vlist-truedemand.vlist)/truedemand.vlist)*100
mape.R <- mean(abs((pred.Vlist-truedemand.vlist)/truedemand.vlist))*100

#To figure out the inflences of Memorail data 
#Full.dataset$demand_ready[3794]

#Plot model's performance in the validation dataset

truedemand.n<- as.numeric(Full.dataset$demand_ready[c(3652:4377)])
truedemandplot <- data.frame(date = Full.dataset$date_ready[c(3652:4377)],demand = truedemand.n)
truedemandplot$date <- as.Date(truedemandplot$date )
truedemandplot.ts <- timeSeries(truedemandplot$demand,truedemandplot$date)

preddemandplot <- data.frame(date = Full.dataset$date_ready[c(3652:4377)],demand =pred.Vlist)
preddemandplot$date <- as.Date(preddemandplot$date)
preddemandplot <- timeSeries(preddemandplot$demand,preddemandplot$date)

plot(preddemandplot, ylab = "PJM daily demand", main = "Liner regression result in the validation dataset
     ",col = "red")

lines(truedemandplot.ts)
legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))

# Resudual plot between 2013 and 2014

in.sample <- window(timeSeries(Full.dataset$demand_ready, Full.dataset$date_ready), 
                    start = "2013-01-01", end ="2014-12-30")
demand.train <- in.sample$TS.1

#duang <- as.numeric(fit [(364*2+2):(364*4+2)])

Refdate.R<- data.frame(date = Full.dataset$date_ready[c(2920:3648)])
Refdate.R$date <- as.Date(Refdate.R$date)

rua <- data.frame(residual = fit$residuals [(364*2+2):(364*4+2)], date = Refdate.R$date )

plot(timeSeries(rua$residual,rua$date), ylab = "residual", main =  "Residual plot of regression bwtween 2013 and 2014")
