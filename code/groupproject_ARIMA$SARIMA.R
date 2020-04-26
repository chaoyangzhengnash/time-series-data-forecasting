#
# Program: groupproject_ARIMA$SARIMA.R
#
# Purpose: 
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 30 March 2019
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


# Data loading
full_dataset <- read.csv("full_dataset.csv")
full_dataset$date_ready<- as.Date(full_dataset$date_ready)

# Defining in.sampe and out.sample data
in.sample <- window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready), 
                         start = "2011-01-01", end ="2014-12-31")
out.sample <-window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                          start = "2015-01-01", end ="2016-12-31")
test.sample <-window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                    start = "2017-01-01", end ="2018-12-31")
all.population <- window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready), 
                       start = "2011-01-01", end ="2018-12-31")
mean(all.population$TS.1)
# A general visualization 
plot(in.sample)
lag1.plot(in.sample,7) # The highest phi appeared in lag 1 and lag 7 
acf2(as.numeric(in.sample),max.lag = 300)# ACF show seasonality,
# PACF:looks like nothing after specific lag 


# Before fiting ARIMA model, Let'try differencing to make our data stationary 
# Try first differencing 
plot(diff(as.numeric(in.sample)),main="(1-B)(data:in.sample)")
acf2(diff(as.numeric(in.sample)),max.lag = 364*4)

#Try lag 7 differencing  
acf2(diff(as.numeric(in.sample),7),max.lag = 364)

# Try to fit non-seasonal ARIMA model 
# Tips:You can try as much as differernt parameters
#      While for me it looks doesn't work¡­¡­
fit1 <- sarima(as.numeric(in.sample),p=1,d=1,q=1)     # Fitting an ARIMA(1,1,1)

fit2 <- sarima(as.numeric(in.sample),p=2,d=1,q=2)     # Fitting an ARIMA(2,1,2)

fit3 <- sarima(as.numeric(in.sample),p=7,d=1,q=2)     

fit3 <- sarima(as.numeric(in.sample),p=7,d=2,q=1)     
#  Try auto.arima to get some suggesetions 
so3 <-auto.arima(as.numeric(in.sample))
so3
# ARIMA(5,0,5) with non-zero mean 


# Try to fit seasonal ARIMA model 
# Good tool to examine seasonal data...
#par(mfrow=c(2,1))
seasonplot(ts(in.sample,freq=7),col=gray((1:50)/50))
seasonplot(ts(in.sample,freq=12),col=gray((1:50)/50))
seasonplot(ts(in.sample,freq=364),col=gray((1:50)/50))


# Try sarima 
#p=17,d=1,q=0,P=2,D=1,Q=2,S=7best so far
so2 <- sarima(as.numeric(in.sample),p=0,d=0,q=0,P=1,D=1,Q=1,S=7)
title(
  sub="SARIMA model£º p=17,d=1,q=0,P=2,D=1,Q=2,S=7")

#so3 <- sarima(as.numeric(in.sample),p=1,d=1,q=1,P=1,D=1,Q=1,S=7)
#title(
#  sub="SARIMA model£º p=1,d=1,q=1,P=1,D=1,Q=1,S=7")

# Try to change the insampLe to see if we can get better result
in.samplenew <- window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready), 
                    start = "2013-01-01", end ="2014-12-31")
so5 <- sarima(as.numeric(in.samplenew),p=7,d=1,q=1,P=2,D=1,Q=1,S=7)

so6 <- sarima(as.numeric(out.sample),p=7,d=0,q=1,P=4,D=1,Q=0,S=7)

# Using the "best so far" model (17,1,0)(2,1,2)[7]  to do forecast with rolling

# Rolling to get forecast result in the validation data
sarimadate.f<-list()
sarimademand.f<-list()
sarimaerror.f<-list()
Refdate <- data.frame(date = full_dataset$date_ready[c(4378:5105)])
Refdate.t<- data.frame(date = full_dataset$date_ready[c(2920:4377)])

#length(Refdate$date)

#for(i in 1:length(Refdate$date)){
#  fore<-sarima.for(in.sample, 1, p=17,d=1,q=0,P=2,D=1,Q=2,S=7)
#  sarimademand.f[i] <- as.numeric(fore[[1]])
#  sarimadate.f[i]<- as.character(Refdate$date[i])
#  sarimaerror.f[i] <-  as.numeric(fore[[2]])
#  Rua <- append(in.sample[-1,],out.sample[i])
#  sua <- data.frame(date = Refdate.t$date[(1+i):(1459+i)], demand = Rua) #1459
#  in.sample <- timeSeries(sua$demand,sua$date)
#  i = i+1
#}
length(Refdate$date)
# However, there are some unknown errors happened when we try to use 
# such a huge model to do forecast, therefore we decided to use the 
# easiest model to do forecast: p=1,d=1,q=1,P=1,D=1,Q=1,S=7
for(i in 1:length(Refdate$date)){
  fore<-sarima.for(in.samplenew, 1, p=6,d=1,q=1,P=0,D=1,Q=1,S=7)
  
  sarimademand.f[i] <- as.numeric(fore[[1]])
  sarimadate.f[i]<- as.character(Refdate$date[i])
  sarimaerror.f[i] <-  as.numeric(fore[[2]])
  
  Rua <- append(in.samplenew[-1,],out.sample[i])
  sua <- data.frame(date = Refdate.t$date[(1+i):(730+i)], demand = Rua) #1459
  in.samplenew <- timeSeries(sua$demand,sua$date)
  i = i+1
}

# Get its performance in the test data
for(i in 1:length(Refdate$date)){
  fore<-sarima.for(in.samplenew, 1, p=6,d=1,q=1,P=0,D=1,Q=1,S=7)
  
  sarimademand.f[i] <- as.numeric(fore[[1]])
  sarimadate.f[i]<- as.character(Refdate$date[i])
  sarimaerror.f[i] <-  as.numeric(fore[[2]])
  
  Rua <- append(in.samplenew[-1,],test.sample[i])
  sua <- data.frame(date = Refdate.t$date[(1+i):(730+i)], demand = Rua) #1459
  in.samplenew <- timeSeries(sua$demand,sua$date)
  i = i+1
}



#Concluding table of forecasts

sarimademand.f <- as.numeric( sarimademand.f)
sarimadate.f <- as.character(sarimadate.f)
sarimaerror.f <-as.numeric(sarimaerror.f)
sarimaforecast.conclude <- data.frame(date=sarimadate.f, forecast = sarimademand.f,
                                se = sarimaerror.f )

# Computing bias and MAPE
Taylordemand.n <- sarimaforecast.conclude$forecast
truedemand.n<- as.numeric(full_dataset$demand_ready[c(4378:5105)])
bias.R <- mean(Taylordemand.n-truedemand.n)
pbias.R <- mean((Taylordemand.n-truedemand.n)/truedemand.n)*100
mape.R <- mean(abs((Taylordemand.n-truedemand.n)/truedemand.n))*100

# Plot

taylorplot <- data.frame(date = full_dataset$date_ready[c(4378:5105)],demand = Taylordemand.n)
taylorplot.ts <- timeSeries(taylorplot$demand,taylorplot$date)
truedemandplot <- data.frame(date = full_dataset$date_ready[c(4378:5105)],demand = truedemand.n)
truedemandplot.ts<- timeSeries(truedemandplot$demand,truedemandplot$date)

#test.conclude$date = as.Date(test.conclude$date)


plot(taylorplot.ts, ylab = "PJM daily demand", main = "SARIMA model (p=6,d=1 ,q=1,P=0,D=1,Q=1,S=7)result in the test dataset",col = "red",ylim = c(2000,8000))


lines(truedemandplot.ts)

lines(fore$pred-1.96*fore$se,lty=3,col="blue")
lines(fore$pred+1.96*fore$se,lty=3,col="blue")

legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))

#
write.csv(sarimaforecast.conclude, file = "SARIMAforecast_conclude.csv" )

# However, there are some unknown errors happened when we try to use 
# such a huge model to do forecast, therefore we decided to use the 
# easiest model to do forecast: p=1,d=1,q=1,P=1,D=1,Q=1,S=7



