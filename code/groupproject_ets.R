#
# Program: groupproject_State space model .R
#
# Purpose: 
#
# Written by: Katie Gimon, Yang Yu, Meng Peng, Chaoyang Zheng
# 
# Updated: 22 March 2019
#        
# ------------------------------------------------------.
#
# Set locale to English language and preparation work
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(forecast)
options(digits=3)
library(zoo)
setwd("C:/Users/Zheng Chaoyang/Desktop/part_2/data")

# Data loading
full_dataset <- read.csv("full_dataset.csv")
full_dataset$date_ready<- as.Date(full_dataset$date_ready)

# Defining in.sample and out.sample data


in.sample <- msts(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready), 
                         start = "2011-01-01", end ="2014-12-31"),seasonal.periods=7)  

out.sample <- msts(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                          start = "2015-01-01", end ="2016-12-31"),seasonal.periods=7)

# Try five different models and compare the values of the
# different criteria on the in.sample data.

ann <- ets(in.sample,"ANN")
aan <- ets(in.sample,"AAN")
aaa <- ets(in.sample,"AAA")
mam <- ets(in.sample,"MAM")
mmm <- ets(in.sample,"MMM")

print(c(ann$aic,ann$bic,ann$aicc))
print(c(aan$aic,aan$bic,aan$aicc))
print(c(aaa$aic,aaa$bic,aaa$aicc))
print(c(mam$aic,mam$bic,mam$aicc))
print(c(mmm$aic,mmm$bic,mmm$aicc))

# Let ets choose the best model for in.sample data.
# Use all ets default values for function arguments.
print(ets(in.sample))

# The system choose MNM by default, 
print(ets(in.sample,allow.multiplicative.trend=T))
# Still choose MNM(27365 27365 27418) rather than MMM(27368 27436 27368 )
# Therefore, we decide to use MNM to do forecast 


# Regarding the forecast part¡­¡­


Refdate <- data.frame(date = full_dataset$date_ready[c(3650:4377)])
Refdate.t<- data.frame(date = full_dataset$date_ready[c(2191:4377)])


Taylordate.p<-list()
Taylordemand.p<-list()
lo.80 <- list()
hi.80 <- list()
lo.95 <- list()
hi.95 <- list()

#length(Refdate$date)

for(i in 1:length(Refdate$date)){
  fit <- ets(in.sample, allow.multiplicative.trend=T)
  fore<- forecast(fit,h=1) 
  Taylordemand.p[i] <- as.numeric(fore[[2]][1])
  Taylordate.p[i]<- as.character(Refdate$date[i])
  lo.80[i] <-fore$lower[1]
  hi.80[i] <-fore$upper[1]
  lo.95[i] <-fore$lower[2]
  hi.95[i] <-fore$upper[2]
  
  Rua <- append(in.sample[-1,],out.sample[i])
  sua <- data.frame(date = Refdate.t$date[(1+i):(1459+i)], demand = Rua) #1459
  in.sample <- msts(timeSeries(sua$demand,sua$date),seasonal.periods=c(7,364))
  i = i+1
}
#Concluding table of forecasts

Taylordemand.n <- as.numeric( Taylordemand.p)
Taylordate.n <- as.character(Taylordate.p)
lo.80num <-as.numeric(lo.80)
hi.80num <-as.numeric(hi.80)
lo.95num <-as.numeric(lo.95)
hi.95num <-as.numeric(hi.95)

forecast.conclude <- data.frame(date=Taylordate.n, forecast = Taylordemand.n,
                                lo80=lo.80num,hi80=hi.80num,lo95=lo.95num,hi95=hi.95num)
#write.csv(forecast.conclude,file = "forecast.conclude")

# Caculating the model's performance in half of validation dataset 


truedemand.n<- as.numeric(full_dataset$demand_ready[c(3650:4377)])

### To make life easier, we save the result in the file "forecast.conclude"

#dfforecast.conclude<- read.csv("forecast.conclude.csv")
Taylordemand.n <- forecast.conclude$forecast
# Computing bias and MAPE
bias.R <- mean(Taylordemand.n-truedemand.n)
pbias.R <- mean((Taylordemand.n-truedemand.n)/truedemand.n)*100
mape.R <- mean(abs((Taylordemand.n-truedemand.n)/truedemand.n))*100

# Plot the performance of MNM model

taylorplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = Taylordemand.n)
taylorplot.ts <- timeSeries(taylorplot$demand,taylorplot$date)
truedemandplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = truedemand.n)
truedemandplot.ts<- timeSeries(truedemandplot$demand,truedemandplot$date)

dfforecast.conclude$date = as.Date(dfforecast.conclude$date)
lo80ts<- timeSeries(dfforecast.conclude$lo80,charvec = dfforecast.conclude$date)
hi80ts<- timeSeries(dfforecast.conclude$hi80,charvec = dfforecast.conclude$date)
lo95ts<- timeSeries(dfforecast.conclude$lo95,charvec = dfforecast.conclude$date)
hi95ts<- timeSeries(dfforecast.conclude$lo95,charvec = dfforecast.conclude$date)

plot(taylorplot.ts, ylab = "PJM daily demand", main = "State space model(MNM) result",col = "red",ylim = c(2000,8000))


lines(truedemandplot.ts)
lines(lo80ts,col = "gray")
lines(hi80ts,col = "gray")
lines(lo95ts,col = "azure4")
lines(hi95ts,col = "azure4")



lines(truedemandplot.ts)

legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))

# Plot the performance of MNM model in two weeks 

taylorplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = Taylordemand.n)
taylorplot.ts <- window(timeSeries(taylorplot$demand,taylorplot$date),start="2015-05-01",end = "2015-05-31")
truedemandplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = truedemand.n)
truedemandplot.ts<- window(timeSeries(truedemandplot$demand,truedemandplot$date),start="2015-05-01",end = "2015-05-31")

dfforecast.conclude$date = as.Date(dfforecast.conclude$date)
lo80ts<- window(timeSeries(dfforecast.conclude$lo80,charvec = dfforecast.conclude$date),start="2015-05-01",end = "2015-05-31")
hi80ts<- window(timeSeries(dfforecast.conclude$hi80,charvec = dfforecast.conclude$date),start="2015-05-01",end = "2015-05-31")
lo95ts<- window(timeSeries(dfforecast.conclude$lo95,charvec = dfforecast.conclude$date),start="2015-05-01",end = "2015-05-31")
hi95ts<- window(timeSeries(dfforecast.conclude$lo95,charvec = dfforecast.conclude$date),start="2015-05-01",end = "2015-05-31")

plot(taylorplot.ts, ylab = "PJM daily demand", main = "State space model(MNM) result",col = "red",ylim = c(2000,8000))


lines(truedemandplot.ts)
lines(lo80ts,col = "gray")
lines(hi80ts,col = "gray")
lines(lo95ts,col = "azure4")
lines(hi95ts,col = "azure4")
legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))





