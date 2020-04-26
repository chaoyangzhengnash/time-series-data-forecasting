#
# Program: groupproject_TBATS.R
#
# Purpose: To make the TBATS model and mesure its performance in one month validation dataset. 
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

# Defining in.sampe and out.sample data

in.sample <- msts(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready), 
                         start = "2011-01-01", end ="2014-12-31"),seasonal.periods=c(7,364))  

out.sample <- msts(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                          start = "2015-01-01", end ="2016-12-31"),seasonal.periods=c(7,364))


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
  fit <- tbats(in.sample, use.box.cox=NULL, use.parallel=TRUE, num.cores = NULL, 
               use.trend=NULL, use.damped.trend=NULL, use.arma.errors=TRUE,                           
               model=NULL,seasonal.periods = 7 )
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

dfforecast.conclude<- read.csv("forecast.conclude.csv")
Taylordemand.n <- dfforecast.conclude$forecast
# Computing bias and MAPE
bias.R <- mean(Taylordemand.n-truedemand.n)
pbias.R <- mean((Taylordemand.n-truedemand.n)/truedemand.n)*100
mape.R <- mean(abs((Taylordemand.n-truedemand.n)/truedemand.n))*100

# Plot the performance of TBATS model

taylorplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = Taylordemand.n)
taylorplot.ts <- timeSeries(taylorplot$demand,taylorplot$date)
truedemandplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = truedemand.n)
truedemandplot.ts<- timeSeries(truedemandplot$demand,truedemandplot$date)

dfforecast.conclude$date = as.Date(dfforecast.conclude$date)
lo80ts<- timeSeries(dfforecast.conclude$lo80,charvec = dfforecast.conclude$date)
hi80ts<- timeSeries(dfforecast.conclude$hi80,charvec = dfforecast.conclude$date)
lo95ts<- timeSeries(dfforecast.conclude$lo95,charvec = dfforecast.conclude$date)
hi95ts<- timeSeries(dfforecast.conclude$lo95,charvec = dfforecast.conclude$date)

plot(taylorplot.ts, ylab = "PJM daily demand", main = "TBATS result",col = "red",ylim = c(2000,8000))


lines(truedemandplot.ts)
lines(lo80ts,col = "gray")
lines(hi80ts,col = "gray")
lines(lo95ts,col = "azure4")
lines(hi95ts,col = "azure4")
lines(truedemandplot.ts)

legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))

#Plot the residual of TBATS (Only one iteration)

in.sample <- window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready), 
                    start = "2013-01-01", end ="2014-12-30")
demand.train <- in.sample$TS.1

duang <- as.numeric(fit$fitted.values[(364*2+2):(364*4+2)])

Refdate.R<- data.frame(date = full_dataset$date_ready[c(2920:3648)])
Refdate.R$date <- as.Date(Refdate.R$date)

rua <- data.frame(residual = demand.train- duang, date = Refdate.R$date )

plot(timeSeries(rua$residual,rua$date), ylab = "residual", main =  "Residual plot of TBATS bwtween 2013 and 2014")


# To get the TBATS performance in the winter of test dataset 


test.sample <- msts(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                          start = "2017-01-01", end ="2018-12-31"),seasonal.periods=c(7,364))

# To update the reference date 

Refdate <- data.frame(date = full_dataset$date_ready[c(4378:5105)])
Refdate.t<- data.frame(date = full_dataset$date_ready[c(3650:5105)])


Taylordate.p<-list()
Taylordemand.p<-list()
lo.80 <- list()
hi.80 <- list()
lo.95 <- list()
hi.95 <- list()

#length(Refdate$date)

for(i in 1:length(Refdate$date)){
  fit <- tbats(out.sample, use.box.cox=NULL, use.parallel=TRUE, num.cores = NULL, 
               use.trend=NULL, use.damped.trend=NULL, use.arma.errors=TRUE,                           
               model=NULL,seasonal.periods = 7 )
  fore<- forecast(fit,h=1) 
  Taylordemand.p[i] <- as.numeric(fore[[2]][1])
  Taylordate.p[i]<- as.character(Refdate$date[i])
  lo.80[i] <-fore$lower[1]
  hi.80[i] <-fore$upper[1]
  lo.95[i] <-fore$lower[2]
  hi.95[i] <-fore$upper[2]
  
  Rua <- append(out.sample[-1,],test.sample[i])
  sua <- data.frame(date = Refdate.t$date[(1+i):(728+i)], demand = Rua) #728
  out.sample <- msts(timeSeries(sua$demand,sua$date),seasonal.periods=c(7,364))
  i = i+1
}
#Concluding table of forecasts in validation dataset

Taylordemand.n <- as.numeric( Taylordemand.p)
Taylordate.n <- as.character(Taylordate.p)
lo.80num <-as.numeric(lo.80)
hi.80num <-as.numeric(hi.80)
lo.95num <-as.numeric(lo.95)
hi.95num <-as.numeric(hi.95)

test.conclude <- data.frame(date=Taylordate.n, forecast = Taylordemand.n,
                                lo80=lo.80num,hi80=hi.80num,lo95=lo.95num,hi95=hi.95num)
write.csv(test.conclude,file = "test.conclude") 

# Run the tabats model in test dataset 
# Data loading
#full_dataset <- read.csv("full_dataset.csv")
#full_dataset$date_ready<- as.Date(full_dataset$date_ready)

# Defining test dataset

in.sample <- msts(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready), 
                         start = "2011-01-01", end ="2014-12-31"),seasonal.periods=c(7,364))
test.sample <- msts(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready), 
                    start = "2017-01-01", end ="2018-12-31"),seasonal.periods=c(7,364))

Refdate <- data.frame(date = full_dataset$date_ready[c(4378:5105)])
Refdate.t<- data.frame(date = full_dataset$date_ready[c(2191:4377)])

Taylordate.p<-list()
Taylordemand.p<-list()
lo.80 <- list()
hi.80 <- list()
lo.95 <- list()
hi.95 <- list()

#length(Refdate$date)

for(i in 1:length(Refdate$date)){
  fit <- tbats(in.sample, use.box.cox=NULL, use.parallel=TRUE, num.cores = NULL, 
               use.trend=NULL, use.damped.trend=NULL, use.arma.errors=TRUE,                           
               model=NULL,seasonal.periods = 7 )
  fore<- forecast(fit,h=1) 
  Taylordemand.p[i] <- as.numeric(fore[[2]][1])
  Taylordate.p[i]<- as.character(Refdate$date[i])
  lo.80[i] <-fore$lower[1]
  hi.80[i] <-fore$upper[1]
  lo.95[i] <-fore$lower[2]
  hi.95[i] <-fore$upper[2]
  
  Rua <- append(in.sample[-1,],test.sample[i])
  sua <- data.frame(date = Refdate.t$date[(1+i):(1459+i)], demand = Rua) #1459
  in.sample <- msts(timeSeries(sua$demand,sua$date),seasonal.periods=c(7,364))
  i = i+1
}
#Concluding table of forecasts in test dataset

Taylordemand.n <- as.numeric( Taylordemand.p)
Taylordate.n <- as.character(Taylordate.p)
lo.80num <-as.numeric(lo.80)
hi.80num <-as.numeric(hi.80)
lo.95num <-as.numeric(lo.95)
hi.95num <-as.numeric(hi.95)

test.conclude <- data.frame(date=Taylordate.n, forecast = Taylordemand.n,
                            lo80=lo.80num,hi80=hi.80num,lo95=lo.95num,hi95=hi.95num)
write.csv(test.conclude,file = "realtest.conclude") 

####
#dfforecast.conclude<- read.csv("forecast.conclude.csv")

truedemand.n<- as.numeric(full_dataset$demand_ready[c(4378:5105)])

Taylordemand.n <- test.conclude$forecast
# Computing bias and MAPE
bias.R <- mean(Taylordemand.n-truedemand.n)
pbias.R <- mean((Taylordemand.n-truedemand.n)/truedemand.n)*100
mape.R <- mean(abs((Taylordemand.n-truedemand.n)/truedemand.n))*100


# Plot the performance of TBATS model in the real test data

taylorplot <- data.frame(date = full_dataset$date_ready[c(4378:5105)],demand = Taylordemand.n)
taylorplot.ts <- timeSeries(taylorplot$demand,taylorplot$date)
truedemandplot <- data.frame(date = full_dataset$date_ready[c(4378:5105)],demand = truedemand.n)
truedemandplot.ts<- timeSeries(truedemandplot$demand,truedemandplot$date)

test.conclude$date = as.Date(test.conclude$date)
lo80ts<- timeSeries(test.conclude$lo80,charvec = test.conclude$date)
hi80ts<- timeSeries(test.conclude$hi80,charvec = test.conclude$date)
lo95ts<- timeSeries(test.conclude$lo95,charvec = test.conclude$date)
hi95ts<- timeSeries(test.conclude$lo95,charvec = test.conclude$date)

plot(taylorplot.ts, ylab = "PJM daily demand", main = "TBATS result in the test dataset",col = "red",ylim = c(2000,8000))


lines(truedemandplot.ts)
lines(lo80ts,col = "gray")
lines(hi80ts,col = "gray")
lines(lo95ts,col = "azure4")
lines(hi95ts,col = "azure4")
lines(truedemandplot.ts)

legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))







