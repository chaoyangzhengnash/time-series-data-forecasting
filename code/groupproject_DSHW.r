#
# Program: groupproject_DHSW.R
#
# Purpose: To make the DHSW model and mesure its performance in one year validation dataset. 
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
                         start = "2011-01-01", end ="2014-12-31"),seasonal.periods=c(7,364))  

out.sample <- msts(window(timeSeries(full_dataset$demand_ready, full_dataset$date_ready),
                          start = "2015-01-01", end ="2016-12-31"),seasonal.periods=c(7,364))


Refdate <- data.frame(date = full_dataset$date_ready[c(3650:4377)])# From validation 
Refdate.t<- data.frame(date = full_dataset$date_ready[c(2191:4377)])


Taylordate.p<-list()
Taylordemand.p<-list()


for(i in 1:length(Refdate$date) ){
  Taylor <- dshw(in.sample,period1=7, period2=364, alpha = NULL, beta = NULL, gamma = NULL, 
                 omega = NULL,phi = NULL, lambda = NULL, biasadj = FALSE, armethod = TRUE,model = NULL)
  
  fore<- forecast(Taylor,h=1) 
  Taylordemand.p[i] <- as.numeric(fore[1])
  Taylordate.p[i]<- as.character(Refdate$date[i])
  
  Rua <- append(in.sample[-1,],out.sample[i])
  sua <- data.frame(date = Refdate.t$date[(1+i):(1459+i)], demand = Rua) #1459
  in.sample <- msts(timeSeries(sua$demand,sua$date),seasonal.periods=c(7,364))
  i = i+1
}
  


# Caculating the model's performance in validation dataset 

Taylordemand.n <- as.numeric( Taylordemand.p)
#Taylordemand.n <- Taylordemand.n[1:365]

truedemand.n<- as.numeric(full_dataset$demand_ready[c(3650:4377)])

### To make life easier, we save the result in the file "forecast.conclude"
dfforecast.conclude<- read.csv("dshwresult.csv")
Taylordemand.n <- dfforecast.conclude$forecast


# Computing bias and MAPE
bias.R <- mean(Taylordemand.n-truedemand.n)
pbias.R <- mean((Taylordemand.n-truedemand.n)/truedemand.n)*100
mape.R <- mean(abs((Taylordemand.n-truedemand.n)/truedemand.n))*100

# Plot the result 

taylorplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = Taylordemand.n)
taylorplot.ts <- timeSeries(taylorplot$demand,taylorplot$date)
truedemandplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = truedemand.n)
truedemandplot.ts<- timeSeries(truedemandplot$demand,truedemandplot$date)

plot(taylorplot.ts, ylab = "PJM daily demand", main = "DSHW result",col = "red")
lines(truedemandplot.ts)
legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))





# Plot the performance of MNM model in two weeks 
window(timeSeries(dfforecast.conclude$lo80,charvec = dfforecast.conclude$date),start="2015-05-01",end = "2015-05-31")

taylorplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = Taylordemand.n)
taylorplot.ts <- window(timeSeries(taylorplot$demand,taylorplot$date),start="2015-05-01",end = "2015-05-31")
truedemandplot <- data.frame(date = full_dataset$date_ready[c(3650:4377)],demand = truedemand.n)
truedemandplot.ts<- window(timeSeries(truedemandplot$demand,truedemandplot$date),start="2015-05-01",end = "2015-05-31")

plot(taylorplot.ts, ylab = "PJM daily demand", main = "DSHW result",col = "red",ylim = c(2000,8000))
lines(truedemandplot.ts)
legend("bottomright",legend=c("fore","obs"),lty=c(1,1),
       col=c("red","black"))

