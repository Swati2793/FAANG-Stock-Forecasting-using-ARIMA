#libraries
library("forecast")     # 
library("tseries") 		  # reqired for adf.test of stationarity
library("ggplot2")      # to plot  
library("DataCombine")


#loading files for FAANG companies
aapl <- read.csv("AAPL.csv")
amzn <- read.csv("AMZN.csv")
fb <- read.csv("FB.csv")
goog <- read.csv("GOOG.csv")
nflx <- read.csv("NFLX.csv")


#creating timeseries out of the files

aapl_ts = ts(aapl$Close, frequency = 251,start = c(2019,1))		# coverts sales data as time series object with start date and frequency (weekly here)
#plot.ts(aapl_ts)												# ALWAYS plot time series to see patterns: trend, cycle, variance over time
autoplot(aapl_ts)


#Determine d , Check if trend is stationary
adf.test(aapl_ts)
# not stationary,as p > 0.1. Take difference
diff_1 <- diff(aapl_ts,1)
adf.test(diff_1)  
# p < 0.1 , thus we take I = 1

#Determine p,q
pacf(diff_1,lag.max = 10) # 2 reveals?
#dev.off()
acf(diff_1,lag.max = 10) # 1 reveal

#?arima

m1 = Arima(aapl_ts, order=c(2),2,1))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)
m2 = Arima(aapl_ts, order=c(1,1,0))			

m1.predict = forecast:::forecast.Arima(m1, h = 30, level = c(68, 90))
plot(m1.predict,title = "Apple orecast plot")


m6 = auto.arima(aapl_ts)		# fits ARIMA(p,d,q) x (P, D, Q) automatically

m6.predict = forecast:::forecast.Arima(m6, h = 15, level = c(68, 90))
plot(m6.predict,title = "Apple orecast plot")

summary(m6)
aapl_pred = m6.predict$mean[]
aapl_pred[11:15]



# Amazon
amzn_ts = ts(amzn$Close, frequency = 251,start = c(2019,1))
# coverts sales data as time series object with start date and frequency (weekly here)
#plot.ts(aapl_ts)												# ALWAYS plot time series to see patterns: trend, cycle, variance over time
autoplot(amzn_ts)

#Determine d , Check if trend is stationary
adf.test(amzn_ts)
# not stationary,as p > 0.1. Take difference
diff_1 <- diff(amzn_ts,1)
adf.test(diff_1)  

#Determine p,q
pacf(diff_1,lag.max = 10) # 0 reveals?
#dev.off()
acf(diff_1,lag.max = 25) # 21 reveal

m1 = Arima(amzn_ts, order=c(2,1,1))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)
m1.predict = forecast:::forecast.Arima(m1, h = 15, level = c(68, 90))
summary(m1)
plot(m1.predict)
amzn_pred <- m1.predict$mean

#facebook
fb_ts = ts(fb$Close,frequency = 251,start = c(2019,1))
autoplot(fb_ts)

#Determine d , Check if trend is stationary
adf.test(fb_ts)
# not stationary,as p > 0.1. Take difference
diff_1 <- diff(fb_ts,1)
adf.test(diff_1)  

#Determine p,q
pacf(diff_1,lag.max = 10) # 0 reveals?
#dev.off()
acf(diff_1,lag.max = 25) # 1 reveal

m1 = Arima(fb_ts, order=c(1,1,1))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)
m1.predict = forecast:::forecast.Arima(m1, h = 15, level = c(68, 90))
summary(m1)
plot(m1.predict)

  m1.predict$mean

  m7 = auto.arima(fb_ts)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
  
  m7.predict = forecast:::forecast.Arima(m7, h = 15, level = c(68, 90))
  plot(m7.predict)
fb_pred <- m7.predict$mean

    
# google
goog_ts = ts(goog$Close,frequency = 251,start = c(2019,1))
autoplot(goog_ts)

diff_1 <- diff(goog_ts,2)
adf.test(diff_1)  

#Determine p,q
pacf(diff_1) # 4 reveals?
#dev.off()
acf(diff_1) # 3 reveal, 2


m1 = Arima(goog_ts, order=c(0,2,1))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)
m1.predict = forecast:::forecast.Arima(m1, h = 15, level = c(68, 90))
summary(m1)
plot(m1.predict)

m1.predict$mean

goog_pred <-  m1.predict$mean


#auto arima
m8 = auto.arima(goog_ts)		# fits ARIMA(p,d,q) x (P, D, Q) automatically

m8.predict = forecast:::forecast.Arima(m8, h = 15, level = c(68, 90))
plot(m8.predict)
goog_pred <- m8.predict$mean


#netflix

nflx_ts = ts(nflx$Close,frequency = 251,start = c(2019,1))
autoplot(nflx_ts)
autoplot(diff_1)


decompose(nflx_ts)
gglagplot(nflx_ts)

adf.test(nflx_ts)  

diff_1 <-  diff(nflx_ts,1)
adf.test(diff_1)
# thus ,d = 1

pacf(diff_1) # 0 reveals, seasonal at 6?
#dev.off()
acf(diff_1,lag.max = 100) # 1 reveal, 2

#arima model

m1 = Arima(nflx_ts, order=c(0,1,1))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)
m1.predict = forecast:::forecast.Arima(m1, h = 15, level = c(68, 90))
summary(m1)
plot(m1.predict)
m1.predict$mean


#taking seasonality into account
m1 = Arima(nflx_ts, order=c(0,1,2), seasonal = list(order = c(4,6,0), period = 251))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)
m1.predict = forecast:::forecast.Arima(m1, h = 15, level = c(68, 90))
summary(m1)
plot(m1.predict)
m1.predict$mean[6:10]
stl(nflx_ts)

m9 = auto.arima(nflx_ts,seasonal = TRUE)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
m9.predict = forecast:::forecast.Arima(m9, h = 15, level = c(68, 90))
plot(m9.predict)
#auto arima yields mean as the model
#summary(m9)


nflx_pred <- m9.predict$mean

#FInal 5 x 5 matrix
Result_matrix <- rbind(aapl_pred,amzn_pred,fb_pred,goog_pred,nflx_pred)

View(Result_matrix)


