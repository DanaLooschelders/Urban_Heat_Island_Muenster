#time series analysis
#significant zero crossings 
#install.packages("SiZer")
library(SiZer)
library(tseries)
library(forecast)

#create list with objects of class time series
test=list_iButton_corr_tidy[[2]]
str(test)
test=test[,3]
str(test)
time_series=ts(test, start=c(14), frequency = 24*6)
#frequency -> 24*6 for daily seasonality (10 min data)
#start 14 -> for 14th August 2019
#change start time to day of the year (1 to 365)

test2=list_iButton_corr_tidy[[3]]
str(test2)
test2=test2[,3]
str(test2)
time_series2=ts(test, start=c(14), frequency = 24*6)

#data exploration
any(is.na(time_series)) #check for na -> no missing values

#decompose ts into trend, seasonality and noise
times_series_decomp=decompose(time_series) 
plot(times_series_decomp) #plot decomp ts

#autokorrelation
acf(time_series, lag.max=2000) #autokorreliert
#partielle autokorrelation
pacf(time_series, lag.max=2000)

#test, if ts is stationary
#Augmented Dickey-Fuller Test H0:not stationary H1:stationary
adf_result=adf.test(time_series) #p-value 0.01 -> stationary

#kwiatkowski-Philips-Schmidt-Shin test H0: staionary h1: not stationary
kpss_result=kpss.test(time_series) #p value 0.01

#moving average
ma_ts=ma(time_series, 6)

#ARIMA
arima(time_series)

#SiZer
SiZer_res=SiZer(time_series, time_series2)
