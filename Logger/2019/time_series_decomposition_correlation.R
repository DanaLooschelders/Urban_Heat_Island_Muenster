#time series analysis
#significant zero crossings 
#install.packages("SiZer")
library(SiZer)
library(tseries)
library(forecast)

#create list with objects of class time series
test=list_iButton_corr_tidy[[1]]
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
mean_TS=mean(time_series, time_series2)

#Do a proper ARIMA
#create list with objects of class time series
test=list_iButton_corr_tidy[[1]]
str(test)
range(test$Datetime.1)
test_cut=test[,3]
plot(test_cut, type="l")
time_series=ts(test_cut, start=c(7), frequency = 24*6)
str(time_series)
plot(time_series)
#frequency -> 24*6 for daily seasonality (10 min data)
#start 7 -> for 7th July 2020
#change start time to day of the year (1 to 365)


#Exploratory analysis
#1. autocorrelation analysis (serial dependence) -> p,d,q estimates
#2. spectral analysis (cyclic behavior) 
#3. trend estimation and decomposition


#from https://blogs.oracle.com/datascience/introduction-to-forecasting-with-arima-in-r

#1. Examine data 
#plot data
plot(test$Datetime.1, test$Temperature_C_w_off,type="l")
#clean outliers/missing values
test$time_series_clean=tsclean(x=time_series, replace.missing = TRUE, lambda = "auto")
#plot again
plot(test$Datetime.1, test$time_series_clean, type="l")
#smooth
test$cnt_ma = ma(test$time_series_clean, order=14*24) #hourly
test$cnt_ma_day = ma(test$time_series_clean, order=14) #daily

ggplot() + 
  geom_line(data = test, aes(x = Datetime.1, y = time_series_clean, colour = "Counts")) +
  geom_line(data = test, aes(x = Datetime.1, y = cnt_ma,   colour = "Hourly Moving Average"))  +
  geom_line(data = test, aes(x = Datetime.1, y = cnt_ma_day, colour = "Daily Moving Average"))  +
  ylab('Temperature') 

#2. decompose data
decomp_ts=decompose(time_series_clean)
plot(decomp_ts)

#calculate seasonal component
count_ma = ts(na.omit(test$cnt_ma), frequency=14*24) 
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp) 

#test assumption -> stationary?
adf.test(time_series_clean, alternative = "stationary") #stationary

#autokorrelation
acf(time_series_clean, lag.max=2000) #autokorreliert
#partielle autokorrelation
pacf(time_series_clean, lag.max=2000)

arima(time_series_clean)
#check residuals (no pattern, normally distributed)
#visible pattern/bias -> plot ACF/PACF
#refit model if needed