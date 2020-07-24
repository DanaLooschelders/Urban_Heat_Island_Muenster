#time series analysis
#significant zero crossings 
#install.packages("SiZer")
library(SiZer)
library(tseries)
library(forecast)


#data exploration
any(is.na(time_series)) #check for na -> no missing values

#decompose ts into trend, seasonality and noise
times_series_decomp=decompose(time_series) 
plot(times_series_decomp) #plot decomp ts


#moving average
ma_ts=ma(time_series, 6)

#ARIMA
arima(time_series)

#SiZer
SiZer_res=SiZer(time_series, time_series2)
mean_TS=mean(time_series, time_series2)

#Do a proper SARIMA
#create list with objects of class time series
test=list_iButton_corr_tidy[[1]] #first vector as test
#use only temp
test_cut=test[,3]
time_series=ts(test_cut, start=c(7), frequency = 24*6)
#frequency -> 24*6 for daily seasonality (10 min data) 
#because daily seasonality, with 24 hour in a day * 6 measurements in an hour
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
acf(time_series_clean, lag.max=200) #autokorreliert
#partielle autokorrelation
pacf(time_series_clean, lag.max=2000)
?arima
arima(time_series_clean)
#order (non-seasonal part) 
  #p -> auto regression model
  #d -> degree of differencing
  #q -> moving average order/number of terms to include
#check residuals (no pattern, normally distributed)
#example from book with:
plot(y=rstudent(model3),x=as.vector(time(tempdub)),
     xlab='Time',ylab='Standardized Residuals',type='o')

#visible pattern/bias -> plot ACF/PACF
#refit model if needed