#time series analysis
#significant zero crossings 
#install.packages("SiZer")
#install.packages("fpp2")
library(fpp2)
library(tseries)
library(forecast)

time_series=ts(list_iButton_corr[[1]][2],start=c(list_iButton_corr[[1]][1,2]),frequency = 24*6)

#data exploration
any(is.na(time_series)) #check for na -> no missing values

#decompose ts into trend, seasonality and noise
times_series_decomp=decompose(time_series) 
plot(times_series_decomp) #plot decomp ts

acf(time_series, lag.max=80)
ts_diff=diff(time_series, 1)
plot(ts_diff)

mean(ts_diff) #close to zero
#pacf and acf for differenced data
pacf(ts_diff)
acf(ts_diff)

auto.arima()
model1=arima(time_series,c() )
acuracy(model1)

#moving average
ma_ts=ma(time_series, 6)

#ARIMA
arima(time_series)

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
time_series_clean=tsclean(time_series, replace.missing = TRUE, lambda = "auto")
#plot again
plot(test$Datetime.1, test$time_series_clean, type="l")
#smooth
test$cnt_ma = ma(test$time_series_clean, order=6) #hourly
test$cnt_ma_day = ma(test$time_series_clean, order=14*6) #daily
?ma
ggplot() + 
  geom_line(data = test, aes(x = Datetime.1, y = time_series_clean, colour = "Counts")) +
  geom_line(data = test, aes(x = Datetime.1, y = cnt_ma,   colour = "Hourly Moving Average"))  +
  geom_line(data = test, aes(x = Datetime.1, y = cnt_ma_day, colour = "Daily Moving Average"))  +
  ylab('Temperature') 

#2. decompose data
decomp_ts=decompose(test$time_series_clean)
plot(decomp_ts)

#calculate seasonal component
count_ma = ts(na.omit(test$cnt_ma), frequency=6*24) 
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp) 

#test assumption -> stationary?
adf.test(test$time_series_clean, alternative = "stationary") #stationary

#autokorrelation
acf(test$time_series_clean, lag.max=20) #autokorreliert
#partielle autokorrelation
pacf(test$time_series_clean, lag.max=20)
test$log_data=log(test$time_series_clean)
plot(test$log_data)




decompose()
?arima
arima(test$time_series_clean)
#SARIMA (p,d,q)(P,D,Q)m
#order (non-seasonal part) 
  #p -> auto regression model (AR)
  #d -> degree of differencing (I)
  #q -> moving average order/number of terms to include (MA)
  #P -> seasonal AR order
  #D -> seasonal differencing
  #Q -> seasonal MA order
  #m -> time span of repeating seasonal pattern

#when using auto.arima:
auto.arima(time_series_clean) 
#result: ARIMA(3,0,0)(0,1,0)[144] 

#AIC=3598   AICc=3598   BIC=3619
#with fpp2 package from Scott (YT video)
model1=Arima(time_series_clean, order=c(3,0,0), seasonal = c(0,1,0))

(model1=Arima(time_series_clean, order=c(3,0,0), seasonal = c(0,1,0)))
#Series: time_series_clean 
#ARIMA(3,0,0)(0,1,0)[144] 
#Coefficients:
#  ar1     ar2    ar3
#0.783  -0.036  0.223
#s.e.  0.027   0.035  0.027
#sigma^2 estimated as 0.935:  log likelihood=-1795
#AIC=3598   AICc=3598   BIC=3619
#
#visible pattern/bias -> plot ACF/PACF
checkresiduals(model1)
#refit model if needed

#use stepwise=FALSE and approximation=FALSE to make it twork harder
auto.arima(time_series_clean, 
           seasonal = TRUE,
           stepwise =FALSE,
           approximation=FALSE) 



model1=Arima(time_series_clean, order=c(3,0,0), seasonal = c(0,1,0))

(model1=Arima(time_series_clean, order=c(3,0,0), seasonal = c(0,1,0)))

#check residuals (no pattern, normally distributed)
#visible pattern/bias -> plot ACF/PACF
checkresiduals(model2)
#refit model if needed
