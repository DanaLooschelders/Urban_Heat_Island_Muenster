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

#***********************************************************************
#use method from paper
#with VL_temp and SL_temp
plot(VL_Temp, type="l")
#coerce to time series
VL_Temp_ts=ts(VL_Temp,
              start=c(list_iButton_corr[[1]][1,2]),
              frequency = 24*6)
#clean ts
VL_Temp_clean=tsclean(VL_Temp_ts)
#check acf and pacf
acf(VL_Temp_clean, lag.max=200)
pacf(VL_Temp_clean, lag.max=200)
#check which model would fit
model_VL=auto.arima(VL_Temp_clean, 
                    seasonal = TRUE)
#ARIMA(0,1,3)(0,1,0)[144]

#Coefficients:
#ma1     ma2    ma3
#0.071  -0.136  0.050
#s.e.   0.025   0.026  0.025

#sigma^2 estimated as 0.212:  log likelihood=-1020
#AIC=2047   AICc=2047   BIC=2069

#test resiudals
qqnorm(model_VL[["residuals"]])
qqline(model_VL[["residuals"]])

checkresiduals(model_VL)

model_VL=auto.arima(VL_Temp_clean, 
                    seasonal = TRUE,
                    stepwise=F)
#ARIMA(0,1,3)(0,1,0)[144] 

#Coefficients:
#ma1     ma2    ma3
#-0.071  -0.136  0.050
#s.e.   0.025   0.026  0.025

#sigma^2 estimated as 0.212:  log likelihood=-1020
#AIC=2047   AICc=2047   BIC=2069
#*********************************************
#for SL_temp
plot(SL_Temp, type="l")
#coerce to time series
SL_Temp_ts=ts(SL_Temp,
              start=c(list_iButton_corr[[1]][1,2]),
              frequency = 24*6)
#clean ts
SL_Temp_clean=tsclean(SL_Temp_ts)
#check acf and pacf
acf(SL_Temp_clean, lag.max=200)
pacf(SL_Temp_clean, lag.max=200)
#check which model would fit
model_SL=auto.arima(SL_Temp_clean, 
                    seasonal = TRUE)
#ARIMA(2,1,0)(0,1,0)[144] 

#Coefficients:
#  ar1     ar2
#-0.249  -0.232
#s.e.   0.024   0.024

#sigma^2 estimated as 0.476:  log likelihood=-1662
#AIC=3329   AICc=3329   BIC=3345

#test resiudals
qqnorm(model_SL[["residuals"]])
qqline(model_SL[["residuals"]])

checkresiduals(model_SL)

model_SL=auto.arima(SL_Temp_clean, 
                    seasonal = TRUE,
                    stepwise=F)
 
#ARIMA(2,1,3)(0,1,0)[144] 

#Coefficients:
#  ar1    ar2   ma1   ma2     ma3
#-1.04  -0.62  0.79  0.19  -0.230
#s.e.   0.11   0.12  0.11  0.12   0.042

#sigma^2 estimated as 0.474:  log likelihood=-1657
#AIC=3325   AICc=3325   BIC=3357