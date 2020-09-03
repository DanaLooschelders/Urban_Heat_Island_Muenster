#for merged temperature in vegetated areas
#plot the merged data
plot(VL_Temp, type="l")
#coerce to time series
VL_Temp_ts=ts(VL_Temp,
              start=c(list_iButton_corr[[1]][1,2]),
              frequency = 24*6)
#clean ts
VL_Temp_clean=tsclean(VL_Temp_ts)
#plot to check
plot(VL_Temp_clean)
#do unit root test
kpss.test(VL_Temp_clean)
#test assumption -> stationary?
adf.test(VL_Temp_clean, alternative = "stationary") #p-value 0.01 --> stationary
#difference
ndiffs(VL_Temp_clean) #0 -> no differencing required
#test how series looks like if differenced
tsstationary = diff(VL_Temp_clean, differences=1)
plot(tsstationary) #--> looks better when differenced

#check acf and pacf
acf(VL_Temp_clean, lag.max=200)
pacf(VL_Temp_clean, lag.max=200)
#check which model would fit
model_VL=auto.arima(VL_Temp_clean, 
                    seasonal = TRUE)
#output
#ARIMA(0,1,3)(0,1,0)[144]

#Coefficients:
#ma1     ma2    ma3
#0.071  -0.136  0.050
#s.e.   0.025   0.026  0.025

#sigma^2 estimated as 0.212:  log likelihood=-1020
#AIC=2047   AICc=2047   BIC=2069

#test if residuals are normally distributed 
qqnorm(model_VL[["residuals"]])
qqline(model_VL[["residuals"]])
#check residual plots
checkresiduals(model_VL)
#try auto.arima again with stepwise=F for more accurate results
model_VL=auto.arima(VL_Temp_clean, 
                    seasonal = TRUE,
                    stepwise=FALSE)
#output -> same as previous result
#ARIMA(0,1,3)(0,1,0)[144] 

#Coefficients:
#ma1     ma2    ma3
#-0.071  -0.136  0.050
#s.e.   0.025   0.026  0.025

#sigma^2 estimated as 0.212:  log likelihood=-1020
#AIC=2047   AICc=2047   BIC=2069

#interpretation: 
#AIC and BIC should be low 
summary(model_VL)
# set error measures:
#                 ME RMSE  MAE    MPE MAPE MASE    ACF1
#Training set -0.0028 0.44 0.28 -0.025  1.5 0.15 -0.0012

#interpretation:
#MAE and RMSE --> low is good
#MAPE -> don't use for temperatures
#MASE -> low is good
#*********************************************
#for SL_temp
plot(SL_Temp, type="l")
#coerce to time series
SL_Temp_ts=ts(SL_Temp,
              start=c(list_iButton_corr[[1]][1,2]),
              frequency = 24*6)
#clean ts
SL_Temp_clean=tsclean(SL_Temp_ts)
plot(SL_Temp_clean)

#check acf and pacf
acf(SL_Temp_clean, lag.max=200)
pacf(SL_Temp_clean, lag.max=200)

#do unit root test
kpss.test(SL_Temp_clean)
#test assumption -> stationary?
adf.test(SL_Temp_clean, alternative = "stationary") #stationary
#difference
ndiffs(SL_Temp_clean) #0 -> no differencing required

tsstationary = diff(SL_Temp_clean, differences=2)
plot(tsstationary)

#check which model would fit
model_SL=auto.arima(SL_Temp_clean, 
                    seasonal = TRUE)
#output: different than previous result
#ARIMA(2,1,0)(0,1,0)[144] 

#Coefficients:
#  ar1     ar2
#-0.249  -0.232
#s.e.   0.024   0.024

#sigma^2 estimated as 0.476:  log likelihood=-1662
#AIC=3329   AICc=3329   BIC=3345

#test if resiudals are normally distributed
qqnorm(model_SL[["residuals"]])
qqline(model_SL[["residuals"]])
#check residual plot
checkresiduals(model_SL)
#try auto.arima again with stepwise=F for more accurate results
model_SL=auto.arima(SL_Temp_clean, 
                    seasonal = TRUE,
                    stepwise=F)

#output
#ARIMA(2,1,3)(0,1,0)[144] 

#Coefficients:
#  ar1    ar2   ma1   ma2     ma3
#-1.04  -0.62  0.79  0.19  -0.230
#s.e.   0.11   0.12  0.11  0.12   0.042

#sigma^2 estimated as 0.474:  log likelihood=-1657
#AIC=3325   AICc=3325   BIC=3357
summary(model_SL)
#Training set error measures:
#  ME RMSE  MAE    MPE MAPE MASE   ACF1
#Training set -0.0039 0.66 0.39 -0.049  1.9  0.2 0.0051