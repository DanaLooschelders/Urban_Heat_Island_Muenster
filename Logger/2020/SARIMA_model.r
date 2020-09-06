library(fpp2)
library(tseries)
library(forecast)
library(lmtest)
#for merged temperature in vegetated areas
#plot the merged data
plot(VL_Temp, type="l")
plot(VL_Temp_clean, type="l")

#coerce to time series
VL_Temp_ts=ts(VL_Temp,
              start=c(list_iButton_corr[[1]][1,2]),
              frequency = 24*6)
#clean ts
VL_Temp_clean=tsclean(VL_Temp_ts, replace.missing=T, lambda="auto")
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
acf(VL_Temp_clean, lag.max=200, na.action = "na.pass")
pacf(VL_Temp_clean, lag.max=200, na.action="na.pass")
#check which model would fit
#try auto.arima again with stepwise=F for more accurate results
model_VL=auto.arima(VL_Temp_clean, 
                    seasonal = TRUE)
summary(model_VL)

#Series: VL_Temp_clean 
ARIMA(1,0,3)(0,1,0)[144] 

#Coefficients:
#  ar1    ma1     ma2    ma3
#0.986  0.025  -0.150  0.066
#s.e.  0.003  0.018   0.018  0.017

#sigma^2 estimated as 0.188:  log likelihood=-1850
#AIC=3710   AICc=3710   BIC=3741

#Training set error measures:
 # ME RMSE  MAE    MPE MAPE MASE    ACF1
#Training set 0.0014 0.42 0.27 -0.018  1.5 0.14 0.00051
 

qqnorm(model_VL[["residuals"]])
qqline(model_VL[["residuals"]])
shapiro.test(model_VL[["residuals"]])
#check residual plots
checkresiduals(model_VL)
coeftest(model_VL)
#z test of coefficients:
  
#  Estimate Std. Error z value Pr(>|z|)    
#ar1  0.98643    0.00298  331.02  < 2e-16 ***
 # ma1  0.02465    0.01797    1.37  0.17016    
#ma2 -0.14960    0.01771   -8.45  < 2e-16 ***
 # ma3  0.06633    0.01746    3.80  0.00015 ***
  
confint(model_VL)# -> fitted coefficients and standad errors
#2.5 % 97.5 %
#  ar1  0.981   0.99
#ma1 -0.011   0.06
#ma2 -0.184  -0.11
#ma3  0.032   0.10


#interpretation: 
#AIC and BIC should be low 
#MAE and RMSE --> low is good
#MAPE -> don't use for temperatures
#MASE -> low is good

#maunually refit model
model_refit1=arima(VL_Temp_clean, order=c(1,1,3), seasonal=c(0,1,0))
summary(model_refit1)
#arima(x = VL_Temp_clean, order = c(1, 1, 3), seasonal = c(0, 1, 0))

#Coefficients:
#  ar1     ma1      ma2     ma3
#-0.0487  0.0668  -0.1554  0.0529
#s.e.   0.2940  0.2937   0.0196  0.0503
#sigma^2 estimated as 0.1893:  log likelihood = -1862.51,  aic = 3735.02
#Training set error measures:
#  ME      RMSE       MAE         MPE     MAPE     MASE
#Training set -0.0007672693 0.4255102 0.2696951 -0.01701394 1.533386 1.204081
#ACF1
#Training set 4.596765e-05
checkresiduals(model_refit1)
#residuals still not normally distributed, try again
model_refit2=arima(VL_Temp_clean, order=c(1,2,3), seasonal=c(0,1,0))
summary(model_refit2)

#Call:
#  arima(x = VL_Temp_clean, order = c(1, 2, 3), seasonal = c(0, 1, 0))

#Coefficients:
#  ar1      ma1      ma2     ma3
#-0.3208  -0.6588  -0.4931  0.1519
#s.e.   0.0808   0.0797   0.0726  0.0188

#sigma^2 estimated as 0.1914:  log likelihood = -1878.41,  aic = 3766.82

#Training set error measures:
 # ME      RMSE       MAE          MPE     MAPE     MASE
#Training set 0.0003485993 0.4277068 0.2705385 -0.008420672 1.536804 1.205682
#ACF1
#Training set -0.0013507

qqnorm(model_refit2[["residuals"]])
qqline(model_refit2[["residuals"]])
shapiro.test(model_refit2[["residuals"]])
#check residual plots

model_refit3=arima(VL_Temp_clean, order=c(1,0,3), seasonal=c(0,2,0))
summary(model_refit2)

#Call:
 # arima(x = VL_Temp_clean, order = c(1, 2, 3), seasonal = c(0, 1, 0))

#Coefficients:
#  ar1      ma1      ma2     ma3
#-0.3208  -0.6588  -0.4931  0.1519
#s.e.   0.0808   0.0797   0.0726  0.0188

#sigma^2 estimated as 0.1914:  log likelihood = -1878.41,  aic = 3766.82

#Training set error measures:
 # ME      RMSE       MAE          MPE     MAPE     MASE
#Training set 0.0003485993 0.4277068 0.2705385 -0.008420672 1.536804 1.205682
#ACF1
#Training set -0.0013507
qqnorm(model_refit3[["residuals"]])
qqline(model_refit3[["residuals"]])
shapiro.test(model_refit3[["residuals"]])
#check residual plots

model_refit4=arima(VL_Temp_clean, order=c(4,0,3), seasonal=c(0,2,0))
summary(model_refit4)
qqnorm(model_refit4[["residuals"]])
qqline(model_refit4[["residuals"]])
shapiro.test(model_refit4[["residuals"]])

#for ts mit missing values
#Series: VL_Temp_clean 
#ARIMA(5,0,2)(0,1,0)[144] 

#Coefficients:
#  ar1    ar2     ar3     ar4      ar5     ma1      ma2
#0.5207  0.352  0.1117  0.0229  -0.0278  0.5282  -0.0561
#s.e.     NaN    NaN     NaN     NaN   0.0340     NaN      NaN

#sigma^2 estimated as 0.1859:  log likelihood=-1797.03
#AIC=3610.06   AICc=3610.11   BIC=3658.55

#Training set error measures:
#  ME      RMSE       MAE         MPE     MAPE      MASE
#Training set 0.001192358 0.4206402 0.2672083 -0.02011882 1.533653 0.1363233
#ACF1
#Training set 0.002642887
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
#try auto.arima again with stepwise=F for more accurate results
model_SL=auto.arima(SL_Temp_clean, 
                    seasonal = TRUE,
                    stepwise=F)

#test if resiudals are normally distributed
qqnorm(model_SL[["residuals"]])
qqline(model_SL[["residuals"]])

shapiro.test(model_SL[["residuals"]])

#check residual plot
checkresiduals(model_SL)

summary(model_SL)
#Series: SL_Temp_clean 
#ARIMA(1,0,4)(0,1,0)[144] 

#Coefficients:
#  ar1     ma1     ma2    ma3     ma4
#0.986  -0.198  -0.200  0.070  -0.039
#s.e.  0.003   0.018   0.018  0.017   0.018

#sigma^2 estimated as 0.535:  log likelihood=-3504
#AIC=7021   AICc=7021   BIC=7057

#Training set error measures:
#  ME RMSE  MAE    MPE MAPE MASE   ACF1
#Training set 0.0021 0.72 0.42 -0.056  2.2 0.19 -3e-04

coeftest(model_SL)
#z test of coefficients:

#Estimate Std. Error z value Pr(>|z|)    
# ar1  0.98580    0.00314  313.85  < 2e-16 ***
# ma1 -0.19778    0.01800  -10.99  < 2e-16 ***
# ma2 -0.20007    0.01842  -10.86  < 2e-16 ***
# ma3  0.07007    0.01747    4.01  6.1e-05 ***
# ma4 -0.03871    0.01769   -2.19    0.029 *  

confint(model_SL)# -> fitted coefficients and standad errors
#     2.5 % 97.5 %
#ar1  0.980  0.992
#ma1 -0.233 -0.162
#ma2 -0.236 -0.164
#ma3  0.036  0.104
#ma4 -0.073 -0.004