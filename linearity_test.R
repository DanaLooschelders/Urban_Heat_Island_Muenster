#time series analysis
#de-seasonalzse the data
#then test for linearity

require(tseries)
library(forecast)
?ts
test=list_iButton_corr_tidy[[1]]
testdata=list_iButton_corr_tidy[[1]][,4]
range=range(list_iButton_corr_tidy[[1]][,3])
ts(data=testdata, start = start_time, end = end_time, frequency=1)
time_series=ts(testdata,start=c(2019,10),frequency=24*6)
time_series=ts(testdata, start=c(14), frequency = 24*6)
plot(time_series)

#set frequency to 24*60 to get a daily seasonality
#number of days into 2019
str(time_series)
des_ts=seasadj(time_series)
#try reading in one series as test
#use raw but tidy data (without offset correction)

ts.stl <- stl(TS,"periodic")  # decompose the TS
ts.sa <- seasadj(ts.stl)  # de-seasonalize
plot(AirPassengers, type="l")  # original series
plot(ts.sa, type="l")  # seasonal adjusted
seasonplot(ts.sa, 12, col=rainbow(12), 
           year.labels=TRUE, main="Seasonal plot: Airpassengers") # seasonal frequency set as 12 for monthly data.