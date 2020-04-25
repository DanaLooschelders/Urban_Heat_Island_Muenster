#time series analysis
#de-seasonalzse the data
#then test for linearity

#try reading in one series as test
#use raw but tidy data (without offset correction)
require(tseries)
library(forecast)
test=list_iButton_corr_tidy[[1]]
testdata=list_iButton_corr_tidy[[1]][,4]
time_series=ts(testdata, start=c(14), frequency = 24*6)
  #frequency -> 24*6 for daily seasonality (10 min data)
  #start 14 -> for 14th August 2019
  #change start time to day of the year (1 to 365)
plot(time_series)
str(time_series)
#decompose time series
time_series_decomp=stl(time_series, "periodic", na.action = na.exclude)


#try with raw data (only offset corrected, but with spikes)
testdata=list_iButton_corr[[1]][,4] #use one temperature series as test
time_series=ts(testdata, start=c(14), frequency = 24*6) #create time series
plot(time_series) #plot ts
any(is.na(time_series)) #check for na -> no missing values
time_series_decomp=stl(time_series, "periodic") #decompose ts
plot(time_series_decomp) #plot decomp ts

times_series_decomp2=decompose(time_series) #decomose ts with another function
plot(times_series_decomp2) #plot decomp ts

time_series_sa=seasadj(time_series_decomp) #adjust for seasonality
plot(time_series_sa) #plot deseasonlized ts

#run decomposition for all data
str(list_iButton_corr)
#create column for julien day
day=lapply(list_iButton_corr, '[',3)
day_of_year=lapply(day, function (x) as.integer(strftime(x$Datetime.1, format = "%j")))
list_iButton_corr <- mapply(cbind, list_iButton_corr, "Day"=day_of_year, SIMPLIFY=F)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/plots_ts_14.08")

for (i in 1:length(list_iButton_corr)){
  if (!any(is.na(list_iButton_corr[[i]][,4]))){
  dataname=names(list_iButton_corr)[i] #save logger ID 
  name=paste("time_series","plot",dataname,".pdf") #set filename
  data=list_iButton_corr[[i]][,4] #retrieve column with temperature data from list
  time_series=ts(data, start=c(list_iButton_corr[[i]][1,5]), frequency = 24*6) #create time series
  time_series_decomp=stl(time_series, "periodic") #decompose ts
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(time_series_decomp) #plot decomp ts
  dev.off()
}else{}
}
savehistory()
save()
save.image()
