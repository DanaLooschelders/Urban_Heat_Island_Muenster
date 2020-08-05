#time series analysis
#de-seasonalzse the data
#then test for linearity

#use raw but tidy data (without offset correction)
require(tseries)
library(forecast)
  #frequency -> 24*6 for daily seasonality (10 min data)
  #start 14 -> for 14th August 2019
  #change start time to day of the year (1 to 365)
#with raw data (only offset corrected, but with spikes)

#create column for julien day
day=lapply(list_iButton_corr_set, '[',2)
day_of_year=lapply(day, function (x) as.integer(strftime(x$Datetime.1, format = "%j")))
list_iButton_corr <- mapply(cbind, list_iButton_corr, "Day"=day_of_year, SIMPLIFY=F)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/time_series")

for (i in 1:length(list_iButton_corr)){
  if (!any(is.na(list_iButton_corr[[i]][,1]))){
  dataname=names(list_iButton_corr)[i] #save logger ID 
  name=paste(substring(list_iButton_corr[[i]][1,2], 1,10),"time series plot",dataname,".pdf") #set filename
  data=list_iButton_corr[[i]][,1] #retrieve column with temperature data from list
  time_series=ts(data, start=c(list_iButton_corr[[i]][1,1]), frequency = 24*6) #create time series
  time_series_decomp=stl(time_series, "periodic") #decompose ts
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(time_series_decomp, #plot decomp ts
       main=paste("decomposed time series of logger", 
                  names(list_iButton_corr)[i],
                  metadata$Standort[metadata$Logger_ID==as.numeric(names(list_iButton_corr)[i])])) 
  dev.off()
}else{}
}
