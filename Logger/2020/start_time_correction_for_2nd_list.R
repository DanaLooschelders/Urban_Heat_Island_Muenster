require(zoo)
require(xts)
require(splines)

#interpolate the data to minute intervalls to set all loggers to the same starting point
#create empty vector of minute data for the same timeframe as temp data
#assume linearity and approximate values 
head(list_second_iButton_corr)

#Create time sequence by minute
#loop through list
date_time_complete <- seq.POSIXt(from=start_time,
                                 to=end_time,by="min") #create minute timeframe
list_second_iButton_corr_set=list_second_iButton_corr
list_second_iButton_corr_set=lapply(list_second_iButton_corr_set, `[`, 1:2) #use only 2nd and 3rd column
#create new list to use as output
for(i in 1:length(list_second_iButton_corr)){
  #create time series with datetime and temperature
  test=xts(list_second_iButton_corr[[i]][,1],list_second_iButton_corr[[i]][,2]) 
  #merge logger time series with emtpy one minute time series
  test2=merge(test,date_time_complete)
  #replace NA values (created by merging with higher res) with spline interpolated values
  test2=na.spline(test2)
  test2=data.frame("Temperature_corr"=test2) #name the new column
  test2$Datetime.1=rownames(test2) #use the newly set times to replace previous time data
  rownames(test2)=NULL #delete rownames
  test3=test2[c(TRUE,rep(FALSE,9)),] #keep only every 10th value to get 10min res
  test3$test=round(test3$test/.5)*.5 #round to .5 decimal place 
  list_second_iButton_corr_set[[i]][1:length(test3[,1]),1:2]=test3[,1:2] #replace the time and temp column with the new values
  list_second_iButton_corr_set[[i]]=list_second_iButton_corr_set[[i]][-length(test3[,1]+1),1:2] #delete last row in every dataframe (sometimes NA)
}

rm(test, test2, test3)
#test spline interpolation
#test=xts(list_second_iButton_corr[[5]][,2],list_second_iButton_corr[[5]][,3])
#test2 = merge(test,date_time_complete)
#test_spline=na.spline(test2)
#str(test_spline)
#test_linear=na.approx(test2)
#str(test_linear)

#test_spline=as.data.frame(test_spline)
#test_spline$Datetime.1=rownames(test_spline)
#rownames(test_spline)=NULL
#test_spline$Datetime.1=strptime(x = test_spline$Datetime.1, format="%Y-%m-%d %H:%M:%S")
#test_spline=test_spline[1:length(test_linear),]

#test_linear=as.data.frame(test_linear)
#test_linear$Datetime.1=rownames(test_linear)
#rownames(test_linear)=NULL
#test_linear$Datetime.1=strptime(x = test_linear$Datetime.1, format="%Y-%m-%d %H:%M:%S")

#plot(test_linear$Datetime.1, test_linear$test, type="l", col="green")
#lines(test_spline$Datetime.1, test_spline$test, col="red")
#diff=test_linear$test-test_spline$test
#str(test_linear)
#str(test_spline)
#plot(diff, type="l")

#qqnorm(test_linear$test)
#qqline(test_linear$test)

#wilcox.test(test_linear$test, test_spline$test)
