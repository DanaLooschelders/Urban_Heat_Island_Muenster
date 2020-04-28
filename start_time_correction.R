require(zoo)
require(xts)
require(splines)
#interpolate the data to minute intervalls to set all loggers to the same starting point
#create empty vector of minute data for the same timeframe as temp data
#assume linearity and approximate values 
head(list_iButton_corr)

#Create time sequence by minute
date_time_complete <- seq.POSIXt(from=start_time,
                                 to=end_time,by="min") 
str(date_time_complete)

test=xts(list_iButton_corr[[1]][,2],list_iButton_corr[[1]][,3])

test2 = merge(test,date_time_complete)
test2=na.approx(test2)
test2=as.data.frame(test2)
test2$Datetime.1=rownames(test2)
rownames(test2)=NULL
test3=test2[c(rep(FALSE,9),TRUE), ]

#loop through list
date_time_complete <- seq.POSIXt(from=start_time,
                                 to=end_time,by="min") #create minute timeframe
list_iButton_corr_set=list_iButton_corr
list_iButton_corr_set=lapply(list_iButton_corr_set, `[`, 2:3) #use only 2nd and 3rd column
#create new list to use as output
for(i in 1:length(list_iButton_corr)){
  test=xts(list_iButton_corr[[i]][,2],list_iButton_corr[[i]][,3])
  test2=merge(test,date_time_complete)
  test2=na.approx(test2)
  test2=data.frame("Temperature_corr"=test2)
  test2$Datetime.1=rownames(test2)
  rownames(test2)=NULL
  test3=test2[c(rep(FALSE,9),TRUE),]
  list_iButton_corr_set[[i]][1:length(test3[,1]),1:2]=test3[,1:2]
  list_iButton_corr_set[[i]][length(test3[,1]+1),1:2]=NA
}
#eventuell if Schleife einbauen, 
#falls nicht zwangsläufig der letzte Wert ersetzt werden muss

#test spline interpolation
test=xts(list_iButton_corr[[1]][,2],list_iButton_corr[[1]][,3])
test2 = merge(test,date_time_complete)
test_spline=na.spline(test2)
str(test_spline)
test_linear=na.approx(test2)
str(test_linear)

test_spline=as.data.frame(test_spline)
test_spline$Datetime.1=rownames(test_spline)
rownames(test_spline)=NULL
test_spline$Datetime.1=strptime(x = test_spline$Datetime.1, format="%Y-%m-%d %H:%M:%S")
test_spline=test_spline[1:length(test_linear),]

test_linear=as.data.frame(test_linear)
test_linear$Datetime.1=rownames(test_linear)
rownames(test_linear)=NULL
test_linear$Datetime.1=strptime(x = test_linear$Datetime.1, format="%Y-%m-%d %H:%M:%S")

plot(test_linear$Datetime.1, test_linear$test, type="l")
lines(test_spline$Datetime.1, test_spline$test)
diff=test_linear$test-test_spline$test
str(test_linear)
str(test_spline)
plot(diff, type="l")
