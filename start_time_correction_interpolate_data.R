require(zoo)
require(xts)
#interpolate the data to minute intervalls to set all loggers to the same starting point
#create empty vector of minute data for the same timeframe as temp data
#assume linearity and approximate values 
head(list_iButton_corr)

#Create time sequence by minute
date_time_complete <- seq.POSIXt(from=start_time,
                                 to=end_time,by="min") 
str(date_time_complete)
str(test)

test=xts(list_iButton_corr[[1]][,4],list_iButton_corr[[1]][,3])

test2 = merge(test,xts(,date_time_complete))
test2=na.approx(test2)
test2=as.data.frame(test2)
test2$Datetime.1=rownames(test2)
rownames(test2)=NULL
test3=test2[c(rep(FALSE,9),TRUE), ]

#loop through list
date_time_complete <- seq.POSIXt(from=start_time,
                                 to=end_time,by="min") #create minute timeframe
list_iButton_corr_set=list_iButton_corr #create new list to use as output
for(i in 1:length(list_iButton_corr)){
  test=xts(list_iButton_corr[[i]][,4],list_iButton_corr[[i]][,3])
  test2=merge(test,xts(,date_time_complete))
  test2=na.approx(test2)
  test2=as.data.frame("Temperature_corr"=test2)
  test2$Datetime.1=rownames(test2)
  rownames(test2)=NULL
  test3=test2[c(rep(FALSE,9),TRUE),]
  list_iButton_corr_set[[i]][,]
}