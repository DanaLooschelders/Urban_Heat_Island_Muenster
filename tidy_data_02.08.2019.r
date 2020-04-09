#iButtons tidy up - tidy up temperature spikes 
list_iButton_corr #used from iButtons-check script (check that date corresponds)

#iterate through all dataframes on list
#iterate trough all temperature values 
#set threshold value corresponding to literature regarding spikes
#if difference between two values is larger then threshold -> set to NA

#test with one dataframe
test=list_iButton_corr[[1]]
test$diff=rep(NA)
test$diff[1:1439]=diff(test$Temperature_C)*-1 #get positive temperatur difference from one value to next
test$diff[test$diff>=10]=NA
