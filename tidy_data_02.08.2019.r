#iButtons tidy up - tidy up temperature spikes 
#for 02.08.2019
list_iButton_corr #used from iButtons-check script (check that date corresponds)

#iterate through all dataframes on list
#iterate trough all temperature values 
#set threshold value corresponding to literature regarding spikes
#if difference between two values is larger then threshold -> set to NA


list_iButton_corr_tidy=list_iButton_corr #create new, tidy list

report.na=rep(NA, length(list_iButton_corr_tidy))
#replace all temperature spikes (rise in >5°C in 10 mins) by NAs and report missing values
for (i in 1:length(list_iButton_corr_tidy)) {
  name_save=names(list_iButton_corr_tidy[])
  test=list_iButton_corr_tidy[[i]]
  test$diff=rep(NA)
  test$diff[1:length(test$Temperature_C)-1]=diff(test$Temperature_C)
  test$Temperature_C[test$diff>=5]=NA
  test$Temperature_C[test$diff<= -5]=NA
  list_iButton_corr_tidy[[i]]=test
  names(list_iButton_corr_tidy[])=name_save
  report.na[i]=sum(is.na(test$Temperature_C))
}

report.na #check how many NAs were in data
