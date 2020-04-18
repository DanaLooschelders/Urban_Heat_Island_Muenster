#iButtons tidy up - correct offset and tidy up temperature spikes 
#for 02.08.2019
#and for 01.09.2019
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")

str(list_iButton_corr) #used from iButtons-check script (check that date corresponds)
#create temporary list
list_iButton_corr_temp=list_iButton_corr
#read in offset values
offset_stats=read.table("iButton_Statistics.csv", sep=",", dec=".", header=T)
str(offset_stats)

list_iButton_corr[[1]]=NULL #remove data without ID

for (i in names(list_iButton_corr)){
  if(any(offset_stats$ID_iButton==i)){
  off_value=offset_stats$Diff_T[offset_stats$ID_iButton==i]
  data=list_iButton_corr[[i]]
  data$Temperature_C_w_off=data$Temperature_C+off_value
  list_iButton_corr[[i]]=data
  } else{
    data=list_iButton_corr[[i]]
    data$Temperature_C_miss_off=data$Temperature_C
    list_iButton_corr[[i]]=data
  }
}


#iterate through all dataframes on list
#iterate trough all temperature values 
#set threshold value corresponding to literature regarding spikes
#if difference between two values is larger then threshold -> set to NA


list_iButton_corr_tidy=list_iButton_corr #create new, tidy list
str(list_iButton_corr)
report.na=rep(NA, length(list_iButton_corr_tidy))
#replace all temperature spikes (rise in >5°C in 10 mins) by NAs and report missing values
#for (i in 1:length(list_iButton_corr_tidy)) {
  #name_save=names(list_iButton_corr_tidy[])
 # test=list_iButton_corr_tidy[[i]]
  #test$diff=rep(NA)
  #test$diff[1:length(test$Temperature_C_w_off)-1]=diff(test$Temperature_C_w_off)
  #test$Temperature_C_w_off[test$diff>=5]=NA
  #test$Temperature_C_w_off[test$diff<= -5]=NA
  #list_iButton_corr_tidy[[i]]=test
  #names(list_iButton_corr_tidy[])=name_save
  #report.na[i]=sum(is.na(test$Temperature_C_miss_off))
#}

#-> use the second loop as one dataframe has values that are not corrected for the offset
for (i in 1:length(list_iButton_corr_tidy)) {
  #name_save=names(list_iButton_corr_tidy[])
  test=list_iButton_corr_tidy[[i]]
  test$diff=rep(NA)
  test$diff[1:length(test[,4])-1]=diff(test[,4])
  test[,4][test$diff>=5]=NA
  test[,4][test$diff<= -5]=NA
  list_iButton_corr_tidy[[i]]=test
  #names(list_iButton_corr_tidy[])=name_save
  report.na[i]=sum(is.na(test[,4]))
}
report.na #check how many NAs were in data

#for the 02.08.2019
list_iButton_corr_tidy[["33"]] #somehow the offset for this one is missing


