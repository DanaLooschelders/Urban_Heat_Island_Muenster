#iButtons tidy up - correct offset and tidy up temperature spikes 
#for 02.08.2019
setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit")

str(list_iButton_corr) #used from iButtons-check script (check that date corresponds)

#read in offset values
offset_stats=read.table("iButton_Statistics.csv", sep=",", dec=".", header=T)
offset_stats=offset_stats[1:107,] #remove columns with NA

#test with one dataframe:
str(offset_stats)
off_value=offset_stats$T_offset[offset_stats$ID_iButton=="100"]

#try if indexing with int number works
ztest=list_iButton_corr[["100"]]
ztest$Temperature_C=ztest$Temperature_C+off_value

list_iButton_corr[[1]]=NULL

for (i in names(list_iButton_corr)){
  if(exists(x = i, where=offset_stats$ID_iButton){
  off_value=offset_stats$T_offset[offset_stats$ID_iButton==i]
  data=list_iButton_corr[[i]]
  data$Temperature_C_w_off=data$Temperature_C+off_value
  list_iButton_corr[[i]]=data
  } else{
    data=list_iButton_corr[[i]]
    data$Temperature_C_miss_off=data$Temperature_C
    list_iButton_corr[[i]]=data
  }
}
#number 99 exists two times?
#use for loop to assign offset to name?
off_value=offset_stats$T_offset[offset_stats$ID_iButton=="94"]
data=list_iButton_corr[["94"]]
data$Temperature_C_w_off=data$Temperature_C+off_value
list_iButton_corr[["94"]]=data

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
