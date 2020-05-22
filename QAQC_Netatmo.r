#QAQC Netatmo (Meier, Fenner et al. 2017 - 
#Crowdsourcing air temperature from citizen science 
#weather stations for urban climate research)

#Data Quality Level A - inconsistent Metadata
#filter out stations with no lat/lon value
any(is.na(metadata_merge$lon)) #FALSE
any(is.na(metadata_merge$lat)) #FALSE
#Check if stations have identical metadata (remove if >2)
any(duplicated(metadata_merge[2:9])) #FALSE
#filter missing data seperately for August and September
  #more then 10 NAs per day
  #more than 20% per month

#test the foor loop with artificially inserted NA values -> works
#list_netatmo_merge[[3]]$temperature[20:35]=NA

#For August
NA_test=function(month="August"){
for (i in names(list_netatmo_merge)){
  data=list_netatmo_merge[[i]] #use only one dataset
  for( x in data$Date[data$Month==month]){ #subset for one day in specified month
    nas=sum(is.na(data$temperature[data$Date==x&data$Month==month])) #count NAs in data
    if( nas >= 10) { #if more than 10 NAs data from that day will be set NA
      data$temperature[data$Date==x&data$Month==month]=NA
    }else{} #otherwise keep data
  }
  nas=sum(is.na(data$temperature[data$Month==month])) #count monthly NA data
  if(nas>=length(data$temperature[data$Month==month])*0.2){ #if more than 20% of monthly data is NA
    data[data$Month==month,]=NA #set data for that month NA
    list_netatmo_merge[[i]]=data
  }else{list_netatmo_merge[[i]]=data} #otherwise keep data
}
}
NA_test(month="August")
NA_test(month="September")
length(list_netatmo_merge) #still same as before (none were removed)
#check how many NAs were added to data
NAs=sapply(list_netatmo_merge, function(x) sum(is.na(x$temperature))) #none

#Data Quality Level B - identify real outdoor measurements
#five times the sd in TNref (arithmetic mean ofUCON and DWD stations)
#and in SDref.
#calculate daily min air temp and sd 
list_netatmo_level_B=list_netatmo_merge #create output list
#create output dataframe
daily_min_table=data.frame("date"=seq.Date(from=as.Date("2019-08-01"), to=as.Date("2019-09-05"), by=1), "daily_min"=rep(NA), "SD"=rep(NA))
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  for (x in data$Date){
    daily_min_table$daily_min[daily_min_table$date==x]=min(data$temperature[data$Date==x])
    daily_min_table$SD[daily_min_table$date==x]=sd(data$temperature[data$Date==x])
    }
list_netatmo_level_B[[i]]=daily_min_table
  }

str(temp) #DWD reference data
daily_min_ref=data.frame("date"=seq.Date(from=as.Date("2019-08-01"), to=as.Date("2019-09-05"), by=1), "daily_min"=rep(NA), "SD"=rep(NA))

x=daily_min_ref$date[1]
for (x in daily_min_ref$date){
  daily_min_ref$daily_min[daily_min_ref$date==x]=min(temp$TT_TU[as.Date(temp$MESS_DATUM)==x], na.rm=T)
  daily_min_ref$SD[daily_min_ref$date==x]=sd(temp$TT_TU[as.Date(temp$MESS_DATUM)==x], na.rm=T)
  }
#check if data values are higher than five times the SD of ref
#mit any() 
#Data Quality Level C - filter systematic/single radiative errors

#Data Quality Level D - remaining outliers
