library(dplyr)
library(zoo)
#*************************************************************************
#Data Quality Level C - filter systematic/single radiative errors
#*************************************************************************
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
rad=read.table("Dana_GeoDach.csv", sep=";", dec=",", header=T, na.strings = "-" )
str(rad)
names(rad)[1]="Datetime" #rename first column
rad=data.frame("Datetime"=rad$Datetime, "SWrad"=rad$Shortwave.Radiation) #create column with just two variables
rad$Datetime=strptime(rad$Datetime, format="%d.%m.%Y %H:%M", tz="Europe/Berlin") #convert to Posixlt
rad$Datetime=as.POSIXct(rad$Datetime, tz="Europe/Berlin") #convert to Posixct
rad=rad[complete.cases(rad),] #remove rows with all NAs
#subset data to timeframe
rad2=rad[rad$Datetime>="2019-08-01 02:00:00"&rad$Datetime<="2019-09-30 23:59:59",]
str(rad2) #check
#!!!!!!!!!!!!!!!!!!!!!!!!! remove October values from netatmo data
#aggregate data to hourly means
rad2$Hour <- cut(as.POSIXct(rad2$Datetime, 
             format="%Y-%m-%Y %H:%M:%S"), breaks="hour",
             tz="Europe/Berlin") #create new column with hour
hourly_rad <- aggregate(SWrad ~ Hour, rad2, mean) #aggregate values to hourly means
rm(rad, rad2) #tidy script by removing dataframes

#aggregate netatmo data to hourly means
list_netatmo_hourly=list_netatmo_merge #create new list
#for loop to aggregate data by hour for each station
for (i in 1:length(list_netatmo_hourly)){
data=list_netatmo_merge[[i]]
data$Hour = cut(as.POSIXct(data$Datetime, 
                           format="%Y-%m-%d %H:%M:%S",
                           tz="Europe/Berlin"), breaks="hour")
hourly= aggregate(temperature ~ Hour, data, mean)
hourly$Hour=as.POSIXct(hourly$Hour, format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin")
list_netatmo_hourly[[i]]=hourly
}


#create temperature difference between netatmo and reference data
data=list_netatmo_hourly[[1]]
str(hourly_rad)
tail(hourly_rad)
str(data)
tail(data)
#use only values with rad >10 Wm-1
lm(data$temperature~hourly_rad$SWrad)
#perform linear regression between the two

#******************************************************************************
#Data Quality Level D -  outliers
#**********************************************************************************
level_D=function(month="August"){
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  data_month=data[data$month==month,]
  sd_month=sd(data_month$Temperature)
  for (x in 1:length(data_month$Temperature)){
    if (x >= sd_month*3) {
      data_month$Temperature[x]=NA #set value NA
    }else{} #keep value
  }
  data[data$month==month,]=data_month #put controlled data back in dataframe
} 
  list_netatmo_merge[[i]]=data #put controlled data back in list
}
