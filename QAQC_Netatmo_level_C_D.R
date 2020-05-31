library(dplyr)
library(zoo)
#*************************************************************************
#Data Quality Level C - filter systematic/single radiative errors
#*************************************************************************
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
rad=read.table("Dana_GeoDach.csv", sep=";", dec=",", header=T, na.strings = "-" )
str(rad)
names(rad)[1]="Datetime" #rename first column
rad=data.frame("Datetime"=rad$Datetime, "SWrad"=rad$Shortwave.Radiation, "Temperature"=rad$Temperature) #create column with just two variables
rad$Datetime=strptime(rad$Datetime, format="%d.%m.%Y %H:%M", tz="Europe/Berlin") #convert to Posixlt
rad$Datetime=as.POSIXct(rad$Datetime, tz="Europe/Berlin") #convert to Posixct
rad=rad[complete.cases(rad),] #remove rows with all NAs

#subset data to timeframe
rad2=rad[rad$Datetime>="2019-08-01 02:00:00"&rad$Datetime<="2019-09-30 23:59:59",]
str(rad2) #check
#aggregate data to hourly means
rad2$Hour <- cut(as.POSIXct(rad2$Datetime, 
             format="%Y-%m-%Y %H:%M:%S"), breaks="hour",
             tz="Europe/Berlin") #create new column with hour
hourly_rad <- aggregate(cbind(Temperature,SWrad) ~ Hour, rad2, mean) #aggregate values to hourly means
hourly_rad$Hour=as.POSIXct(hourly_rad$Hour)
rm(rad, rad2) #tidy script by removing dataframes

#aggregate netatmo data to hourly means
list_netatmo_hourly=list_netatmo_merge #create new list
#for loop to aggregate data by hour for each station
for (i in 1:length(list_netatmo_hourly)){
data=list_netatmo_merge[[i]]
data$Hour = cut(as.POSIXct(data$Datetime, 
                           format="%Y-%m-%d %H:%M:%S",
                           tz="Europe/Berlin"), breaks="hour")
hourly= aggregate(temperature ~ Hour, data, mean, na.action=na.pass)
hourly$Hour=as.POSIXct(hourly$Hour, format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin")
list_netatmo_hourly[[i]]=hourly
}

#create output list
list_netatmo_level_C=list_netatmo_hourly
for (i in 1:length(list_netatmo_level_C)){
  #create one dataframe
  data=cbind(hourly_rad, list_netatmo_level_C[[i]]$temperature)
  #name dataframe
  names(data)=c("Hour", "ref_Temperature","SWrad","netatmo_Temperature")
  #calculate Temperature difference between Netatmo and reference station
  data$Temp_diff=data$netatmo_Temperature-data$ref_Temperature
  #compute pearson correlation between radiation and 
  #temperature difference for all rad values >10 Wm2
  cor_rad=cor.test(data$SWrad[data$SWrad>10],data$Temp_diff[data$SWrad>10], method = "pearson")
  #test if correlation has p-value <0.01 and correlation >0.5
  #then station has systematic radiative error and is removed
  if(cor_rad$p.value<0.01&cor_rad$estimate>0.5){
    list_netatmo_hourly[[i]]=NULL #remove station
  }else{ #test if single values need to be filtered out
    #filter temp diff values that are >3 times SD of ref temp
    SD=sd(data$ref_Temperature)
    for (x in 1:length(data$netatmo_Temperature)){
      if( x>SD*3|x<SD*-3){
        #if single value differs more then SD*3 in any direction set NA
        data$netatmo_Temperature[x]=NA 
      }else{}
      #replace corrected values
    }
    list_netatmo_level_C[[i]]$temperature=data$netatmo_Temperature 
  }
}

#add month indices back to list
#add column with month index for August and September
#add month index to dataframe
#and create new output list
list_netatmo_month <- lapply(list_netatmo_level_C, `[`, 1)
list_netatmo_month=lapply(list_netatmo_month, function(x) strftime(x$Hour, "%B", tz="Europe/Berlin"))
list_netatmo_level_D <- mapply(cbind, list_netatmo_level_C, "month"=list_netatmo_month, SIMPLIFY=F)
rm(list_netatmo_month)
#******************************************************************************
#Data Quality Level D -  outliers
#**********************************************************************************
level_D=function(month="August"){
for (i in 1:length(list_netatmo_level_D)){
  data=list_netatmo_level_D[[i]]
  data_month=data[data$month==month,]
  sd_month=sd(data_month$temperature, na.rm=T)
  for (x in 1:length(data_month$temperature)){
    if (x > sd_month*3) {
      data_month$temperature[x]=NA #set value NA
    }else{} #keep value
  }
  data[data$month==month,]=data_month #put controlled data back in dataframe
} 
  list_netatmo_level_D[[i]]=data #put controlled data back in list
}

list_netatmo_level_D=level_D(month="August")
list_netatmo_level_D_2=level_D(month="September")
