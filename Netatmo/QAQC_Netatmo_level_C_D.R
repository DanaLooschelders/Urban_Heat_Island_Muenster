library(dplyr)
library(zoo)
#*************************************************************************
#Data Quality Level C - filter systematic/single radiative errors
#*************************************************************************
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
rad=read.table("GeoDach_Dana_2020.csv", sep=";", dec=",", header=T, na.strings = "-" )
str(rad)
names(rad)[1]="Datetime" #rename first column
rad=data.frame("Datetime"=rad$Datetime, "SWrad"=rad$Shortwave.Radiation, "Temperature"=rad$Temperature) #create column with just two variables
rad$Datetime=strptime(rad$Datetime, format="%d.%m.%Y %H:%M", tz="Europe/Berlin") #convert to Posixlt
rad$Datetime=as.POSIXct(rad$Datetime, tz="Europe/Berlin") #convert to Posixct
rad=rad[complete.cases(rad),] #remove rows with all NAs

#subset data to timeframe
rad2=rad[rad$Datetime>="2020-07-07 02:00:00"&rad$Datetime<="2020-07-28 23:59:59",]
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

#add month indices back to list
#add column with month index for August and September
#add month index to dataframe
#create output list
list_netatmo_month <- lapply(list_netatmo_hourly, `[`, 1)
list_netatmo_month=lapply(list_netatmo_month, function(x) strftime(x$Hour, "%B", tz="Europe/Berlin"))
list_netatmo_level_C <- mapply(cbind, list_netatmo_hourly, "month"=list_netatmo_month, SIMPLIFY=F)
rm(list_netatmo_month)
#filter systematic errors
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
  }else{}
}
  
  #single radiative errors
  #test if single values need to be filtered out
    #filter temp diff values that are >3 times SD of ref temp
    #for August and September

for (i in 1:length(list_netatmo_level_C)){
  #create one dataframe
  data=cbind(hourly_rad, list_netatmo_level_C[[i]]$temperature, list_netatmo_level_C[[i]]$month)
  #name dataframe
  names(data)=c("Hour", "ref_Temperature","SWrad","netatmo_Temperature", "month")
  #calculate Temperature difference between Netatmo and reference station
  data$Temp_diff=data$netatmo_Temperature-data$ref_Temperature
  SD_aug=sd(data$ref_Temperature[data$month=="Juli"], na.rm=T)
  #SD_sep=sd(data$ref_Temperature[data$month=="September"], na.rm=T)
    for (x in 1:length(data$Temp_diff[data$month=="Juli"])){
      value=data$Temp_diff[data$month=="Juli"][x]
      if( any(value>SD_aug*3,value<SD_aug*-3, is.na(value))){
        #if single value differs more then SD*3 in any direction set NA
        data$Temp_diff[data$month=="Juli"][x]=NA 
      }else{}
      #replace corrected values
    }#for September
    #for (y in 1:length(data$Temp_diff[data$month=="September"])){
      #value=data$Temp_diff[data$month=="September"][y]
      #if( any(value>SD_sep*3,value<SD_sep*-3, is.na(value))){
        #if single value differs more then SD*3 in any direction set NA
       # data$netatmo_Temperature[data$month=="September"][y]=NA 
      #}else{}
    #}
    list_netatmo_level_C[[i]]$temperature=data$netatmo_Temperature 
  }

ggplot(bind_rows(list_netatmo_level_C, .id="df"), aes(Hour, temperature, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
  theme(legend.position="none")
ggsave(filename = "overview_netatmo_level_C.pdf", width=14, height=7)


#and create new output list
list_netatmo_level_D = list_netatmo_level_C
#******************************************************************************
#Data Quality Level D -  outliers
#**********************************************************************************
level_D=function(month="Juli"){
for (i in 1:length(list_netatmo_level_D)){
  data=list_netatmo_level_D[[i]]
  data_month=data[data$month==month,]
  sd_month=sd(data_month$temperature, na.rm=T)
  for (x in 1:length(data_month$temperature)){
    value=data_month$temperature[1]
    if (any(value > sd_month*3, value< sd_month*-3,is.na(value))) {
      data_month$temperature[x]=NA #set value NA
    }else{} #keep value
  }
  data[data$month==month,]=data_month #put controlled data back in dataframe
} 
  list_netatmo_level_D[[i]]=data #put controlled data back in list
  return(list_netatmo_level_D)
}

list_netatmo_level_D=level_D(month="Juli")
#list_netatmo_level_D_2=level_D(month="September")

ggplot(bind_rows(list_netatmo_level_D, .id="df"), aes(Hour, temperature, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')
ggsave(filename = "overview_netatmo_level_D.pdf", width=14, height=7)

#update metadata table
#shorten metadatalist by excluding the IDs that had no data
rownames(metadata_merge)=metadata_merge$device_id #set ID as rownames
ids_to_keep=names(list_netatmo_level_D) #get character vector of ids to keep
metadata_merge=metadata_merge[ids_to_keep,] #subset metadata with ids from data

#save metadata_merge to csv file
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
write.csv2(file="Netatmo_metadata.csv", metadata_merge)

rm(ids_to_keep, list_netatmo_hourly)

#transform coordiantes to lat lon and create spatial points
points=SpatialPointsDataFrame(coords = metadata_merge[2:3], 
                              proj4string=CRS("+proj=longlat +datum=WGS84"),
                              data=metadata_merge)

#final test: plotting points in shapefile
leaflet(MS_shape) %>%
  addPolygons() %>%
  addTiles() %>%
  addCircles(data=points)

