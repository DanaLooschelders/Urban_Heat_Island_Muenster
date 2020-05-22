#QAQC Netatmo (Meier, Fenner et al. 2017 - 
#Crowdsourcing air temperature from citizen science 
#weather stations for urban climate research)

#Data Quality Level A - inconsistent Metadata
#filter out stations with no lat/lon value
any(is.na(metadata_merge$lon)) #FALSE
any(is.na(metadata_merge$lat)) #FALSE
#Check if stations have identical metadata (remove if >2)
any(duplicated(metadata_merge[2:9])) #FALSE
#filter missing data 
  #more then 10 NAs per day
  #more than 20% per month

#test the foor loop with artificially inserted NA values -> works
#list_netatmo_merge[[3]]$temperature[20:35]=NA

for (i in names(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  for( x in data$Date){
    nas=sum(is.na(data$temperature[data$Date==x]))
    if( nas >= 10) {
      data$temperature[data$Date==x]=NA
    }else{}
  }
  nas=sum(is.na(data$temperature))
  if(nas>=length(data$temperature)*0.2){
    list_netatmo_merge[[i]]=NULL
  }else{list_netatmo_merge[[i]]=data}
}

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
library(ggplot2)
library(dplyr)
ggplot(bind_rows(list_netatmo_level_B, .id="df"), aes(date, daily_min, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
  theme(legend.position="none")+
geom_line(data=daily_min_ref, aes(x = daily_min_ref$date, y= daily_min_ref$daily_min), colour="black")+
  geom_ribbon(data = daily_min_ref, aes(ymin=daily_min, ymax=daily_min+SD*3, colour="grey"),fill="grey")

ggsave(filename = "overview_netatmo_daily_min.pdf", width=14, height=7)

#scatterplot mean temp vs SD
mean=data.frame("ID"=names(list_netatmo_merge), 
                "mean_temp"=sapply(list_netatmo_merge, function(x) mean(x$temperature)),
                "sd"=sapply(list_netatmo_merge, function(x) sd(x$temperature)))
plot(mean$mean_temp, mean$sd)

#check if data values are higher than five times the SD of ref
#mit any() 
#Data Quality Level C - filter systematic/single radiative errors

#Data Quality Level D - remaining outliers
