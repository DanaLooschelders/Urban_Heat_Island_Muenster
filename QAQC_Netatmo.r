#QAQC Netatmo (Meier, Fenner et al. 2017 - 
#Crowdsourcing air temperature from citizen science 
#weather stations for urban climate research)
library(ggplot2)
library(ggforce)
library(hexbin)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo")
#**************************************************************************
#Data Quality Level A - inconsistent Metadata
#*****************************************************************************

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

#****************************************************************************
#Data Quality Level B - identify real outdoor measurements
#****************************************************************************

#five times the sd in TNref (arithmetic mean ofUCON and DWD stations)
#and in SDref.
#calculate daily min air temp and sd 
list_netatmo_level_B=list_netatmo_merge #create output list
#create output dataframe
daily_min_table=data.frame("date"=seq.Date(from=as.Date("2019-08-01"), to=as.Date("2019-09-25"), by=1), "daily_min"=rep(NA), "SD"=rep(NA))
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  for (x in data$Date){
    daily_min_table$daily_min[daily_min_table$date==x]=min(data$temperature[data$Date==x])
    daily_min_table$SD[daily_min_table$date==x]=sd(data$temperature[data$Date==x])
    }
list_netatmo_level_B[[i]]=daily_min_table
  }

str(temp) #DWD reference data
daily_min_ref=data.frame("date"=seq.Date(from=as.Date("2019-08-01"), to=as.Date("2019-09-25"), by=1), "daily_min"=rep(NA), "SD"=rep(NA))

for (x in daily_min_ref$date){
  daily_min_ref$daily_min[daily_min_ref$date==x]=min(temp$TT_TU[as.Date(temp$MESS_DATUM)==x], na.rm=T)
  daily_min_ref$SD[daily_min_ref$date==x]=sd(temp$TT_TU[as.Date(temp$MESS_DATUM)==x], na.rm=T)
}

level_B_1=function(month="August"){
#scatterplot mean temp vs SD
list_netatmo_level_B_aug=lapply(list_netatmo_level_B, function(x) subset(x, strftime(x$date, "%B")==month))
#list_netatmo_level_B_sep=lapply(list_netatmo_level_B, function(x) subset(x, strftime(x$date, "%B")=="September"))
#caluculate monthly means for reference data
mean_aug_temp_ref=mean(daily_min_ref$daily_min[strftime(daily_min_ref$date, "%B")==month])
mean_aug_sd_ref=mean(daily_min_ref$SD[strftime(daily_min_ref$date, "%B")==month])

sd_aug_temp_ref=sd(daily_min_ref$daily_min[strftime(daily_min_ref$date, "%B")==month])
sd_aug_sd_ref=sd(daily_min_ref$SD[strftime(daily_min_ref$date, "%B")==month])

#calculate monthly means for netatmo data
mean.aug=data.frame("ID"=names(list_netatmo_level_B_aug), 
                "mean_min_temp"=sapply(list_netatmo_level_B_aug, function(x) mean(x$daily_min)),
                "mean_sd"=sapply(list_netatmo_level_B_aug, function(x) mean(x$SD)))

ggplot(data=mean.aug, aes(mean_min_temp, mean_sd))+
  geom_point()+ #netatmo mean monthly daily min values
  geom_point(aes(x=mean(mean.aug$mean_min_temp), y=mean(mean.aug$mean_sd, na.rm=T)), color="green", shape=15)+ #one point for netatmo mean and sd
  #one point for reference data mean and sd point
  geom_point(aes(x=mean_aug_temp_ref, y=mean_aug_sd_ref), color="red", shape=15)+
  #ellipse for 5 times the sd for mean and sd of ref
  geom_ellipse(aes(a=sd_aug_sd_ref*5, x0=mean_aug_temp_ref, b=sd_aug_temp_ref*5, y0=mean_aug_sd_ref, angle=0))
}
#execute function for August and September
level_B_1("August")
ggsave(filename = "Level_B_1_netatmo_August.pdf", width=14, height=7)

level_B_1("September")
ggsave(filename = "Level_B_1_netatmo_September.pdf", width=14, height=7)

#how to: geom_ellipse
  #x0 -> center coordinate on x axis
  #y0 -> center coordinate on y axis
  #a -> length of ellipse on y axis
  #b -> length of ellipse on x axis
  #angle 

#level B peart 2
daily_min_ref_aug=daily_min_ref[strftime(daily_min_ref$date, "%B")=="August",]
hist(daily_min_ref_aug$daily_min, breaks=10)
?hist
hist(daily_min_ref$SD, breaks=10)
#2D Histogram
list_netatmo_level_B_aug=lapply(list_netatmo_level_B, function(x) subset(x, strftime(x$date, "%B")=="August"))
mean.aug=data.frame("ID"=names(list_netatmo_level_B_aug), 
                    "mean_min_temp"=sapply(list_netatmo_level_B_aug, function(x) mean(x$daily_min)),
                    "mean_sd"=sapply(list_netatmo_level_B_aug, function(x) mean(x$SD)))

h=hexbin(x=mean.aug$mean_min_temp, y=mean.aug$mean_sd, xlab = "SD", ylab="temp" )
h@count=h@count/sum(h@count, na.rm=T)
length(mean.aug$ID)
length(h@count)
h@count
plot(h)
?hexbin
#Data Quality Level C - filter systematic/single radiative errors

#Data Quality Level D -  outliers
