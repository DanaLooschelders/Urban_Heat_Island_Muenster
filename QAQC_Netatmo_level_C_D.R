library(dplyr)
library(zoo)
library(xts)
library(splines)
#Data Quality Level C - filter systematic/single radiative errors
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
rad=read.table("GeoDach2019.csv", sep=";", dec=",", header=T, na.strings = "-" )
str(rad)
names(rad)[1]="Datetime"
rad=data.frame("Datetime"=rad$Datetime, "SWrad"=rad$Shortwave.Radiation)
rad$Datetime=strptime(rad$Datetime, format="%d.%m.%Y %H:%M")
rad$Datetime=as.POSIXct(rad$Datetime)
rad=rad[complete.cases(rad),]
rad2=rad[rad$Datetime>="2019-08-01 00:00:00"&rad$Datetime<="2019-09-30 23:59:59",]
str(rad2)

mean_rad=rollmeanr(rad2$SWrad, k = 3, fill=NA)
head(mean_rad)
class(rad2$Datetime)
View(mean_rad)
mean_rad2=rollapply(rad2, FUN=mean, width=61)
length(rad2$SWrad)
?rollmean
length(mean_rad)

time=list_netatmo_merge[[1]]$Datetime
range(rad_30_avg$time)


ts_rad=xts(rad2$SWrad, rad2$Datetime)
ts_merge=merge(ts_rad,time)
ts_merge=na.spline(ts_merge)

#aggregate radiation values by 30 mins (same timepoints as netatmo)
#create temperature difference between netatmo and reference data
#use only values with rad >10 Wm-1
#perform linear regression between the two

#Data Quality Level D -  outliers
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
