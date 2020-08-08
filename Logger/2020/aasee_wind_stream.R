#for map
require(leaflet)
require(htmltools)
require(htmlwidgets)
#for ts
require(forecast)
install.packages("TSclust")
require(TSclust)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
coords=read.table(file="Sensortabelle Kartierung Stand 22.7.csv", dec=",", sep=";", header=T)
coords=coords[-1,]
leaflet(data=coords)%>%
  addTiles()%>%
addCircles(data=coords, lat=coords$Lat2, lng=coords$Lon2)%>%
  addPopups(data=coords, lat=coords$Lat2, 
            lng=coords$Lon2,popup = htmlEscape(coords$Ortsbeschreibung))

#use: all Aegidii logger, Ehrenpark, Aaseemensa VL and SL Spätzl
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/windstream/merge")

#first: check Aegidiiloger for transect
Aeg_Logger=metadata$Logger_ID[metadata$Standort=="Aegidiistr"]
#subset normal data list
list_iButton_corr_tidy_Aegidii=list_iButton_corr_tidy[names(list_iButton_corr_tidy)%in%Aeg_Logger]
#subset 24h mean list
list_iButton_24h_Aegidii=list_iButton_24h_mms[names(list_iButton_24h_mms)%in%Aeg_Logger]
#subset day mean
list_iButton_24h_Aegidii=list_iButton_24h_mms[names(list_iButton_24h_mms)%in%Aeg_Logger]
#subset daytime
list_iButton_day_Aegidii=list_iButton_day_mms[names(list_iButton_day_mms)%in%Aeg_Logger]
#subset hourly means
list_iButton_hourly_Aegidii=list_iButton_hourly[names(list_iButton_hourly)%in%Aeg_Logger]
#subset factor list
list_iButton_factor_Aegidii=list_iButton_corr_tidy_date_factor[names(list_iButton_corr_tidy_date_factor)%in%Aeg_Logger]
#subset day list
list_iButton_Aegidii_day=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)%in%Aeg_Logger]
#subset night list
list_iButton_Aegidii_night=list_iButton_corr_tidy_date_night[names(list_iButton_corr_tidy_date_night)%in%Aeg_Logger]

#rename lists
for(i in 1:length(list_iButton_corr_tidy_Aegidii)){
  names(list_iButton_corr_tidy_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_corr_tidy_Aegidii)[[i]]]
  names(list_iButton_24h_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_24h_Aegidii)[[i]]]
 names(list_iButton_day_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_day_Aegidii)[[i]]]
 names(list_iButton_hourly_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_hourly_Aegidii)[[i]]]
 names(list_iButton_factor_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_factor_Aegidii)[[i]]]
 names(list_iButton_Aegidii_day)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_Aegidii_day)[[i]]]
 names(list_iButton_Aegidii_night)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_Aegidii_night)[[i]]]
  }

#reorder names 
list_iButton_corr_tidy_Aegidii <- list_iButton_corr_tidy_Aegidii[order(names(list_iButton_corr_tidy_Aegidii))]
list_iButton_24h_Aegidii <- list_iButton_24h_Aegidii[order(names(list_iButton_24h_Aegidii))]
list_iButton_day_Aegidii <- list_iButton_day_Aegidii[order(names(list_iButton_day_Aegidii))]
list_iButton_hourly_Aegidii <- list_iButton_hourly_Aegidii[order(names(list_iButton_hourly_Aegidii))]
list_iButton_factor_Aegidii <- list_iButton_factor_Aegidii[order(names(list_iButton_factor_Aegidii))]
list_iButton_Aegidii_day <- list_iButton_Aegidii_day[order(names(list_iButton_Aegidii_day))]
list_iButton_Aegidii_night <- list_iButton_Aegidii_night[order(names(list_iButton_Aegidii_night))]

#set colors for plot
colors=c("orange", "green", "blue", "red", "yellow")

#plot all Aegidii logger
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(list_iButton_corr_tidy_Aegidii[[1]][2:3],
     type="l", col=colors[1], ylim=c(5,35),
     ylab="Temperature [°C]", xlab="Date", main="Temperature at Aegidiistr in 2020")
  for(i in 2:length(list_iButton_corr_tidy_Aegidii)){
  lines(list_iButton_corr_tidy_Aegidii[[i]][2:3], col=colors[i])
}
legend("topright",legend = names(list_iButton_corr_tidy_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#daily median with ggplot
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_median_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(list_iButton_24h_Aegidii[[1]][c(1,3)],
     type="l", col=colors[1], ylim=c(13,22),
     ylab="Temperature [°C]", xlab="Date", main="Median Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_24h_Aegidii)){
  lines(list_iButton_24h_Aegidii[[i]][c(1,3)], col=colors[i])
}
legend("topright",legend = names(list_iButton_24h_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#daily mean with ggplot
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_mean_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(list_iButton_24h_Aegidii[[1]][c(1,2)],
     type="l", col=colors[1], ylim=c(13,22),
     ylab="Temperature [°C]", xlab="Date", main="Mean Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_24h_Aegidii)){
  lines(list_iButton_24h_Aegidii[[i]][c(1,2)], col=colors[i])
}
legend("topright", legend = names(list_iButton_24h_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#daytime mean with ggplot
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_day_mean_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(list_iButton_day_Aegidii[[1]][c(1,2)],
     type="l", col=colors[1], ylim=c(13,25),
     ylab="Temperature [°C]", xlab="Date", main="Mean daytime Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_day_Aegidii)){
  lines(list_iButton_day_Aegidii[[i]][c(1,2)], col=colors[i])
}
legend("topright", legend = names(list_iButton_day_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#hourly mean with ggplot
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_hourly_mean_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(as.POSIXlt(list_iButton_hourly_Aegidii[[1]][,1]),
     list_iButton_hourly_Aegidii[[1]][,2],
     type="l", col=colors[1], ylim=c(5,30),
     ylab="Temperature [°C]", xlab="Date", 
     main="Mean hourly Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_hourly_Aegidii)){
  lines(as.POSIXlt(list_iButton_hourly_Aegidii[[i]][,1]),
        list_iButton_hourly_Aegidii[[i]][,2], col=colors[i])
}
legend("topright", legend = names(list_iButton_hourly_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#test dissimilarities between ts
aegidii=matrix(nrow=length(list_iButton_hourly_Aegidii), 
               ncol=length(list_iButton_hourly_Aegidii[[1]][,1]))
for(i in 1:length(list_iButton_hourly_Aegidii)){
  aegidii[i,]=list_iButton_hourly_Aegidii[[i]][,2]
}

#Decide on dissimilarity method
diss(SERIES=na.spline(aegidii), METHOD="CORT")

ccf(na.spline(aegidii[1,]), na.spline(aegidii[2,]), lag.max=200)

#read in wind data
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/DWD data/")
wind=read.table("produkt_zehn_min_ff_20190130_20200801_01766.txt", 
                sep=";", dec=".",header=T)

wind$MESS_DATUM=strptime(wind$MESS_DATUM, format="%Y%m%d%H%M")
any(wind$QN>3) #quality control 
#change names
names(wind)[4:5]=c("wind_speed", "wind_dir")
#subset to measuring time
wind=wind[wind$MESS_DATUM>=list_iButton_corr_tidy[[1]][1,2]&wind$MESS_DATUM<=list_iButton_corr_tidy[[1]][dim(list_iButton_corr_tidy[[1]])[1],2],]
which((is.na(c(wind$wind_speed, wind$wind_dir))))
wind=wind[-c(1:12),]
#filter values with SW wind
#SSW: 191.25
#WSW: 258.75
#subset for south west wind
wind_sw=wind
wind_sw$wind_speed[wind_sw$wind_dir<191|wind_sw$wind_dir>258]=NA
#subset for windspeed >0
wind_sw$wind_speed[wind_sw$wind_speed<0]=NA
#relate wind speed to air temp in aegidiistrs
#plot all Aegidii logger with wind
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/windstream/merge")

pdf(file="Windstream_Aasee_sw_wind.pdf", paper = "a4r", height=7, width=14)
plot(list_iButton_corr_tidy_Aegidii[[1]][2:3],
     type="l", col=colors[1], ylim=c(0,35),
     ylab="Temperature [°C]", xlab="Date", main="Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_corr_tidy_Aegidii)){
  lines(list_iButton_corr_tidy_Aegidii[[i]][2:3], col=colors[i])
}
points(wind_sw$MESS_DATUM,wind_sw$wind_speed)
legend("topright",legend = names(list_iButton_corr_tidy_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()
#correlation stronger for first logger?


#hourly mean with wind 
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_hourly_mean_Aasee_with_wind.pdf"), paper = "a4r", height=7, width=14)
plot(wind_sw$MESS_DATUM, wind_sw$wind_speed, type="p",
     ylim=c(0,30),
     ylab="Temperature [°C]", xlab="Date", 
     main="Mean hourly Temperature at Aegidiistr in 2020")
for(i in 1:length(list_iButton_hourly_Aegidii)){
  lines(as.POSIXlt(list_iButton_hourly_Aegidii[[i]][,1]),
        list_iButton_hourly_Aegidii[[i]][,2], col=colors[i])
}
legend("topright", legend = names(list_iButton_hourly_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_daily_mean_Aasee_with_wind.pdf"), paper = "a4r", height=7, width=14)
plot(wind_sw$MESS_DATUM, wind_sw$wind_speed, type="p",
     ylim=c(0,30),
     ylab="Temperature [°C]", xlab="Date", 
     main="Mean hourly Temperature at Aegidiistr in 2020")
for(i in 1:length(list_iButton_24h_Aegidii)){
  lines(as.POSIXlt(list_iButton_24h_Aegidii[[i]][,1]),
        list_iButton_24h_Aegidii[[i]][,2], col=colors[i])
}
legend("topright", legend = names(list_iButton_hourly_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#check temperature difference between Aegidii logger (first and last)
mean(list_iButton_Aegidii_day[[1]][,3], na.rm=T)
mean(list_iButton_Aegidii_day[[5]][,3], na.rm=T)

mean(list_iButton_Aegidii_night[[1]][,3],na.rm=T)
mean(list_iButton_Aegidii_night[[5]][,3],na.rm=T)

#prep data
temp=list_iButton_corr_tidy_Aegidii[[1]][,3]
temp_ref=list_iButton_corr_tidy_Aegidii[[5]][,3]
temp_two=list_iButton_corr_tidy_Aegidii[[2]][,3]
date=list_iButton_corr_tidy_Aegidii[[1]][,2]
factor=list_iButton_factor_Aegidii[[1]][,6]

#check correlation of windspeed and temperature
cor(wind_sw$wind_speed,temp, use = "na.or.complete")
cor.test(wind_sw$wind_speed,temp, use = "na.or.complete")

#compare only temperatures with windspeeds >0 and wind dir from SW
temp_wind=data.frame(wind_sw$MESS_DATUM, 
                     wind_sw$wind_speed,
                     temp,
                     temp_two,
                     temp_ref,
                     date,
                     factor)
temp_wind$temp[is.na(temp_wind$wind_sw.wind_speed)]=NA
#filter temp values for which windspeed is higher 0
temp_wind$temp_ref[is.na(temp_wind$wind_sw.wind_speed)]=NA

mean(temp_wind$temp[temp_wind$factor=="day"], na.rm=T)
mean(temp_wind$temp_ref[temp_wind$factor=="day"], na.rm=T)

mean(temp_wind$temp[temp_wind$factor=="night"], na.rm=T)
mean(temp_wind$temp_ref[temp_wind$factor=="night"], na.rm=T)

#compare Ehrenpark (87) and Haus Kump (64)
Ehrenpark=list_iButton_corr_tidy_date_factor[["87"]]
Haus_Kump=list_iButton_corr_tidy_date_factor[["64"]]

mean(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"],na.rm=T)
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"],na.rm=T)

mean(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"],na.rm=T)
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"],na.rm=T)

#test significance
#normality:
shapiro.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"])
wilcox.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"], 
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])

shapiro.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"])
wilcox.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"])

#compare Haus Kump and first Aegidiilogger
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"],na.rm=T)
mean(temp_wind$temp[temp_wind$factor=="day"], na.rm=T)

mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"],na.rm=T)
mean(temp_wind$temp[temp_wind$factor=="night"], na.rm=T)

shapiro.test(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])
wilcox.test(temp_wind$temp[temp_wind$factor=="day"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])

shapiro.test(temp_wind$temp[temp_wind$factor=="night"])
wilcox.test(temp_wind$temp[temp_wind$factor=="night"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"])
#check for second aegiodi logger
mean(temp_wind$temp_two[temp_wind$factor=="day"], na.rm=T)
mean(temp_wind$temp[temp_wind$factor=="night"], na.rm=T)

diff=Ehrenpark$Temperature_C_w_off-Haus_Kump$Temperature_C_w_off

#plot difference between Ehrenpark and Haus Kump with wind
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),
               "diff_Ehrenpark_Haus_K.pdf"), 
    paper = "a4r", height=7, width=14)
plot(Ehrenpark$Datetime.1, diff, type="l",ylim=c(-5,10),
     sub=">0: warmer in Ehrenpark, <0 warmer in Haus K")
points(wind_sw$MESS_DATUM, wind_sw$wind_speed, col="red")
abline(v=sun2$sunrise, col="green")
abline(v=sun2$sunset, col="blue")
dev.off()

cor.test(diff, wind_sw$wind_speed)
