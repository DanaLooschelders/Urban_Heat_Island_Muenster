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
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/windstream")

#first: check Aegidiiloger for transect
Aeg_Logger=metadata$Logger_ID[metadata$Standort=="Aegidiistr"]
#subset normal data list
list_iButton_corr_tidy_Aegidii=list_iButton_corr_tidy[names
                                                      (list_iButton_corr_tidy)%in%Aeg_Logger]
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

#rename lists
for(i in 1:length(list_iButton_corr_tidy_Aegidii)){
  names(list_iButton_corr_tidy_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_corr_tidy_Aegidii)[[i]]]
  names(list_iButton_24h_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_24h_Aegidii)[[i]]]
 names(list_iButton_day_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_day_Aegidii)[[i]]]
 names(list_iButton_hourly_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_hourly_Aegidii)[[i]]]
 names(list_iButton_factor_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_factor_Aegidii)[[i]]]
 }

#reorder names 
list_iButton_corr_tidy_Aegidii <- list_iButton_corr_tidy_Aegidii[order(names(list_iButton_corr_tidy_Aegidii))]
list_iButton_24h_Aegidii <- list_iButton_24h_Aegidii[order(names(list_iButton_24h_Aegidii))]
list_iButton_day_Aegidii <- list_iButton_day_Aegidii[order(names(list_iButton_day_Aegidii))]
list_iButton_hourly_Aegidii <- list_iButton_hourly_Aegidii[order(names(list_iButton_hourly_Aegidii))]
list_iButton_factor_Aegidii <- list_iButton_factor_Aegidii[order(names(list_iButton_factor_Aegidii))]

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

#to do:
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/DWD data/")
wind=read.table("produkt_zehn_min_ff_20190130_20200801_01766.txt", 
                sep=";", dec=".",header=T)

wind$MESS_DATUM=strptime(wind$MESS_DATUM, format="%Y%m%d%H%M")
any(wind$QN>3) #quality control 
#change names
names(wind)[4:5]=c("wind_speed", "wind_dir")
#subset to measuring time
wind=wind[wind$MESS_DATUM>=start_time&wind$MESS_DATUM<=end_time,]
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
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/windstream")

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

list_iButton_factor_Aegidii[[1]][]