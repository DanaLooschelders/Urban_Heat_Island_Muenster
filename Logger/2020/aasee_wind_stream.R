#for map
require(leaflet)
require(htmltools)
require(htmlwidgets)
#for ts
require(forecast)
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

#rename lists
for(i in 1:length(list_iButton_corr_tidy_Aegidii)){
  names(list_iButton_corr_tidy_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_corr_tidy_Aegidii)[[i]]]
  names(list_iButton_24h_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_24h_Aegidii)[[i]]]
  names(list_iButton_day_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_day_Aegidii)[[i]]]
  names(list_iButton_hourly_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_hourly_Aegidii)[[i]]]
  }

#reorder names 
list_iButton_corr_tidy_Aegidii <- list_iButton_corr_tidy_Aegidii[order(names(list_iButton_corr_tidy_Aegidii))]
list_iButton_24h_Aegidii <- list_iButton_24h_Aegidii[order(names(list_iButton_24h_Aegidii))]
list_iButton_day_Aegidii <- list_iButton_day_Aegidii[order(names(list_iButton_day_Aegidii))]
list_iButton_hourly_Aegidii <- list_iButton_hourly_Aegidii[order(names(list_iButton_hourly_Aegidii))]

#set colors for plot
colors=c("orange", "green", "blue", "red", "yellow")

#plot all Aegidii logger
pdf(file="Windstream_Aasee.pdf", paper = "a4r", height=7, width=14)
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
pdf(file="Windstream_median_Aasee.pdf", paper = "a4r", height=7, width=14)
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
pdf(file="Windstream_mean_Aasee.pdf", paper = "a4r", height=7, width=14)
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
pdf(file="Windstream_day_mean_Aasee.pdf", paper = "a4r", height=7, width=14)
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
pdf(file="Windstream_hourly_mean_Aasee.pdf", paper = "a4r", height=7, width=14)
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
