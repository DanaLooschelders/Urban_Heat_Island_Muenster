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

#first: check Aegidiiloger for transect
Aeg_Logger=metadata$Logger_ID[metadata$Standort=="Aegidiistr"]
list_iButton_corr_tidy_Aegidii=list_iButton_corr_tidy[names
                                                      (list_iButton_corr_tidy)%in%Aeg_Logger]
#rename list
for(i in 1:length(list_iButton_corr_tidy_Aegidii)){
names(list_iButton_corr_tidy_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_corr_tidy_Aegidii)[[i]]]
}

#reorder names 
list_iButton_corr_tidy_Aegidii <- list_iButton_corr_tidy_Aegidii[order
                                                                 (names(list_iButton_corr_tidy_Aegidii))]

#calculate moving average to get hourly means
list_iButton_aegidii_temp <- lapply(list_iButton_corr_tidy_Aegidii, `[`, 3)
list_iButton_aegidii_temp =lapply(list_iButton_aegidii_temp, function(x) ts(x$Temperature_C_w_off,start=c(7), frequency = 24*6))
list_iButton_aegidii_temp=lapply(list_iButton_aegidii_temp,function(x) ma(x ,order=6))

numbers <- c(3, 5, 3, 6, 4, 5, 3, 4, 4, 6, 3, 5, 4, 6, 3, 5, 4, 4, 6, 3, 5)
# apply the 'ma' function
ma(numbers, order = 5)
(sum(3, 5, 3, 6, 4))/5


colors=c("orange", "green", "blue", "red", "yellow")

pdf(file="Windstream_Aasee.pdf", paper = "a4r", height=7, width=14)
plot(list_iButton_corr_tidy_Aegidii[[1]][2:3],
     type="l", col=colors[1], ylim=c(5,35),
     ylab="Temperature [°C]", xlab="Date", main="Temperature at Aegidiistrin 2020")
  for(i in 2:length(list_iButton_corr_tidy_Aegidii)){
  lines(list_iButton_corr_tidy_Aegidii[[i]][2:3], col=colors[i])
}
legend("topright",legend = metadata$Standort_ID[metadata$Standort=="Aegidiistr"], 
       fill =colors, cex=0.5 )
dev.off()

#daily median with ggplot
pdf(file="Windstream_median_Aasee.pdf", paper = "a4r", height=7, width=14)
plot(list_iButton_corr_tidy_Aegidii[[1]][2:3],
     type="l", col=colors[1], ylim=c(5,35),
     ylab="Temperature [°C]", xlab="Date", main="Temperature at Aegidiistrin 2020")
for(i in 2:length(list_iButton_corr_tidy_Aegidii)){
  lines(list_iButton_corr_tidy_Aegidii[[i]][2:3], col=colors[i])
}
legend("topright",legend = metadata$Standort_ID[metadata$Standort=="Aegidiistr"], 
       fill =colors, cex=0.5 )
dev.off()
