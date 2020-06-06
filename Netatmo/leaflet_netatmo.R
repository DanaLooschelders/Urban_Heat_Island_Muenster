#plot netatmo distribution in Muenster
library(leaflet)
library(tidyverse)
library(mapview)
webshot::install_phantomjs()
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
netatmo=read.table("netatmo_coords_id.csv", sep=";", dec=".")


#plot netatmo distribution
leaflet(data=metadata_merge) %>%
  addTiles() %>%
  addCircles() %>%
addPolygons(data=MS_shape , color="black", fillOpacity = 0, weight = 1)

#plot downloaded data distribution
leaflet(data=stations_latlon) %>%
  addTiles() %>%
  addCircles() %>%
  addPolygons(data=MS_shape , color="black", fillOpacity = 0, weight = 1)

#plot netatmo ditribution with temperature
#add column with mean for August to metadata_merge
for (i in names(list_netatmo_level_D_2)){
  data=list_netatmo_level_D_2[[i]]
  mean_aug=mean(data$temperature[data$month=="August"], na.rm=T)
  mean_sep=mean(data$temperature[data$month=="September"], na.rm=T)
  metadata_merge$mean_aug_temp[metadata_merge$device_id==i]=mean_aug
  metadata_merge$mean_sep_temp[metadata_merge$device_id==i]=mean_sep
}
#create palette for binned colors
binpal <- colorBin(palette=c("yellow", "orange", "red", "brown"), metadata_merge$mean_sep_temp, 4, pretty = FALSE)

#plot the mean temperature for August and September
map_sep=leaflet(data=metadata_merge)%>%
  addTiles() %>%
  addCircles(color=~binpal(mean_sep_temp), weight = 10, fillOpacity = 10)%>%
  addPolygons(data=MS_shape , color="black", fillOpacity = 0, weight = 1)%>%
  addLegend(pal=binpal, values=metadata_merge$mean_sep_temp)
mapshot(map_sep, file="netatmo_mean_sep_2019.png")
