#process Netatmo data
library(lubridate)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(tidyverse)

setwd("F:/satellite_data_Muenster/MODIS_neu")
MS_shape=readOGR("stadtgebiet.shp")

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
crs(MS_shape) #get crs
#transform coordinates to lat lon
MS_shape=spTransform(x = MS_shape, CRSobj = "+proj=longlat +datum=WGS84")
#crs(MS_shape) #check
#plot(MS_shape)
#plot points of netatmo stations
metadata_1=read.csv("data_August/stations.csv")
metadata_2=read.csv("data_August_September/stations.csv")
metadata_3=read.csv("data_September/stations.csv")
metadata_4=read.csv("data_September_2/stations.csv")

prep_plot=function(datapath="data_August/net_2019-08-01_to_2019-08-20.csv",
                     metadata=metadata_1,
                     startdate="01.08"){
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
data=read.csv(datapath)

stations=unique(data$device_id)

#transform coordiantes to lat lon and create spatial points
points=SpatialPointsDataFrame(coords = metadata[2:3], 
                              proj4string=CRS("+proj=longlat +datum=WGS84"),
                              data=metadata)
#test: plotting points in shapefile
#leaflet(MS_shape) %>%
#  addPolygons() %>%
#  addTiles() %>%
#  addCircles(data=points)

#subset points by shapefile -> get only points within Muenster
station_subset <- points[MS_shape, ]

ID_in_MS=station_subset@data$device_id #get vector with netatmo IDs in MS

#test: plotting points in shapefile
#leaflet(MS_shape) %>%
#  addPolygons(fillOpacity = 0) %>%
#  addTiles() %>%
#  addCircles(data = station_subset, col="black")

#create list for netatmo data
#subset data by IDs and write data for each ID in a list element
list_netatmo=list()
for (i in ID_in_MS){
  list_netatmo[[i]]=data[data$device_id==i,]
}

list_netatmo[[1]]$isoTime[1] #ISO-8601 date format

list_netatmo_datetime <- lapply(list_netatmo, `[`, 5)
list_netatmo_datetime=lapply(list_netatmo_datetime, function(x) ymd_hms(x$isoTime))
list_netatmo <- mapply(cbind, list_netatmo, "Datetime"=list_netatmo_datetime, SIMPLIFY=F)


ggplot(bind_rows(list_netatmo, .id="df"), aes(Datetime, temperature, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS') 
ggsave(filename = paste("overview_netatmo", startdate,".pdf"), width=14, height=7)

return(list_netatmo)
}

