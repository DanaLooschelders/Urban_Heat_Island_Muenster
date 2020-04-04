#Heatmap

library(leaflet)
library(sp)
library(tidyverse)

setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit")
#read in coordiante data
coords=read.table("Lat_Lon_Logger.csv", sep=";", dec=",", header=T)
str(coords)
coords=coords[,-4] #drop last empty column
str(coords)
names(coords)=c("ID", "Lat", "Lon")

coords_test=SpatialPoints(coords = coords[,2:3], proj4string=CRS("+proj=longlat +datum=WGS84"))
plot(coords_test)
#test plotting coordiantes
leaflet(data=coords) %>%
addTiles() %>%
addMarkers(lng = ~Lon, lat = ~Lat)

#read in temperature data with iButtons_Check script
