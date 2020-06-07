#create map with loggers and netatmo stations

library(leaflet)
library(sp)
library(ggplot2)
library(htmltools)
library(maptools)
library(maps)
library(GISTools)
#load Netatmo coordinates
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo")
netatmo=read.csv2("Netatmo_metadata.csv")
str(netatmo)
netatmo$popup_text=paste("altitude:",netatmo$altitude, "m NN")
#load Logger coordinates
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data/")
logger=read.table("Lat_Lon_Logger.csv", sep=";", dec=",", header=T)
str(logger)
logger$popup_text=paste(logger$Place_name,logger$Place_number,logger$Place_type,logger$Description)
#map
leaflet(data=netatmo) %>%
  addTiles() %>%
  addCircles(color="blue") %>%
  addMarkers(data=netatmo, popup=~htmlEscape(popup_text))%>%
  addCircles(data=logger, color="red")%>%
  addMarkers(data=logger, popup = ~htmlEscape(popup_text))%>%
  addPolygons(data=MS_shape , color="black", fillOpacity = 0, weight = 1)%>%
  addLegend(colors = c("blue", "red"), 
            labels=c("Netatmo station", "Thermochron iButton"))%>%
  addScaleBar()
  
  #map.scale(xc = 52, yc = 7,len = 10, ndivs = 10, )%>%
  #north.arrow()
?map.scale
?north.arrow
  #addMiniMap()

