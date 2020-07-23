require(leaflet)
require(htmltools)
require(htmlwidgets)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
coords=read.table(file="Sensortabelle Kartierung Stand 22.7.csv", dec=",", sep=";", header=T)
coords=coords[-1,]
leaflet(data=coords)%>%
  addTiles()%>%
addCircles(data=coords, lat=coords$Lat2, lng=coords$Lon2)%>%
  addPopups(data=coords, lat=coords$Lat2, lng=coords$Lon2,popup = htmlEscape(coords$Ortsbeschreibung))

#use: all Aegidii logger, Ehrenpark, Aaseemensa VL and SL Spätzl
