#leaflet for thesis
metadata_map=metadata
#format coordinates
metadata_map$Lat=metadata_map$Lat/1000000
metadata_map$Lon=metadata_map$Lon/1000000
metadata_map$color[metadata_map$color=="darkgrey"]="black"
metadata_map$color[metadata_map$color=="lightblue"]="red"
#generate map
library(leaflet)
leaflet()%>%
  addTiles()%>%
  addCircles(data=metadata_map, 
             color = metadata_map$color,
             opacity = 1)%>%
  addLegend(position="topright",
            labels = unique(metadata_map$Loggertyp),
            colors=unique(metadata_map$color))

