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

#test dissimilarities between ts
aegidii=matrix(nrow=length(list_iButton_hourly_Aegidii), 
               ncol=length(list_iButton_hourly_Aegidii[[1]][,1]))
for(i in 1:length(list_iButton_hourly_Aegidii)){
  aegidii[i,]=list_iButton_hourly_Aegidii[[i]][,2]
}

#Decide on dissimilarity method
diss(SERIES=na.spline(aegidii), METHOD="CORT")

ccf(na.spline(aegidii[1,]), na.spline(aegidii[2,]), lag.max=200)

