library(leaflet)
library(sp)
library(htmltools)
Aegiditor=c(51.957416, 7.618800)
Schuhladen=c(51.957809, 7.620229)
Baum_54=c(51.956666, 7.620051)
Anfang_Aegidistr=c(51.957569, 7.620603)

points=rbind(Aegiditor, Schuhladen, Baum_54, Anfang_Aegidistr)
colnames(points)=c("Lat", "Lon")
points=as.data.frame(points)
leaflet(data=points)%>%
  addTiles()%>%
  addCircles%>%
  addMarkers()

