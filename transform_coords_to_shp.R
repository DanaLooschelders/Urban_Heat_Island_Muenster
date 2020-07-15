#Arbeitsverzeichnis setzen
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
#libraries laden
library(leaflet)
library(raster)
#Daten einladen
coords=read.table("Koordinaten_Plots.csv", sep=";", dec=".", header=T)
#Spalten umbenennen
names(coords)[2:3]=c("lng", "lat")
#plot in leaflet
leaflet(data=coords)%>%
  addTiles()%>%
  addCircles()
#in SpatialPoints umwandeln und Koordinatensystem setzen
coords_df=SpatialPoints(coords=coords[2:3], proj4string = CRS("+proj=longlat"))
#in shapefile schreiben und abspeichern
raster::shapefile(coords_df, "shapefile_coords_plot.shp")