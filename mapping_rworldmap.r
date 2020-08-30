options(digits=10)
#**********************************************************************
#prep data
metadata_map=metadata
#format coordinates
metadata_map$Lat=metadata_map$Lat/1000000
metadata_map$Lon=metadata_map$Lon/1000000
metadata_map$color[metadata_map$color=="darkgrey"]="black"
metadata_map$color[metadata_map$color=="lightblue"]="red"
#*********************************************************************
#map with leaflet
#generate map
library(leaflet)

leaflet()%>%
  addTiles()%>%
  addCircles(data=metadata_map, 
             color = metadata_map$color,
             opacity = 1)%>%
  addLegend(position="topright",
            labels = c("Water surface Logger", "Sealed surface Logger", "Water Logger", "Vegetated surface Logger"),
            colors=unique(metadata_map$color),
            title="Logger types")%>%
  addScaleBar(position="bottomleft")%>%
  addMiniMap()

north.arrow(xb=52.55, yb=7.7, lab="NORTH", len=1, cex.lab=0.5)

#*********************************************************************
#map with rworldmap and ggplot
library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(GISTools)
world=getMap(resolution="high")
plot(world, 
     main="World", 
     ylim=c(49, 52),
     xlim=c(6,8))

setwd("F:/satellite_data_Muenster/MODIS_neu")
MS_shape=readOGR("stadtgebiet.shp")
crs(MS_shape) #get crs
#transform coordinates to lat lon
MS_shape=spTransform(x = MS_shape, CRSobj = "+proj=longlat +datum=WGS84")
MS_shape_plot=fortify(MS_shape)

world_DE=world[world@data$ADMIN %in% "Germany",]
par(plot.new=T)
ggplot()+
  #geom_polygon(data=world_DE,
               #aes(x=long, y=lat), fill=NA, color="black")+
  geom_polygon(data=MS_shape_plot,
               aes(x=long, y=lat), color="black", fill="white")+
  geom_point(data=metadata_map, aes(x=Lon, y=Lat), color=metadata_map$color)+
  coord_quickmap()+
  #xlim(6,9)+
  #ylim(51,52)+
  theme_classic()+
  xlab("Longitude")+
  ylab("Latitude")
 map.scale(len=10, ndivs=5, xc=7.6, yc=51.85)

#try with tmap
library(tmap) 
 data("World")
 tmap_mode("view")
 
 tm_shape(World)+
   tm_shape(shp=MS_shape,is.master = T)
 