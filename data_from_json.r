#library(rjson)
#library(jsonlite)

#rjson and rjsonlite are probably the better packages but because I'm stupid I used 
#RJSONIO and therefore this code only works with RJSONIO
library(RJSONIO)
#library(curl)

setwd("C:/Users/danan/Desktop")
metadata=fromJSON("data_for_metadata.json", flatten=TRUE)

#extract station id from metadata 
id=rep(NA,length(metadata[["body"]]))
for (i in 1:length(metadata[["body"]])) {
id[i]=metadata[["body"]][[i]]$`_id`
}

#extract station id and place from metadata
metadata_tidy=list()
id=rep(NA,length(metadata[["body"]]))

for (i in 1:length(metadata[["body"]])) {
  #extract metadata
 temp.data=as.data.frame(metadata[["body"]][[i]]$place)
 #extract latlon values
 lonlat=temp.data$location
  #remove redunant first row of dataframe (between the two rows only locations differs)
 if(ncol(temp.data)==6){ #as sometimes the column "street" is missing
 temp.data=temp.data[1,2:6]
 temp.data$lon=lonlat[1] #assign prev extracted longitude value
 temp.data$lat=lonlat[2]#assign prev extracted latitude value
 metadata_tidy[[i]]=temp.data #write dataframe in list
 id[i]=metadata[["body"]][[i]]$`_id`#assign station id to a vector of ids
 names(metadata_tidy)[i]=id[i] #assign station id as name for metadata list
 }else{
   if(is.null(temp.data$street)){
   temp.data=temp.data[1,2:5]
  temp.data$street=NA #create column with street and set to NA
  temp.data$lon=lonlat[1] #assign prev extracted longitude value
  temp.data$lat=lonlat[2]#assign prev extracted latitude value

  metadata_tidy[[i]]=temp.data #write dataframe in list
  id[i]=metadata[["body"]][[i]]$`_id`#assign station id to a vector of ids
  names(metadata_tidy)[i]=id[i] #assign station id as name for metadata list
 }else{print(names(metadata[i]))}
 }
}

setwd("~/Urban_Heat_Island_Muenster")
metadata_table=do.call(rbind.data.frame, metadata_tidy)
metadata_table$id=row.names(metadata_table)
row.names(metadata_table)=NULL
write.table(metadata_table, "metadata_netatmo.csv", sep=";", dec=",", row.names = F)

detach("package:RJSONIO", unload=TRUE)
library(rjson)
library(jsonlite)

metadata2=fromJSON("data_for_metadata.json", flatten=TRUE)
ids=metadata2[["body"]][1]

library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(sf)
setwd("F:/satellite_data_Muenster/MODIS_neu")
MS_shape=readOGR("stadtgebiet.shp")
plot(MS_shape)
crs(MS_shape)
#transform coordinates to lat lon
MS_shape=spTransform(x = MS_shape, CRSobj = "+proj=longlat +datum=WGS84")
crs(MS_shape)
plot(MS_shape)
#plot coords of netatmo stations
coords=metadata_table[,6:8]
names(coords)=c("Lon", "Lat","ID")
coords_test=SpatialPoints(coords = coords[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

plot(coords_test)
attributes(coords_test)=as.list(coords[,3])
#MS_shape = spTransform(MS_shape, "+init=epsg:4326")
plot(MS_shape)

#test plotting coordiantes
leaflet(MS_shape) %>%
  addPolygons() %>%
  addTiles() %>%
  addMarkers(data=coords, lng = ~Lon, lat = ~Lat)

subset <- coords_test[MS_shape, ]
plot(subset)
ID_in_MS=coords$ID[intersect(coords[,1], subset@coords[,1])]
test.subset=intersect(coords[,1], subset@coords[,1])

test.subset2=intersect(coords[,1], subset@coords[,1])
ID=data.frame("IDs"=rep(NA, length(test.subset)), "Lon"=test.subset, "Lat"=rep(NA, length(test.subset)))
for(i in test.subset){
  ID$IDs[ID$Lon==i]=coords$ID[coords$Lat==i]
  ID$Lat[ID$Lon==i]=coords$Lat[coords$Lat==i]
}

write.table(ID_in_MS, "netatmo_ids.csv", sep=";", dec=".")

#get unix time to download data
start.date=as.numeric(as.POSIXct("2019-08-01", format="%Y-%m-%d"))
end.time=as.numeric(as.POSIXct("2019-08-30", format="%Y-%m-%d"))

#try to download data from first station
#70:ee:50:13:29:da
#timelapse 30 min
#1378418400
#temperature
#1567116000
