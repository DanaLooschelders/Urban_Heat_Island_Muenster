#library(rjson)
#library(jsonlite)

#rjson and rjsonlite are probably the better packages but because I'm stupid I used 
#RJSONIO and therefore this code only works with RJSONIO
library(RJSONIO)
#library(curl)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
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

#*****************************************************************
#use alternative package
detach("package:RJSONIO", unload=TRUE)
library(rjson)
library(jsonlite)

metadata2=fromJSON("data_for_metadata.json", flatten=TRUE)
ids=metadata2[["body"]][1]

#******************************************************************
#spatial subset to select netatmo stations within Muenster
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(sf)
#read in shapefile of Muenster
setwd("F:/satellite_data_Muenster/MODIS_neu")
MS_shape=readOGR("stadtgebiet.shp")
plot(MS_shape) #plot
crs(MS_shape) #get crs
#transform coordinates to lat lon
MS_shape=spTransform(x = MS_shape, CRSobj = "+proj=longlat +datum=WGS84")
crs(MS_shape) #check
plot(MS_shape)
#plot points of netatmo stations
points=metadata_table[,6:8] #get lat lon data from points
names(points)=c("Lon", "Lat","ID")
#transform coordiantes to lat lon and create spatial points
points_test=SpatialPoints(coords = points[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))
#add ID as attribute to and create spatial points dataframe
points_dataframe=SpatialPointsDataFrame(data = points, proj4string = CRS("+proj=longlat +datum=WGS84"), coords = points_test)
plot(points_dataframe) #plot points
plot(MS_shape)

#test: plotting points in shapefile
leaflet(MS_shape) %>%
  addPolygons() %>%
  addTiles() %>%
  addMarkers(data=points, lng = ~Lon, lat = ~Lat)
#subset points by shapefile -> get only points within Muenster
subset <- points_dataframe[MS_shape, ]
plot(subset) #plot

ID_in_MS=subset@data$ID #get vector with netatmo IDs in MS
netatmo_in_MS=subset@data
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
write.table(ID_in_MS, "netatmo_ids.csv", sep=";", dec=".")
write.table(netatmo_in_MS, "netatmo_coords_id.csv", sep=";", dec=".")
#get unix time to download data
start.date=as.numeric(as.POSIXct("2020-04-01 00:00:00", format="%Y-%m-%d %H:%M:%S"))
end.date=as.numeric(as.POSIXct("2020-04-30 00:00:00", format="%Y-%m-%d %H:%M:%S"))

client_ID=5ebbc836069acb6e1e7f7a4e
client_secret=GZDQSz0KjqxdbrMdu3GLSYsz35
#try to download data from first station
#70:ee:50:00:eb:6e
#timelapse 30 min
#1378418400
#temperature
#1567116000
