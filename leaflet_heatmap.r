#Heatmap

library(leaflet)
library(sp)
library(tidyverse)
library(rlist)

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
#subset from 03.08.2019 to 14.08.2019
str(list_iButton_corr) 
iButton_ID_multi #vector with iButton IDs

#create new column with just the date
list_iButton_col <- lapply(list_iButton_corr, `[`, 1) #extract column to be transformed
list_iButton_col=lapply(list_iButton_col, function(x) as.Date(format(x$Datetime)))#format column
list_iButton_corr <- mapply(cbind, list_iButton_corr, "Date"=list_iButton_col, SIMPLIFY=F)#bind transformed column and dtaframe together

#aggregate the daily mean temperatures in new list
list_iButton_corr=list_iButton_corr[lapply(list_iButton_corr,nrow)>0] #remove all data.frames with empty columns
dat=seq(from=1, by=1, to=length(list_iButton_corr)) #list to ID dataframes
list_iButton_agg_mean=list() #create list for output data from for loop
#use for loop to aggregate through list 
for (i in dat) {
  dummy=as.data.frame(list_iButton_corr[[i]])
  list_iButton_agg_mean[[i]]=aggregate(dummy$Temperature_C, by=list(date=dummy$Date), FUN=mean)
}
list.save(list_iButton_agg_mean, "list_agg.rdata") #save list as Rdata object
names(list_iButton_agg_mean)=iButton_ID_multi$V2

#write for loop to create dataframe with mean temperatures for one day for all IDs
#create dataframe to fill in for loop
list_iButton_agg_mean[[1]]=NULL #drop first list element as logger is missing its ID)
mapdata=data.frame(ID=names(list_iButton_agg_mean), lat=rep(NA,32), lon=rep(NA,32), temp=rep(NA,32))
coords=coords %>% drop_na() #drops rows that contains NA values (as some coordinates have no logger ID)
coords
for (i in coords$ID){
  mapdata$lat[mapdata$ID==i]=coords$Lat[coords$ID==i]
  mapdata$lon[mapdata$ID==i]=coords$Lon[coords$ID==i]
  if()
  dummy.data.frame=list_iButton_agg_mean[[i]]
  dummy.value=dummy.data.frame[1,2]
 mapdata$temp[mapdata$ID==i]=dummy.value
}
str(coords)
coords$ID
