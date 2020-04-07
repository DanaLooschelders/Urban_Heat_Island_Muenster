#Heatmap

library(leaflet)
library(sp)
library(tidyverse)
library(rlist)
library(maptools)
library(KernSmooth) #for kernel density estimate?
library(leaflet.extras)
setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit")
#read in coordiante data
coords=read.table("Lat_Lon_Logger.csv", sep=";", dec=",", header=T)
str(coords)
coords=coords[,-4] #drop last empty column
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
}
for(i in mapdata$ID) {
  dummy.data.frame=list_iButton_agg_mean[[i]]
  dummy.value=dummy.data.frame[1,2]
  mapdata$temp[mapdata$ID==i]=dummy.value
}

mapdata=mapdata %>% drop_na() #drop columns with NAs

# create magnitude range to define the type as follows 
range(mapdata$temp)
mapdata$magrange = cut(mapdata$temp, 
                       breaks = c(18, 19, 20,21, 22, 23, 24), right=FALSE)

# Define a color pallete corresponding to the magnitude ranges
pal = colorFactor(palette = c("yellow","orange", "darkorange" ,"red", "purple", "brown"), domain=mapdata$magrange)

#plot pointmap first try
leaflet(data=mapdata) %>%
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat, color=~pal(magrange), radius = 60)%>%
  addLegend(pal = pal, values=mapdata$magrange)

#for heat map: use kernal density 
?bkde2D

#try heat map with stat summery
get_map(location = c(lon= 7.670,lat= 51.925))
#nececessary to register for google

ggmap(ggmap = map, extent="device")+
  stat_summary_2d(data=mapdata, aes(x=lon, y=lat, z=temp), fun=mean, alpha=0.6, bins=10)+
  scale_fill_gradient(name="Temperature", low="green", high="red")

#heatmap with leaflet
leaflet(data=mapdata) %>%
  addTiles() %>%
  addHeatm