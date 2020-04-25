#Heatmap

library(leaflet)
library(sp)
library(tidyverse)
library(rlist)
library(maptools)
library(KernSmooth) #for kernel density estimate?
library(RColorBrewer)
library(leaflet.extras)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
#read in coordiante data
coords=read.table("Lat_Lon_Logger.csv", sep=";", dec=".", header=T)
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
names(list_iButton_agg_mean)=names(list_iButton_corr)[-1]

#write for loop to create dataframe with mean temperatures for one day for all IDs
#create dataframe to fill in for loop
list_iButton_agg_mean[[1]]=NULL #drop first list element as logger is missing its ID)
mapdata=data.frame(ID=names(list_iButton_agg_mean), lat=rep(NA, length(list_iButton_agg_mean)), lon=rep(NA, length(list_iButton_agg_mean)), temp=rep(NA, length(list_iButton_agg_mean)))
coords=coords %>% drop_na() #drops rows that contains NA values (as some coordinates have no logger ID)
coords
for (i in coords$ID){
  mapdata$lat[mapdata$ID==i]=coords$Lat[coords$ID==i]
  mapdata$lon[mapdata$ID==i]=coords$Lon[coords$ID==i]
}
for(i in mapdata$ID) {
  dummy.data.frame=list_iButton_agg_mean[[i]]
  dummy.value=dummy.data.frame[3,2]
  mapdata$temp[mapdata$ID==i]=dummy.value
}

mapdata=mapdata %>% drop_na() #drop columns with NAs

# create magnitude range to define the type as follows 
range(mapdata$temp)
mapdata$magrange = cut(mapdata$temp, 
                       breaks = c(17,18, 19, 20,21, 22, 23, 24,25), right=FALSE)

# Define a color pallete corresponding to the magnitude ranges
pal = colorFactor(palette = c("blue", "green","yellow", "darkorange" ,"red", "darkred", "purple", "brown"), domain=mapdata$magrange)
pal2=colorNumeric(domain = mapdata$temp, palette="7-class YlOrRd")
pal2
pal3=colorFactor(palette=heat.colors(7, rev = T), domain = mapdata$magrange)
#plot pointmap first try
leaflet(data=mapdata) %>%
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat, color=~pal3(magrange), 
             radius = 100, stroke = F, opacity = 50, fillOpacity = 0.5)%>%
  addLegend(pal = pal3, values=mapdata$magrange)

#heatmap with leaflet

display.brewer.pal(6, "YlOrRd")

#the heatmap somehow doesnt display the values correctly
leaflet(data=mapdata) %>%
  addTiles() %>%
  addHeatmap(lng = ~lon, lat = ~lat, 
             intensity = ~temp, radius = 12, 
             cellSize = 15, blur = 25, minOpacity = 20, max = 30, gradient = terrain.colors(mapdata$temp)) %>%
  addCircles(lng = ~lon, lat = ~lat, color=~pal(magrange), radius = 50) %>%
  addLegend(pal = pal, values=mapdata$magrange)
#add a legend
?addHeatmap
#try with dummy values
dummy=rnorm(30, mean = 50, sd=50)
str(mapdata)
mapdata$dummy=dummy

leaflet(data=mapdata) %>%
  addTiles() %>%
  addHeatmap(lng = ~lon, lat = ~lat, 
             intensity = ~dummy, radius = 12, 
             cellSize = 10, blur = 25)

#test with quake data
leaflet(quakes) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( 178, -20, 5 ) %>%
  addHeatmap(
    lng = ~long, lat = ~lat, intensity = ~mag,
    blur = 20, max = 0.05, radius = 15
  )


#heatmap with one layer for vegetation and one for sealed areas
#try as filled contour plot
#sort coordinates in ascending order
mapdata_ordered=mapdata[,1:4]
mapdata_ordered=mapdata_ordered[order(mapdata_ordered$lat, mapdata_ordered$lon),]

?filled.contour()
filled.contour(x=mapdata_ordered$lat, y = mapdata_ordered$lon, z=mapdata_ordered$temp, color=terrain.colors)
#data isn't ordered correctly?

#try again with kernel density and contour
bkde2D()