#isarithmic map 
#with interpolation of points 
#through inverse path distance weighing
library(rgdal)
library(ipdw)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
#load coordinate data
coords=read.table("Lat_Lon_Logger.csv", sep=";", dec=".", header=T)
str(coords)
names(coords)[1]="ID"
coords=coords[coords$ID!="-",] #drop columns with missing ID
#rownames(coords)=coords[,1] #set logge id as rownames
coords=coords[,1:3] #keep only lat and long values
str(coords)
#get temperature data (use 24h means) for first day as test dataset
temp=lapply(list_iButton_24h_mms, function(x) x[1,2])
#create dataframe with mean temp and ID (to match temp to coords)
temperature=data.frame("name"=names(temp), "temp"=unlist(temp))
#match temp to ID
for(i in coords[,1]){
  coords$temp[coords[,1]==i]=temperature$temp[temperature$name==i]
  }
str(coords) #check
SP_coords=SpatialPointsDataFrame(coords=coords[,2:3], 
                              proj4string = CRS("+proj=longlat +datum=WGS84"),
                              data=coords[,1&4])
str(SP_coords)

#use ipdw to create a cost raster
costras=costrasterGen()
