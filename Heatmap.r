#create a heat map 
library(ggplot2)
library(ggmap)
library(RColorBrewer)

library(maps)
library(ggplot2)
library(OpenStreetMap)
library(leaflet)
library(osmar)
library(rJava)

library(rlist)

setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit/2019-Urban_Heat_Island/2019-Urban_Heat_Island/Data/Data_raw/UHI_20190802-20190930")

#get map from Muenster -> borrowed from coordinates sampling points
map = openmap(upperLeft=c(lat= 52.025,   lon= 7.58), lowerRight = c(lat= 51.92,   lon= 7.68), type = "osm")
map = openmap(upperLeft=c(lat= 51.985,   lon= 7.58), lowerRight = c(lat= 51.925,   lon= 7.670), type = "osm-german",
              minNumTiles = 15)
#map = openmap(upperLeft=c(lat= 52.025,   lon= 7.58), lowerRight = c(lat= 51.92,   lon= 7.68), type = "opencyclemap")


mapLatLon <- openproj(map)

autoplot(mapLatLon)

Sampling_point = c("Aa","Aasee", "Aaseewiesen","Aegidiistr", "Domplatz","Geistviertel green settlement",
                   "Geistviertel settlement", "Geistviertel Suedpark","Gievenbach","Hafen","Haus Kump",
                   "Kanal Industriegebiet", "Kanal Schleuse","Kleingartenanlage MÃ¼hlenfeld", 
                   "Kreuzviertel settlement","Parkplatz Hawerkamp","Pluggendorf settlement", "Schlossparkplatz",
                   "Schlossgarten","Schlossgarten See","Promenade", "Sentruper Hoehe", "Wienburgpark","Wienburgpark See")

Coordinate_lat = c(51.973522,51.956826,51.954594,51.974000,51.959252,51.941530,51.962643,51.953432,51.948526,51.955919,
                   51.950276,51.939229,51.943965,51.984049,51.970737,51.945107,51.953709,51.964184,51.963804,51.963695,
                   51.967040,51.957655,51.981307,51.981008)

51.969556
7.606352
Coordinate_lon = c(7.633765,7.616461, 7.608547,7.634639,7.622491,7.618439,7.625990,7.623235,7.627253,7.584968,7.647912,7.584847,7.641204,
                   7.660262,7.622594,7.637644,7.617641,7.616971,7.608420,7.608956,7.624614,7.588178,7.626956,7.629379)

7.629379
51.981008

UHI_data = data.frame(Sampling_point,Coordinate_lat,Coordinate_lon)

# Temperatur Sonic
UHI_map <- autoplot(mapLatLon)  +
  labs(title = "UHI Measurement Points",x = "Longitude", y="Latitude") +
  geom_point(data=UHI_data, aes(x = Coordinate_lon, y = Coordinate_lat), alpha=0.5, size = 4)

plot(UHI_map)

#calculate mean for every logger for one day (or one night)
#use list created in iButtons-Check: list_iButton_corr

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
#nice to keep
list2env(list_iButton_col ,.GlobalEnv) #unlist list and store dataframes in global environment

#now get dataframe with the mean temperatures of the first day for every sampling location
logger=read.table("Loggerliste_UHI.csv", skip=1, header=T, sep=";", dec=".")
