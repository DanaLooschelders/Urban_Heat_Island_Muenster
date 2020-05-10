#Wind data from DWD
#downloaded via: https://www.dwd.de/DE/leistungen/klimadatendeutschland/klarchivstunden.html
#for Muenster/Osnabrück 
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/DWD data/stundenwerte_FF_01766_akt/")

#read metadata
meta_geog=read.table("Metadaten_Geographie_01766.txt", sep=";", dec=".", header=T)
meta_wind_speed=read.table("Metadaten_Geraete_Windgeschwindigkeit_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file
meta_wind_dir=read.table("Metadaten_Geraete_Windrichtung_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file
#Legende: FT  = Folgetag; GZ = Gesetzliche Zeit 
meta_parameter=read.table("Metadaten_Parameter_ff_stunde_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file
meta_station_ID=read.table("Metadaten_Stationsname_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file

#read in data
data=read.table("produkt_ff_stunde_20181106_20200508_01766.txt", sep=";", dec=".", header=T)
data=data[data$MESS_DATUM>=2019080100,]
names(data)[4]="wind_speed"
names(data)[5]="wind_direction"

#QAQC
data$wind_speed[data$wind_speed<0]=NA
plot(data$MESS_DATUM, data$wind_speed, type="l")
hist(data$wind_speed)
hist(data$wind_direction)
