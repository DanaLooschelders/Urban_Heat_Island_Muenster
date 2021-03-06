#Wind data from DWD
#downloaded via: https://www.dwd.de/DE/leistungen/klimadatendeutschland/klarchivstunden.html
#for Muenster/Osnabrück 
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/DWD data/stundenwerte_FF_01766_akt/")

#read metadata
meta_geog_wind=read.table("Metadaten_Geographie_01766.txt", sep=";", dec=".", header=T)
meta_wind_speed=read.table("Metadaten_Geraete_Windgeschwindigkeit_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file
meta_wind_dir=read.table("Metadaten_Geraete_Windrichtung_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file
#Legende: FT  = Folgetag; GZ = Gesetzliche Zeit 
meta_parameter_wind=read.table("Metadaten_Parameter_ff_stunde_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file
meta_station_ID_wind=read.table("Metadaten_Stationsname_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file

#read in data
wind=read.table("produkt_ff_stunde_20181106_20200508_01766.txt", sep=";", dec=".", header=T)
wind=wind[wind$MESS_DATUM>=2019080100,]
names(wind)[4]="wind_speed"
names(wind)[5]="wind_direction"

#QAQC
wind$wind_speed[wind$wind_speed<0]=NA
plot(wind$MESS_DATUM, wind$wind_speed, type="l")
hist(wind$wind_speed)
hist(wind$wind_direction)

#Messdatum to Posixlt
wind$MESS_DATUM=strptime(wind$MESS_DATUM, format="%Y%m%d %H", tz="Europe/Paris")
str(wind$MESS_DATUM)
#temperature data from DWD
#downloaded via: https://www.dwd.de/DE/leistungen/klimadatendeutschland/klarchivstunden.html
#for Muenster/Osnabrück 
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/DWD data/stundenwerte_TU_01766_akt//")

#read metadata
meta_geog_temp=read.table("Metadaten_Geographie_01766.txt", sep=";", dec=".", header=T)
meta_temp=read.table("Metadaten_Geraete_Lufttemperatur_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file
#Legende: FT  = Folgetag; GZ = Gesetzliche Zeit 
meta_parameter_temp=read.table("Metadaten_Parameter_tu_stunde_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file
meta_station_ID_temp=read.table("Metadaten_Stationsname_01766.txt", sep=";", dec=".", header=T) #deleted last line in .txt file

#read in data
temp=read.table("produkt_tu_stunde_20181106_20200508_01766.txt", sep=";", dec=".", header=T)
temp=temp[temp$MESS_DATUM>=2019080100,]
temp$MESS_DATUM=strptime(temp$MESS_DATUM, format="%Y%m%d")
str(temp)
