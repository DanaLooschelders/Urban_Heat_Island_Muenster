#Uebersichtskarte
library(leaflet)
library(sp)

#Netatmo Logger
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo")
netatmo=read.csv2("Netatmo_metadata.csv")
str(netatmo)
netatmo$popup_text=paste("altitude:",netatmo$altitude, "m NN")

#aktuell vorhandene Logger
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
logger=read.table("Logger_Feldprotokoll.csv", sep=";", dec=",", header=T, na.strings=c("", "-"))
str(logger)
names(logger)[1]="ID"
names(logger)[4]="hoehe"
#logger=logger[logger$Vorhanden.am.05.06.2020=="Ja",]
replace=read.table("zu_ersetzende_logger.csv", sep=";", dec=",", header=T)
#neue/zu ersetzende Logger
Aegiditor=c(51.957416, 7.618800)
Schuhladen=c(51.957809, 7.620229)
Baum_54=c(51.956666, 7.620051)
Anfang_Aegidistr=c(51.957569, 7.620603)

new_logger=rbind(Aegiditor, Schuhladen, Baum_54, Anfang_Aegidistr)
colnames(new_logger)=c("Lat", "Long")
new_logger=as.data.frame(new_logger)
new_logger$description=rownames(new_logger)
#wasseroberflaechentemperatur Logger
#Busch neben Totholbaumstumpf gegenueber Aula am Aasee
Aula=c(51.951097, 7.606878) 
#Baum hinter Torminbruecke
Torminbruecke_ost=c(51.949684, 7.605047)
#Tothalzbaum in Renaturierung hinter der 2. Brücke
Renaturierung=c(51.942213, 7.594722)
#Trauerweide gegenueber von Grillhuetten
Weide_west=c(51.943479, 7.594918)
#Busch gegenueber von großem, einzeln stehendem Ahorn
Busch_ahorn=c(51.948602, 7.599938)
#Busch gegenueber von 2 Baenken und Baumgruppe
Busch_baenke=c(51.950013, 7.603038)

pot_logger=rbind(Aula, Torminbruecke_ost, 
                 Renaturierung, Weide_west,
                 Busch_ahorn, Busch_baenke)
pot_logger=as.data.frame(pot_logger)
names(pot_logger)=c("lat", "long")
pot_logger$description=rownames(pot_logger)

#plot on map
leaflet()%>%
  addTiles()%>%
  addCircles(data=pot_logger, color="blue")%>% #potential water surface
  addCircles(data=netatmo, color="orange")%>%
  addCircles(data=logger[logger$Vorhanden.am.05.06.2020=="Ja",], color="green")%>%
  addCircles(data=logger[logger$Vorhanden.am.05.06.2020!="Ja",], color="red")%>%
  addCircles(data=replace, color="black")%>%
  addCircles(data=new_logger, color="black")%>%
  addLegend(colors=c("blue", "orange", "green", "red", 
                     "black"), 
            labels=c("Potential water surface", "Netatmo stations", 
                     "Existing logger", "Missing logger",
                     "Logger to be replaced"))
  