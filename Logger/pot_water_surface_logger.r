#potential near water surface air temperature points
library(leaflet)

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
pot_logger
leaflet()%>%
  addTiles()%>%
  addCircles(data=pot_logger)
