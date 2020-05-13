#plot netatmo distribution in Muenster
library(leaflet)
library(tidyverse)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
netatmo=read.table("netatmo_coords_id.csv", sep=";", dec=".")
#plot netatmo distribution
leaflet(data=netatmo) %>%
  addTiles() %>%
  addCircles() %>%
addPolygons(data=MS_shape , color="black", fillOpacity = 0, weight = 1)

