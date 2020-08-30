list_netatmo_col=lapply(list_netatmo_level_D, `[`, 2)
d_netatmo <- data.frame(x = unlist(list_netatmo_col), 
                           site = rep(names(list_netatmo_col),
                                      times = sapply(list_netatmo_col,length)))
range(d_netatmo$x, na.rm=T)
mean(d_netatmo$x, na.rm=T)
median(d_netatmo$x, na.rm=T)
sd(d_netatmo$x, na.rm=T)

#transform coordiantes to lat lon and create spatial points
points=SpatialPointsDataFrame(coords = metadata_merge[2:3], 
                              proj4string=CRS("+proj=longlat +datum=WGS84"),
                              data=metadata_merge)

#final test: plotting points in shapefile
leaflet(MS_shape) %>%
  addPolygons() %>%
  addTiles() %>%
  addCircles(data=points)
