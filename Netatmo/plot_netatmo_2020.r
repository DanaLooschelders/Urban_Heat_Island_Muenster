library(leaflet)
library(tidyverse)
library(mapview)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/Netatmo")
list_netatmo_col=lapply(list_netatmo_level_D, `[`, 2)
d_netatmo <- data.frame(x = unlist(list_netatmo_col), 
                           site = rep(names(list_netatmo_col),
                                      times = sapply(list_netatmo_col,length)))
range(d_netatmo$x, na.rm=T)
mean(d_netatmo$x, na.rm=T)
median(d_netatmo$x, na.rm=T)
sd(d_netatmo$x, na.rm=T)

#boxplot
Netatmo_col=lapply(list_netatmo_level_D, `[`, 2)
netatmo_d <- data.frame(x = unlist(Netatmo_col), 
                   site = rep(names(Netatmo_col),
                              times = sapply(Netatmo_col,length)))
ggplot(netatmo_d,aes(x = site, y = x)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ #rotates the axix labels on x axis
  xlab("Sites")+ #adds title for x axis
  ylab("Temperature [°C]")+ #adds title for y axis
  ggtitle("Temperature in July 2020")+ #adds plot title
  stat_summary(fun.y="mean", geom="point", size=1, col="red")
ggsave(filename = "Netatmo_July_2020_Boxplot.pfd",  device="pdf",width = 14, height=7, units = "in")

#add column with mean for August to metadata_merge
for (i in names(list_netatmo_level_D)){
  data=list_netatmo_level_D[[i]]
  metadata_merge$mean_temp[metadata_merge$device_id==i]=mean(data$temperature, na.rm=T)
  metadata_merge$meadian_temp[metadata_merge$device_id==i]=median(data$temperature, na.rm=T)
  metadata_merge$max_temp[metadata_merge$device_id==i]=max(data$temperature, na.rm=T)
  metadata_merge$min_temp[metadata_merge$device_id==i]=min(data$temperature, na.rm=T)
  metadata_merge$sd_temp[metadata_merge$device_id==i]=sd(data$temperature, na.rm=T)
  
}
metadata_merge$mean_temp=round(metadata_merge$mean_temp,digits = 1)

write.table(x = metadata_merge, file = "metadata_with_stats.csv", sep=";",dec=",")
#plot netatmo distribution in Muenster

webshot::install_phantomjs()
#setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
#netatmo=read.table("netatmo_metadata.csv", sep=";", dec=".", header=T)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/Netatmo")

#plot netatmo distribution
leaflet(data=metadata_merge) %>%
  addTiles() %>%
  addCircles() %>%
  addPolygons(data=MS_shape , color="black", fillOpacity = 0, weight = 1)

#plot netatmo ditribution with temperature
#create palette for binned colors
binpal <- colorBin(palette=c("yellow", "orange", "red", "brown"), metadata_merge$mean_temp, 4, pretty = FALSE)
options(digits=3)
#plot the mean temperature for August and September
map=  leaflet(data=metadata_merge)%>%
  addTiles() %>%
  addCircles(color=~binpal(mean_temp), fill=~binpal(mean_temp), 
             weight = 10, fillOpacity = 200,stroke = T,opacity = 20)%>%
  addPolygons(data=MS_shape , color="black", fillOpacity = 0, weight = 1)%>%
  addLegend(pal=binpal, values=metadata_merge$mean_temp)
mapshot(map, file="netatmo_mean_2020.png")


#transform coordiantes to lat lon and create spatial points
points=SpatialPointsDataFrame(coords = metadata_merge[2:3], 
                              proj4string=CRS("+proj=longlat +datum=WGS84"),
                              data=metadata_merge)

#final test: plotting points in shapefile
leaflet(MS_shape) %>%
  addPolygons() %>%
  addTiles() %>%
  addCircles(data=points)
