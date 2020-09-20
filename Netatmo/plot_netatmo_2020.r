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
names(Netatmo_col)=as.character(seq(1, length(Netatmo_col)))
names_netatmo=names(Netatmo_col)
netatmo_d <- data.frame(temp = unlist(Netatmo_col), 
                   site = as.integer(rep(names(Netatmo_col)),
                              times = sapply(Netatmo_col,length)))
means <- aggregate(temp ~  site, netatmo_d, median)
means=round(means, 1)
ggplot(data=netatmo_d, aes(x = as.factor(site), y = temp)) +
  geom_boxplot()+
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ #rotates the axix labels on x axis
  xlab("")+ #adds title for x axis
  ylab("Temperature [°C]")+ #adds title for y axis
  stat_summary(fun="mean", geom="point", size=1, col="black")+
  theme_classic()+
 #geom_text(data=metadata_merge, aes(label=mean_temp, y=mean_temp+1), size=2)
ggsave(filename = "Netatmo_July_2020_Boxplot.pfd",  device="pdf",width = 14, height=7, units = "in")

#
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
binpal <- colorBin(palette=c("#e66101","#fdb863","#b2abd2","#5e3c99"), 
                   round(metadata_merge$mean_temp,digits = 1), 4, 
                   pretty = FALSE,)
options(digits=8)
metadata_merge$new_ID=names_netatmo
metadata_merge=metadata_merge[-37,]
#plot the mean temperature for August and September
  leaflet(data=metadata_merge)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  addCircles(color=~binpal(mean_temp), fill=~binpal(mean_temp), 
             weight = 10, fillOpacity = 200,stroke = T,opacity = 20)%>%
  addPolygons(data=MS_shape , color="black", fillOpacity = 0, weight = 1)%>%
  addLegend(pal=binpal, values=metadata_merge$mean_temp)%>%
  #addLabelOnlyMarkers(data=metadata_merge, label=~metadata_merge$new_ID,
                    #labelOptions=labelOptions(noHide=T,
                                             # direction="right", 
                                              #textOnly=T))%>%
  addScaleBar()

mapshot(map, file="netatmo_mean_2020.png")
#CONTINUE HERE
#do map with numbers indead of dots
binpal[["colorArgs"]]$bins
leaflet(data=metadata_merge)%>%
  addProviderTiles(provider="CartoDB.VoyagerNoLabels")%>%
  addCircles(color=~binpal(mean_temp), fill = T, 
             fillOpacity = 2000, opacity = 2000,
             fillColor = ~binpal(mean_temp))%>%
  addLegend(pal=binpal, values=round(metadata_merge$mean_temp,1),
            opacity = 200, labFormat=labelFormat(digits = 1))%>%
  addScaleBar()
  
#transform coordiantes to lat lon and create spatial points
points=SpatialPointsDataFrame(coords = metadata_merge[2:3], 
                              proj4string=CRS("+proj=longlat +datum=WGS84"),
                              data=metadata_merge)

#final test: plotting points in shapefile
leaflet(MS_shape) %>%
  addPolygons() %>%
  addTiles() %>%
  addCircles(data=points)
