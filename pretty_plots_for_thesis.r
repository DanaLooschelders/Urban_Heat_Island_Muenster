library(leaflet)
library(sp)
library(ggplot2)
library(htmltools)
library(maptools)
library(maps)
library(GISTools)

#plots for thesis
#Histogram with Air temp?

#Boxplot for all sealed, vegetated sites? Or overall air temp graph?
#for sealed sites
list_iButton_corr_tidy_SL_col=lapply(list_iButton_corr_tidy_SL, `[`, 3)
d_SL <- data.frame(x = unlist(list_iButton_corr_tidy_SL_col), 
                site = rep(names(list_iButton_corr_tidy_SL_col),
                          times = sapply(list_iButton_corr_tidy_SL_col,length)))
d_SL$type=rep("sealed") #add grouping factor
#for vegetated sites
list_iButton_corr_tidy_VL_col=lapply(list_iButton_corr_tidy_VL, `[`, 3)
d_VL <- data.frame(x = unlist(list_iButton_corr_tidy_VL_col), 
                   site = rep(names(list_iButton_corr_tidy_VL_col),
                              times = sapply(list_iButton_corr_tidy_VL_col,length)))
d_VL$type=rep("vegetated") #add grouping factor
ggplot(d_SL,aes(x = site, y = x)) +
  geom_boxplot()+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ #rotates the axix labels on x axis
  xlab("Sites")+ #adds title for x axis
  ylab("Temperature [°C]")+ #adds title for y axis
  ggtitle("Temperature in July 2020") #adds plot title

#combine both
d_air=rbind(d_VL, d_SL)
#plot both
ggplot(d_air,aes(x = site, y = x, color=type)) +
  geom_boxplot(notch=TRUE)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6 ))+ #rotates the axix labels on x axis
  xlab("Sites")+ #adds title for x axis
  ylab("Temperature [°C]")+ #adds title for y axis
  ggtitle("Temperature in July 2020")+ #adds plot title
  theme(legend.position="right")

#general  
hist(d_air$x)
min(d_air$x, na.rm=T)
d_air[which.min(d_air$x),]

max(d_air$x, na.rm=T)
which.max(d_air$x)
d_air[which.max(d_air$x),]

sd(d_air$x, na.rm=T)
mean(d_air$x, na.rm=T)
median(d_air$x, na.rm=T)
#all logger
list_iButton_corr_tidy_col=lapply(list_iButton_corr_tidy, `[`, 3)
d <- data.frame(x = unlist(list_iButton_corr_tidy_col), 
                   site = rep(names(list_iButton_corr_tidy_col),
                              times = sapply(list_iButton_corr_tidy_col,length)))
ggplot(d,aes(x = site, y = x)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ #rotates the axix labels on x axis
  xlab("Sites")+ #adds title for x axis
  ylab("Temperature [°C]")+ #adds title for y axis
  ggtitle("Temperature in July 2020") #adds plot title


#Example line plot of logger pair
#****************************************************************************

#******************************************************************************
#plot smooth scatter plot with temp/temp diff

#***********************************************************************
#Integration plot
source("~/Urban_Heat_Island_Muenster/Logger/2020/integrate_differences_green_blue.r") 
AUC_data_frame
#drop cols with NA
AUC_data_frame<- AUC_data_frame[ , colSums(is.na(AUC_data_frame)) < nrow(AUC_data_frame)]
AUC_data_frame_t=AUC_data_frame[-3,-6]
AUC_data_frame_t=as.data.frame(t(AUC_data_frame_t))
AUC_data_frame_t$site=rownames(AUC_data_frame_t)
rownames(AUC_data_frame_t)=NULL
AUC_long=data.frame("site"=rep(AUC_data_frame_t$site,2), 
                    "potential"=rep(c("cooling","warming"),5))
AUC_long$intergal=rep(NA)
AUC_long$intergal[1:5]=AUC_data_frame_t$`1`

ggplot(data=AUC_data_frame)+
  geom_bar()

#*********************************************************************
options(digits=10)
#data for map
setwd("F:/satellite_data_Muenster/MODIS_neu")
MS_shape=readOGR("stadtgebiet.shp")
#transform coordinates to lat lon
MS_shape=spTransform(x = MS_shape, CRSobj = "+proj=longlat +datum=WGS84")


#map for Aasee wind stream
meta_map=metadata[metadata$Standort_ID=="Aasee_3_VL"|metadata$Standort=="Ehrenpark"|metadata$Standort_ID=="Aegidiistr_1_SL",]
meta_map$Lat=meta_map$Lat/1000000
meta_map$Lon=meta_map$Lon/1000000

meta_map$popup_text=c(".  Aegidiistr",".  Haus Kump", ".  Ehrenpark")
leaflet(data=meta_map) %>%
  addTiles() %>%
  #setView()%>% #set screen
  addProviderTiles("Esri.WorldGrayCanvas")%>%
   addCircles(color = "black")%>%
  addLabelOnlyMarkers(data=meta_map, label=~meta_map$popup_text,
                      labelOptions=labelOptions(noHide=T,
                                                direction="right", textOnly=T))%>%
  addScaleBar()
#************************************************************************
#overall map with logger sites
#plot in black/white
metadata$Lat=metadata$Lat/100000
metadata$Lon=metadata$Lon/1000000

leaflet(data=metadata) %>%
  addTiles() %>%
  setView(lat = 51.957900, lng=	7.623341, zoom=12.5	)%>% #set screen
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  addCircles(color = "black")%>%
   addScaleBar()

