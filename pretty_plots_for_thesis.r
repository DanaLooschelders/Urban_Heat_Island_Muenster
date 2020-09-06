library(leaflet)
library(sp)
library(ggplot2)
library(htmltools)
library(maptools)
library(maps)
library(GISTools)
#plots for thesis

#line plot with sealed/vegetated
#***************************************************************************
options(digits=2)
VL_Temp <-rowMeans(cbind(list_iButton_corr_tidy_VL[[1]][,3],
                         list_iButton_corr_tidy_VL[[2]][,3],
                         list_iButton_corr_tidy_VL[[3]][,3], 
                         list_iButton_corr_tidy_VL[[4]][,3], 
                         list_iButton_corr_tidy_VL[[5]][,3], 
                         list_iButton_corr_tidy_VL[[6]][,3], 
                         list_iButton_corr_tidy_VL[[7]][,3], 
                         list_iButton_corr_tidy_VL[[8]][,3], 
                         list_iButton_corr_tidy_VL[[9]][,3], 
                         list_iButton_corr_tidy_VL[[10]][,3], 
                         na.rm=T))

SL_Temp <-rowMeans(cbind(list_iButton_corr_tidy_SL[[1]][,3],
                         list_iButton_corr_tidy_SL[[2]][,3],
                         list_iButton_corr_tidy_SL[[3]][,3], 
                         list_iButton_corr_tidy_SL[[4]][,3], 
                         list_iButton_corr_tidy_SL[[5]][,3], 
                         list_iButton_corr_tidy_SL[[6]][,3], 
                         list_iButton_corr_tidy_SL[[7]][,3], 
                         list_iButton_corr_tidy_SL[[8]][,3], 
                         list_iButton_corr_tidy_SL[[9]][,3], 
                         list_iButton_corr_tidy_SL[[10]][,3], 
                         list_iButton_corr_tidy_SL[[11]][,3], 
                         list_iButton_corr_tidy_SL[[12]][,3], 
                         na.rm=T))
#dataframe for results
merge_VL_SL=data.frame(VL_Temp, SL_Temp, 
                       "date"=list_iButton_corr_tidy_SL[[1]][,2])

ggplot(data=merge_VL_SL)+
  geom_line(aes(x=date, y=VL_Temp, linetype="vegetated"), color="grey")+
  geom_line(aes(x=date, y=SL_Temp, linetype="sealed"))+
  theme(legend.position="right")+
  labs(title="Temperature in July 2020",
       x="Date",
       y="Temperature [°C]",
       color="Site type")+
  theme_classic()+
  scale_linetype_manual(values=c("dotted", "solid"))

#****************************************************************************
#smooth scatter plot for temp ~ temp-diff
#plot data with linear regression line with sd in ggplot (in pretty)
#use alpha to change opacity of points
#e.g. alpha=I(1/5) -> total opacity is reaced when 5 points overlap
merge_VL_SL$diff=merge_VL_SL$SL_Temp-merge_VL_SL$VL_Temp #calculate difference

ggplot(data=merge_VL_SL, aes(SL_Temp,diff))+
  geom_point(col="darkgrey")+
  geom_smooth(method="lm", formula=y~x, col="black")+
  theme_classic()+
  ylab("Difference" ~T[SI]~ "-" ~T[GI]~ "[°C]")+
  xlab(bquote("Temperature" ~T[SI]~ "[°C]"))
#*****************************************************************************
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

#****************************************************************************
#boxplot for water sites
list_for_boxplot <- lapply(list_iButton_corr_tidy_WL, `[`, 3)
#remove Muehlenhof and Aaseeanleger
list_for_boxplot[[3]]=NULL
#change names for label to indicate water bodies
names(list_for_boxplot)=c("Aa stream (Georgskommende)", 
                          "Aa stream (ULB)",
                          "Aasee (Anleger)",
                          "Aa stream (Renaturierung)",
                          "Aasee (Muehlenhof)")
dataframe_for_boxplot=do.call(cbind, list_for_boxplot)
colnames(dataframe_for_boxplot)=names(list_for_boxplot)
dataframe_for_boxplot=dataframe_for_boxplot%>%
  pivot_longer(.,cols=colnames(dataframe_for_boxplot), 
               names_to="Sites",values_to="Temperature [°C]")
#plot
ggplot(data=dataframe_for_boxplot, aes(y=`Temperature [°C]`, x=Sites))+
  geom_boxplot(notch = TRUE, na.rm=T, show.legend = T)+
  stat_summary(fun.y="mean", na.rm=T)+
  coord_flip()+
  theme_bw()
#****************************************************************************
#Example line plot of logger pair
#for ULB
metadata_ULB=metadata[metadata$Standort=="ULB",]
metadata_ULB$Logger_ID
ULB_water=list_iButton_corr_tidy[["27"]] #water logger
ULB_sealed=list_iButton_corr_tidy[["80"]] #sealed
ULB_vegetated=list_iButton_corr_tidy[["109"]] #vegetation
#HOW TO PLOT THREE LINES IN BW?
ggplot()+
  geom_line(data=ULB_water, aes(x=Datetime.1, y=Temperature_C_w_off),
            linetype="solid", color="darkblue")+
  geom_line(data=ULB_sealed, aes(x=Datetime.1, y=Temperature_C_w_off),
            linetype="solid", color="black")+
  geom_line(data=ULB_vegetated, aes(x=Datetime.1, y=Temperature_C_w_off),
            linetype="solid", color="darkgreen")+
  theme_classic()
#in bw
ggplot()+
  geom_line(data=ULB_water, aes(x=Datetime.1, y=Temperature_C_w_off),
            linetype="solid", color="black")+
  geom_line(data=ULB_sealed, aes(x=Datetime.1, y=Temperature_C_w_off),
            linetype="solid", color="black")+
  geom_line(data=ULB_vegetated, aes(x=Datetime.1, y=Temperature_C_w_off),
            linetype="solid", color="grey")+
  theme_classic()
#*****************************************************************************

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
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1, 
                                   size = 6 ))+ #rotates the axix labels on x axis
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
#****************************************************************************

#plot air/water difference and WOL for Mühlenhof
metadata_MUE=metadata[metadata$Standort=="Muehlenhof",]
metadata_MUE$Logger_ID
list_iButton_corr_tidy[["27"]] #water logger
list_iButton_corr_tidy[["80"]] #sealed
list_iButton_corr_tidy[["109"]] #vegetation


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
#plot  with temp diff and traffic
Ehrenpark=list_iButton_corr_tidy_date_factor[["87"]]
Haus_Kump=list_iButton_corr_tidy_date_factor[["64"]]

ggplot()+
  geom_line(aes(Ehrenpark$Datetime.1, 
                Ehrenpark$Temperature_C_w_off-Haus_Kump$Temperature_C_w_off),
            linetype="longdash")+
  geom_line(aes(traffic_sub$datetime, traffic_sub$cars/1000))+
  theme_bw()+
  ylab(bquote("Difference" ~T[downwind]~ "-" ~T[upwind]~ "[°C]"))+
  xlab("Time")+
  scale_y_continuous(sec.axis=sec_axis(trans = ~.*1000, name="Number of Cars passing"))

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

