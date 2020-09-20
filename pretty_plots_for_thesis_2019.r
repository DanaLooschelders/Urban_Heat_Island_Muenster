library(ggplot2)
#plots for thesis

#Histogram with Air temp?

#*****************************************************************************
#Boxplot for all sealed, vegetated sites? Or overall air temp graph?
#for sealed sites
list_iButton_corr_tidy_Sealed_area_col=lapply(list_iButton_corr_tidy_Sealed_area, `[`, 3)
d_Sealed_area <- data.frame(x = unlist(list_iButton_corr_tidy_Sealed_area_col), 
                site = rep(names(list_iButton_corr_tidy_Sealed_area_col),
                          times = sapply(list_iButton_corr_tidy_Sealed_area_col,length)))
d_Sealed_area$type=rep("sealed") #add grouping factor
#for vegetated sites
list_iButton_corr_tidy_Vegetation_col=lapply(list_iButton_corr_tidy_Vegetation, `[`, 3)
d_Vegetation <- data.frame(x = unlist(list_iButton_corr_tidy_Vegetation_col), 
                   site = rep(names(list_iButton_corr_tidy_Vegetation_col),
                              times = sapply(list_iButton_corr_tidy_Vegetation_col,length)))
d_Vegetation$type=rep("vegetated") #add grouping factor
ggplot(d_Sealed_area,aes(x = site, y = x)) +
  geom_boxplot()+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ #rotates the axix labels on x axis
  xlab("Sites")+ #adds title for x axis
  ylab("Temperature [°C]")+ #adds title for y axis
  ggtitle("Temperature in August 2019") #adds plot title

#combine both
d_air=rbind(d_Vegetation, d_Sealed_area)
#plot both
ggplot(d_air,aes(x = site, y = x, color=type)) +
  geom_boxplot(notch=TRUE)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6 ))+ #rotates the axix labels on x axis
  xlab("Sites")+ #adds title for x axis
  ylab("Temperature [°C]")+ #adds title for y axis
  theme(legend.position="right")+
  theme_classic()+
  scale_color_grey()
d_air$site=rep("September 2019")

setwd("~/Urban_Heat_Island_Muenster/Logger")

write.table(d_air, file="data_comp_boxplot_sep_2019.csv", sep=";", dec=",", row.names=F)
#general  
hist(d_air$x)
min(d_air$x, na.rm=T)
#for 02 Aug: 11
#for 14 Aug 11
#for 20 Aug 7.8
#for 01 Sep 4.8
d_air[which.min(d_air$x),]

max(d_air$x, na.rm=T)
#for 02 Aug: 36
#for 14 Aug 41
#for 20 Aug 43
#for 01 Sep 31
which.max(d_air$x)
d_air[which.max(d_air$x),]

sd(d_air$x, na.rm=T)
#for 02 Aug: 4.2
#for 14 AUg 6
#for 20 Aug 6.8
#for 01 Sep 3.8
mean(d_air$x, na.rm=T)
#for 02 Aug: 21
#for 14 Aug 21
#for 20 Aug 24
#for 01 Sep 16
median(d_air$x, na.rm=T)
#for 02 Aug: 21
#for 14 Aug 20
#for 20. Aug 23
#for 01 Sep 16
#Integration plot
source("~/Urban_Heat_ISealed_areaand_Muenster/Logger/2020/integrate_differences_green_blue.r") 
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

#***********************************************************************
#plot September Aasee temp
#10 and 92
Aasee_water=list_iButton_corr_tidy[["10"]]
Aasee_veg=list_iButton_corr_tidy[["92"]]
ggplot()+
  geom_line(data=Aasee_water, aes(x=Datetime.1, y=Temperature_C_w_off, 
                                 color="water"),  size=1.1)+
  geom_line(data=Aasee_veg, aes(x=Datetime.1, y=Temperature_C_w_off, color="air"))+
  labs(x="Date",
       y="Temperature [°C]",
       color="Site type")+
  #theme_classic()+
  scale_color_manual(values=c("black","grey"))+
  theme_classic()


