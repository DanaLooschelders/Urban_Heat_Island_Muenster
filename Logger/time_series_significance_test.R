library(SiZer)
library(dplyr)
#calculate statistics
#significant difference between sealed area and vegetation?
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
des=read.table("Lat_Lon_Logger.csv", sep=";", dec=".", header=T) #read in metadata
des=des[,1:7] #drop column with water depth (for now)
names(des)[1]="Logger_ID"
des=na.omit(des) #drop rows with NA
des$Logger_ID=as.character(des$Logger_ID) #set logger ID as character to match names
str(des)

str(list_iButton_corr_tidy)

#omit all water logger:
#get ID of water logger
water_logger=intersect(names(list_iButton_corr_tidy), des$Logger_ID[des$Place_type=="Water"])
#remove all water logger data from list
list_iButton_corr_green_grey=list_iButton_corr_tidy
for(i in water_logger) {list_iButton_corr_green_grey[[i]]=NULL}

#remove water logger from metadata
metadata_stats=metadata[metadata$type!="Water",]
str(metadata_stats)
#create list for individual places (grey/green)
list_iButton_stats=list()

#loop through list of logger and create new list with green and grey logger for every place
for (i in unique(metadata_stats$PlaceID)){
  dataname=i #save logger ID 
  Loggers=metadata_stats$ID[metadata_stats$PlaceID==i]
  if(length(Loggers)==2){
  label1=metadata_stats$type[metadata_stats$ID==Loggers[1]]
  label2=metadata_stats$type[metadata_stats$ID==Loggers[2]]
  dat_logger1=list_iButton_corr_green_grey[names(list_iButton_corr_green_grey)==Loggers[1]]
  dat_logger2=list_iButton_corr_green_grey[names(list_iButton_corr_green_grey)==Loggers[2]]
  dat_join=data.frame()
  dat_join=cbind(dat_logger1[[1]][2], dat_logger1[[1]][3], dat_logger2[[1]][3])
  names(dat_join)=c("Time", label1, label2)
  list_iButton_stats[[i]]=dat_join
  }else{}
}

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/stats")
#create dynamically named dataframe
dat=data.frame("place"=names(list_iButton_stats), "ttest"=rep(NA), "wilcox"=rep(NA))
#test normality and significance for every place
for (i in 1:length(list_iButton_stats)){
  data=list_iButton_stats[[i]]
  if(exists(x = "Vegetation", where=data)&exists(x="Sealed_area", where=data)){
    norm.test1=shapiro.test(data$Sealed_area)
    norm.test2=shapiro.test(data$Vegetation)
    pdf(paste("ccf",substring(list_iButton_stats[[1]]$Time[1],1, 10),i,".pdf"))
    ccf(data$Sealed_area, data$Vegetation, na.action = na.pass, lag.max=200)
    dev.off()
    pdf(paste("SiZer",substring(list_iButton_stats[[1]]$Time[1],1, 10),i,".pdf"))
    plot(SiZer(data$Sealed_area, data$Vegetation))
    dev.off()
  if(norm.test1$p.value>=0.05&norm.test2$p.value>=0.05){
    ttest.res=t.test(data$Sealed_area, data$Vegetation)
    dat[i,2]=ttest.res$p.value
  }else{
    wilcox.res=wilcox.test(data$Sealed_area, data$Vegetation)
    dat[i,3]=wilcox.res$p.value
  }
  }else{}
}

#save to file
write.csv2(x=dat, file=paste("significance", substring(list_iButton_stats[[1]]$Time[1],1, 10), ".csv"))

#merge all green/grey time series and calculate significant difference between them
median_sealed=sapply(list_iButton_corr_tidy_sealed, function(x) median(x[,3], na.rm=T))
median_vegetation=sapply(list_iButton_corr_tidy_vegetation, function(x) median(x[,3], na.rm=T))

#test for normality
shapiro_sealed=shapiro.test(median_sealed) 
shapiro_vegetation=shapiro.test(median_vegetation) 
#test for homogenity of variance
var_result=var.test(median_sealed,median_vegetation)

if(shapiro_vegetation[["p.value"]]>0.05&shapiro_sealed[["p.value"]]>0.5){
  if(var_result[["p.value"]]>0.05){
  t.test(median_vegetation, median_sealed, paired =FALSE, var.equal = TRUE)
  }else{
    t.test(median_vegetation, median_sealed, paired =FALSE, var.equal = FALSE)
  }
}else{
wilcox.test(median_sealed, median_vegetation)
  }

