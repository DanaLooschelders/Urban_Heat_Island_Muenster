library(SiZer)
library(dplyr)
#calculate statistics
#significant difference between sealed area and VL?
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")

#omit all water logger:
#get ID of water logger
water_logger=intersect(as.numeric(names(list_iButton_corr_tidy)), metadata$Logger_ID[metadata$Loggertyp=="WL"|metadata$Loggertyp=="WOL"])
#remove all water logger data from list
list_iButton_corr_green_grey=list_iButton_corr_tidy
for(i in water_logger){list_iButton_corr_green_grey[[as.character(i)]]=NULL}
#list_iButton_corr_green_grey=list_iButton_corr_green_grey[names(list_iButton_corr_green_grey) %in% as.character(water_logger) == FALSE]

#remove water logger from metadata
metadata_stats=metadata[metadata$Loggertyp!="WL",]
metadata_stats=metadata_stats[metadata_stats$Loggertyp!="WOL",]

#create list for individual places (grey/green)
list_iButton_stats=list()

#loop through list of logger and create new list with green and grey logger for every place
for (i in unique(metadata_stats$Standort)){
  dataname=i #save logger ID 
  Loggers=metadata_stats$Logger_ID[metadata_stats$Standort==i]
  if(length(Loggers)==2){
  label1=as.character(metadata_stats$Loggertyp[metadata_stats$Logger_ID==Loggers[1]])
  label2=as.character(metadata_stats$Loggertyp[metadata_stats$Logger_ID==Loggers[2]])
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
  if(exists(x = "VL", where=data)&exists(x="SL", where=data)){
    norm.test1=shapiro.test(data$SL)
    norm.test2=shapiro.test(data$VL)
    pdf(paste("ccf",substring(list_iButton_stats[[1]]$Time[1],1, 10),i,".pdf"))
    ccf(data$SL, data$VL, na.action = na.pass, lag.max=200)
    dev.off()
    #(paste("SiZer",substring(list_iButton_stats[[1]]$Time[1],1, 10),i,".pdf"))
    #plot(SiZer(data$SL, data$VL))
    #dev.off()
  if(norm.test1$p.value>=0.05&norm.test2$p.value>=0.05){
    ttest.res=t.test(data$SL, data$VL)
    dat[i,2]=ttest.res$p.value
  }else{
    wilcox.res=wilcox.test(data$SL, data$VL)
    dat[i,3]=wilcox.res$p.value
  }
  }else{}
}

#save to file
write.csv2(x=dat, file=paste("significance", substring(list_iButton_stats[[1]]$Time[1],1, 10), ".csv"))

#merge all green/grey time series and calculate significant difference between them
median_sealed=sapply(list_iButton_corr_tidy_sealed, function(x) median(x[,3], na.rm=T))
median_VL=sapply(list_iButton_corr_tidy_VL, function(x) median(x[,3], na.rm=T))

#test for normality
shapiro_sealed=shapiro.test(median_sealed) 
shapiro_VL=shapiro.test(median_VL) 
#test for homogenity of variance
var_result=var.test(median_sealed,median_VL)

if(shapiro_VL[["p.value"]]>0.05&shapiro_sealed[["p.value"]]>0.5){
  if(var_result[["p.value"]]>0.05){
  t.test(median_VL, median_sealed, paired =FALSE, var.equal = TRUE)
  }else{
    t.test(median_VL, median_sealed, paired =FALSE, var.equal = FALSE)
  }
}else{
wilcox.test(median_sealed, median_VL)
  }

