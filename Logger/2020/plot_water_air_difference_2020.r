setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/water_air")
#plot difference between air temp and WL
#create dataframe for difference between WL temp and VL logger air temp
WL_veg_diff_data_frame=matrix(data=NA,ncol=length(unique(metadata$Standort)), 
                                 nrow=length(list_iButton_corr_tidy[[1]][,1]))
WL_veg_diff_data_frame=as.data.frame(WL_veg_diff_data_frame)
colnames(WL_veg_diff_data_frame)=unique(metadata$Standort)
#create dataframe for difference between WL temperature and grey air temperature
WL_grey_diff_data_frame=matrix(data=NA,ncol=length(unique(metadata$Standort)), 
                                  nrow=length(list_iButton_corr_tidy[[1]][,1]))
WL_grey_diff_data_frame=as.data.frame(WL_grey_diff_data_frame)
colnames(WL_grey_diff_data_frame)=unique(metadata$Standort)

for (i in unique(metadata$Standort)){
  dataname=i #save place ID 
  date=substring(as.character(list_iButton_corr_tidy[[1]][1,2]), 1,10)
  Loggers=metadata$Logger_ID[metadata$Standort==i] #save ID of Loggers belonging to place
  #check if place has logger in grey and green infrastructure
  if(any(metadata$Loggertype[metadata$Standort==i]=="VL")&any(metadata$Loggertype[metadata$Standort==i]=="WL")){ 
    name=paste(date,"Water air temp (GI) difference","plot",dataname,".pdf")
    VL=metadata$Logger_ID[metadata$Standort==i&metadata$Loggertype=="VL"] #get ID of logger in veg
    if(length(VL)==1){ #if place has one logger in vegetated area (most do)
    WL=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertype=="WL"]] #get ID of logger in sealed area
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==VL]] #get veg logger data
    dat_logger_WL=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==WL]] #get sealed logger data
    diff_temp=dat_logger_WL[[1]][,3] - dat_logger_veg[[1]][,3]  #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (GI) in", i),
         sub="Diff > 0 : Water temp was higher then air temp")
     abline(h=0, col="red")
     dev.off()
    #save in dataframe
    WL_veg_diff_data_frame[i]=diff_temp}
    else{ #if place has more than one logger in vegetated area
      name=paste(date,"Water air temp (GI) difference","plot",dataname, 
                 "(1)", ".pdf")
    VL1=VL[1]  #use first logger
    WL=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertype=="WL"]] #get ID of logger in sealed area
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==VL1]] #get veg logger data
    dat_logger_WL=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==WL]] #get sealed logger data
    diff_temp= dat_logger_WL[[1]][,3] - dat_logger_veg[[1]][,3]  #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (GI) in", i, "(1)"),
         sub="Diff > 0 : Water temp was higher then air temp")
    abline(h=0, col="red")
    abline(v=sun2$sunrise, col="orange")
    abline(v=sun2$sunset, col="red")
    dev.off()
    #same for second logger
    name=paste(date,"Water air temp (GI) difference","plot",dataname, "(2)", ".pdf")
    VL2=VL[2]
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==VL2]] #get veg logger data
    diff_temp=dat_logger_WL[[1]][,3] - dat_logger_veg[[1]][,3]  #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (GI) in", i, "(2)"),
         sub="Diff > 0 : Water temp was higher then air temp")
    abline(h=0, col="red")
    dev.off()}
    #save in dataframe
   # WL_veg_diff_data_frame[i]=diff_temp}
  }else if(any(metadata$Loggertype[metadata$Standort==i]=="SL")&any(metadata$Loggertype[metadata$Standort==i]=="WL")){
    name=paste(date,"WL air temp (sealed) difference","plot",dataname,".pdf")
     WL=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertype=="WL"]] #get ID of logger in veg
    sealed=metadata$Logger_ID[metadata$Standort==i&metadata$Loggertype=="SL"] #get ID of logger in sealed area
    if(length(sealed)==1){
    dat_logger_WL=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==WL]] #get veg logger data
   dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed]] #get sealed logger data
   diff_temp=dat_logger_WL[[1]][,3] - dat_logger_sealed[[1]][,3]  #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_WL[[1]][,2], diff_temp, #plot difference
        type="l", ylab="Temperature difference [°C]", xlab="Date",
        main=paste("Temperature Difference between \nwater and air temperature (sealed area) in", i),
        sub="Diff > 0 : Water temp was higher then air temp")
    abline(h=0, col="red")
    dev.off()
    }else{
      name=paste(date,"Water air temp (sealed) difference","plot",dataname, "(1)", ".pdf")
    sealed1=sealed[1]  #use first logger
    WL=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertype=="WL"]] #get ID of logger in sealed area
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed1]] #get veg logger data
    dat_logger_WL=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==WL]] #get sealed logger data
    diff_temp=dat_logger_WL[[1]][,3] - dat_logger_sealed[[1]][,3]  #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_sealed[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (sealed) in", i, "(1)"),
         sub="Diff > 0 : Water temp was higher then air temp")
    abline(h=0, col="red")
    dev.off()
    #same for second logger
    name=paste(date,"Water air temp (sealed) difference","plot",dataname, "(2)", ".pdf")
    sealed2=sealed[2]
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed2]] #get veg logger data
    diff_temp=dat_logger_WL[[1]][,3] - dat_logger_sealed[[1]][,3]  #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_sealed[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nWL and air temperature (sealed) in", i, "(2)"),
         sub="Diff > 0 : Water temp was higher then air temp")
    abline(h=0, col="red")
    dev.off()}
}
else{}
}

str(WL_veg_diff_data_frame)
#remove empty columns
WL_veg_diff_data_frame=WL_veg_diff_data_frame[,colSums(is.na(WL_veg_diff_data_frame))<nrow(WL_veg_diff_data_frame)]
#add Date to Dataframe
WL_veg_diff_data_frame$Date=list_iButton_corr_tidy[[1]][,2]
#write data to file
write.table(WL_veg_diff_data_frame, 
            file=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                              1,10),"Temp_diff_data.csv"), 
            sep = ";", dec=".")

