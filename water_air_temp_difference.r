setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/water_air")
#plot difference between air temp and water
#create dataframe for difference between water temp and vegetation logger air temp
water_veg_diff_data_frame=matrix(data=NA,ncol=length(unique(metadata$PlaceID)), nrow=length(list_iButton_corr_tidy[[1]][,1]))
water_veg_diff_data_frame=as.data.frame(water_veg_diff_data_frame)
colnames(water_veg_diff_data_frame)=unique(metadata$PlaceID)
#create dataframe for difference between water temperature and grey air temperature
water_grey_diff_data_frame=matrix(data=NA,ncol=length(unique(metadata$PlaceID)), nrow=length(list_iButton_corr_tidy[[1]][,1]))
water_grey_diff_data_frame=as.data.frame(water_grey_diff_data_frame)
colnames(water_grey_diff_data_frame)=unique(metadata$PlaceID)
str(metadata)
i=unique(metadata$PlaceID)[1]
for (i in unique(metadata$PlaceID)){
  dataname=i #save place ID 
  date=substring(as.character(list_iButton_corr_tidy[[1]][1,2]), 1,10)
  Loggers=metadata$ID[metadata$PlaceID==i] #save ID of Loggers belonging to place
  #check if place has logger in grey and green infrastructure
  if(any(metadata$type[metadata$PlaceID==i]=="Vegetation")&any(metadata$type[metadata$PlaceID==i]=="Water")){ 
    name=paste(date,"water air temp (GI) difference","plot",dataname,".pdf")
    vegetation=metadata$ID[metadata$PlaceID==i&metadata$type=="Vegetation"] #get ID of logger in veg
    if(length(vegetation)==1){ #if place has one logger in vegetated area (most do)
    water=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Water"]] #get ID of logger in sealed area
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==vegetation]] #get veg logger data
    dat_logger_water=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get sealed logger data
    diff_temp=dat_logger_veg[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (GI) in", i))
     abline(h=0, col="red")
     dev.off()
    #save in dataframe
    water_veg_diff_data_frame[i]=diff_temp}
    else{ #if place has more than one logger in vegetated area
      name=paste(date,"water air temp (GI) difference","plot",dataname, "(1)", ".pdf")
    vegetation1=vegetation[1]  #use first logger
    water=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Water"]] #get ID of logger in sealed area
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==vegetation1]] #get veg logger data
    dat_logger_water=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get sealed logger data
    diff_temp=dat_logger_veg[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (GI) in", i, "(1)"))
    abline(h=0, col="red")
    dev.off()
    #same for second logger
    name=paste(date,"water air temp (GI) difference","plot",dataname, "(2)", ".pdf")
    vegetation2=vegetation[2]
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==vegetation2]] #get veg logger data
    diff_temp=dat_logger_veg[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (GI) in", i, "(2)"))
    abline(h=0, col="red")
    dev.off()}
    #save in dataframe
   # water_veg_diff_data_frame[i]=diff_temp}
  }else if(any(metadata$type[metadata$PlaceID==i]=="Sealed_area")&any(metadata$type[metadata$PlaceID==i]=="Water")){
    name=paste(date,"water air temp (sealed) difference","plot",dataname,".pdf")
     water=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Water"]] #get ID of logger in veg
    sealed=metadata$ID[metadata$PlaceID==i&metadata$type=="Sealed_area"] #get ID of logger in sealed area
    if(length(sealed)==1){
    dat_logger_water=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get veg logger data
   dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed]] #get sealed logger data
   diff_temp=dat_logger_sealed[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_water[[1]][,2], diff_temp, #plot difference
        type="l", ylab="Temperature difference [°C]", xlab="Date",
        main=paste("Temperature Difference between \nWater and air temperature (sealed area) in", i))
    abline(h=0, col="red")
    dev.off()
    }else{
      name=paste(date,"water air temp (sealed) difference","plot",dataname, "(1)", ".pdf")
    sealed1=sealed[1]  #use first logger
    water=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Water"]] #get ID of logger in sealed area
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed1]] #get veg logger data
    dat_logger_water=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get sealed logger data
    diff_temp=dat_logger_sealed[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_sealed[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (sealed) in", i, "(1)"))
    abline(h=0, col="red")
    dev.off()
    #same for second logger
    name=paste(date,"water air temp (sealed) difference","plot",dataname, "(2)", ".pdf")
    sealed2=sealed[2]
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed2]] #get veg logger data
    diff_temp=dat_logger_sealed[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_sealed[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \nwater and air temperature (sealed) in", i, "(2)"))
    abline(h=0, col="red")
    dev.off()}
}
else{}
}

str(water_veg_diff_data_frame)
#remove empty columns
water_veg_diff_data_frame=water_veg_diff_data_frame[,colSums(is.na(water_veg_diff_data_frame))<nrow(water_veg_diff_data_frame)]
#add Date to Dataframe
water_veg_diff_data_frame$Date=list_iButton_corr_tidy[[1]][,2]
#write data to file
date=as.character(list_iButton_corr_tidy[[1]][1,2])
date=substr(date, 1,10)
write.table(water_veg_diff_data_frame, file=paste(date,"Temp_diff_data.csv"), sep = ";", dec=".")
