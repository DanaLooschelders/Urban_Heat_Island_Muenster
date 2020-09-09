library(flux) #package for integration

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/difference_plots")

#integrate area under curve to compare potential

AUC_data_frame=matrix(data=NA,ncol=length(unique(metadata$PlaceID)), nrow=3)
AUC_data_frame=as.data.frame(AUC_data_frame)
colnames(AUC_data_frame)=unique(metadata$PlaceID)
AUC_data_frame$potential=c("cooling","warming", "total")

#for august: remove 2nd Aasee vegetation logger and unknown IDS
metadata=metadata[metadata$PlaceID!="unknown",]
metadata=metadata[-5,]
#for september also
metadata=metadata[-22,]

for (i in unique(metadata$PlaceID)){
  Loggers=metadata$ID[metadata$PlaceID==i] #save ID of Loggers belonging to place
  #check if place has logger in grey and green infrastructure
  if(any(metadata$type[metadata$PlaceID==i]=="Water")&any(metadata$type[metadata$PlaceID==i]=="Sealed_area")){ 
    #get water logger ID
    water=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Water"]] #get ID of logger in water
    #get sealed logger ID
    sealed=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Sealed_area"]] #get ID of logger in sealed area
    #get water temp data
    dat_logger_water=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get water logger data
    #get air temp data
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed]] #get sealed logger data
    #calculate difference between air and water temp
    diff_temp_cool=dat_logger_sealed[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between grey infrastructure and water temp
    diff_temp_warm=dat_logger_water[[1]][,3] - dat_logger_sealed[[1]][,3]  #calculate difference between water temp and grey infrastructure 
    #create data farme with date and temperature difference
    data_join=data.frame("date"=as.POSIXct(dat_logger_water[[1]][,2]), "temp_cool"=diff_temp_cool, "temp_warm"=diff_temp_warm)
    #do interpolation for warming area/cooling area
    AUC_data_frame[1,as.character(i)]=flux::auc(x = data_join$date,
                                                data_join$temp_cool,
                                                thresh=0)
    AUC_data_frame[2,as.character(i)]=flux::auc(x = data_join$date,
                                                data_join$temp_warm,
                                                thresh=0)
    #if(AUC_data_frame[1,as.character(i)]>AUC_data_frame[2,as.character(i)]){
     # AUC_data_frame$total[3,as.character(i)]="cooling"
    #} else {AUC_data_frame[3,as.character(i)]="warming"}
  } else if(any(metadata$type[metadata$PlaceID==i]=="Water")&any(metadata$type[metadata$PlaceID==i]=="Vegetation")) { 
    #get water logger ID
    water=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Water"]] #get ID of logger in water
    #get vegetated logger ID
    vegetated=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Vegetation"]] #get ID of logger in sealed area
    #get water temp data
    dat_logger_water=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get water logger data
    #get air temp data
    dat_logger_vegetated=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==vegetated]] #get sealed logger data
    #calculate difference between air and water temp
    diff_temp_cool=dat_logger_vegetated[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between green infrastructure and water temp
    diff_temp_warm=dat_logger_water[[1]][,3] - dat_logger_vegetated[[1]][,3]  #calculate difference between water temp and green infrastructure 
    #create data farme with date and temperature difference
    data_join=data.frame("date"=as.POSIXct(dat_logger_water[[1]][,2]), "temp_cool"=diff_temp_cool, "temp_warm"=diff_temp_warm)
    #do interpolation for warming area/cooling area
    AUC_data_frame[1,as.character(i)]=flux::auc(x = data_join$date,
                                                data_join$temp_cool,
                                                thresh=0)
    AUC_data_frame[2,as.character(i)]=flux::auc(x = data_join$date,
                                                data_join$temp_warm,
                                                thresh=0)
  #  if(AUC_data_frame[1,as.character(i)]>AUC_data_frame[2,as.character(i)]){
   #   AUC_data_frame$total[3,as.character(i)]="cooling"
    #} else {AUC_data_frame[3,as.character(i)]="warming"}
    
  } else{}
}

#do a seperate one for Muelenhof because there is only a vegetated logger
write.table(AUC_data_frame, 
            file=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                              1,10),"AUC2_september_data.csv"), sep = ";", dec=".", row.names=FALSE)

