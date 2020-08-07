library(MESS)

#plot the differences between sealed areas and water
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/difference_plots/merge/")

Temp_diff_data_frame=matrix(data=NA,ncol=length(unique(metadata$Standort)), nrow=length(list_iButton_corr_tidy[[1]][,1]))
Temp_diff_data_frame=as.data.frame(Temp_diff_data_frame)
colnames(Temp_diff_data_frame)=unique(metadata$Standort)

for (i in unique(metadata$Standort)){
   name=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                      1,10),"GI_SI_difference","plot",i,".pdf")
  Loggers=metadata$Logger_ID[metadata$Standort==i] #save ID of Loggers belonging to place
  #check if place has logger in grey and green infrastructure
  if(any(metadata$Loggertyp[metadata$Standort==i]=="VL")&any(metadata$Loggertyp[metadata$Standort==i]=="SL")){ 
    water=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertyp=="VL"]] #get ID of logger in veg
    sealed=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertyp=="SL"]] #get ID of logger in sealed area
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get veg logger data
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed]] #get sealed logger data
    diff_temp=dat_logger_sealed[[1]][,3] - dat_logger_veg[[1]][,3] #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \ngrey and green Infrastructure in", i))
    dev.off()
    #save in dataframe
    Temp_diff_data_frame[i]=diff_temp
  }else{}
}

#remove empty columns
Temp_diff_data_frame=Temp_diff_data_frame[,colSums(is.na(Temp_diff_data_frame))<nrow(Temp_diff_data_frame)]
#add Date to Dataframe
Temp_diff_data_frame$Date=list_iButton_corr_tidy[[1]][,2]
#write data to file
write.table(Temp_diff_data_frame, 
            file=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                              1,10),"Temp_diff_data.csv"), sep = ";", dec=".", row.names=FALSE)

#********************************************************************************************************************
#differences between sealed area air temp and water temp
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/difference_plots/merge/")

WA_Temp_diff_data_frame=matrix(data=NA,ncol=length(unique(metadata$Standort)), nrow=length(list_iButton_corr_tidy[[1]][,1]))
WA_Temp_diff_data_frame=as.data.frame(WA_Temp_diff_data_frame)
colnames(WA_Temp_diff_data_frame)=unique(metadata$Standort)

for (i in unique(metadata$Standort)){
  name=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                    1,10),"BI_SI_difference","plot",i,".pdf")
  Loggers=metadata$Logger_ID[metadata$Standort==i] #save ID of Loggers belonging to place
  #check if place has logger in blue and green infrastructure
  if(any(metadata$Loggertyp[metadata$Standort==i]=="WL")&any(metadata$Loggertyp[metadata$Standort==i]=="SL")){ 
    water=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertyp=="WL"]] #get ID of logger in veg
    sealed=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertyp=="SL"]] #get ID of logger in sealed area
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get veg logger data
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed]] #get sealed logger data
    diff_temp=dat_logger_sealed[[1]][,3] - dat_logger_veg[[1]][,3] #calculate difference between grey infrastructure and water temp
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,2], diff_temp, #plot difference
         type="l", ylab="Temperature difference [°C]", xlab="Date",
         main=paste("Temperature Difference between \ngrey Air Temp and water temp in", i))
    abline(h=0, col="red")
    abline(v=sun2$sunrise, col="orange")
    abline(v=sun2$sunset, col="darkred")
    legend("topright", 
           legend=c("Diff. Temperature",  "Sunrise", "Sunset"), 
           fill=c("black", "orange", "darkred"), cex=0.7)
    dev.off()
    #save in dataframe
    WA_Temp_diff_data_frame[i]=diff_temp
  }else{}
}

#remove empty columns
WA_Temp_diff_data_frame=WA_Temp_diff_data_frame[,colSums(is.na(WA_Temp_diff_data_frame))<nrow(WA_Temp_diff_data_frame)]
#add Date to Dataframe
WA_Temp_diff_data_frame$Date=list_iButton_corr_tidy[[1]][,2]
#write data to file
write.table(WA_Temp_diff_data_frame, 
            file=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                              1,10),"WA_Temp_diff_data.csv"), sep = ";", dec=".", row.names=FALSE)
#************************************************************************************************
#integrate area under curve to compare potential
AUC_data_frame=matrix(data=NA,ncol=length(unique(metadata$Standort)), nrow=2)
AUC_data_frame=as.data.frame(AUC_data_frame)
colnames(AUC_data_frame)=unique(metadata$Standort)
AUC_data_frame$potential=c("cooling","warming")

#INTEGRATION IS NOT WORKING

AUC2_data_frame=AUC_data_frame
for (i in unique(metadata$Standort)){
  Loggers=metadata$Logger_ID[metadata$Standort==i] #save ID of Loggers belonging to place
  #check if place has logger in grey and green infrastructure
  if(any(metadata$Loggertyp[metadata$Standort==i]=="WL")&any(metadata$Loggertyp[metadata$Standort==i]=="SL")){ 
    #get water logger ID
    water=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertyp=="WL"]] #get ID of logger in water
    #get sealed logger ID
     sealed=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertyp=="SL"]] #get ID of logger in sealed area
    #get water temp data
    dat_logger_water=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==water]] #get water logger data
    #get air temp data
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed]] #get sealed logger data
    #calculate difference between air and water temp
    diff_temp=dat_logger_sealed[[1]][,3] - dat_logger_water[[1]][,3] #calculate difference between grey infrastructure and water temp
    #create data farme with date and temperature difference
    data_join=data.frame("date"=as.POSIXct(dat_logger_water[[1]][,2]), "temp"=diff_temp)
    #subset data for warming/cooling impact
    data_join_cool=data_join[data_join$temp>0,]
    data_join_warm=data_join[data_join$temp<0,]
    #data_join_warm$temp=data_join_warm$temp*-1 #use positive values to integrate
    #do a spline interpolation for warming area/cooling area
    AUC_data_frame[1,as.character(i)]=MESS::auc(x = data_join_cool$date,data_join_cool$temp, 
                                          type="spline", absolutearea = FALSE, subdivisions = 1000L)
    AUC_data_frame[2,as.character(i)]=MESS::auc(x = data_join_warm$date,data_join_warm$temp, 
                                          type="spline", absolutearea = FALSE, subdivisions = 1000L)
    
    AUC2_data_frame[1,as.character(i)]=auc(x = data_join$date,data_join$temp, 
                                                type="spline", absolutearea = FALSE, 
                                                subdivisions = 1000L)
    AUC2_data_frame[2,as.character(i)]=auc(x = data_join$date,
                                                 data_join$temp, 
                                                type="spline", 
                                                absolutearea = TRUE, 
                                                subdivisions = 1000L)
    
    }else{}
}
write.table(AUC2_data_frame, 
            file=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                              1,10),"AUC2_data.csv"), sep = ";", dec=".", row.names=FALSE)
