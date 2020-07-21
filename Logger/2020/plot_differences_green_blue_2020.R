#plot the differences between sealed areas and vegetation
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/difference_plots/")

Temp_diff_data_frame=matrix(data=NA,ncol=length(unique(metadata$Standort)), nrow=length(list_iButton_corr_tidy[[1]][,1]))
Temp_diff_data_frame=as.data.frame(Temp_diff_data_frame)
colnames(Temp_diff_data_frame)=unique(metadata$Standort)
i="Kanonengraben"
for (i in unique(metadata$Standort)){
   name=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                      1,10),"difference","plot",i,".pdf")
  Loggers=metadata$Logger_ID[metadata$Standort==i] #save ID of Loggers belonging to place
  #check if place has logger in grey and green infrastructure
  if(any(metadata$Loggertyp[metadata$Standort==i]=="VL")&any(metadata$Loggertyp[metadata$Standort==i]=="SL")){ 
    vegetation=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertyp=="VL"]] #get ID of logger in veg
    sealed=Loggers[Loggers==metadata$Logger_ID[metadata$Standort==i&metadata$Loggertyp=="SL"]] #get ID of logger in sealed area
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==vegetation]] #get veg logger data
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

str(Temp_diff_data_frame)
#remove empty columns
Temp_diff_data_frame=Temp_diff_data_frame[,colSums(is.na(Temp_diff_data_frame))<nrow(Temp_diff_data_frame)]
#add Date to Dataframe
Temp_diff_data_frame$Date=list_iButton_corr_tidy[[1]][,2]
#write data to file
write.table(Temp_diff_data_frame, 
            file=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 
                              1,10),"Temp_diff_data.csv"), sep = ";", dec=".")

