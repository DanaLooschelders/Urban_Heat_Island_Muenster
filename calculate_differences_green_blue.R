#plot the differences between sealed areas and vegetation
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/difference_plots_14.08")

#create dataframe to save difference data
Temp_diff_data_frame=matrix(data=NA,ncol=length(unique(metadata$PlaceID)), nrow=length(list_iButton_corr_tidy[[1]][,1]))
Temp_diff_data_frame=as.data.frame(Temp_diff_data_frame)
colnames(Temp_diff_data_frame)=unique(metadata$PlaceID)

for (i in unique(metadata$PlaceID)){
  dataname=i #save place ID 
  name=paste("difference","plot",dataname,".pdf")
  Loggers=metadata$ID[metadata$PlaceID==i] #save ID of Loggers belonging to place
  #check if place has logger in grey and green infrastructure
  if(any(metadata$type[metadata$PlaceID==i]=="Vegetation")&any(metadata$type[metadata$PlaceID==i]=="Sealed_area")){ 
    vegetation=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Vegetation"]] #get ID of logger in veg
    sealed=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Sealed_area"]] #get ID of logger in sealed area
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==vegetation]] #get veg logger data
    dat_logger_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[Loggers==sealed]] #get sealed logger data
    diff_temp=dat_logger_sealed[[1]][,4] - dat_logger_veg[[1]][,4] #calculate difference between grey and green infrastructure
    pdf(file=name, paper = "a4r", height=7, width=14) #create pdf
    plot(dat_logger_veg[[1]][,3], diff_temp, #plot difference
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
#write data to file
write.table(Temp_diff_data_frame, file="Temp_diff_data.csv", sep = ";", dec=".")
