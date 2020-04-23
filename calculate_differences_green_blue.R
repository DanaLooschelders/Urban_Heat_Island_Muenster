#plot the differences between sealed areas and vegetation
i="Kanonengraben_1"
for (i in unique(metadata$PlaceID)){
  dataname=i #save logger ID 
  name=paste("difference","plot",dataname,".pdf")
  Loggers=metadata$ID[metadata$PlaceID==i]
  if(any(metadata$type[metadata$PlaceID==i]=="Vegetation")&any(metadata$type[metadata$PlaceID=="Kanonengraben_1"]=="Sealed_area")){
    vegetation=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Vegetation"]]
    sealed=Loggers[Loggers==metadata$ID[metadata$PlaceID==i&metadata$type=="Sealed_area"]]
    dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[vegetation]]
    dat_logger_sealed= dat_logger_veg=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[sealed]]
    diff_temp=dat_logger
     }
    pdf(file=name, paper = "a4r", height=7, width=14)
    dat_logger1=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[1]]
    dat_logger2=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[2]]
    dat_logger3=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[3]]
    water_logger=metadata$ID[metadata$PlaceID==i&metadata$type=="Vegetation"]
    plot(dat_logger1[[1]]$Datetime.1, dat_logger1[[1]][,4], 
         type="l", ylim=c(10,45), ylab="Temperature [°C]", xlab="Date",
         col=metadata$color[metadata$ID==Loggers[1]],
         main=paste("Plot", i))
    lines(dat_logger2[[1]]$Datetime.1, dat_logger2[[1]][,4],
          col=metadata$color[metadata$ID==Loggers[2]])
    lines(dat_logger3[[1]]$Datetime.1, dat_logger3[[1]][,4], 
          col=metadata$color[metadata$ID==Loggers[3]])
    legend("topright", legend=c("Sealed_area", "Vegetation", "Water"), 
           fill = c("darkgrey", "green", "blue"))
    dev.off()
    
    getwd()

list_iButton_corr_tidy