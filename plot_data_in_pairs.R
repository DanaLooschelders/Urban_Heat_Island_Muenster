#plot in logger pairs
#set ylim 
  #for 01 August: ylim=c(10,35)
  #for September: ylim=c(5,30)
  #for 20 August: ylim=c(10,45)
  #for 14 August: ylim=c(10,35)


#write seperate lists for each place_ID (with for loop?)
#then plot pairs depending on number of dataframes in list
#add color depending on type of place 
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/paired_plots/")

for (i in unique(metadata$PlaceID)){
  dataname=i #save logger ID 
  date=as.character(list_iButton_corr_tidy[[1]][1,2])
  date=substr(date, 1,10)
  name=paste(date,"day","paired_plot",dataname,".pdf")
  Loggers=metadata$ID[metadata$PlaceID==i]
    if(length(Loggers)==3){
      pdf(file=name, paper = "a4r", height=7, width=14)
    dat_logger1=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[1]]
    dat_logger2=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[2]]
    dat_logger3=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[3]]
    plot(dat_logger1[[1]]$Datetime.1, dat_logger1[[1]][,3], 
         type="l", ylim=c(5,45), ylab=expression("Temperature [°C]"), xlab="Date",
         col=metadata$color[metadata$ID==Loggers[1]],
         main=paste("Plot", i))
    lines(dat_logger2[[1]]$Datetime.1, dat_logger2[[1]][,3],
          col=metadata$color[metadata$ID==Loggers[2]])
    lines(dat_logger3[[1]]$Datetime.1, dat_logger3[[1]][,3], 
          col=metadata$color[metadata$ID==Loggers[3]])
    legend("topright", legend=c("Sealed_area", "Vegetation", "Water"), 
           fill = c("darkgrey", "green", "blue"))
    dev.off()
    }
    else if(length(Loggers)==2){
      pdf(file=name, paper="a4r", height=7, width=14)
      #save color for legend
      col1=metadata$color[metadata$ID==Loggers[1]]
      col2=metadata$color[metadata$ID==Loggers[2]]
      label1=metadata$type[metadata$ID==Loggers[1]]
      label2=metadata$type[metadata$ID==Loggers[2]]
      dat_logger1=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[1]]
      dat_logger2=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[2]]
      plot(dat_logger1[[1]]$Datetime.1, dat_logger1[[1]][,3], 
           type="l", ylim=c(5,45), ylab=expression("Temperature [°C]"), xlab="Date",
           col=metadata$color[metadata$ID==Loggers[1]],
           main=paste("Plot", i))
      lines(dat_logger2[[1]]$Datetime.1, dat_logger2[[1]][,3],
            col=metadata$color[metadata$ID==Loggers[2]])
      legend("topright", legend=c(label1, label2), fill=c(col1, col2))
      dev.off()
    }
  else{}
}
