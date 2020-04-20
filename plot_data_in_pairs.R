#plot in logger pairs

#assign description for plots
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
#read in csv with lat lon and description of places
des=read.table("Lat_Lon_Logger.csv", sep=";", dec=",", header=T)
des=des[,1:7] #drop column with water depth (for now)
des=na.omit(des) #drop rows with NA
des$ï..Logger.ID=as.character(des$ï..Logger.ID) #set logger ID as character to match names
des$place_ID=paste(des$Place_name,des$Place_number, sep="_")
str(des)
ID=unique(des$place_ID)

#create a metadata table for logger
metadata=data.frame("ID"=as.integer(names(list_iButton_corr_tidy_date)))
metadata$PlaceID=rep(NA)
metadata$type=rep(NA)
for(i in metadata$ID){
  if(any(des$ï..Logger.ID==i)) {
  metadata$PlaceID[metadata$ID==i]=des$place_ID[des$ï..Logger.ID==i]
  metadata$type[metadata$ID==i]=as.character(des$Place_type[des$ï..Logger.ID==i])
  }else{}
}
metadata[is.na(metadata$PlaceID),2:3]="unknown"
#write seperate lists for each place_ID (with for loop?)
#then plot pairs depending on number of dataframes in list
#add color depending on type of place 

#test
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/paired_plots")
for (i in unique(metadata$PlaceID)){
  dataname=i #save logger ID 
  name=paste("day","paired_plot",dataname,".jpg")
  Loggers=metadata$ID[metadata$PlaceID==i]
    if(length(Loggers)==3){
      jpeg(filename=name)
    dat_logger1=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[1]]
    dat_logger2=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[2]]
    dat_logger3=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[3]]
    plot(dat_logger1[[1]]$Datetime.1, dat_logger1[[1]][,4], 
         type="l", ylim=c(10,35), ylab="Temperature [°C]", xlab="Date")
    lines(dat_logger2[[1]]$Datetime.1, dat_logger2[[1]][,4],col="red")
    lines(dat_logger3[[1]]$Datetime.1, dat_logger3[[1]][,4], col="green")
    dev.off()
    }
    else if(length(Loggers)==2){
      jpeg(filename=name)
      dat_logger1=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[1]]
      dat_logger2=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[2]]
      plot(dat_logger1[[1]]$Datetime.1, dat_logger1[[1]][,4], 
           type="l", ylim=c(10,35), ylab="Temperature [°C]", xlab="Date")
      lines(dat_logger2[[1]]$Datetime.1, dat_logger2[[1]][,4], col="red")
      dev.off()
    }
  else{}
}
