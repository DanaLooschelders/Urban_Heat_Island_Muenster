#plot in logger pairs
#set ylim 
  #for 01 August: ylim=c(10,35)
  #for September: ylim=c(5,30)
  #for 20 August: ylim=c(10,40)

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
metadata=data.frame("ID"=as.integer(names(list_iButton_corr_tidy)))
metadata=na.omit(metadata)
metadata$PlaceID=rep(NA)
metadata$type=rep(NA)
for(i in metadata$ID){
  if(any(des$ï..Logger.ID==i)) {
  metadata$PlaceID[metadata$ID==i]=des$place_ID[des$ï..Logger.ID==i]
  metadata$type[metadata$ID==i]=as.character(des$Place_type[des$ï..Logger.ID==i])
  }else{}
}
metadata[is.na(metadata$PlaceID),2:3]="unknown"
metadata$color=rep(NA)

#set column with color for plots
metadata$color[metadata$type=="Water"]="blue"
metadata$color[metadata$type=="Sealed_area"]="darkgrey"
metadata$color[metadata$type=="Vegetation"]="green"

#write seperate lists for each place_ID (with for loop?)
#then plot pairs depending on number of dataframes in list
#add color depending on type of place 
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/paired_plots/")
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/paired_plots_02.09/")
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/paired_plots_20.08/")
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/paired_plots_14.08/")

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
         type="l", ylim=c(10,35), ylab="Temperature [°C]", xlab="Date",
         col=metadata$color[metadata$ID==Loggers[1]],
         main=paste("Plot", i))
    lines(dat_logger2[[1]]$Datetime.1, dat_logger2[[1]][,4],
          col=metadata$color[metadata$ID==Loggers[2]])
    lines(dat_logger3[[1]]$Datetime.1, dat_logger3[[1]][,4], 
          col=metadata$color[metadata$ID==Loggers[3]])
    legend("topright", legend=c("Sealed_area", "Vegetation", "Water"), 
           fill = c("darkgrey", "green", "blue"))
    dev.off()
    }
    else if(length(Loggers)==2){
      jpeg(filename=name)
      #save color for legend
      col1=metadata$color[metadata$ID==Loggers[1]]
      col2=metadata$color[metadata$ID==Loggers[2]]
      label1=metadata$type[metadata$ID==Loggers[1]]
      label2=metadata$type[metadata$ID==Loggers[2]]
      dat_logger1=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[1]]
      dat_logger2=list_iButton_corr_tidy[names(list_iButton_corr_tidy)==Loggers[2]]
      plot(dat_logger1[[1]]$Datetime.1, dat_logger1[[1]][,4], 
           type="l", ylim=c(10,35), ylab="Temperature [°C]", xlab="Date",
           col=metadata$color[metadata$ID==Loggers[1]],
           main=paste("Plot", i))
      lines(dat_logger2[[1]]$Datetime.1, dat_logger2[[1]][,4],
            col=metadata$color[metadata$ID==Loggers[2]])
      legend("topright", legend=c(label1, label2), fill=c(col1, col2))
      dev.off()
    }
  else{}
  
}
