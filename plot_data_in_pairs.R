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
  dataname=names(metadata$placeID==i) #save logger ID 
  name=paste("day","paired_plot",dataname,".jpg")
  Loggers=metadata$ID[metadata$PlaceID==i]
    if(length(Loggers)==3){
      jpeg(filename=name)
    dat_logger1=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)==Loggers[1]]
    dat_logger2=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)==Loggers[2]]
    dat_logger3=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)==Loggers[3]]
    plot(dat_logger1[[1]]$Datetime.1, dat_logger1[[1]][,4])
    lines(dat_logger2$Datetime.1[[1]], dat_logger2[[1]][,4])
    lines(dat_logger3$Datetime.1[[1]], dat_logger3[[1]][,4])
    dev.off()
    }
    else if(length(Loggers)==2){
      jpeg(filename=name)
      dat_logger1=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)==Loggers[1]]
      dat_logger2=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)==Loggers[2]]
      plot(dat_logger1[[1]]$Datetime.1, dat_logger1[[1]][,4])
      lines(dat_logger2[[1]]$Datetime.1, dat_logger2[[1]][,4])
      dev.off()
    }
  else{}
}

metadata=na.omit(metadata)

placeIID=unique(metadata$PlaceID)
test=metadata$ID[metadata$PlaceID==placeIID[1]]
test
dat_logger1=list_iButton_corr_tidy_date_day[[test[1]]]
dat_logger1=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)==test[1]]

#test
Logger=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date)==des$ï..Logger.ID[des$place_ID=="Aasee_1"]]
Logger2=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date)==des$ï..Logger.ID[des$place_ID=="ULB_1"]]
plot(Logger2[[1]]$Datetime.1, Logger2[[1]][,4], type="l")
lines(Logger2[[2]]$Datetime.1, Logger2[[2]][,4])


for(i in 1:unique(des$place_ID)){
  dataname=names(list_iButton_corr_tidy_date_day)[i] #save logger ID 
  name=paste("day","plot",dataname,".jpg") #set filename
  title=paste("Day Temperature",dataname) #set title for plot
  #get parameters for subtitle
  place.name=des$Place_name[des$ï..Logger.ID==dataname]
  place.number=des$Place_number[des$ï..Logger.ID==dataname]
  place.type=des$Place_type[des$ï..Logger.ID==dataname]
  description=des$Description[des$ï..Logger.ID==dataname]
  subtitle=paste("Logger was placed in", place.name, place.number, "in", place.type,
                 description)
  data=list_iButton_corr_tidy_date_day[[i]] #retrieve dataframe from list
  jpeg(filename = name)
  plot(data$Datetime.1, data[,4], main=title, 
       sub=paste("Logger was placed in", place.name, place.number, "in", place.type, "\n",
                 description),
       ylab="Temperature [°C]", xlab=" ")
  abline(v=sun2$sunrise, col="blue")
  abline(v=sun2$sunset, col="red")
  legend("topright", 
         legend=c("Day Temperature", "Sunrise", "Sunset"), 
         fill=c("black", "orange", "red"), cex=0.7)
  dev.off()
}