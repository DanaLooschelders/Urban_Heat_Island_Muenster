
#assign description for plots
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
#read in csv with lat lon and description of places
des=read.table("Lat_Lon_Logger.csv", sep=";", dec=",", header=T)
des=des[,1:7] #drop column with water depth (for now)
des=na.omit(des) #drop rows with NA
des$ï..Logger.ID=as.character(des$ï..Logger.ID) #set logger ID as character to match names

str(des)
str(list_iButton_corr_tidy_date_day[[1]])

#plot all the plots for cleared and split data
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/plots_day_night_01.08/")
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/plots_day_night_02.09/")
#plot and save day data

for(i in 1:length(list_iButton_corr_tidy_date_day)){
  dataname=names(list_iButton_corr_tidy_date_day)[i] #save logger ID 
  name=paste("day","plot",dataname,".jpg") #set filename
  title=paste("Day Temperature",dataname) #set title for plot
  #get parameters for subtitle
    place.name=des$Place_name[des$ï..Logger.ID==dataname]
    place.number=des$Place_number[des$ï..Logger.ID==dataname]
    place.type=des$Place_type[des$ï..Logger.ID==dataname]
    subtitle=paste("Logger was placed in", place.name, place.number, "in", place.type)
  data=list_iButton_corr_tidy_date_day[[i]] #retrieve dataframe from list
  jpeg(filename = name)
  plot(data$Datetime.1, data[,4], main=title, 
       sub=subtitle, ylab="Temperature [°C]", xlab="Date")
  abline(v=sun2$sunrise, col="blue")
  abline(v=sun2$sunset, col="red")
  legend("topright", 
         legend=c("Day Temperature", "Sunrise", "Sunset"), 
         fill=c("black", "orange", "red"), cex=0.7)
  dev.off()
  }

#plot and save night data

for(i in 1:length(list_iButton_corr_tidy_date_night)){
  dataname=names(list_iButton_corr_tidy_date_night)[i]
  name=paste("night","plot",dataname,".jpg")
  title=paste("Night Temperature",dataname)
  #get parameters for subtitle
  place.name=des$Place_name[des$ï..Logger.ID==dataname]
  place.number=des$Place_number[des$ï..Logger.ID==dataname]
  place.type=des$Place_type[des$ï..Logger.ID==dataname]
  subtitle=paste("Logger was placed in", place.name, place.number, "in", place.type)
  data=list_iButton_corr_tidy_date_night[[i]]
  jpeg(filename = name)
  plot(data$Datetime.1, data[,4], main=title,
       sub=subtitle, ylab="Temperature [°C]", xlab="Date")
  abline(v=sun2$sunrise, col="blue")
  abline(v=sun2$sunset, col="red")
  legend("topright", 
         legend=c("Night Temperature", "Sunrise", "Sunset"), 
         fill=c("black", "orange", "red"), cex=0.7)
  dev.off()
}

