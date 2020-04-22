
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
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/plots_day_night_20.08/")
#plot and save day data

for(i in 1:length(list_iButton_corr_tidy_date_day)){
  dataname=names(list_iButton_corr_tidy_date_day)[i] #save logger ID 
  name=paste("day","plot",dataname,".pdf") #set filename
  title=paste("Day Temperature",dataname) #set title for plot
  #get parameters for subtitle
    place.name=des$Place_name[des$ï..Logger.ID==dataname]
    place.number=des$Place_number[des$ï..Logger.ID==dataname]
    place.type=des$Place_type[des$ï..Logger.ID==dataname]
    description=des$Description[des$ï..Logger.ID==dataname]
    subtitle=paste("Logger was placed in", place.name, place.number, "in", place.type,
                   description)
  data=list_iButton_corr_tidy_date_day[[i]] #retrieve dataframe from list
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(data$Datetime.1, data[,4], main=title, 
       sub=paste("Logger was placed in", place.name, place.number, "in", place.type, "\n",
                 description),
       ylab="Temperature [°C]", xlab=" ",pch=20, cex=2)
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
  name=paste("night","plot",dataname,".pdf")
  title=paste("Night Temperature",dataname)
  #get parameters for subtitle
  place.name=des$Place_name[des$ï..Logger.ID==dataname]
  place.number=des$Place_number[des$ï..Logger.ID==dataname]
  place.type=des$Place_type[des$ï..Logger.ID==dataname]
  description=des$Description[des$ï..Logger.ID==dataname]
  data=list_iButton_corr_tidy_date_night[[i]]
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(data$Datetime.1, data[,4], main=title,
       sub=paste("Logger was placed in", place.name, place.number, "in", place.type,"\n",
                 description),
       ylab="Temperature [°C]", xlab=" ", pch=20, cex=2)
  abline(v=sun2$sunrise, col="blue")
  abline(v=sun2$sunset, col="red")
  legend("topright", 
         legend=c("Night Temperature", "Sunrise", "Sunset"), 
         fill=c("black", "orange", "red"), cex=0.7)
  dev.off()
}


#plot both day and night together

for(i in 1:length(list_iButton_corr_tidy_date_night)){
  dataname=names(list_iButton_corr_tidy_date_night)[i]
  name=paste("both","plot",dataname,".pdf")
  title=paste("Temperature",dataname)
  #get parameters for subtitle
  place.name=des$Place_name[des$ï..Logger.ID==dataname]
  place.number=des$Place_number[des$ï..Logger.ID==dataname]
  place.type=des$Place_type[des$ï..Logger.ID==dataname]
  description=des$Description[des$ï..Logger.ID==dataname]
  data.day=list_iButton_corr_tidy_date_day[[i]]
  data.night=list_iButton_corr_tidy_date_night[[i]]
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(data.day$Datetime.1, data.day[,4], main=title,
       sub=paste("Logger was placed in", place.name, place.number, "in", place.type,"\n",
                 description),
       ylab="Temperature [°C]", xlab=" ", col="darkgreen", pch=20, cex=2)
  points(data.night$Datetime.1, data.night[,4], col="darkblue", pch=20, cex=2)
  abline(v=sun2$sunrise, col="orange")
  abline(v=sun2$sunset, col="red")
  legend("topright", 
         legend=c("Day Temperature", "Night Temperature", "Sunrise", "Sunset"), 
         fill=c("darkgreen", "darkblue", "orange", "red"), cex=0.7)
  dev.off()
}

#plot as line graph

for(i in 1:length(list_iButton_corr_tidy)){
  dataname=names(list_iButton_corr_tidy)[i]
  name=paste("line","plot",dataname,".pdf")
  title=paste("Temperature",dataname)
  #get parameters for subtitle
  place.name=des$Place_name[des$ï..Logger.ID==dataname]
  place.number=des$Place_number[des$ï..Logger.ID==dataname]
  place.type=des$Place_type[des$ï..Logger.ID==dataname]
  description=des$Description[des$ï..Logger.ID==dataname]
  data=list_iButton_corr_tidy[[i]]
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(data$Datetime.1, data[,4], main=title,
       sub=paste("Logger was placed in", place.name, place.number, "in", place.type,"\n",
                 description),
       ylab="Temperature [°C]", xlab=" ", col="darkblue", type="l")
  abline(v=sun2$sunrise, col="orange")
  abline(v=sun2$sunset, col="red")
  legend("topright", 
         legend=c("Temperature",  "Sunrise", "Sunset"), 
         fill=c("darkblue", "orange", "red"), cex=0.7)
  dev.off()
}

