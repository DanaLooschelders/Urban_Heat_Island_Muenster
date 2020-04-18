
#assign description for plots
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
#read in csv with lat lon and description of places
des=read.table("Lat_Lon_Logger.csv", sep=";", dec=",", header=T)
str(des)
des$ï..Logger.ID=as.character(des$ï..Logger.ID)
str(list_iButton_corr_tidy_date_day[[1]])
names(list_iButton_corr_tidy_date_day[1])
names(list_iButton_corr_tidy_date_day)
#test with one
name=names(list_iButton_corr_tidy_date_day[5])
des[des$ï..Logger.ID==name,]
#plot all the plots for cleared and split data
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/plots_day_night_01.08/")
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/plots_day_night_02.09/")
#plot and save day data

for(i in 1:length(list_iButton_corr_tidy_date_day)){
  dataname=names(list_iButton_corr_tidy_date_day)[i]
  name=paste("day","plot",dataname,".jpg")
  title=paste("Day Temperature",dataname)
  data=list_iButton_corr_tidy_date_day[[i]]
  jpeg(filename = name)
  plot(data$Datetime.1, data[,4], main=title)
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
  data=list_iButton_corr_tidy_date_night[[i]]
  jpeg(filename = name)
  plot(data$Datetime.1, data[,4], main=title)
  abline(v=sun2$sunrise, col="blue")
  abline(v=sun2$sunset, col="red")
  legend("topright", 
         legend=c("Night Temperature", "Sunrise", "Sunset"), 
         fill=c("black", "orange", "red"), cex=0.7)
  dev.off()
}

