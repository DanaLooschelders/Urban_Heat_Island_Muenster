
#plot all the plots for cleared and split data
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/plots_day_night/merge/")

#plot and save day data
for(i in 1:length(list_iButton_corr_tidy_date_day)){
  dataname=names(list_iButton_corr_tidy_date_day)[i] #save logger ID 
  date=as.character(list_iButton_corr_tidy_date_day[[1]][1,2])
  date=substr(date, 1,10)
   name=paste(date,"day","plot",dataname,".pdf") #set filename
  title=paste("Day Temperature",dataname) #set title for plot
  #get parameters for subtitle
    place.name=metadata$Standort_ID[metadata$Logger_ID==dataname]
    description=metadata$Ortsbeschreibung[metadata$Logger_ID==dataname]
    subtitle=paste("Logger was placed in", place.name,
                   description)
  data=list_iButton_corr_tidy_date_day[[i]] #retrieve dataframe from list
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(data$Datetime.1, data[,3], main=title, 
       sub=paste("Logger was placed in", place.name, "\n",
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
  date=as.character(list_iButton_corr_tidy_date_night[[1]][1,2])
  date=substr(date, 1,10)
  name=paste(date,"night","plot",dataname,".pdf")
  title=paste("Night Temperature",dataname)
  #get parameters for subtitle
  place.name=metadata$Place_name[metadata$Logger_ID==dataname]
  description=metadata$Ortsbeschreibung[metadata$Logger_ID==dataname]
  data=list_iButton_corr_tidy_date_night[[i]]
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(data$Datetime.1, data[,3], main=title,
       sub=paste("Logger was placed in", place.name, "\n",
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
  date=as.character(list_iButton_corr_tidy_date_night[[1]][1,2])
  date=substr(date, 1,10)
  name=paste(date,"both","plot",dataname,".pdf")
  title=paste("Temperature",dataname)
  #get parameters for subtitle
  place.name=metadata$Place_name[metadata$Logger_ID==dataname]
  description=metadata$Ortsbeschreibung[metadata$Logger_ID==dataname]
  data.day=list_iButton_corr_tidy_date_day[[i]]
  data.night=list_iButton_corr_tidy_date_night[[i]]
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(data.day$Datetime.1, data.day[,3], main=title,
       sub=paste("Logger was placed in", place.name, "\n",
                 description),
       ylab="Temperature [°C]", xlab=" ", col="darkgreen", pch=20, cex=2)
  points(data.night$Datetime.1, data.night[,3], col="darkblue", pch=20, cex=2)
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
  date=as.character(list_iButton_corr_tidy[[1]][1,2])
  date=substr(date, 1,10)
  name=paste(date,"line","plot",dataname,".pdf")
  title=paste(date,"Temperature",dataname)
  #get parameters for subtitle
  place.name=metadata$Place_name[metadata$Logger_ID==dataname]
  description=metadata$Ortsbeschreibung[metadata$Logger_ID==dataname]
  data=list_iButton_corr_tidy[[i]]
  pdf(file = name, width=14, height=7, paper="a4r")
  plot(data$Datetime.1, data[,3], main=title,
       sub=paste("Logger was placed in", place.name, "\n",
                 description),
       ylab="Temperature [°C]", xlab=" ", col="darkblue", type="l")
  abline(v=sun2$sunrise, col="orange")
  abline(v=sun2$sunset, col="red")
  legend("topright", 
         legend=c("Temperature",  "Sunrise", "Sunset"), 
         fill=c("darkblue", "orange", "red"), cex=0.7)
  dev.off()
}

