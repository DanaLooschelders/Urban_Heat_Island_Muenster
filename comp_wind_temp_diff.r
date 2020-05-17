setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/weather")
#plot wind with temperature difference 
str(Temp_diff_data_frame)
str(wind)
#subset wind to length of data
beginn=min(Temp_diff_data_frame$Date)
ende=max(Temp_diff_data_frame$Date)
wind_subset=wind[wind$MESS_DATUM>beginn&wind$MESS_DATUM<ende,]
#check
range(wind_subset$MESS_DATUM, na.rm=T)
range(Temp_diff_data_frame$Date)

#plot for every place
 #set margins to accomadate second axis 
for (i in 2:length(Temp_diff_data_frame)-1){ #plot for every column except last column which contains date
  png(filename = paste("wind_diff_correlation",
                       substring(Temp_diff_data_frame[1,length(Temp_diff_data_frame)],1,10),
                       names(Temp_diff_data_frame)[i],".png"))
  op=par(mar = c(4,5,2,4))
  plot(Temp_diff_data_frame$Date, Temp_diff_data_frame[,i], 
     type="l", 
     ylim=c(range(wind$wind_speed, Temp_diff_data_frame[,i], na.rm=T)))
lines(wind_subset$MESS_DATUM, wind_subset$wind_speed, col="red")
axis(4, col="red")
mtext("Windspeed [m/s] FMO", side=4, line=2)
dev.off()
par(op) #remove set margins
}

