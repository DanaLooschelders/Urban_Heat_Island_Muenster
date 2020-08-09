#Plot windtsream
#set colors for plot
colors=c("orange", "green", "blue", "red", "yellow")

#plot all Aegidii logger
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(list_iButton_corr_tidy_Aegidii[[1]][2:3],
     type="l", col=colors[1], ylim=c(5,35),
     ylab="Temperature [°C]", xlab="Date", main="Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_corr_tidy_Aegidii)){
  lines(list_iButton_corr_tidy_Aegidii[[i]][2:3], col=colors[i])
}
legend("topright",legend = names(list_iButton_corr_tidy_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#daily median with ggplot
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_median_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(list_iButton_24h_Aegidii[[1]][c(1,3)],
     type="l", col=colors[1], ylim=c(13,22),
     ylab="Temperature [°C]", xlab="Date", main="Median Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_24h_Aegidii)){
  lines(list_iButton_24h_Aegidii[[i]][c(1,3)], col=colors[i])
}
legend("topright",legend = names(list_iButton_24h_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#daily mean with ggplot
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_mean_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(list_iButton_24h_Aegidii[[1]][c(1,2)],
     type="l", col=colors[1], ylim=c(13,22),
     ylab="Temperature [°C]", xlab="Date", main="Mean Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_24h_Aegidii)){
  lines(list_iButton_24h_Aegidii[[i]][c(1,2)], col=colors[i])
}
legend("topright", legend = names(list_iButton_24h_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#daytime mean with ggplot
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_day_mean_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(list_iButton_day_Aegidii[[1]][c(1,2)],
     type="l", col=colors[1], ylim=c(13,25),
     ylab="Temperature [°C]", xlab="Date", main="Mean daytime Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_day_Aegidii)){
  lines(list_iButton_day_Aegidii[[i]][c(1,2)], col=colors[i])
}
legend("topright", legend = names(list_iButton_day_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#hourly mean with ggplot
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_hourly_mean_Aasee.pdf"), paper = "a4r", height=7, width=14)
plot(as.POSIXlt(list_iButton_hourly_Aegidii[[1]][,1]),
     list_iButton_hourly_Aegidii[[1]][,2],
     type="l", col=colors[1], ylim=c(5,30),
     ylab="Temperature [°C]", xlab="Date", 
     main="Mean hourly Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_hourly_Aegidii)){
  lines(as.POSIXlt(list_iButton_hourly_Aegidii[[i]][,1]),
        list_iButton_hourly_Aegidii[[i]][,2], col=colors[i])
}
legend("topright", legend = names(list_iButton_hourly_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

#relate wind speed to air temp in aegidiistrs
#plot all Aegidii logger with wind
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/windstream/merge")

pdf(file="Windstream_Aasee_sw_wind.pdf", paper = "a4r", height=7, width=14)
plot(list_iButton_corr_tidy_Aegidii[[1]][2:3],
     type="l", col=colors[1], ylim=c(0,35),
     ylab="Temperature [°C]", xlab="Date", main="Temperature at Aegidiistr in 2020")
for(i in 2:length(list_iButton_corr_tidy_Aegidii)){
  lines(list_iButton_corr_tidy_Aegidii[[i]][2:3], col=colors[i])
}
points(wind_sw$MESS_DATUM,wind_sw$wind_speed)
legend("topright",legend = names(list_iButton_corr_tidy_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()
#correlation stronger for first logger?


#hourly mean with wind 
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_hourly_mean_Aasee_with_wind.pdf"), paper = "a4r", height=7, width=14)
plot(wind_sw$MESS_DATUM, wind_sw$wind_speed, type="p",
     ylim=c(0,30),
     ylab="Temperature [°C]", xlab="Date", 
     main="Mean hourly Temperature at Aegidiistr in 2020")
for(i in 1:length(list_iButton_hourly_Aegidii)){
  lines(as.POSIXlt(list_iButton_hourly_Aegidii[[i]][,1]),
        list_iButton_hourly_Aegidii[[i]][,2], col=colors[i])
}
legend("topright", legend = names(list_iButton_hourly_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()

pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),"Windstream_daily_mean_Aasee_with_wind.pdf"), paper = "a4r", height=7, width=14)
plot(wind_sw$MESS_DATUM, wind_sw$wind_speed, type="p",
     ylim=c(0,30),
     ylab="Temperature [°C]", xlab="Date", 
     main="Mean hourly Temperature at Aegidiistr in 2020")
for(i in 1:length(list_iButton_24h_Aegidii)){
  lines(as.POSIXlt(list_iButton_24h_Aegidii[[i]][,1]),
        list_iButton_24h_Aegidii[[i]][,2], col=colors[i])
}
legend("topright", legend = names(list_iButton_hourly_Aegidii), 
       fill =colors, cex=0.7 )
dev.off()
