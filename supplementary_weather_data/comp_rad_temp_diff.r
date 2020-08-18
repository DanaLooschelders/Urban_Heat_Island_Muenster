#see if temperature difference between water and air 
#correlate with global solar radiation
#use GS_10 (global solar rad) and temp_diff dataframe
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/weather")
#convert radiation from J/cm^2 to kJ/m^2
rad$GS_10=rad$GS_10*0.0001*1000
for(i in 1:length(Temp_diff_data_frame)-1){
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),
               names(Temp_diff_data_frame)[i],
               "Temp_diff_with_rad_data.pdf"), 
    paper = "a4r", height=7, width=14)
plot(Temp_diff_data_frame$Date, 
       Temp_diff_data_frame$Georgskommende, 
       type="l", main=paste("Temperature difference between air/water temp \n and global radiation in",
                            names(Temp_diff_data_frame)[i]),
     ylab="Temperature Difference [°C]", xlab="Date",
     ylim=c(-4,20))
lines(rad$MESS_DATUM, rad$GS_10, col="red")
dev.off()
}

for(i in 1:length(Temp_diff_data_frame)-1){
  cor(Temp_diff_data_frame[,i], rad$GS_10)
}

shapiro.test(Temp_diff_data_frame$Georgskommende)
cor(Temp_diff_data_frame$Georgskommende, rad$GS_10, 
    method = "pearson", use = "na.or.complete")


shapiro.test(Temp_diff_data_frame$ULB)
cor(Temp_diff_data_frame$ULB, rad$GS_10, 
    method = "pearson", use = "na.or.complete")


shapiro.test(Temp_diff_data_frame$Renaturierung)
cor(Temp_diff_data_frame$Renaturierung, rad$GS_10, 
    method = "pearson", use = "na.or.complete")


shapiro.test(Temp_diff_data_frame$Kanonengraben)
cor(Temp_diff_data_frame$Georgskommende, rad$GS_10, 
    method = "pearson", use = "na.or.complete")

shapiro.test(Temp_diff_data_frame$Spiekerhof)
cor(Temp_diff_data_frame$Spiekerhof, rad$GS_10, 
    method = "pearson", use = "na.or.complete")

#no good correlation found