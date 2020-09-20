#correlation air stream with traffic
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/windstream/merge")
#subset traffic to temp data
traffic_sub=traffic[traffic$datetime>="2020-07-07 00:00:00"&traffic$datetime<"2020-07-30 00:00:00",]

#plot difference between Ehrenpark and Haus Kump with wind
#pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),
#               "traffic_wind2_diff_Ehrenpark_Haus_K.pdf"), 
#    paper = "a4r", height=7, width=14)
#plot(Ehrenpark$Datetime.1, diff, type="l",ylim=c(-5,10),
#     sub=">0: warmer in Ehrenpark, <0 warmer in Haus K")
#points(wind_sw$MESS_DATUM, wind_sw$wind_speed, col="red", cex=.5)
#lines(traffic_sub$datetime, traffic_sub$cars/1000, col="blue")
#abline(v=sun2$sunrise, col="green")
#abline(v=sun2$sunset, col="blue")
#dev.off()


#use wind

temp_wind$Ehrenpark=list_iButton_corr_tidy_date_factor[["87"]]$Temperature_C_w_off
temp_wind$Haus_Kump=list_iButton_corr_tidy_date_factor[["64"]]$Temperature_C_w_off
#aggregate temperature data to hourly means
temp_wind$datehour <- cut(temp_wind$wind.MESS_DATUM, breaks="hour") 
#claculate hourly mean
mean_Ehrenpark <- aggregate(Ehrenpark ~ datehour, 
                            temp_wind, mean, na.action=na.pass)
mean_Haus_Kump <- aggregate(Haus_Kump ~ datehour, 
                            temp_wind, mean, na.action=na.pass)

hourly_diff=mean_Ehrenpark$Ehrenpark-mean_Haus_Kump$Haus_Kump

shapiro.test(hourly_diff)
cor.test(hourly_diff, traffic_sub$cars, method="spearman")
#-0.742 pvalue <2e16

