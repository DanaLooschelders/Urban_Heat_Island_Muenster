#check for signficant difference to map windstream
#check temperature difference between Aegidii logger (first and last)
mean(list_iButton_Aegidii_day[[1]][,3], na.rm=T)
mean(list_iButton_Aegidii_day[[5]][,3], na.rm=T)

mean(list_iButton_Aegidii_night[[1]][,3],na.rm=T)
mean(list_iButton_Aegidii_night[[5]][,3],na.rm=T)

#prep data
temp=list_iButton_corr_tidy_Aegidii[[1]][,3]
temp_ref=list_iButton_corr_tidy_Aegidii[[5]][,3]
temp_two=list_iButton_corr_tidy_Aegidii[[2]][,3]
date=list_iButton_corr_tidy_Aegidii[[1]][,2]
factor=list_iButton_factor_Aegidii[[1]][,6]

#check correlation of windspeed and temperature
cor(wind_sw$wind_speed,temp, use = "na.or.complete")
cor.test(wind_sw$wind_speed,temp, use = "na.or.complete")

#compare only temperatures with windspeeds >0 and wind dir from SW
temp_wind=data.frame(wind_sw$MESS_DATUM, 
                     wind_sw$wind_speed,
                     temp,
                     temp_two,
                     temp_ref,
                     date,
                     factor)
temp_wind$temp[is.na(temp_wind$wind_sw.wind_speed)]=NA
#filter temp values for which windspeed is higher 0
temp_wind$temp_ref[is.na(temp_wind$wind_sw.wind_speed)]=NA

mean(temp_wind$temp[temp_wind$factor=="day"], na.rm=T)
mean(temp_wind$temp_ref[temp_wind$factor=="day"], na.rm=T)

mean(temp_wind$temp[temp_wind$factor=="night"], na.rm=T)
mean(temp_wind$temp_ref[temp_wind$factor=="night"], na.rm=T)

#compare Ehrenpark (87) and Haus Kump (64)
Ehrenpark=list_iButton_corr_tidy_date_factor[["87"]]
Haus_Kump=list_iButton_corr_tidy_date_factor[["64"]]

mean(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"],na.rm=T)
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"],na.rm=T)

mean(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"],na.rm=T)
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"],na.rm=T)

#test significance
#normality:
shapiro.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"])
wilcox.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"], 
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])

shapiro.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"])
wilcox.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"])

#compare Haus Kump and first Aegidiilogger
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"],na.rm=T)
mean(temp_wind$temp[temp_wind$factor=="day"], na.rm=T)

mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"],na.rm=T)
mean(temp_wind$temp[temp_wind$factor=="night"], na.rm=T)

shapiro.test(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])
wilcox.test(temp_wind$temp[temp_wind$factor=="day"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])

shapiro.test(temp_wind$temp[temp_wind$factor=="night"])
wilcox.test(temp_wind$temp[temp_wind$factor=="night"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"])
#check for second aegiodi logger
mean(temp_wind$temp_two[temp_wind$factor=="day"], na.rm=T)
mean(temp_wind$temp[temp_wind$factor=="night"], na.rm=T)

diff=Ehrenpark$Temperature_C_w_off-Haus_Kump$Temperature_C_w_off

#plot difference between Ehrenpark and Haus Kump with wind
pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),
               "diff_Ehrenpark_Haus_K.pdf"), 
    paper = "a4r", height=7, width=14)
plot(Ehrenpark$Datetime.1, diff, type="l",ylim=c(-5,10),
     sub=">0: warmer in Ehrenpark, <0 warmer in Haus K")
points(wind_sw$MESS_DATUM, wind_sw$wind_speed, col="red")
abline(v=sun2$sunrise, col="green")
abline(v=sun2$sunset, col="blue")
dev.off()

cor.test(diff, wind_sw$wind_speed)

#use stiftsherrenstrasselogger as comparison for other aegidiilogger