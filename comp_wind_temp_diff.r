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

#plot 
plot(Temp_diff_data_frame$Date, Temp_diff_data_frame$Promenade_1, 
     type="l", 
     ylim=c(min(wind$wind_speed, Temp_diff_data_frame$Promenade_1, na.rm=T), max(wind_subset$wind_speed, Temp_diff_data_frame$Georgskommende_1, na.rm=T)))
lines(wind_subset$MESS_DATUM, wind_subset$wind_speed, col="red")

