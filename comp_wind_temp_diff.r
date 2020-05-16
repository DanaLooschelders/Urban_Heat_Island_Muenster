#plot wind with temperature difference 
str(Temp_diff_data_frame)
str(wind)
#subset wind to length of data
beginn=min(Temp_diff_data_frame$Date)
ende=max(Temp_diff_data_frame$Date)
wind_subset=wind[wind$MESS_DATUM>beginn&wind$MESS_DATUM<ende,]
range(wind_subset$MESS_DATUM, na.rm=T)
range(Temp_diff_data_frame$Date)
plot(Temp_diff_data_frame$Date, )