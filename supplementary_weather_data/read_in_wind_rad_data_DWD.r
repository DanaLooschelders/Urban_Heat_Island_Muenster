#read in wind data from DWD
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/DWD data/")
wind=read.table("produkt_zehn_min_ff_20190130_20200801_01766.txt", 
                sep=";", dec=".",header=T)

wind$MESS_DATUM=strptime(wind$MESS_DATUM, format="%Y%m%d%H%M")
any(wind$QN>3) #quality control 
#change names
names(wind)[4:5]=c("wind_speed", "wind_dir")
#subset to measuring time
wind=wind[wind$MESS_DATUM>=list_iButton_corr_tidy[[1]][1,2]&wind$MESS_DATUM<=list_iButton_corr_tidy[[1]][dim(list_iButton_corr_tidy[[1]])[1],2],]
which((is.na(c(wind$wind_speed, wind$wind_dir))))
wind=wind[-c(1:12),]
#filter values with SW wind
#SSW: 191.25
#WSW: 258.75
#subset for south west wind
wind_sw=wind
wind_sw$wind_speed[wind_sw$wind_dir<191|wind_sw$wind_dir>258]=NA
#subset for windspeed >0
wind_sw$wind_speed[wind_sw$wind_speed<0]=NA
#