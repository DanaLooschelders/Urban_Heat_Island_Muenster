#descriptive statistics:
#overall, 24-hour, daily, nightly 
#mean, median, standard deviation (mms)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/stats_plot_mms/")

#*************************************************************************
#prep data
#****************************************************************************

#use list_iButton_corr_tidy_date and create new column for factor
time_factor=rep(NA, length(list_iButton_corr_tidy_date))
list_iButton_corr_tidy_date_factor=mapply(cbind, list_iButton_corr_tidy_date, "Time_factor"=time_factor, SIMPLIFY=F)

#create times for sunriseplusdawn, sunriseminusdawn etx
sun2$sunrise_plusDawn=sun2$sunrise+0.5*60*60 #add 30 min dawn
sun2$sunset_minusDusk=sun2$sunset-0.5*60*60 #substract 30min dawn
sun2$sunrise_minusDawn=sun2$sunrise-0.5*60*60 #substract 30 min dawn
sun2$sunset_plusDusk=sun2$sunset+0.5*60*60 #add 30min dawn

#for loop to go through loggers 
#use sunrise/sunset data
#add day/night/dawn/dusk as factor to list
for(x in 1:length(list_iButton_corr_tidy_date_factor)){
  dat=list_iButton_corr_tidy_date_factor[[x]]
  for(i in 1:length(sun2$date)){
    sun=sun2$date[i] #get date 
    dat_day=dat[dat$Date==sun,] #subset the day that matches i from sun from dataframe
    #add factor "day" to time from sunrise to sunset (without dawn)
    dat_day$Time_factor[dat_day$Datetime.1>=sun2$sunrise_plusDawn[sun2$date==sun]&dat_day$Datetime.1<=sun2$sunset_minusDusk[sun2$date==sun]]="day" 
    #add factor "night" to time from sunset to sunrise (without dawn)
    dat_day$Time_factor[dat_day$Datetime.1<=sun2$sunrise_minusDawn[sun2$date==sun]|dat_day$Datetime.1>=sun2$sunset_plusDusk[sun2$date==sun]]="night" 
    #add factor "dawn" to the hour of sunrise
    dat_day$Time_factor[dat_day$Datetime.1>=sun2$sunrise_minusDawn[sun2$date==sun]&dat_day$Datetime.1<=sun2$sunrise_plusDawn[sun2$date==sun]]="dawn"
    #add factor "dusk" to hour of sunset
    dat_day$Time_factor[dat_day$Datetime.1>=sun2$sunset_minusDusk[sun2$date==sun]&dat_day$Datetime.1<=sun2$sunset_plusDusk[sun2$date==sun]]="dusk"
    #replace ????
    dat[dat$Date==sun2$date[i],]=dat_day 
  }
  list_iButton_corr_tidy_date_factor[[x]]=dat
}

#***************************************************************
#get overall mean, median and sd per logger
#********************************************************************

#create temporary dataframe to use in list
mms.overall=data.frame("logger"=as.character(names(list_iButton_corr_tidy_date_factor)),"mean"=rep(NA), "median"=rep(NA), "sd"=rep(NA))
for (i in 1:length(list_iButton_corr_tidy_date_factor)){
  data=list_iButton_corr_tidy_date_factor[[i]] #loop through every iButton on list
  name=as.character(names(list_iButton_corr_tidy_date_factor)[i])  
  mms.overall$mean[mms.overall$logger==name]=mean(data[,3], na.rm=T) #calculate mean for every logger
    mms.overall$median[mms.overall$logger==name]=median(data[,3], na.rm=T) #calculate median for every logger
    mms.overall$sd[mms.overall$logger==name]=sd(data[,3], na.rm=T) #calculate standard deviation for every logger
}
#****************************************************************
#get 24h mean, median and standard deviation
#****************************************************************

#create list with dataframes for every iButton 
#containing date, mean, meadian, sd 
list_iButton_24h_mms=list() #create output list
#create temporary dataframe to use in list
temp.data=data.frame("date"=unique(list_iButton_corr_tidy_date_factor[[1]][,5]),"mean"=rep(NA), "median"=rep(NA), "sd"=rep(NA))
for (i in 1:length(list_iButton_corr_tidy_date_factor)){
  data=list_iButton_corr_tidy_date_factor[[i]] #loop through every iButton on list
  for (x in unique(data$Date)){ #loop through every day for iButton
    temp.data$mean[temp.data$date==x]=mean(data[,3][data$Date==x], na.rm=T) #calculate mean for every day
    temp.data$median[temp.data$date==x]=median(data[,3][data$Date==x], na.rm=T) #calculate median for every day
    temp.data$sd[temp.data$date==x]=sd(data[,3][data$Date==x], na.rm=T) #calculate standard deviation for every day
}
list_iButton_24h_mms[[i]]=temp.data
}
#set logger IDs as names of dataframes
names(list_iButton_24h_mms)=names(list_iButton_corr_tidy_date_factor)

#**************************************************************
#get day/night mean, median and standard deviation
#**************************************************************
list_iButton_day_mms=list()
list_iButton_night_mms=list()
temp.data.day=data.frame("date"=unique(list_iButton_corr_tidy_date_factor[[1]][,5]),"mean"=rep(NA), "median"=rep(NA), "sd"=rep(NA))
temp.data.night=data.frame("date"=unique(list_iButton_corr_tidy_date_factor[[1]][,5]),"mean"=rep(NA), "median"=rep(NA), "sd"=rep(NA))

for (i in 1:length(list_iButton_corr_tidy_date_factor)){
  data=list_iButton_corr_tidy_date_factor[[i]] #loop through every iButton on list
  for (x in unique(data$Date)){ #loop through every day for iButton
    #calculate stats for daytime
    data.day=data[data$Date==x&data$Time_factor=="day",]
    temp.data.day$mean[temp.data$date==x]=mean(data.day[,3][data.day$Date==x], na.rm=T) #calculate mean for every day
    temp.data.day$median[temp.data$date==x]=median(data.day[,3][data.day$Date==x], na.rm=T) #calculate median for every day
    temp.data.day$sd[temp.data$date==x]=sd(data.day[,3][data.day$Date==x], na.rm=T) #calculate standard deviation for every day
    #calculate stats for nighttime
    data.night=data[data$Date==x&data$Time_factor=="night",]
    temp.data.night$mean[temp.data$date==x]=mean(data.night[,3][data.night$Date==x], na.rm=T) #calculate mean for every day
    temp.data.night$median[temp.data$date==x]=median(data.night[,3][data.night$Date==x], na.rm=T) #calculate median for every day
    temp.data.night$sd[temp.data$date==x]=sd(data.night[,3][data.night$Date==x], na.rm=T) #calculate standard deviation for every day
    }
  list_iButton_day_mms[[i]]=temp.data.day
  list_iButton_night_mms[[i]]=temp.data.night
}

#set logger IDs as names of dataframes
names(list_iButton_day_mms)=names(list_iButton_corr_tidy_date_factor)
names(list_iButton_night_mms)=names(list_iButton_corr_tidy_date_factor)

#calculate hourly means
list_iButton_hourly=lapply(list_iButton_corr_tidy, 
                           function(x) aggregate(x$Temperature_C_w_off, 
                                                 list(hour=cut(x$Datetime.1, 
                                                               breaks="hour")),
                                                 mean, na.rm=T))
