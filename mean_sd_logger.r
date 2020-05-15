#descriptive statistics:
#overall, 24-hour, daily, nightly 
#mean, median, standard deviation (mms)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/stats")

### use adaption of Lauras script for overall mms:
# Calculation of mean for every iButton in the list "list_iButton"
# First select only the second column of each file (= temperature record for each iButton)
list_iButton_temp <- lapply(list_iButton_corr_tidy, `[`, 3)

# Calculate the mean for every iButton and save in the list "list_iButton_mean"
list_iButton_mean <- lapply(list_iButton_temp, function(x) mean(x$Temperature, na.rm = TRUE))

# Calculate the sd for every iButton and save in the list "list_iButton_sd"
list_iButton_sd <- lapply(list_iButton_temp, function(x) sd(x$Temperature, na.rm = TRUE))

# Calculate the median for every iButton and save in the list "list_iButton_median"
list_iButton_median <- lapply(list_iButton_temp, function(x) median(x$Temperature, na.rm = TRUE))

###
# Transform list to dataframe
# Unlist the list containing the means of each iButton
d_iButton_mean <- data.frame(matrix(unlist(list_iButton_mean), nrow=length(list_iButton_mean), byrow=T))

# Round mean to one digit
d_iButton_mean <- round(d_iButton_mean, digits = 1)

# Unlist the list containing the sds of each iButton
d_iButton_sd <- data.frame(matrix(unlist(list_iButton_sd), nrow=length(list_iButton_sd), byrow=T))

# Round sd to one digit
d_iButton_sd <- round(d_iButton_sd, digits = 1)

# Unlist the list containing the medians of each iButton
d_iButton_median <- data.frame(matrix(unlist(list_iButton_median), nrow=length(list_iButton_median), byrow=T))


# Bind the names, means and sds of the respective iButton into one dataframe
d_iButton_overall_stats <- cbind(list_header, iButton_ID_multi, d_iButton_mean, d_iButton_median, d_iButton_sd)

# Assign new column names to dataframe containing means of each iButtons
colnames(d_iButton_overall_stats) = c("Name_iButton", "ID_iButton", "Temperature_mean_C", "Temperature_median_C", "Standard_deviation_C")

# Check the statistics of all iButtons
summary(d_iButton_stat)

#write results to file
write.csv2(d_iButton_overall_stats, file = paste("overall_stats", substring(list_iButton_corr_tidy[[1]][1,2], first = 1, last=10), ".csv"))

#24h mms
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
    temp.data$mean[temp.data$date==x]=mean(data[,3][data$Date==x]) #calculate mean for every day
    temp.data$median[temp.data$date==x]=median(data[,3][data$Date==x]) #calculate median for every day
    temp.data$sd[temp.data$date==x]=sd(data[,3][data$Date==x]) #calculate standard deviation for every day
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
    temp.data.day$mean[temp.data$date==x]=mean(data.day[,3][data.day$Date==x]) #calculate mean for every day
    temp.data.day$median[temp.data$date==x]=median(data.day[,3][data.day$Date==x]) #calculate median for every day
    temp.data.day$sd[temp.data$date==x]=sd(data.day[,3][data.day$Date==x]) #calculate standard deviation for every day
    #calculate stats for nighttime
    data.night=data[data$Date==x&data$Time_factor=="night",]
    temp.data.night$mean[temp.data$date==x]=mean(data.night[,3][data.night$Date==x]) #calculate mean for every day
    temp.data.night$median[temp.data$date==x]=median(data.night[,3][data.night$Date==x]) #calculate median for every day
    temp.data.night$sd[temp.data$date==x]=sd(data.night[,3][data.night$Date==x]) #calculate standard deviation for every day
    }
  list_iButton_day_mms[[i]]=temp.data.day
  list_iButton_night_mms[[i]]=temp.data.night
}

#set logger IDs as names of dataframes
names(list_iButton_day_mms)=names(list_iButton_corr_tidy_date_factor)
names(list_iButton_night_mms)=names(list_iButton_corr_tidy_date_factor)

