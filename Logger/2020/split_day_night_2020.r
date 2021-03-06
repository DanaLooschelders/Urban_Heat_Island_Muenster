#filter temperature data into day/night
#works for all datasets (if the check_iButton and tidy_data scripts were run before)
library(dplyr)
#create column with only date in temperature
# select first column and write into seperate list
list_iButton_corr_tidy_col <- lapply(list_iButton_corr_tidy, `[`, 2)
# Transform datetime to only date
list_iButton_corr_tidy_col <- lapply(list_iButton_corr_tidy_col, function(x) as.Date(x$Datetime.1, tz="Europe/Paris"))

# add Date as additional column
list_iButton_corr_tidy_date <- mapply(cbind, list_iButton_corr_tidy, "Date"=list_iButton_corr_tidy_col, SIMPLIFY=F)


#read in sunrise/sunset data
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
sun=read.table("Sunrise_sunset_times.csv", sep=";", 
               dec=",", header=T, stringsAsFactors = F,
               na.strings = c("", "NA"))
names(sun)[1]="Datum"
sun$Datum=strptime(sun$Datum, "%d.%m.%Y", tz="Europe/Paris")

sun = sun[,1:4]
sun=na.omit(sun)
#restructure dataframe to paste the date onto the times and convert to POSIXct
sun2=data.frame("sunrise"= as.POSIXct(paste(sun$Datum, sun$Sonnenaufgang)))
sun2$sunset=as.POSIXct(paste(sun$Datum, sun$Sonnenuntergang))
sun2$date=sun$Datum


#use start_time and end_time to subset sun2 data to correct length
sun2=sun2[sun2$date>=list_iButton_corr_set[[1]][1,2]&sun2$date<=list_iButton_corr_set[[1]][dim(list_iButton_corr_set[[1]])[1],2],]

#****************************************************************
#subset for day
#*****************************************************************
#-> create new column for sunrise/sunset data with time corrected for dawn
sun2$sunrise_wDawn=sun2$sunrise+0.5*60*60 #add 30 min dawn
sun2$sunset_wDawn=sun2$sunset-0.5*60*60 #substract 30min dawn

#match days in sunrise and temperature data
#filter temp data for range between sunrise (+ 30 min) and sunset (-30 min)  to include dawn

#subset the dataframes of the list and create new list with only values for the day
list_iButton_corr_tidy_date_day=list()

save.names=names(list_iButton_corr_tidy_date) #save the names in a vector

for(x in 1:length(list_iButton_corr_tidy_date)){
  dat=list_iButton_corr_tidy_date[[x]]
for(i in 1:length(sun2$date)){
  sun=sun2$date[i]
  dat_day=dat[dat$Date==sun2$date[i],] #subset the day that matches i from sun from dataframe
  dat=dat[dat$Date!=sun2$date[i],] 
  dat_day=dat_day[dat_day$Datetime.1>=sun2$sunrise_wDawn[sun2$date==sun]&dat_day$Datetime.1<=sun2$sunset_wDawn[sun2$date==sun],] #subset the day with sunrise and sunset value from sun for i
  dat=rbind(dat, dat_day) 
}
  list_iButton_corr_tidy_date_day[[x]]=dat
}

names(list_iButton_corr_tidy_date_day)=save.names #add names to list

#*******************************************************************
#do the same for the night
#*******************************************************************
#-> create new column for sunrise/sunset data with time corrected for dawn
sun2$sunrise_wDawn=sun2$sunrise-0.5*60*60 #substract 30 min dawn
sun2$sunset_wDawn=sun2$sunset+0.5*60*60 #add 30min dawn

#subset the dataframes of the list and create new list with only values for the day
list_iButton_corr_tidy_date_night=list()

for(x in 1:length(list_iButton_corr_tidy_date)){
  dat=list_iButton_corr_tidy_date[[x]]
  for(i in 1:length(sun2$date)){
    sun=sun2$date[i]
    dat_night=dat[dat$Date==sun2$date[i],] #subset the day that matches i from sun from dataframe
    dat=dat[dat$Date!=sun2$date[i],] 
    dat_night=dat_night[dat_night$Datetime.1<=sun2$sunrise_wDawn[sun2$date==sun]|dat_night$Datetime.1>=sun2$sunset_wDawn[sun2$date==sun],] #subset the day with sunrise and sunset value from sun for i
    dat=rbind(dat, dat_night) 
  }
  list_iButton_corr_tidy_date_night[[x]]=dat
}

names(list_iButton_corr_tidy_date_night)=save.names #add the names to the list