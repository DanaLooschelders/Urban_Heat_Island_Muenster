#filter temperature data into day/night
#read in sunrise/sunset data
setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit")
sun=read.table("Sunrise_sunset_times.csv", sep=";", dec=",", header=T, stringsAsFactors = F)
sun$ï..Datum=strptime(sun$ï..Datum, "%d.%m.%Y")
str(sun)

#restructure dataframe to paste the date onto the times and convert to POSIXct
sun2=data.frame("sunrise"= as.POSIXct(paste(sun$ï..Datum, sun$Sonnenaufgang)))
sun2$sunset=as.POSIXct(paste(sun$ï..Datum, sun$Sonnenuntergang))
sun2$date=sun$ï..Datum
str(sun2)

#-> create new column for sunrise/sunset data with time corrected for dawn
sun2$sunrise_wDawn=sun2$sunrise+0.5*60*60 #add 30 min dawn
sun2$sunset_wDawn=sun2$sunset-0.5*60*60 #substract 30min dawn

#use start_time and end_time to subset sun2 data to correct length
sun2=sun2[sun2$date>=start_time&sun2$date<=end_time,]

#create column with only date in temperature
# select first column and write into seperate list
list_iButton_corr_tidy_col <- lapply(list_iButton_corr_tidy, `[`, 1)
# Transform datetime to only date
list_iButton_corr_tidy_col <- lapply(list_iButton_corr_tidy_col, function(x) as.Date(x$Datetime,format = "%Y-%m-%d"))
# add Date as additional column
list_iButton_corr_tidy_date <- mapply(cbind, list_iButton_corr_tidy, "Date"=list_iButton_corr_tidy_col, SIMPLIFY=F)

#match days in sunrise and temperature data
#filter temp data for range between sunrise (+ 30 min) and sunset (-30 min)  to include dawn

#test with one dataframe

test=list_iButton_corr_tidy_date[[1]]
str(test)

test_day=test[test$Date==sun2$date[1],]
test_day=test_day[test_day$Datetime.1>=sun2$sunrise_wDawn[sun2$date=="2019-08-03 BST"]&test_day$Datetime.1<=sun2$sunset_wDawn[sun2$date=="2019-08-03 BST"],]
test[test$Date==sun2$date[1],]=subset(test_day, Datetime.1>=sun2$sunrise_wDawn[sun2$date=="2019-08-03 BST"]&Datetime.1<=sun2$sunset_wDawn[sun2$date=="2019-08-03 BST"])
sub=subset(test_day, Datetime.1>=sun2$sunrise_wDawn[sun2$date=="2019-08-03 BST"]&Datetime.1<=sun2$sunset_wDawn[sun2$date=="2019-08-03 BST"])

test=test[test$Date==sun2$date[1]&test$Datetime.1>=sun2$sunrise_wDawn[sun2$date=="2019-08-03 BST"]&test$Datetime.1<=sun2$sunset_wDawn[sun2$date=="2019-08-03 BST"],]
#str
                                    
#test[test$Date==sun2$date[1],]=paste(test_day, rep(NA, length(test$Temperature_C[test$Date==sun2$date[1]])-length(test_day$Temperature_C)))
#str(test_day)
#dim(test[test$Date==sun2$date[1],])
#dim(test_day)

for(i in sun2$date)
test_day=test[test$Date==i,] #subset the day that matches i from sun from dataframe
test_day=test_day[test_day$] #subset the day with sunrise and sunset value from sun for i
test[test$Date==i,]=test_day #replace orinigal day with shortend day and fill with NAs
test_day=test$Temperature_C[test$Date==sun2$date[5]]



for (i in 1:length(test_day)) {
  name_save=names(test_day)
  test=list_iButton_corr_tidy[[i]]
  
  
  list_iButton_corr_tidy[[i]]=test
  names(list_iButton_corr_tidy)=name_save
 
}