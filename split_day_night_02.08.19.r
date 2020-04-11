library(ggplot2)
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

#try: write date rows to new file, delete original columns, drop values outside day, rbind subset to file

test=list_iButton_corr_tidy_date[[2]]
str(test)

for(i in 1:length(sun2$date)){
sun=sun2$date[i]
test_day=test[test$Date==sun2$date[i],] #subset the day that matches i from sun from dataframe
test=test[test$Date!=sun2$date[i],] 
test_day=test_day[test_day$Datetime.1>=sun2$sunrise_wDawn[sun2$date==sun]&test_day$Datetime.1<=sun2$sunset_wDawn[sun2$date==sun],] #subset the day with sunrise and sunset value from sun for i
test=rbind(test, test_day) 
}

#check of it work by plotting temperature values with sundown values  
par(new=F)
plot(test$Datetime.1, test$Temperature_C)
abline(v=sun2$sunrise_wDawn, col="blue")
abline(v=sun2$sunset_wDawn, col="red")

list_iButton_corr_tidy_date_new=list()
#test for whole list
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

test2=list_iButton_corr_tidy_date_day[[6]]
#check
par(new=F)
plot(test2$Datetime.1, test2$Temperature_C)
abline(v=sun2$sunrise_wDawn, col="blue")
abline(v=sun2$sunset_wDawn, col="red")

#test with random number ist
test_list=list()
test_object1=data.frame("Test1"=rnorm(10), "Test2"=rnorm(10))
test_object2=data.frame("Test1"=rnorm(10,mean = 20), "Test2"=rnorm(10, mean=20))
test_list[[1]]=test_object1
test_list[[2]]=test_object2

test_list2=list()
for(x in 1:length(test_list)){
  test_object=test_list[[x]]
  for(i in 1:length(test_object)){
    test_col=test_object[i,]
    test_col=test_col*2
    test_object[i,]=test_col
  }
  test_list2[[x]]=test_object
}
