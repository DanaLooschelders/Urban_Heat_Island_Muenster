#use only for data from 03.07 to subset all ts to same intervall
range(list_iButton_corr_set[[1]]$Datetime.1)
range(list_iButton_corr_set[[2]]$Datetime.1)
start_time=strptime("2020-07-07 00:00:00", "%Y-%m-%d %H:%M:%S")
end_time=strptime("2020-07-17 00:00:00", "%Y-%m-%d %H:%M:%S")
list_iButton_corr_set = lapply(list_iButton_corr_set, function(x) {subset(x, x[,2] >= start_time & x[,2] < end_time)})

