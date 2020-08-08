#rowbind dataframes two one list
for(i in names(list_iButton_corr_set)){
  dat=list_iButton_corr_set[[i]]
  dat_2=list_second_iButton_corr_set[[i]]
  dat_bind=rbind(dat, dat_2)
  list_iButton_corr_set[[i]]=dat_bind
}

#add consecutive and NA in order to dispaly it correctly in the plot 
date_time <- seq.POSIXt(from=list_iButton_corr_set[[1]][1,2],
             to=list_iButton_corr_set[[1]][dim(list_iButton_corr_set[[1]])[1],2],
             by="10 min") #create complete timeframe

for(i in 1:length(list_iButton_corr_set)){
  test=xts(list_iButton_corr_set[[i]][,1],list_iButton_corr_set[[i]][,2]) 
  #merge logger time series with emtpy one minute time series
  test2=merge(test,date_time)
  test2=data.frame("Temperature_C"=test2) #name the new column
  test2$Datetime.1=rownames(test2) #use the newly set times to replace previous time data
  rownames(test2)=NULL #delete rownames
  colnames(test2)=c("Temperature_C", "Datetime.1")
  list_iButton_corr_set[[i]]=test2
}

list_iButton_corr_set_date <- lapply(list_iButton_corr_set, `[`, 2)
list_iButton_corr_set_date <- lapply(list_iButton_corr_set_date, function(x) as.POSIXct(x$Datetime.1,format="%Y-%m-%d %H:%M:%S"))
list_iButton_corr_set = map2(list_iButton_corr_set, list_iButton_corr_set_date, ~ mutate(., Datetime.1 = .y)) 

