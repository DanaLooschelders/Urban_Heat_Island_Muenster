setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
#rowbind Netatmo data to form consecutive timeline
#create new list
#individually name lists to puzzle them together later
merge_netatmo=function(){
list_netatmo_1=prep_plot(datapath="data_August/net_2019-08-01_to_2019-08-20.csv",
                         metadata=metadata_1,
                         startdate="01.08")
list_netatmo_2=prep_plot(datapath="data_August_September/net_2019-08-20_to_2019-09-05.csv",
                         metadata=metadata_2,
                         startdate="20.08")
list_netatmo_3=prep_plot(datapath="data_September/net_2019-09-05_to_2019-09-25.csv",
                         metadata=metadata_3,
                         startdate="05.09")
list_netatmo_4=prep_plot(datapath="data_September_2/net_2019-09-25_to_2019-09-30.csv",
                         metadata=metadata_4,
                         startdate="25.09")
list_netatmo_merge=list_netatmo_1
#Map list 1 and list 2 together
list_netatmo_merge[names(list_netatmo_2)] <- Map(rbind, list_netatmo_merge[names(list_netatmo_2)], list_netatmo_2)
#Map 1 (merged with 2) and 3 together
list_netatmo_merge[names(list_netatmo_3)] <- Map(rbind, list_netatmo_merge[names(list_netatmo_3)], list_netatmo_3)
#Map 1 (merged with 2,3) and 4 together
list_netatmo_merge[names(list_netatmo_4)] <- Map(rbind, list_netatmo_merge[names(list_netatmo_4)], list_netatmo_4)
return(list_netatmo_merge)
}

#execute function
list_netatmo_merge=merge_netatmo()

length(unique(names(list_netatmo_merge)))

#convert to UTC+2 
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  data$Datetime=as.POSIXct(format(data$Datetime, tz="Europe/Berlin"))
  list_netatmo_merge[[i]]=data
}

#drop all dataframes with 0 rows
list_netatmo_merge=Filter(function(x) dim(x)[1] > 0, list_netatmo_merge)
length(list_netatmo_merge)

#remove all double values (overlapping measurements)
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  data=data[!duplicated(data$timestamp),]
  list_netatmo_merge[[i]]=data
}

#add column with date
list_netatmo_merge_date <- lapply(list_netatmo_merge, `[`, 6)
list_netatmo_merge_date=lapply(list_netatmo_merge_date, function(x) as.Date(x$Datetime, tz="Europe/Berlin"))
list_netatmo_merge <- mapply(cbind, list_netatmo_merge, "Date"=list_netatmo_merge_date, SIMPLIFY=F)

#check and remove stations that didn't record every day
for (i in names(list_netatmo_merge)){
  if(length(unique(list_netatmo_merge[[i]]$Date))==
     length(seq(from=min(list_netatmo_merge[[i]]$Date), 
                to=max(list_netatmo_merge[[i]]$Date), by="day"))){
  }else{ list_netatmo_merge[[i]]=NULL}
}

#use only stations that recorded over whole period (check for start and end date)
#until the 01. of October because of time conversion
for (i in names(list_netatmo_merge)){
   data=list_netatmo_merge[[i]]
  if(data$Date[1]=="2019-08-01"&data$Date[length(data$date)]=="2019-10-01"){}
  else{list_netatmo_merge[[i]]=NULL}
}
#as there are missing values inbetween create consecutive time sequence
#and create NAs for missing rows in order to get all dataframes to the same length
list_netatmo_whole=list_netatmo_merge
for (i in 1:length(list_netatmo_whole)){
  time=data.frame("Datetime"=seq(from=as.POSIXct("2019-08-01 02:15:00", tz="Europe/Berlin"), 
                                 to=as.POSIXct("2019-10-01 23:45:00", tz="Europe/Berlin"), by="30 min"))
data=list_netatmo_merge[[i]]
 time=merge(x=time,y=data, all.x=TRUE, by="Datetime")
 list_netatmo_whole[[i]]=time
}

list_netatmo_merge=list_netatmo_whole
#replace NAs in Date column by date
for (i in 1:length(list_netatmo_merge)){
  dat=list_netatmo_merge[[i]]
  dat$Date=as.Date(dat$Datetime, tz="Europe/Berlin") #fill column with date
  dat$device_id=names(list_netatmo_merge)[i] #add device id
  list_netatmo_merge[[i]]=dat
}

#subset to the 30th of September
for (i in 1:length(list_netatmo_merge)){
  dat=list_netatmo_merge[[i]]
  dat=dat[dat$Datetime<=as.POSIXct("2019-09-30 23:59:59"),]
  list_netatmo_merge[[i]]=dat
}

 #plot with no legend (as legend takes most of the screen)
ggplot(bind_rows(list_netatmo_merge, .id="df"), aes(Datetime, temperature, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
  theme(legend.position="none")
ggsave(filename = "overview_netatmo_merge.pdf", width=14, height=7)


metadata_merge=rbind(metadata_1, metadata_2, metadata_3, metadata_4)
metadata_merge=metadata_merge[!duplicated(metadata_merge$device_id),]
length(unique(metadata_merge$device_id))

#shorten metadatalist by excluding the IDs that had no data
rownames(metadata_merge)=metadata_merge$device_id #set ID as rownames
ids_to_keep=names(list_netatmo_merge) #get character vector of ids to keep
metadata_merge=metadata_merge[ids_to_keep,] #subset metadata with ids from data

#add column with month index for August and September
#add month index to dataframe
list_netatmo_month <- lapply(list_netatmo_merge, `[`, 7)
list_netatmo_month=lapply(list_netatmo_month, function(x) strftime(x$Date, "%B", tz="Europe/Berlin"))
list_netatmo_merge <- mapply(cbind, list_netatmo_merge, "Month"=list_netatmo_month, SIMPLIFY=F)

#tidy up environnment
rm(metadata_1, metadata_2, metadata_3, metadata_4, 
   list_netatmo_merge_date, list_netatmo_month, list_netatmo_whole,
   ids_to_keep) 
