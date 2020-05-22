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

names(list_netatmo_merge)
names=c(names(list_netatmo_1), names(list_netatmo_2),names(list_netatmo_3), names(list_netatmo_4))
length(unique(names))

#drop all dataframes with 0 rows
list_netatmo_merge=Filter(function(x) dim(x)[1] > 0, list_netatmo_merge)
length(list_netatmo_merge)

#check if time is consecutive
length(list_netatmo_merge[[1]]$timestamp)
length(unique(list_netatmo_merge[[1]]$timestamp))

#remove all double values (overlapping measurements)
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  data=data[!duplicated(data$timestamp),]
  list_netatmo_merge[[i]]=data
}

#test if it worked
length(unique(list_netatmo_merge[[1]]$timestamp))
length(list_netatmo_merge[[1]]$timestamp)

#plot with no legend (as legend takes most of the screen)
ggplot(bind_rows(list_netatmo_merge, .id="df"), aes(Datetime, temperature, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
  theme(legend.position="none")
ggsave(filename = "overview_netatmo_merge.pdf", width=14, height=7)

#add column with date
list_netatmo_merge_date <- lapply(list_netatmo_merge, `[`, 6)
list_netatmo_merge_date=lapply(list_netatmo_merge_date, function(x) as.Date(x$Datetime))
list_netatmo_merge <- mapply(cbind, list_netatmo_merge, "Date"=list_netatmo_merge_date, SIMPLIFY=F)
return(list_netatmo_merge)
}
#execute function
list_netatmo_merge=merge_netatmo()

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
list_netatmo_month=lapply(list_netatmo_month, function(x) strftime(x$Date, "%B"))
list_netatmo_merge <- mapply(cbind, list_netatmo_merge, "Month"=list_netatmo_month, SIMPLIFY=F)
