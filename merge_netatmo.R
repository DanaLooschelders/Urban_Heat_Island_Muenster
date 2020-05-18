setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo/")
#rowbind Netatmo data to form consecutive timeline
length(list_netatmo_1) #from 01.08.2019
length(list_netatmo_2) #from 20.08.2019
length(list_netatmo_3) #from 05.09.2019

#create new list
list_netatmo_merge=list_netatmo_1
#Map list 1 and list 2 together
list_netatmo_merge[names(list_netatmo_2)] <- Map(rbind, list_netatmo_merge[names(list_netatmo_2)], list_netatmo_2)
#Map 1 (merged with 2) and 3 together
list_netatmo_merge[names(list_netatmo_3)] <- Map(rbind, list_netatmo_merge[names(list_netatmo_3)], list_netatmo_3)

names(list_netatmo_merge)
names=c(names(list_netatmo_1), names(list_netatmo_2),names(list_netatmo_3))
length(unique(names))

#drop all dataframes with 0 rows
list_netatmo_merge=Filter(function(x) dim(x)[1] > 0, list_netatmo_merge)
length(list_netatmo_merge)

#check if time is consecutive
length(list_netatmo_merge[[1]]$timestamp)
length(unique(list_netatmo_merge[[1]]$timestamp))

#remove all double values (overlapping measurements)
i=1
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
