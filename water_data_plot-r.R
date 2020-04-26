#plot all water logger
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
water=read.table("Water_logger_lat_lon.csv", sep=";", dec=".", header=T)
names(water)[1]="Logger_ID"
str(water)
str(list_iButton_corr_tidy)

#get index of all water loggers in dataset
waterlogger=intersect(water$Logger_ID, names(list_iButton_corr_tidy))
list_iButton_corr_tidy_water=list() #create empty list with water logger
#fill list with water logger data
list_iButton_corr_tidy_water=list_iButton_corr_tidy[names(list_iButton_corr_tidy)%in%waterlogger] 
#rename list
names=names(list_iButton_corr_tidy_water)
desc=water$Location_ID[water$Logger_ID%in%names(list_iButton_corr_tidy_water)]
names(list_iButton_corr_tidy_water)=paste(desc, " (",names, ")", sep="")
names(list_iButton_corr_tidy_water) #check
#plot all values as test
ggplot(bind_rows(list_iButton_corr_tidy_water, .id="df"), aes(Datetime.1, Temperature_C_w_off, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Waterloggers') 

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/overview_plots")
#save plot
date=as.character(list_iButton_corr_tidy_water[[1]][1,2])
date=substr(date, 1,10)
name=paste("water_logger", date,".pdf", sep="_")
ggsave(filename=name, width = 14, height=7, units = "in")


