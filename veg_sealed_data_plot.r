#overview plot
#plot all vegetation and sealed loggers
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
des=read.table("Lat_Lon_Logger.csv", sep=";", dec=".", header=T)
des=des[,1:7] #drop column with water depth (for now)
names(des)[1]="Logger_ID"
des=na.omit(des) #drop rows with NA
des$Logger_ID=as.character(des$Logger_ID) #set logger ID as character to match names
str(des)
str(list_iButton_corr_tidy)

#get index of all vegetationloggers in dataset
Vegetation=intersect(des$Logger_ID[des$Place_type=="Vegetation"], names(list_iButton_corr_tidy))
list_iButton_corr_tidy_vegetation=list() #create empty list with water logger
#fill list with water logger data
list_iButton_corr_tidy_vegetation=list_iButton_corr_tidy[names(list_iButton_corr_tidy)%in%Vegetation] 
#rename list
names=names(list_iButton_corr_tidy_vegetation)
desc=des$Place_name[des$Logger_ID%in%names(list_iButton_corr_tidy_vegetation)]
names(list_iButton_corr_tidy_vegetation)=paste(desc, " (",names, ")", sep="")
names(list_iButton_corr_tidy_vegetation) #check
#plot all values as test
ggplot(bind_rows(list_iButton_corr_tidy_vegetation, .id="df"), aes(Datetime.1, Temperature_C_w_off, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Logger in Vegetation') 

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/overview_plots")
#save plot
date=as.character(list_iButton_corr_tidy_vegetation[[1]][1,2])
date=substr(date, 1,10)
name=paste("vegetation_logger", date,".pdf", sep="_")
ggsave(filename=name, width = 14, height=7, units = "in")

#******************************************************************************
#do the same for Loggers in sealed areas
#get index of all sealed loggers in dataset
Sealed=intersect(des$Logger_ID[des$Place_type=="Sealed_area"], names(list_iButton_corr_tidy))
list_iButton_corr_tidy_sealed=list() #create empty list with water logger
#fill list with water logger data
list_iButton_corr_tidy_sealed=list_iButton_corr_tidy[names(list_iButton_corr_tidy)%in%Sealed] 
#rename list
names=names(list_iButton_corr_tidy_sealed)
desc=des$Place_name[des$Logger_ID%in%names(list_iButton_corr_tidy_sealed)]
names(list_iButton_corr_tidy_sealed)=paste(desc, " (",names, ")", sep="")
names(list_iButton_corr_tidy_sealed) #check
#plot all values as test
ggplot(bind_rows(list_iButton_corr_tidy_sealed, .id="df"), aes(Datetime.1, Temperature_C_w_off, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Logger in sealed area') 

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/overview_plots")
#save plot
date=as.character(list_iButton_corr_tidy_sealed[[1]][1,2])
date=substr(date, 1,10)
name=paste("sealed_logger", date,".pdf", sep="_")
ggsave(filename=name, width = 14, height=7, units = "in")
