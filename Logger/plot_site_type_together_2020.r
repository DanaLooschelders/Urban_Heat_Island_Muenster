plot.loggertype<-function(loggertype="WL")
  {
  loggertype="SL"
  #plot all loggers of same type
type=metadata[metadata$Loggertyp==loggertype,]
#get index of all loggertypes in dataset
logger=intersect(type$Logger_ID, names(list_iButton_corr_tidy))
list_iButton_corr_tidy_type=list() #create empty list with type of logger
#fill list with type logger data
list_iButton_corr_tidy_type=list_iButton_corr_tidy[names(list_iButton_corr_tidy)%in%logger] 
#rename list
names=names(list_iButton_corr_tidy_type)
desc=type$Standort[type$Logger_ID%in%names(list_iButton_corr_tidy_type)]
desc
names(list_iButton_corr_tidy_type)=paste(desc, " (",names, ")", sep="")
names(list_iButton_corr_tidy_type) #check
#plot all 
ggplot(bind_rows(list_iButton_corr_tidy_type, .id="df"), 
       aes(Datetime.1, Temperature_C_w_off, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color=loggertype)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/overview_plots")
#save plot
name=paste("Overview",loggertype, substr(as.character(list_iButton_corr_tidy_type[[1]][1,2]), 
                                  1,10),".pdf", sep="_")
ggsave(filename=name, width = 14, height=7, units = "in")
}

plot.loggertype("VL")
plot.loggertype("WOL")
plot.loggertype("SL")
plot.loggertype("WL")
