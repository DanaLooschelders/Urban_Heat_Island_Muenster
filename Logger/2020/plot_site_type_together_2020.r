library(tidyr)
library(dplyr)

plot.loggertype<-function(loggertype="WL")
  {
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
names(list_iButton_corr_tidy_type)=paste(desc, " (",names, ")", sep="")

#plot all 
#ggplot(bind_rows(list_iButton_corr_tidy_type, .id="df"), 
#       aes(Datetime.1, Temperature_C_w_off, colour=df)) +
#  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color=loggertype)

#setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/overview_plots/merge")
#save plot
#name=paste("Overview",loggertype, substr(as.character(list_iButton_corr_tidy_type[[1]][1,2]), 
                                 # 1,10),".pdf", sep="_")
#ggsave(filename=name, width = 14, height=7, units = "in")

#create boxplot for Loggertypes
#name2=paste("Boxplot",loggertype, substr(as.character(list_iButton_corr_tidy_type[[1]][1,2]), 
                                       # 1,10),".pdf", sep="_")

#list_for_boxplot <- lapply(list_iButton_corr_tidy_type, `[`, 3)
#dataframe_for_boxplot=do.call(cbind, list_for_boxplot)
#colnames(dataframe_for_boxplot)=names(list_for_boxplot)
#dataframe_for_boxplot=dataframe_for_boxplot%>%
#  pivot_longer(.,cols=colnames(dataframe_for_boxplot), 
#               names_to="Site",values_to="Temperature [°C]")
#ggplot(data=dataframe_for_boxplot, aes(y=`Temperature [°C]`, x=Site))+
#  geom_boxplot(notch = TRUE, na.rm=T, show.legend = T)+
#  stat_summary(fun.y="mean", na.rm=T)+
#  coord_flip()

#ggsave(filename=name2, width = 14, height=7, units = "in")

return(list_iButton_corr_tidy_type)
}

#plot and save for all types
list_iButton_corr_tidy_VL=plot.loggertype("VL")
list_iButton_corr_tidy_WOL=plot.loggertype("WOL")
list_iButton_corr_tidy_SL=plot.loggertype("SL")
list_iButton_corr_tidy_WL=plot.loggertype("WL")

