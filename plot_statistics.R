#plot statistics (mean, median, sd)
library(grid)
library(gridExtra)
#plot 24 h stats
#overview plots
ggplot(bind_rows(list_iButton_24h_mms, .id="df"), aes(date, mean, colour=df)) +
  geom_line()+theme_bw()+ylab("mean Temperature [°C]")+xlab("Date")+ labs(color='mean values') 

ggplot(bind_rows(list_iButton_24h_mms, .id="df"), aes(date, median, colour=df)) +
  geom_line()+theme_bw()+ylab("median Temperature [°C]")+xlab("Date")+ labs(color='median values') 

ggplot(bind_rows(list_iButton_24h_mms, .id="df"), aes(date, sd, colour=df)) +
  geom_line()+theme_bw()+ylab("sd of Temperature [°C]")+xlab("Date")+ labs(color='standard deviation values') 

#plot every logger with mean and sd
ggplot(list_iButton_24h_mms[[1]], aes(date, mean, colour="24h mean"), colour="black")+
    geom_line()+
  geom_line(data = list_iButton_day_mms[[1]], aes(date, mean, colour="daytime mean"))+
  geom_line(data = list_iButton_night_mms[[1]], aes(date, mean,colour="nighttime mean"))+
  geom_line(data=list_iButton_corr_tidy_date[[1]], aes(Date, Temperature_C_w_off, colour="daily range"))+
  labs(title=paste("mean temperature of logger", names(list_iButton_24h_mms)[1]),
       x="Date", y="Temperature [°C]")+
  theme_bw()+
  geom_ribbon(aes(ymax=mean + sd, ymin=mean-sd, fill="standard deviation"), alpha=0.2, colour=NA)+
    scale_fill_manual("", values="grey12")+
    scale_colour_manual("", values=c("black", "darkgrey", "orange", "blue"))

#gridded plot for stats and data
stats_plot=ggplot(list_iButton_24h_mms[[1]], aes(date, mean))+ #daily mean
  geom_line()+
  geom_line(data = list_iButton_day_mms[[1]], aes(date, mean))+ #daytime mean
  geom_line(data = list_iButton_night_mms[[1]], aes(date))+ #nighttime mean
  labs(title=paste("mean temperature of logger", names(list_iButton_24h_mms)[1]),
       x="Date", y="Temperature [°C]")+
  theme_bw()+
  geom_ribbon(aes(ymax=mean + sd, ymin=mean-sd), alpha=0.2, colour=NA) #standard deviation

data_plot=ggplot(data=list_iButton_corr_tidy_date[[1]], aes(Datetime.1, Temperature_C_w_off))+
  geom_line() #plot corrected logger data

grid.arrange(stats_plot, data_plot, ncol=1) #plot both graphs vertically stacked
