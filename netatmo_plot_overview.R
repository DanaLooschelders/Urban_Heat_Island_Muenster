#plot Netatmo overview
library(ggplot2)
library(dplyr)
ggplot(bind_rows(list_netatmo_level_B, .id="df"), aes(date, daily_min, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
  theme(legend.position="none")+
  geom_line(data=daily_min_ref, aes(x = daily_min_ref$date, y= daily_min_ref$daily_min), colour="black")+
  geom_ribbon(data = daily_min_ref, aes(ymin=daily_min, ymax=daily_min+SD*3, colour="grey"),fill="grey")

ggsave(filename = "overview_netatmo_daily_min.pdf", width=14, height=7)
#plot only August data
list_netatmo_merge_aug=lapply(list_netatmo_merge, function(x) subset(x, Month=="August"))

#plot
ggplot(bind_rows(list_netatmo_merge_aug, .id="df"), aes(Datetime, temperature, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("")+ labs(color='Netatmo devices in MS')+
  theme(legend.position="none")

ggsave(filename = "overview_netatmo_August.pdf", width=14, height=7)

#plot only September data
list_netatmo_merge_sep=lapply(list_netatmo_merge, function(x) subset(x, x$Month=="September"))
#plot
ggplot(bind_rows(list_netatmo_merge_sep, .id="df"), aes(Datetime, temperature, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("")+ labs(color='Netatmo devices in MS')+
  theme(legend.position="none")

ggsave(filename = "overview_netatmo_September.pdf", width=14, height=7)


