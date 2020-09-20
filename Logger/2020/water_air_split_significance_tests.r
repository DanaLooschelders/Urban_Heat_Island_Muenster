#This scripts merges all Aasee WOL logger 
#splits the data from all Aasee WOL and Haus Kump VL logger into day and night
#and performs a significance test to compare the two

#merge WOL data from Aasee
Aasee_WOL=metadata$Logger_ID[metadata$Standort!="Kanonengraben"&metadata$Loggertyp=="WOL"]
list_iButton_Aasee_WOL_day=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)%in%Aasee_WOL]
list_iButton_Aasee_WOL_night=list_iButton_corr_tidy_date_night[names(list_iButton_corr_tidy_date_night)%in%Aasee_WOL]

#bind Temp in dataframe
Aasee_WOL_mean_day=median(c(list_iButton_Aasee_WOL_day[[1]][,3],
  list_iButton_Aasee_WOL_day[[2]][,3],
  list_iButton_Aasee_WOL_day[[3]][,3]), 
  na.rm=T)
Aasee_WOL_mean_night <-median(c(list_iButton_Aasee_WOL_night[[1]][,3],
                                    list_iButton_Aasee_WOL_night[[2]][,3],
                                    list_iButton_Aasee_WOL_night[[3]][,3]), 
                                    na.rm=T)
Aasee_WOL_day=c(list_iButton_Aasee_WOL_night[[1]][,3],
  list_iButton_Aasee_WOL_night[[2]][,3],
  list_iButton_Aasee_WOL_night[[3]][,3])
Aasee_WOL_night=c(list_iButton_Aasee_WOL_night[[1]][,3],
                 list_iButton_Aasee_WOL_night[[2]][,3],
                 list_iButton_Aasee_WOL_night[[3]][,3])
#use VL Haus Kump data
Aasee_VL=metadata$Logger_ID[metadata$Standort_ID=="Aasee_3_VL"]
#get data
Aasee_VL_data_day=data.frame(list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)==Aasee_VL])
Aasee_VL_data_night=data.frame(list_iButton_corr_tidy_date_night[names(list_iButton_corr_tidy_date_night)==Aasee_VL])

#test for normality
shapiro.test(Aasee_WOL_day) #not normal
shapiro.test(Aasee_WOL_night) #not normal

#day
median(Aasee_VL_data_day[,1], na.rm=T) 
Aasee_WOL_mean_day
Aasee_wil_day=wilcox.test(Aasee_WOL_day, Aasee_VL_data_day[,1])
#night
Aasee_WOL_mean_night
median(Aasee_VL_data_night[,1], na.rm=T) 
Aasee_wil_night=wilcox.test(Aasee_WOL_night, Aasee_VL_data_night[,1])

#compare Muehlenhof WL to WOL
#WOL
M_VL_day=list_iButton_corr_tidy_date_day[["39"]]
M_VL_night=list_iButton_corr_tidy_date_night[["39"]]

median(M_VL_day[,3], na.rm=T)
median(M_VL_night[,3], na.rm=T)


