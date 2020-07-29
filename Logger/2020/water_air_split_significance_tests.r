#This scripts merges all Aasee WOL logger 
#splits the data from all Aasee WOL and Haus Kump VL logger into day and night
#and performs a significance test to compare the two

#merge WOL data from Aasee
Aasee_WOL=metadata$Logger_ID[metadata$Standort!="Kanonengraben"&metadata$Loggertyp=="WOL"]
list_iButton_Aasee_WOL_day=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)%in%Aasee_WOL]
list_iButton_Aasee_WOL_night=list_iButton_corr_tidy_date_night[names(list_iButton_corr_tidy_date_night)%in%Aasee_WOL]

#bind Temp in dataframe
Aasee_WOL_mean_day <-rowMeans(cbind(list_iButton_Aasee_WOL_day[[1]][,3],
                                list_iButton_Aasee_WOL_day[[2]][,3],
                                list_iButton_Aasee_WOL_day[[3]][,3], 
                                na.rm=T))
Aasee_WOL_mean_night <-rowMeans(cbind(list_iButton_Aasee_WOL_night[[1]][,3],
                                    list_iButton_Aasee_WOL_night[[2]][,3],
                                    list_iButton_Aasee_WOL_night[[3]][,3], 
                                    na.rm=T))
#calculate mean vector
Aasee_WOL_mean_day=data.frame("Temperature"=Aasee_WOL_mean_day,"Date"=as.POSIXct(list_iButton_Aasee_WOL_day[[1]][,2]))
Aasee_WOL_mean_night=data.frame("Temperature"=Aasee_WOL_mean_night,"Date"=as.POSIXct(list_iButton_Aasee_WOL_night[[1]][,2]))

#use VL Haus Kump data
Aasee_VL=metadata$Logger_ID[metadata$Standort_ID=="Aasee_3_VL"]
#get data
Aasee_VL_data_day=data.frame(list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)==Aasee_VL])
Aasee_VL_data_night=data.frame(list_iButton_corr_tidy_date_night[names(list_iButton_corr_tidy_date_night)==Aasee_VL])

#test for normality
shapiro.test(Aasee_WOL_mean_day[,1]) #not normal
shapiro.test(Aasee_WOL_mean_night[,1]) #not normal

#use non-parametric test
wilcox.test(Aasee_WOL_mean_day[,1], Aasee_VL_data_day[,1])
#p-value < 2.220446e-16
#significant difference
median(Aasee_WOL_mean_day[,1], na.rm=T) #13.3206912225
median(Aasee_VL_data_day[,1], na.rm=T) #17.5035468076

wilcox.test(Aasee_WOL_mean_night[,1], Aasee_VL_data_night[,1])
#p-value = 3.10259097e-16
#significant difference
median(Aasee_WOL_mean_night[,1], na.rm=T) #11.4934327057
median(Aasee_VL_data_night[,1], na.rm=T) #12.9977325869
