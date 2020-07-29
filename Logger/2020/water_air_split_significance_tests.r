#merge WOL data from Aasee
Aasee_WOL=metadata$Logger_ID[metadata$Standort!="Kanonengraben"&metadata$Loggertyp=="WOL"]
list_iButton_Aasee_WOL=list_iButton_corr_tidy[names(list_iButton_corr_tidy)%in%Aasee_WOL]

mean_Aasee_WOL=sapply(list_iButton_Aasee_WOL, function(x) mean(x[,3], na.rm=T))

#use VL Haus Kump data

