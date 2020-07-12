setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
Standort=read.table(file="Sensortabelle_Kartierung_R.csv", sep=";", dec=".", header=T)
str(Standort)
names(Standort)[1]="ID"

#add median to table 
Standort$Temp_median=rep(NA)

#use for loop to add median corresponding to ID (take care of replaced loggers/IDS)
for(i in names(list_iButton_corr_tidy)){
  Standort$median[Standort$ID==as.numeric(i)]=median(list_iButton_corr_tidy[[i]]$Temperature_C_w_off)
}

lm(Standort$median~ Standort$Vollversiegelung..Asphalt..Beton.)
