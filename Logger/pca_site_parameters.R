library(ggplot2)
library(FactoInvestigate)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(missMDA)
library(corrplot)
library(RColorBrewer)

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

#set ID as rowname
rownames(Standort)=Standort$ID
#remove unit from values
Standort$Hoehe..m.=substring(Standort$Hoehe..m.,1,4)
#use only some columns
Standort=Standort[,-c(1:3,5,9:10,)]
#remember: eigenvalue -> amount of variance retained by each principal component
PCA(Standort[], )