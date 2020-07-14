#explore the correlation between the median temperature and site specific parameters
library(ggplot2)
library(GGally)
library(Hmisc)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
Standort=read.table(file="Sensortabelle Kartierung Stand 13.7.csv", sep=";", dec=",", header=T)
str(Standort)
names(Standort)[1]="ID"

#use for loop to add median corresponding to ID (take care of replaced loggers/IDS)
for(i in names(list_iButton_corr_tidy)){
  Standort$Temp_median[Standort$ID==as.numeric(i)]=median(list_iButton_corr_tidy[[i]]$Temperature_C_w_off)
}

#explore relationship between aspect ratio and median temp
#plot data with linear regression line
plot(Standort$Temp_median~ Standort$Aspect_ratio)
abline(lm(Standort$Temp_median~ Standort$Aspect_ratio))
#plot data with linear regression line with sd in ggplot (in pretty)
    #use alpha to change opacity of points
    #e.g. alpha=I(1/5) -> total opacity is reaced when 5 points overlap
qplot(Standort$Temp_median, Standort$Aspect_ratio,
      method="lm", geom=c("point", "smooth"))
#plot data with no specified method for trend line
qplot(Standort$Temp_median, Standort$Aspect_ratio,
      geom=c("point", "smooth"))

#calculate pearson correlation
cor(Standort$Temp_median, Standort$Aspect_ratio, use="na.or.complete", method="pearson")
#cacöculate pearson correlation with significance levels
rcorr(Standort$Temp_median, Standort$Aspect_ratio, type = "pearson")

#correlation matrix
ggcorr(Standort, label=TRUE)
Standort$BÃ.ume
#combine multiple variables
ggpairs(Standort, 
        columns=c("Temp_median", "Aspect_ratio", "BÃ.ume"),
        upper=list(continuous = wrap("cor", size=10)),
        lower=list(continuous="smooth"))
