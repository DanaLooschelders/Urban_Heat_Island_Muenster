#Data Quality Level C - filter systematic/single radiative errors
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
rad=read.table("GeoDach2019.csv", sep=";", dec=",", header=T)
str(rad)
#Data Quality Level D -  outliers
