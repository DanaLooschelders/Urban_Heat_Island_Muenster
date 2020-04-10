#filter temperature data into day/night
#read in sunrise/sunset data
setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit")
sun=read.table("Sunrise_sunset_times.csv", sep=";", dec=",", header=T, stringsAsFactors = F)
sun$ï..Datum=strptime(sun$ï..Datum, "%d.%m.%Y")
str(sun)

#restructure datframe to paste the date onto the times and convert to POSIXct
sun2=data.frame("sunrise"= as.POSIXct(paste(sun$ï..Datum, sun$Sonnenaufgang)))
sun2$sunset=as.POSIXct(paste(sun$ï..Datum, sun$Sonnenuntergang))
sun2$date=sun$ï..Datum
str(sun2)

plot(sun2$sunrise)
plot(sun2$sunset)

