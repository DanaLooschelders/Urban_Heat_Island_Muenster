#choose places to map cold air stream from Aasee
library(openair)
library(plotrix)
library(leaflet)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/windstream/merge")
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

str(wind) #use wind from DWD data

#calculate u component
wind$u_comp<-wind$wind_speed*cos(deg2rad(270-wind$wind_direction)) 
# calculate v component
wind$v_comp<-wind$wind_speed*sin(deg2rad(270-wind$wind_direction))

windRose(wind,wd="wind_direction",ws= "wind_speed", 
         paddle=F, 
         annotate=c(paste("wind direction from", 
                          wind$MESS_DATUM[1], "to", 
                          wind$MESS_DATUM[length(wind$MESS_DATUM)])))
#create segments for wind direction
range<-c(0,rad2deg(seq(0,2*pi,by=pi/8)+pi/16))
#set last sequemnt to 360
range[length(range)]<-360
#cut the wind directions in segments
cuts<-cut(wind$wind_direction, breaks=range, labels=c("N1", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO","S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N2"))
#rename levels
levels(cuts)<- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW","N")
#get table with absolute frequencies
abs_freq<-table(cuts)
#calculate relative freqencies
rel_freq<-abs_freq/sum(abs_freq)
#create parameter for positions of labels
deg<-c(rad2deg(seq(0,2*pi-pi/8,by=pi/8)))
#create labels
directions=c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

# relative frequency plot
pdf(file="wind_dir_mean.pdf")
polar.plot(as.numeric(rel_freq),polar.pos=deg,
           main="mean wind direction at \n FMO in July 2020", 
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=deg,labels=directions, line.col="black",
           radial.lim=c(0,max(rel_freq)),
           boxed.radial=TRUE,)
dev.off()

