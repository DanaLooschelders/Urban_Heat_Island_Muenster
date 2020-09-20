#QAQC Netatmo (Meier, Fenner et al. 2017 - 
#Crowdsourcing air temperature from citizen science 
#weather stations for urban climate research)
library(ggplot2)
library(ggforce)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Netatmo")
#**************************************************************************
#Data Quality Level A - inconsistent Metadata
#*****************************************************************************

#filter out stations with no lat/lon value
any(is.na(metadata_merge$lon)) #FALSE
any(is.na(metadata_merge$lat)) #FALSE
#Check if stations have identical metadata (remove if >2)
any(duplicated(metadata_merge[2:9])) #FALSE
#filter missing data seperately for August and September
  #more then 10 NAs per day
  #more than 20% per month

#test the foor loop with artificially inserted NA values -> works
#list_netatmo_merge[[3]]$temperature[20:35]=NA

#For August
NA_test=function(month="Juli"){
for (i in names(list_netatmo_merge)){
  data=list_netatmo_merge[[i]] #use only one dataset
  for( x in data$Date[data$Month==month]){ #subset for one day in specified month
    nas=sum(is.na(data$temperature[data$Date==x&data$Month==month])) #count NAs in data
    if( nas >= 10) { #if more than 10 NAs data from that day will be set NA
      data$temperature[data$Date==x&data$Month==month]=NA
    }else{} #otherwise keep data
  }
  nas=sum(is.na(data$temperature[data$Month==month])) #count monthly NA data
  if(nas>=length(data$temperature[data$Month==month])*0.2){ #if more than 20% of monthly data is NA
    data[data$Month==month,]=NA #set data for that month NA
    list_netatmo_merge[[i]]=data
  }else{list_netatmo_merge[[i]]=data} #otherwise keep data
}
}
NA_test(month="Juli")
#NA_test(month="September")
length(list_netatmo_merge) #still same as before (none were removed)
#check how many NAs were added to data
NAs=sapply(list_netatmo_merge, function(x) sum(is.na(x$temperature))) #none
any(NAs>0)
NAs
#****************************************************************************
#Data Quality Level B - identify real outdoor measurements
#****************************************************************************
#Level B part 1 
#*************************************************************************
#five times the sd in TNref (arithmetic mean ofUCON and DWD stations)
#and in SDref.
#calculate daily min air temp and sd 
list_netatmo_level_B=list_netatmo_merge #create output list

#create output dataframe
daily_min_table=data.frame("date"=unique(list_netatmo_merge[[1]]$Date), "daily_min"=rep(NA), "SD"=rep(NA))
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  for (x in data$Date){
    daily_min_table$daily_min[daily_min_table$date==x]=min(data$temperature[data$Date==x], na.rm=T)
    daily_min_table$SD[daily_min_table$date==x]=sd(data$temperature[data$Date==x], na.rm=T)
    }
list_netatmo_level_B[[i]]=daily_min_table
  }

#temp -> DWD reference data
daily_min_ref=data.frame("date"=seq.Date(from=as.Date("2020-07-07", tz="Europe/Berlin"), to=as.Date("2020-07-28", tz="Europe/Berlin"), by=1), "daily_min"=rep(NA), "SD"=rep(NA))

for (x in daily_min_ref$date){
  daily_min_ref$daily_min[daily_min_ref$date==x]=min(temp$TT_TU[as.Date(temp$MESS_DATUM, tz="Europe/Berlin")==x], na.rm=T)
  daily_min_ref$SD[daily_min_ref$date==x]=sd(temp$TT_TU[as.Date(temp$MESS_DATUM, tz="Europe/Berlin")==x], na.rm=T)
}


#scatterplot mean temp vs SD
month="Juli"
list_netatmo_level_B_aug=lapply(list_netatmo_level_B, function(x) subset(x, strftime(x$date, "%B", tz="Europe/Berlin")==month))
#list_netatmo_level_B_sep=lapply(list_netatmo_level_B, function(x) subset(x, strftime(x$date, "%B")=="September"))
#caluculate monthly means for reference data

mean_aug_temp_ref=mean(daily_min_ref$daily_min[strftime(daily_min_ref$date, "%B", tz="Europe/Berlin")==month], na.rm=T)
mean_aug_sd_ref=mean(daily_min_ref$SD[strftime(daily_min_ref$date, "%B", tz="Europe/Berlin")==month], na.rm=T)

sd_aug_temp_ref=sd(daily_min_ref$daily_min[strftime(daily_min_ref$date, "%B", tz="Europe/Berlin")==month], na.rm=T)
sd_aug_sd_ref=sd(daily_min_ref$SD[strftime(daily_min_ref$date, "%B", tz="Europe/Berlin")==month], na.rm=T)

#calculate monthly means for netatmo data
mean.aug=data.frame("ID"=names(list_netatmo_level_B_aug), 
                "mean_min_temp"=sapply(list_netatmo_level_B_aug, function(x) mean(x$daily_min, na.rm=T)),
                "mean_sd"=sapply(list_netatmo_level_B_aug, function(x) mean(x$SD, na.rm=T)))

test=
  ggplot(data=mean.aug, aes(mean_min_temp, mean_sd))+
  geom_point()+ #netatmo mean monthly daily min values
  geom_point(aes(x=mean(mean.aug$mean_min_temp, na.rm=T), y=mean(mean.aug$mean_sd, na.rm=T)), color="green", shape=15)+ #one point for netatmo mean and sd
  #one point for reference data mean and sd point
  geom_point(aes(x=mean_aug_temp_ref, y=mean_aug_sd_ref), color="red", shape=15)+
  #ellipse for 5 times the sd for mean and sd of ref
geom_ellipse(aes(a=sd_aug_sd_ref*5, x0=mean_aug_temp_ref, b=sd_aug_temp_ref*5, y0=mean_aug_sd_ref, angle=0))
#ggsave(filename = paste("Level_B_1_netatmo",month,".pdf"), width=14, height=7)

#how to: geom_ellipse
#x0 -> center coordinate on x axis
#y0 -> center coordinate on y axis
#a -> length of ellipse on y axis
#b -> length of ellipse on x axis
#angle 

#ggplot_built: builds a ggplot for rendering 
#(outputs a list of dataframe s-> one for each layer)
#and a panel object with axis limits/breaks etc
built_whole=ggplot_build(test)
built <- ggplot_build(test)$data
points <- built[[1]] #first list element are the black points

#ell <- built[[4]] #forth list element is the ellipse
ell <- built[[4]][built[[4]]$group == built[[4]]$group[1],] 

dat <- data.frame(
  "ID"=built_whole[["plot"]][["data"]][["ID"]],
  points[1:2], #first two columns are the coordinates
  in.ell = as.logical(point.in.polygon(point.x=points$x, point.y=points$y, pol.x=ell$x, pol.y=ell$y)))


  
#execute function for August and September

#use for loop to exclude station that were flagged as false
for (i in dat$ID){
  if (dat$in.ell[dat$ID==i]==FALSE){
    #remove station from both lists
    list_netatmo_merge[[i]]=NULL
    list_netatmo_level_B[[i]]=NULL
    metadata[metadata_merge$device_id==i,] #delete from metadata
  }else{}
}

#dat2=level_B_1("September")
#use for loop to exclude station that were flagged as false
#for (i in dat2$ID){
#if (dat2$in.ell[dat2$ID==i]==FALSE){
 #   #remove station from both lists
  #  list_netatmo_merge[[i]]=NULL
   # list_netatmo_level_B[[i]]=NULL
    #metadata[metadata_merge$device_id==i,] #delete from metadata
  #}else{}
#}

#*************************************************************************
#level B part 2
#*************************************************************************

#Instructions from paper
#1. compute histograms of TNref and SDref 
  #bin sizes: max and min of TNref and SDref in ellipse
  #bin numbers: 10
#subset table to August values
daily_min_ref_aug=daily_min_ref[strftime(daily_min_ref$date, "%B")=="Juli",]
#hist(daily_min_ref_aug$daily_min, breaks=10) #histogram of TNref
#hist(daily_min_ref_aug$SD, breaks=10) #histogram of SDref

#2.compute relative frequency for every bin combination of histograms of TN/SD (2D)
#create a subset list that includes only August values
list_netatmo_level_B_aug=lapply(list_netatmo_level_B, function(x) subset(x, strftime(x$date, "%B", tz="Europe/Berlin")=="Juli"))
#calculate mean minimal temperature and standard deviation for every netatmo station
mean.aug=data.frame("ID"=names(list_netatmo_level_B_aug), 
                    "mean_min_temp"=sapply(list_netatmo_level_B_aug, function(x) mean(x$daily_min)),
                    "mean_sd"=sapply(list_netatmo_level_B_aug, function(x) mean(x$SD)))

#use ggplot to plot 2D histogramm
n <- ggplot(mean.aug, aes(mean_min_temp, mean_sd)) 
n <- n + stat_bin2d(bins = 10)
n
#get density values from plot
hist=ggplot_build(n)

#3. flag Netatmo stations in 2D bin with frequency >0.001 as TRUE
#briefly check if any station is below 0.01
any(hist$data[[1]]$density<0.001) #FALSE

#ggplot(bind_rows(list_netatmo_merge, .id="df"), aes(Datetime, temperature, colour=df)) +
 # geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
#  theme(legend.position="none")
#ggsave(filename = "overview_netatmo_levelA_B.pdf", width=14, height=7)
