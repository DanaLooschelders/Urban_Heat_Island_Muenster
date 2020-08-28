setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/traffic_data_july/")
library(tidyr)
library(ggplot2)
#file structure: one file for every day
# headers display: metadata, class, hours: 00:00 to 23:00
#read in files
files_traffic=list.files(pattern =".csv")  

# Loop to read in all files in the list into separate data.frames
for (i in 1:length(files_traffic)) assign(files_traffic[i], read.csv(text=paste0(head(readLines(files_traffic[i]), -1)), 
                                                                                     sep = ";", dec = ".", header = F, skip = 13, 
                                                                                     na.strings = c("<NA>", "NA", "NAN"), 
                                                                                     stringsAsFactors = FALSE))

#remove _24mq.csv ending from filename and keep only date
files_traffic_date = sapply(strsplit(files_traffic, "\\_24mq.csv"), "[", 1)

# all in list
list_traffic <- mget(ls(pattern =  ".csv"))

#  new file names
names(list_traffic) <- files_traffic_date

rm(list = as.character(files_traffic)) #remove csv.files from environment

#use only first column
list_traffic_meta <- lapply(list_traffic, `[`, 1)

#remove first 3 characters from first column
list_traffic_meta=lapply(list_traffic_meta, function(x) substr(x$V1, start=4, stop =14))
#map list together 
list_traffic_new <- mapply(cbind, "meta"=list_traffic_meta, list_traffic, SIMPLIFY=F)
#split the string in three parts
list_traffic_new=lapply(list_traffic_new, 
                         function(x) separate(data=x, col=meta, 
                                              remove=T, sep="_",
                                              into=c("number", "lane", "direction"),
                                              convert=T))
#keep only knot with number 01140
for(i in 1:length(list_traffic_new)){
  dat=list_traffic_new[[i]]
  dat=dat[dat$number=="1140",]
  list_traffic_new[[i]]=dat
}


#add number of cars on lanes: 
  #all FV31 (coming from south) --> rm none
  #FV 4 R (turning right from Ardenauerallee) --> rm FV 4 G and FV 4 L
  #FV2 L (turning left from Aegidiistraße) (FV 22 on map) --> rm FV 2 R and FV 2 G
  #FV1 G (coming from north) (FV 11 on map) --> rm FV 1 L and FV 2 R

for(i in 1:length(list_traffic_new)){
  dat=list_traffic_new[[i]]
  #remove lanes not needed
  dat=dat[!(dat$lane=="FV4"&dat$direction=="G"),]
  dat=dat[!(dat$lane=="FV4"&dat$direction=="L"),]
  dat=dat[!(dat$lane=="FV2"&dat$direction=="R"),]
  dat=dat[!(dat$lane=="FV2"&dat$direction=="G"),]
  dat=dat[!(dat$lane=="FV1"&dat$direction=="L"),]
  dat=dat[!(dat$lane=="FV1"&dat$direction=="R"),]
  #get new row with colSums
  dat[7,]=rep(NA)
  dat[7,6:29]=colSums(dat[,6:29], na.rm=T)
  dat=dat[7,6:29] #remove unneccessary columns and rows (keep only sums)
  list_traffic_new[[i]]=dat
}
#transpose data to order values in rows
list_traffic_new2=lapply(list_traffic_new, function(x) as.data.frame(t(x)))

#create dataframe
traffic=data.frame("datetime"=seq(as.POSIXct("2020-07-01 00:00:00"),
                                  as.POSIXct("2020-07-31 23:00:00"), 
                                  by="hour"),
                   "cars"=cbind(unlist(list_traffic_new2)))
rownames(traffic)=NULL

ggplot()+
geom_line(aes(traffic$datetime, traffic$cars))+
  xlab("Date")+
  ylab("Number of cars")+
  ggtitle("Numbers of cars passing the Aasee")
