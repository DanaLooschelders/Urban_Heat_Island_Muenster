#iButtons QAQC 
#this script: 1. corrects offset and tidies up 2. temperature spikes and 3. outliers
#for 02.08.2019
#and for 01.09.2019
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/logger_data")
library(NCmisc) #for outlier detection

str(list_iButton_corr_set) #the start-time corrected data (check that date corresponds)
#create temporary list
list_iButton_corr_temp=list_iButton_corr_set
#read in offset values
offset_stats=read.table("iButton_Statistics.csv", sep=",", dec=".", header=T)
str(offset_stats)

#1. correct the offset for every logger (offset from lab test)
for (i in names(list_iButton_corr_set)){
  if(any(offset_stats$ID_iButton==i)){
  off_value=offset_stats$Diff_T[offset_stats$ID_iButton==i]
  data=list_iButton_corr_set[[i]]
  data$Temperature_C_w_off=data$Temperature_C+off_value
  list_iButton_corr_set[[i]]=data
  } else{
    data=list_iButton_corr_set[[i]]
    data$Temperature_C_miss_off=data$Temperature_C
    list_iButton_corr_set[[i]]=data
  }
}


#for the 02.08.2019
#list_iButton_corr_tidy[["33"]] #somehow the offset for this one is missing

#iterate through all dataframes on list
#iterate trough all temperature values 
#set threshold value corresponding to literature regarding spikes
#if difference between two values is larger then threshold -> set to NA for a defined timespan
#assign description for plots

list_iButton_corr_tidy=list_iButton_corr_set #create new, tidy list
str(list_iButton_corr)
report.na=rep(NA, length(list_iButton_corr_tidy))

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")
#read in csv with lat lon and description of places
des=read.table("Sensortabelle Kartierung Stand 17.7.csv", 
               sep=";", dec=",",skip=2, header=F)
#read in again only for column names
meta.des=read.table("Sensortabelle Kartierung Stand 17.7.csv", 
               sep=";", dec=",", header=T)
#save column names in vector
col.names=names(meta.des)
#set column names to table
names(des)=col.names
#check
str(des)
#drop unneccessary vectors
rm(col.names, meta.des)

names(des)[1]="Standort_ID" #change first column name

#create a metadata table for logger
#set ID as first column
metadata=data.frame("Logger_ID"=as.integer(names(list_iButton_corr_tidy)))

#get metadata from loggers actually used (by names of loggers in list)
metadata=merge(metadata, des,by = "Logger_ID")

metadata$color=rep(NA) #add coloumn to display color

#set column with color for plots
metadata$color[metadata$Loggertyp=="WL"]="darkblue"
metadata$color[metadata$Loggertyp=="SL"]="darkgrey"
metadata$color[metadata$Loggertyp=="VL"]="green"
metadata$color[metadata$Loggertyp=="WOL"]="lightblue"

#2. loop through data and correct temperature spikes
for (i in 1:length(list_iButton_corr_tidy)) {
  name=names(list_iButton_corr_tidy[i])
if (metadata$Loggertyp[metadata$Logger_ID==name]=="WL") {
test=list_iButton_corr_tidy[[i]]
test$diff=rep(NA)
test$diff[1:length(test[,3])-1]=diff(test[,3])
spike=which(test$diff>=2.5) #3°C/10min as threshold after spike
for (x in spike) {
  test[x:(x+18),3]=NA #two hours of data after spike is removed
}
test[,3][test$diff<= -5]=NA
list_iButton_corr_tidy[[i]]=test
#names(list_iButton_corr_tidy[])=name_save
report.na[i]=sum(is.na(test[,3]))
} else { #for air temperature (sealed area and vegetation and water surface air temperature)
  test=list_iButton_corr_tidy[[i]]
  test$diff=rep(NA)
  test$diff[1:length(test[,3])-1]=diff(test[,3])
  spike=which(test$diff>=5) #5°C/10min as threshold for spike
  for (x in spike) { #half an hour of data after spike is removed
    test[x:(x+3),4]=NA
  }
  test[,3][test$diff<= -5]=NA
  list_iButton_corr_tidy[[i]]=test
  #names(list_iButton_corr_tidy[])=name_save
  report.na[i]=sum(is.na(test[,3]))
}
}

report.na #check how many NAs were in data

#3. correct outliers (outside more than 2.5 IQR)

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/stats/hist")
for (i in 1:length(list_iButton_corr_tidy)){
data=list_iButton_corr_tidy[[i]]
pdf(file = paste("hist",
                 substring(list_iButton_corr_tidy[[i]]$Datetime.1[1],1, 10),
                 names(list_iButton_corr_tidy[i]), ".pdf"))
hist(data[,3], main=paste("Histogramm of",substring(list_iButton_corr_tidy[[i]]$Datetime.1[1],1, 10),
                          names(list_iButton_corr_tidy[i])), xlab="Temperature [°C]")
dev.off()
 data[which.outlier(data[,3], 
                    method="iq", thr=2.5, 
                    high=TRUE, low=TRUE),3]=NA
  list_iButton_corr_tidy[[i]][,3]=data[,3]
  }

