#iButtons tidy up - correct offset and tidy up temperature spikes 
#for 02.08.2019
#and for 01.09.2019
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")

str(list_iButton_corr_set) #the start-time corrected data (check that date corresponds)
#create temporary list
list_iButton_corr_temp=list_iButton_corr_set
#read in offset values
offset_stats=read.table("iButton_Statistics.csv", sep=",", dec=".", header=T)
str(offset_stats)

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

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit")
#read in csv with lat lon and description of places
des=read.table("Lat_Lon_Logger.csv", sep=";", dec=",", header=T)
names(des)[1]="Logger_ID"
des=des[,1:7] #drop column with water depth (for now)
des=na.omit(des) #drop rows with NA
des$Logger_ID=as.character(des$Logger_ID) #set logger ID as character to match names
des$place_ID=paste(des$Place_name,des$Place_number, sep="_")
str(des)
ID=unique(des$place_ID)

#create a metadata table for logger
metadata=data.frame("ID"=as.integer(names(list_iButton_corr_tidy)))
metadata=na.omit(metadata)
metadata$PlaceID=rep(NA)
metadata$type=rep(NA)
for(i in metadata$ID){
  if(any(des$Logger_ID==i)) {
    metadata$PlaceID[metadata$ID==i]=des$place_ID[des$Logger_ID==i]
    metadata$type[metadata$ID==i]=as.character(des$Place_type[des$Logger_ID==i])
  }else{}
}
metadata[is.na(metadata$PlaceID),2:3]="unknown"
metadata$color=rep(NA)

#set column with color for plots
metadata$color[metadata$type=="Water"]="blue"
metadata$color[metadata$type=="Sealed_area"]="darkgrey"
metadata$color[metadata$type=="Vegetation"]="green"

#loop through data and correct temperature spikes
for (i in 1:length(list_iButton_corr_tidy)) {
  name=names(list_iButton_corr_tidy[i])
if (metadata$type[metadata$ID==name]=="Water") {
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
} else { #for air temperature (sealed area and vegetation)
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

