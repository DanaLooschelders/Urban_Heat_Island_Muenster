#library(rjson)
#library(jsonlite)

#rjson and rjsonlite are probably the better packages but because I'm stupid I used 
#RJSONIO and therefore this code only works with RJSONIO
library(RJSONIO)
#library(curl)

setwd("C:/Users/danan/Desktop")
metadata=fromJSON("data_for_metadata.json", flatten=TRUE)

#extract station id from metadata 
id=rep(NA,length(metadata[["body"]]))
for (i in 1:length(metadata[["body"]])) {
id[i]=metadata[["body"]][[i]]$`_id`
}

#extract station id and place from metadata
metadata_tidy=list()
id=rep(NA,length(metadata[["body"]]))

for (i in 1:length(metadata[["body"]])) {
  #extract metadata
 temp.data=as.data.frame(metadata[["body"]][[i]]$place)
 #extract latlon values
 lonlat=temp.data$location
  #remove redunant first row of dataframe (between the two rows only locations differs)
 if(ncol(temp.data)==6){ #as sometimes the column "street" is missing
 temp.data=temp.data[1,2:6]
 temp.data$lon=lonlat[1] #assign prev extracted longitude value
 temp.data$lat=lonlat[2]#assign prev extracted latitude value
 metadata_tidy[[i]]=temp.data #write dataframe in list
 id[i]=metadata[["body"]][[i]]$`_id`#assign station id to a vector of ids
 names(metadata_tidy)[i]=id[i] #assign station id as name for metadata list
 }else{
   if(is.null(temp.data$street)){
   temp.data=temp.data[1,2:5]
  temp.data$street=NA #create column with street and set to NA
  temp.data$lon=lonlat[1] #assign prev extracted longitude value
  temp.data$lat=lonlat[2]#assign prev extracted latitude value

  metadata_tidy[[i]]=temp.data #write dataframe in list
  id[i]=metadata[["body"]][[i]]$`_id`#assign station id to a vector of ids
  names(metadata_tidy)[i]=id[i] #assign station id as name for metadata list
 }else{print(names(metadata[i]))}
 }
}

setwd("~/Urban_Heat_Island_Muenster")
metadata_table=do.call(rbind.data.frame, metadata_tidy)
metadata_table$id=row.names(metadata_table)
row.names(metadata_table)=NULL
write.table(metadata_table, "metadata_netatmo.csv", sep=";", dec=",", row.names = F)

detach("package:RJSONIO", unload=TRUE)
library(rjson)
library(jsonlite)

metadata2=fromJSON("data_for_metadata.json", flatten=TRUE)
ids=metadata2[["body"]][1]
