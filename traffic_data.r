setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/traffic_data/")

#file structure: one file for every day
# headers display: metadata, class, hours: 00:00 to 23:00

# Create vector containing ID #
# We only need the ID # without the first (empty) column, so we set the first column to NULL in colClasses
#--> changed name (to second file from raw data - UHI timeframe)
second_iButton_ID=read.table("0800000051790E21_200817.csv", sep = ",", dec = ".", header = F, skip = 4,
                             nrows = 1, as.is = T, colClasses=c("NULL", NA)) 

#read in files
files_traffic=list.files(pattern =".csv")  

# Loop to read in all files in the list into separate data.frames
for (i in 1:length(files_traffic)) assign(files_traffic[i], read.csv(text=paste0(head(readLines(files_traffic[i]), -1)), 
                                                                                     sep = ";", dec = ".", header = F, skip = 13, 
                                                                                     na.strings = c("<NA>", "NA", "NAN"), 
                                                                                     stringsAsFactors = FALSE))


#remove _24mq.csv ending from filename and keep only date
files_traffic_date = sapply(strsplit(files_traffic, "\\_24mq.csv"), "[", 1)


# Select all csv-files and put them in one list
# By having all dataframes in the same list, you can apply changes to all files simultanuously, e.g. renaming.
list_traffic <- mget(ls(pattern =  ".csv"))

# Assign new file names to every second_iButton-file in list "list_second_iButton"
names(list_traffic) <- files_traffic_date

# Assign the same column names to every second_iButton-file in list "list_second_iButton"
#list_traffic = lapply(list_traffic, setNames, nm = second_iButton_header)

#use only first column
list_traffic_meta <- lapply(list_traffic, `[`, 1)
#remove first 3 characters from first column
list_traffic_meta=lapply(list_traffic_meta, function(x) substr(x, start=7, stop =17))
#split the string in three parts
list_traffic_meta=lapply(list_traffic_meta, function(x) strsplit(x, split="_"))
#write three parts in three seperate columns
list_traffic_meta=lapply(list_traffic_meta, function(x) as.data.frame(do.call(rbind, x)))
#set colnames 
colnames=c("number", "lane", "direction")
list_traffic_meta=lapply(list_traffic_meta, setNames, colnames)
#map both lists together
list_traffic_new=Map(c, list_traffic_meta, list_traffic)

rm(list = as.character(files_traffic)) #remove csv.files from environment

