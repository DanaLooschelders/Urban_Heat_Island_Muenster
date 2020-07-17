# R-Script for QAQC check, calculation of statistics and plotting of Thermochron iButton temperature loggers
# Date: 2019/07/05
# Author: Laura Ehrnsperger

# Sensor list:
# - DS1921-F5 and MF1921G Thermochron iButton 1-wire digital thermometer, Maxim Integrated (ID: 1-110)
# - HC2-S3 Thermometers, Campbell Scientific

# Changelog:
# 2019/07/05: Creation of script: Reading in csv-files, calculation of mean and standard deviation, plotting of single file
# 2019/07/07: Convertion of timestamp to POSIXct format for both single and list of files
# 2019/07/08: Splitting list for calculation, create statistics data table
# 2019/07/10: Extracting the individual ID of each iButton and assign them to the data file names
# 2019/07/16: Creating index to subset by timestamp
# 2019/07/18: Reading in data of reference thermometers, plotting time series, calculating means of reference temperature
# 2019/07/24: Calculation of offset for each iButton, adding extra column to statistics table, export of statistics table
# 2019/07/25: Plotting of offsets, 


# --------------------------------------------------------------------------------------------------------------------------------
#### Global options ####

# Empty console
cat("\014")

# Empty workspace
# Uncomment if necessary
rm(list = ls())

# Adapt R-settings to display of more digits
options(digits.secs=3)
options(digits=12)

# --------------------------------------------------------------------------------------------------------------------------------
### Predefine functions for later use ###
###
# Function to check if a package is already installed
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# --------------------------------------------------------------------------------------------------------------------------------
# Load required packages
# Packages for graphics in R
usePackage("ggplot2")
# Packages to order data
usePackage("magrittr")
usePackage("plyr")
usePackage("dplyr")
usePackage("reshape2")   
usePackage("stringr")
usePackage("tidyverse")


# --------------------------------------------------------------------------------------------------------------------------------
### Read in data ###

# Set Working directory (wd)
# setwd(choose.dir()) # Uncomment if necessary, but only works for Windows operating system
#setwd("V:/klima/Projekte/2019_Urban_Heat_Island/Data/Data_raw/Calibration_test_20190704-20190708")
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/logger_data/UHI_20200703-20200717/")
#the logger IDs 56 and 01 were added manually to the .csv files as they were missing in the original file

#################
# Data iButtons #
#################
# Two different ways of reading in data:
# 1. Read in a single file: If you only need to control a single iButton etc. use this code
# 2. Read in several files that are in the working directory: 
#    If you want to look at multiple iButtons, apply statistics to all data etc. use this code

###
# Column names for iButton data
# 1. Select header from one of the iButton-Files 
# UNCOMMENT IF NEEDED
#iButton_header=read.table("3F000000517B3D21_190708.csv", sep = ",", dec = ".", header = F, skip = 7,
#                          nrows = 1, as.is = T) 

# Customized header
# Create new header as the default one from the files isn't really pretty
iButton_header = c("Datetime", "Temperature_C")

# Create vector containing ID #
# We only need the ID # without the first (empty) column, so we set the first column to NULL in colClasses
#--> changed name (to second file from raw data - UHI timeframe)
iButton_ID=read.table("0800000051790E21_200717.csv", sep = ",", dec = ".", header = F, skip = 4,
                      nrows = 1, as.is = T, colClasses=c("NULL", NA)) 


# 1. Read in a single file 
# Adjust file name to the exact file you want to read in!
# read.csv automatically assumes a comma as separator, which is fine for us.
# The last row contains an end character. This causes trouble while reading in, therefore we skip the last line of each file.
# We also skip the first 8 lines, which only contain meta data on the specific iButton. The customized header is added 
# afterwards with colnames().

#d_iButton_single = read.csv(text=paste0(head(readLines("0D000000519CE121_190814.csv"), -1)), dec = ".", skip = 8, header = F, 
#                           na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE, encoding = 'UTF-8')

# Add correct column names to dataframe
#colnames(d_iButton_single) <- iButton_header

#---> check data
#str(d_iButton_single)
# Transform date time column from text to POSIXct format
#outcommented ---> d_iButton_single$Datetime = as.POSIXct(d_iButton_single$Datetime, format = "%d-%m-%Y %H:%M")
#---> shouldn't it be: "%Y-%m-%d %H:%M"? changed in next line
#d_iButton_single$Datetime = as.POSIXct(d_iButton_single$Datetime, format = "%Y-%m-%d %H:%M")

###
# 2. Read in several files into separate dataframes
# List all files you want to read in by choosing them by name/file type, etc. with "pattern".
# Here every file, which ends with ".csv" is chosen from the current wd.
files_iButtons=list.files(pattern =".csv")  


# Loop to read in all files in the list into separate data.frames
for (i in 1:length(files_iButtons)) assign(files_iButtons[i], read.csv(text=paste0(head(readLines(files_iButtons[i]), -1)), 
                                                                       sep = ",", dec = ".", header = F, skip = 8, 
                                                                       na.strings = c("<NA>", "NA", "NAN"), 
                                                                       stringsAsFactors = FALSE))


### 
# Read in ID from all files from the list and bind them together
# CAUTION: The files have to have the SAME number of columns!
for(i in files_iButtons) {
  if(!exists("iButton_ID_multi")) {
    iButton_ID_multi=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                nrows = 1, as.is = T, colClasses=c("NULL", NA))
  } else {
    temp_iButton_ID_multi=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                     nrows = 1, as.is = T, colClasses=c("NULL", NA))
    iButton_ID_multi=rbind(iButton_ID_multi, temp_iButton_ID_multi)
    remove(temp_iButton_ID_multi)
  }
}

# To check if all iButtons we want are read in, we sort ID # ascending and then display all ID # that occur
iButton_ID_sort = sort(iButton_ID_multi$V2)
iButton_ID_sort

# Create new names for the data files out of ID, serial number (SR) and date of data collection
# First get rid of the file ending .csv in the file name
iButton_SR_date = sapply(strsplit(files_iButtons, "\\."), "[", 1)

# Paste the ID vector with the names derived from the files_iButton list
list_header <- paste(iButton_ID_multi$V2)
list_header
# Select all csv-files and put them in one list
# By having all dataframes in the same list, you can apply changes to all files simultanuously, e.g. renaming.
list_iButton <- mget(ls(pattern =  ".csv"))

# Assign new file names to every iButton-file in list "list_iButton"
names(list_iButton) <- list_header

# Assign the same column names to every iButton-file in list "list_iButton"
list_iButton = lapply(list_iButton, setNames, nm = iButton_header)


# --------------------------------------------------------------------------------------------------------------------------------
# Add timestamp to all iButton-files
# First select only the first column (=timestamp of our data) of each file in the list
list_iButton_datetime <- lapply(list_iButton, `[`, 1)

# Transform timestamp from character to POSIXct format for each file in the sublist list_iButton_datetime
# We get a sublist only containing POSIXct datetime format for each iButton
# outcommented ---> list_iButton_datetime <- lapply(list_iButton_datetime, function(x) as.POSIXct(x$Datetime,format = "%d-%m-%Y %H:%M"))
#---> shouldn't it be: "%Y-%m-%d %H:%M"? changed in next line
list_iButton_datetime <- lapply(list_iButton_datetime, function(x) as.POSIXct(x$Datetime,format = "%Y-%m-%d %H:%M"))


# Add the new transformed timestamp to the original list
# There are two options, uncomment the one you need.
# 1. Add POSIXct timestamp as additional column
list_iButton <- mapply(cbind, list_iButton, "Datetime"=list_iButton_datetime, SIMPLIFY=F)

# 2. Replace the old timestamp with POSIXct timestamp
#list_iButton = map2(list_iButton, list_iButton_datetime, ~ mutate(., Datetime = .y)) 


# --------------------------------------------------------------------------------------------------------------------------------
# Subset data by choosing start and end date of the desired time period
# Unnecessary data can be removed, e.g. data collected before the labtest started.
# For example the data collected during the setting up of iButtons in my office. It contains rather high values of
# about 27 °C, which should not be included in the statistics.
# If no subset should be supplied, just work with the original data table or enter the dates of the entire period.

# Time period for Labtest
#--> outcommented start_Labtest <- strptime("2019-07-04 16:00:00", "%Y-%m-%d %H:%M:%S")
#---> outcommented end_Labtest <- strptime("2019-07-08 13:55:00", "%Y-%m-%d %H:%M:%S")

#---> chose own time period
range(list_iButton_datetime[[1]])
start_time=strptime("2020-07-07 00:00:00", "%Y-%m-%d %H:%M:%S")
end_time=strptime("2020-07-17 00:00:00", "%Y-%m-%d %H:%M:%S")

# Apply the time index on the single data table
# ---> outcommented d_iButton_single_corr <- subset(d_iButton_single, Datetime >= start_Labtest & Datetime <= end_Labtest)
#---> changed for own time span
#d_iButton_single_corr <- subset(d_iButton_single, Datetime >= start_time & Datetime <= end_time)

# Apply the time index on each data table in the list "list_iButton"
# So for each data table the same time period is selected
#---> outcommented list_iButton_corr = lapply(list_iButton, function(x) {subset(x, x[,1] >= start_Labtest & x[,1] <= end_Labtest)})
#---> changed for own time span
list_iButton_corr = lapply(list_iButton, function(x) {subset(x, x[,1] >= start_time & x[,1] <= end_time)})

