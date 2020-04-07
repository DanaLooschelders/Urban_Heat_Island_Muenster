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
setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit/UHI_Muenster_Data/dat_02.08")
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
iButton_ID=read.table("0800000051698721_190820.csv", sep = ",", dec = ".", header = F, skip = 4,
                          nrows = 1, as.is = T, colClasses=c("NULL", NA)) 
                          

# 1. Read in a single file 
# Adjust file name to the exact file you want to read in!
# read.csv automatically assumes a comma as separator, which is fine for us.
# The last row contains an end character. This causes trouble while reading in, therefore we skip the last line of each file.
# We also skip the first 8 lines, which only contain meta data on the specific iButton. The customized header is added 
# afterwards with colnames().
d_iButton_single = read.csv(text=paste0(head(readLines("0D000000519CE121_190814.csv"), -1)), dec = ".", skip = 8, header = F, 
                            na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE, encoding = 'UTF-8')

# Add correct column names to dataframe
colnames(d_iButton_single) <- iButton_header

#---> check data
str(d_iButton_single)
# Transform date time column from text to POSIXct format
#outcommented ---> d_iButton_single$Datetime = as.POSIXct(d_iButton_single$Datetime, format = "%d-%m-%Y %H:%M")
#---> shouldn't it be: "%Y-%m-%d %H:%M"? changed in next line
d_iButton_single$Datetime = as.POSIXct(d_iButton_single$Datetime, format = "%Y-%m-%d %H:%M")

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
#--> some IDs are double?

# Create new names for the data files out of ID, serial number (SR) and date of data collection
# First get rid of the file ending .csv in the file name
iButton_SR_date = sapply(strsplit(files_iButtons, "\\."), "[", 1)

# Paste the ID vector with the names derived from the files_iButton list
list_header <- paste(iButton_ID_multi$V2, iButton_SR_date, sep = "_")


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
start_time=strptime("2019-08-03 00:00:00", "%Y-%m-%d %H:%M:%S")
end_time=strptime("2019-08-13 00:00:00", "%Y-%m-%d %H:%M:%S")

# Apply the time index on the single data table
# ---> outcommented d_iButton_single_corr <- subset(d_iButton_single, Datetime >= start_Labtest & Datetime <= end_Labtest)
#---> changed for own time span
d_iButton_single_corr <- subset(d_iButton_single, Datetime >= start_time & Datetime <= end_time)

# Apply the time index on each data table in the list "list_iButton"
# So for each data table the same time period is selected
#---> outcommented list_iButton_corr = lapply(list_iButton, function(x) {subset(x, x[,1] >= start_Labtest & x[,1] <= end_Labtest)})
#---> changed for own time span
list_iButton_corr = lapply(list_iButton, function(x) {subset(x, x[,1] >= start_time & x[,1] <= end_time)})


# --------------------------------------------------------------------------------------------------------------------------------
# Calculation of mean, standard deviation (sd) and median 

###
# Mean of single iButton file
mean_iButton_single = mean(d_iButton_single_corr$Temperature_C, na.rm = TRUE)
mean_iButton_single

# sd of single iButton file
sd_iButton_single = sd(d_iButton_single_corr$Temperature_C, na.rm = TRUE)
sd_iButton_single


###
# Calculation of mean for every iButton in the list "list_iButton"
# First select only the second column of each file (= temperature record for each iButton)
list_iButton_temp <- lapply(list_iButton_corr, `[`, 2)

# Calculate the mean for every iButton and save in the list "list_iButton_mean"
list_iButton_mean <- lapply(list_iButton_temp, function(x) mean(x$Temperature, na.rm = TRUE))

# Calculate the sd for every iButton and save in the list "list_iButton_sd"
list_iButton_sd <- lapply(list_iButton_temp, function(x) sd(x$Temperature, na.rm = TRUE))

# Calculate the median for every iButton and save in the list "list_iButton_median"
list_iButton_median <- lapply(list_iButton_temp, function(x) median(x$Temperature, na.rm = TRUE))


###
# Replace the long names of the list_iButton_temp with only the ID
# Paste the ID vector with the names derived from the files_iButton list
header_ID <- iButton_ID_multi$V2

# Assign new file names to every iButton-file in list "list_iButton"
names(list_iButton_temp) <- header_ID


###
# Transform list to dataframe
# Unlist the list containing the means of each iButton
d_iButton_mean <- data.frame(matrix(unlist(list_iButton_mean), nrow=length(list_iButton_mean), byrow=T))

# Round mean to one digit
d_iButton_mean <- round(d_iButton_mean, digits = 1)

# Unlist the list containing the sds of each iButton
d_iButton_sd <- data.frame(matrix(unlist(list_iButton_sd), nrow=length(list_iButton_sd), byrow=T))

# Round sd to one digit
d_iButton_sd <- round(d_iButton_sd, digits = 1)

# Unlist the list containing the medians of each iButton
d_iButton_median <- data.frame(matrix(unlist(list_iButton_median), nrow=length(list_iButton_median), byrow=T))


# Bind the names, means and sds of the respective iButton into one dataframe
d_iButton_stat <- cbind(list_header, iButton_ID_multi, d_iButton_mean, d_iButton_median, d_iButton_sd)

# Assign new column names to dataframe containing means of each iButtons
colnames(d_iButton_stat) = c("Name_iButton", "ID_iButton", "Temperature_mean_C", "Temperature_median_C", "Standard_deviation_C")

# Check the statistics of all iButtons
summary(d_iButton_stat)


# --------------------------------------------------------------------------------------------------------------------------------
# To select a certain iButton from the list
# just insert the number of the element the iButton has in the list "list_iButton" in the brackets.
# For example, here we select the first element of the list, which is iButton ID 51
# By using the double brackets you receive a dataframe instead of a list.
# Single brackets will give you a list, depending on what you want to do with your data, choose the right format.

# Dataframe iButton ID 51
# Change the number in the brackets to the position of the desired iButton in the list "list_iButton"
d_iButton_ID_51 <- list_iButton[[1]]

# List iButton ID 51
list_iButton_ID_51 <- list_iButton[1]


# --------------------------------------------------------------------------------------------------------------------------------
# Data of reference thermometers HC2-S3 

# Set Working directory (wd)
# setwd(choose.dir()) # Uncomment if necessary, but only works for Windows operating system
setwd("V:/klima/Projekte/2019_Urban_Heat_Island/Data/Data_converted/Reference_thermometers")

# Two different ways of reading in data:
# 1. Read in a single file: If you only need to control a single file etc. use this code
# 2. Read in several files and bind them to a continuous data set
# Define header for the reference thermometers
# Create vector containing ID #
# We only need the ID # without the first (empty) column, so we set the first column to NULL in colClasses
Tref_header=read.table("TOA5_Temp_Data_1Hz25_2019_07_05_0430.dat", sep = ",", dec = ".", header = F, skip = 1,
                      nrows = 1, as.is = T) 


# Reference thermometer
# 1. Read in a single file 
# Adjust file name to the exact file you want to read in!
d_Tref_single = read.table("TOA5_Temp_Data_1Hz25_2019_07_05_0430.dat", sep = ",", dec = ".", header = F, 
                              skip = 4, na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)

# Add column names to Tref data
colnames(d_Tref_single) <- Tref_header

# 2. Read in several files and bind them together to a large data set
# List all files you want to bind together by choosing them by name.
# Here every file which begins with "TOA5_f" is chosen.
files_Tref=list.files(pattern ="^TOA5_")  

# Read in all files from the list and bind them together
# CAUTION: The files have to have the SAME number of columns!
for(i in files_Tref) {
  if(!exists("d_Tref")) {
    d_Tref=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                           na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
  } else {
    temp_d_Tref=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
    d_Tref=rbind(d_Tref, temp_d_Tref)
    remove(temp_d_Tref)
  }
}

# Add column names to Tref data
colnames(d_Tref) <- Tref_header

# Transform timestamp from character to POSIXct format for each file
d_Tref$TIMESTAMP <- as.POSIXct(d_Tref$TIMESTAMP,format = "%Y-%m-%d %H:%M:%OS")

# Apply the time index on the Tref data table
#---> choose own time frame
#---> outcommented d_Tref_corr <- subset(d_Tref, TIMESTAMP >= start_Labtest & TIMESTAMP <= end_Labtest)
d_Tref_corr <- subset(d_Tref, TIMESTAMP >= start_time & TIMESTAMP <= end_time)

# Calculation of mean of the two reference thermometers
mean_Tref <- colMeans(d_Tref_corr[,c(3,5)]) # = 20.2 for both thermometers
mean_Tref


# --------------------------------------------------------------------------------------------------------------------------------
# Calculation of individual offset of each iButton relative to the mean reference temperature
# = Difference between mean of reference thermometers and each mean of each iButton
# This gives us the offset by which the respective iButton has to be corrected
diff_T <- 20.2 - d_iButton_stat$Temperature_mean_C[1:length(d_iButton_stat$Temperature_mean_C)]

# Round the offsets to only one digit
T_offset <- round(diff_T,digits=1)

# Add the offset to the statistics dataframe as a new column
d_iButton_stat <- cbind(d_iButton_stat, T_offset)

# Sort iButtons by ID
d_iButton_stat <- arrange(d_iButton_stat, ID_iButton)



# --------------------------------------------------------------------------------------------------------------------------------
# Export the statistics data table
# Set Working directory (wd)
# setwd(choose.dir()) # Uncomment if necessary, but only works for Windows operating system
setwd("V:/klima/Projekte/2019_Urban_Heat_Island/Data/Data_corrected")

# Export the data table containing the statistics
write.csv(d_iButton_stat, file = "iButton_Statistics.csv", row.names = FALSE)


# --------------------------------------------------------------------------------------------------------------------------------
# Graphics part

###
# Single iButton
# Time series plot
plot(d_iButton_single_corr$Datetime, d_iButton_single_corr$Temperature_C, col = "dodgerblue2", type = 'l', lwd = 2)

# Boxplot
boxplot(d_iButton_single_corr$Temperature_C, ylab = "Temperature [°C]")


###
# Comparison of all iButtons

# Boxplots
# The list "list_iButton_temp" contains only the temperature data of each iButton
# If you set the option "recursive" = FALSE, you get an overview plot of all iButtons
# If you set it TRUE, you get a single plot for each iButton.
do.call(boxplot, list(unlist(list_iButton_temp, recursive=FALSE)))

# Plot of means of each iButton vs. its ID
par(mar=c(4.1,5.1,1.3,0.3))
plot(d_iButton_stat$ID_iButton, d_iButton_stat$Temperature_mean_C, ylab = "Temperature [°C]", xlab = "ID of iButton", 
     lwd=2, cex=1.5)  
points(mean_Tref, lwd =2, cex=1.5, col = "red", pch = 4)
legend("top", legend = c("Mean iButtons", "Mean reference thermometers"), pch = c(1,4), col = c(1,2), 
       lwd = c(2,2), cex = 1.2,lty = c(NA,NA))


# Plot of temperature offset of all iButtons
plot(d_iButton_stat$ID_iButton, d_iButton_stat$T_offset, ylab = "Temperature offset [°C]", xlab = "ID of iButton", 
     lwd=2, cex=1.5, pch = 4)  
abline(h=0, col = "red", lwd = 2)


# Plot time series of reference thermometers
# Define graphical parameters
par(mar=c(4.1,5.1,1.3,0.3))

plot(d_Tref_corr$TIMESTAMP, d_Tref_corr$Temp_air_1, ylab = "Temperature [°C]", xlab = "Datetime", type = 'p',
     xaxt = 'n') # Thermometer 1
points(d_Tref_corr$TIMESTAMP, d_Tref_corr$Temp_air_2, col = "blue") # Thermometer 2

ggplot(d_Tref_corr, aes(x = TIMESTAMP, y = Temp_air_1, colour="blue")) +
  geom_line() +
  theme(legend.position="none")


# Ensemble time series plot of all iButtons 
# Every line represents an individual iButton
ggplot(bind_rows(list_iButton_corr, .id="df"), aes(x = Datetime, y = Temperature_C, colour=df)) +
  geom_line() +
  theme(legend.position="none")


