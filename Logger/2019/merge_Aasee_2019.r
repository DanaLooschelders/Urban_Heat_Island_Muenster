#make one consecutive time line for July-September 2019
#read in data
#logger Aasee water: 10
#logger Aasee vegetation: 91
#add consecutive and NA in order to dispaly it correctly in the plot 
date_time <- seq.POSIXt(from=list_iButton_corr_set[[1]][1,2],
                        to=list_iButton_corr_set[[1]][dim(list_iButton_corr_set[[1]])[1],2],
                        by="10 min") #create complete timeframe
