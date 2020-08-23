#merge data to one time series
#bind Temp in dataframe

VL_Temp <-rowMeans(cbind(list_iButton_corr_tidy_VL[[1]][,3],
                                    list_iButton_corr_tidy_VL[[2]][,3],
                                    list_iButton_corr_tidy_VL[[3]][,3], 
                                    list_iButton_corr_tidy_VL[[4]][,3], 
                                    list_iButton_corr_tidy_VL[[5]][,3], 
                                    list_iButton_corr_tidy_VL[[6]][,3], 
                                    list_iButton_corr_tidy_VL[[7]][,3], 
                                    list_iButton_corr_tidy_VL[[8]][,3], 
                                    list_iButton_corr_tidy_VL[[9]][,3], 
                                    list_iButton_corr_tidy_VL[[10]][,3], 
                                    na.rm=T))

SL_Temp <-rowMeans(cbind(list_iButton_corr_tidy_SL[[1]][,3],
                         list_iButton_corr_tidy_SL[[2]][,3],
                         list_iButton_corr_tidy_SL[[3]][,3], 
                         list_iButton_corr_tidy_SL[[4]][,3], 
                         list_iButton_corr_tidy_SL[[5]][,3], 
                         list_iButton_corr_tidy_SL[[6]][,3], 
                         list_iButton_corr_tidy_SL[[7]][,3], 
                         list_iButton_corr_tidy_SL[[8]][,3], 
                         list_iButton_corr_tidy_SL[[9]][,3], 
                         list_iButton_corr_tidy_SL[[10]][,3], 
                         list_iButton_corr_tidy_SL[[11]][,3], 
                         list_iButton_corr_tidy_SL[[12]][,3], 
                         na.rm=T))
#dataframe for results
merge_VL_SL=data.frame(VL_Temp, SL_Temp, 
                       "date"=list_iButton_corr_tidy_SL[[1]][,2])

merge_VL_SL$diff=merge_VL_SL$SL_Temp-merge_VL_SL$VL_Temp #calculate difference

max_temp_diff=max(merge_VL_SL$diff, na.rm=T)
mean_temp_diff_VL_SL=mean(merge_VL_SL$diff, na.rm=T) #calculate mean difference

results=data.frame(max_temp_diff, mean_temp_diff_VL_SL)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/difference_plots/merge/")
write.csv2(results, 
           "result_mean_diff_SL_VL.csv", 
           row.names = F)

shapiro.test(merge_VL_SL$VL_Temp)

plot(merge_VL_SL$VL_Temp, merge_VL_SL$diff)
summary(lm(merge_VL_SL$VL_Temp~merge_VL_SL$diff))
