#merge data to one time series
#bind Temp in dataframe

Vegetation_Temp <-rowMeans(cbind(list_iButton_corr_tidy_Vegetation[[1]][,3],
                                    list_iButton_corr_tidy_Vegetation[[2]][,3],
                                    list_iButton_corr_tidy_Vegetation[[3]][,3], 
                                    list_iButton_corr_tidy_Vegetation[[4]][,3], 
                                    list_iButton_corr_tidy_Vegetation[[5]][,3], 
                                    list_iButton_corr_tidy_Vegetation[[6]][,3], 
                                    list_iButton_corr_tidy_Vegetation[[7]][,3], 
                                    list_iButton_corr_tidy_Vegetation[[8]][,3], 
                                    list_iButton_corr_tidy_Vegetation[[9]][,3], 
                                    list_iButton_corr_tidy_Vegetation[[10]][,3], 
                                    na.rm=T))

Sealed_area_Temp <-rowMeans(cbind(list_iButton_corr_tidy_Sealed_area[[1]][,3],
                         list_iButton_corr_tidy_Sealed_area[[2]][,3],
                         list_iButton_corr_tidy_Sealed_area[[3]][,3], 
                         list_iButton_corr_tidy_Sealed_area[[4]][,3], 
                         list_iButton_corr_tidy_Sealed_area[[5]][,3], 
                         list_iButton_corr_tidy_Sealed_area[[6]][,3], 
                         list_iButton_corr_tidy_Sealed_area[[7]][,3], 
                         list_iButton_corr_tidy_Sealed_area[[8]][,3], 
                         list_iButton_corr_tidy_Sealed_area[[9]][,3], 
                         list_iButton_corr_tidy_Sealed_area[[10]][,3], 
                         #list_iButton_corr_tidy_Sealed_area[[11]][,3], 
                         #list_iButton_corr_tidy_Sealed_area[[12]][,3], 
                         na.rm=T))


#dataframe for results
merge_Vegetation_Sealed_area=data.frame(Vegetation_Temp, Sealed_area_Temp, 
                       "date"=list_iButton_corr_tidy_Sealed_area[[1]][,2])

ggplot(data=merge_Vegetation_Sealed_area)+
  geom_line(aes(x=date, y=Vegetation_Temp, col="vegetated"))+
  geom_line(aes(x=date, y=Sealed_area_Temp, col="sealed"))+
  theme(legend.position="right")+
  labs(title="Temperature in August 2019",
       x="Date",
       y="Temperature [°C]",
       color="Site type")

plot(merge_Vegetation_Sealed_area$date, merge_Vegetation_Sealed_area$Vegetation_Temp, 
     type="l", 
     col="green",
     ylab="Temperature [°C]",
     xlab="",
     main="Temperature in JUly 2020")
lines(merge_Vegetation_Sealed_area$date, merge_Vegetation_Sealed_area$Sealed_area_Temp, col="red")
legend("topright", legend=c("vegetated", "sealed"))


merge_Vegetation_Sealed_area$diff=merge_Vegetation_Sealed_area$Sealed_area_Temp-merge_Vegetation_Sealed_area$Vegetation_Temp #calculate difference

max_temp_diff=max(merge_Vegetation_Sealed_area$diff, na.rm=T)
#for 02 Aug 3.9
#for 14 Aug 5.7
#for 20 Aug 4.4
#for 01 Sep 4.1
mean_temp_diff_Vegetation_Sealed_area=mean(merge_Vegetation_Sealed_area$diff, na.rm=T) #calculate mean difference
#for 02.Aug 0.9
#for 14 Aug 1.5
#for 20 Aug 0.5
#for 01 Sep 0.7 
results=data.frame(max_temp_diff, mean_temp_diff_Vegetation_Sealed_area)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/difference_plots/")
write.csv2(results, 
           "01_Sep_2019_result_mean_diff_Sealed_area_Vegetation.csv", 
           row.names = F)

shapiro.test(merge_Vegetation_Sealed_area$Vegetation_Temp)

plot(merge_Vegetation_Sealed_area$Vegetation_Temp, merge_Vegetation_Sealed_area$diff)
summary(lm(merge_Vegetation_Sealed_area$Vegetation_Temp~merge_Vegetation_Sealed_area$diff))

cor.test(merge_Vegetation_Sealed_area$Sealed_area_Temp, merge_Vegetation_Sealed_area$diff, method="spearman")

#for 02. Aug 2019: 0.56 p-value 2e-16
#for 14 Aug: 0.84 p value 2e-16
#for 20 Aug -0.06 p value 0.02 
#for 01 Sep 0.58 p value 2e-16
plot(merge_Vegetation_Sealed_area$date, 
     merge_Vegetation_Sealed_area$Sealed_area_Temp, 
     type="l",
     ylim = c(0,30))
lines(merge_Vegetation_Sealed_area$date, 
      merge_Vegetation_Sealed_area$diff, col="red")
