#merge data to one time series
#bind Temp in dataframe
options(digits=2)
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

shapiro.test(SL_Temp)
July_res=wilcox.test(SL_Temp, VL_Temp) #p-value < 2.2e-16
#dataframe for results
merge_VL_SL=data.frame(VL_Temp, SL_Temp, 
                       "date"=list_iButton_corr_tidy_SL[[1]][,2])

ggplot(data=merge_VL_SL)+
  geom_line(aes(x=date, y=VL_Temp, col="vegetated"))+
  geom_line(aes(x=date, y=SL_Temp, col="sealed"))+
  theme(legend.position="right")+
  labs(title="Temperature in July 2020",
       x="Date",
       y="Temperature [°C]",
       color="Site type")

plot(merge_VL_SL$date, merge_VL_SL$VL_Temp, 
     type="l", 
     col="green",
     ylab="Temperature [°C]",
     xlab="",
     main="Temperature in JUly 2020")
lines(merge_VL_SL$date, merge_VL_SL$SL_Temp, col="red")
legend("topright", legend=c("vegetated", "sealed"))


merge_VL_SL$diff=merge_VL_SL$SL_Temp-merge_VL_SL$VL_Temp #calculate difference
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Tables")
write.table(merge_VL_SL, "GI_SI_dif_2020.csv", dec=".", sep=",",row.names = F)
max_temp_diff=max(merge_VL_SL$diff, na.rm=T)
mean_temp_diff_VL_SL=mean(merge_VL_SL$diff, na.rm=T) #calculate mean difference
p_value_wilcox=July_res[["p.value"]]
results=data.frame(max_temp_diff, mean_temp_diff_VL_SL, p_value_wilcox)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/difference_plots/merge/")
write.csv2(results, 
           "July_2020_result_mean_diff_SL_VL.csv", 
           row.names = F)

shapiro.test(merge_VL_SL$SL_Temp)

plot(merge_VL_SL$SL_Temp, merge_VL_SL$diff)
summary(lm(merge_VL_SL$SL_Temp~merge_VL_SL$diff))

cor.test(merge_VL_SL$SL_Temp,merge_VL_SL$diff, method="spearman")

#plot
plot(merge_VL_SL$SL_Temp,merge_VL_SL$diff)
abline(lm(merge_VL_SL$SL_Temp~merge_VL_SL$diff), col="red")
#plot data with linear regression line with sd in ggplot (in pretty)
#use alpha to change opacity of points
#e.g. alpha=I(1/5) -> total opacity is reaced when 5 points overlap
qplot(merge_VL_SL$SL_Temp,merge_VL_SL$diff,
      method="lm", geom=c("point", "smooth"))+
  theme_classic()+
  ylab("Difference between T in SI and T in GI [°C]")+
  xlab("Temperature in SI [°C]")
#plot data with no specified method for trend line
qplot(merge_VL_SL$SL_Temp,merge_VL_SL$diff,
      geom=c("point", "smooth"))
