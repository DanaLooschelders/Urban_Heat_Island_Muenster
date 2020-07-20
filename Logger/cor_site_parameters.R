#explore the correlation between the median temperature and site specific parameters
library(ggplot2)
library(GGally)
library(Hmisc)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/spatial_data")

Logger_IDs=metadata$Logger_ID
#drop non-numeric columns
metadata_numeric=dplyr::select_if(metadata, is.numeric)
####### uncomment when Logger_IDs are complete
rownames(metadata_numeric)=Logger_IDs
metadata_numeric$Logger_ID=Logger_IDs
#use for loop to add median corresponding to Logger_ID (take care of replaced loggers/Logger_IDS)

for(i in names(list_iButton_corr_tidy)){
  metadata_numeric$Temp_median[metadata_numeric$Logger_ID==as.numeric(i)]=median(list_iButton_corr_tidy[[i]]$Temperature_C_w_off)
}
metadata_numeric=metadata_numeric[,-17] #drop column with Logger_IDS
#explore relationship between aspect ratio and median temp
#plot data with linear regression line
plot(metadata_numeric$Temp_median~ metadata_numeric$Aspect_ratio)
abline(lm(metadata_numeric$Temp_median~ metadata_numeric$Aspect_ratio))
#plot data with linear regression line with sd in ggplot (in pretty)
    #use alpha to change opacity of points
    #e.g. alpha=I(1/5) -> total opacity is reaced when 5 points overlap
qplot(metadata_numeric$Temp_median, metadata_numeric$Aspect_ratio,
      method="lm", geom=c("point", "smooth"))
#plot data with no specified method for trend line
qplot(metadata_numeric$Temp_median, metadata_numeric$Aspect_ratio,
      geom=c("point", "smooth"))

#calculate pearson correlation
cor.test(metadata_numeric$Temp_median, metadata_numeric$Aspect_ratio, 
         use="na.or.complete", method="pearson")
#caclculate pearson correlation with significance levels
rcorr(metadata_numeric$Temp_median, metadata_numeric$Aspect_ratio, type = "pearson")

#correlation matrix
ggcorr(metadata_numeric, label=TRUE)

#combine multiple variables
ggpairs(metadata_numeric, 
        columns=c(names(metadata_numeric[26]), "Aspect_ratio", "BÃ.ume"),
        upper=list(continuous = wrap("cor", size=10)),
        lower=list(continuous="smooth"))
#for Temp_median, Aspect ratio, tree 

setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/cor_site")

#loop through table and plot
for (i in 1:length(metadata_numeric[,-17])){ #use all columns expept median temp
  #plot data with linear regression line
  pdf(file=paste("scatter_lm", names(metadata_numeric[i]), substring(list_iButton_corr_tidy[[1]][1,2], 
                                         first=1, last=10), ".pdf"))
  plot(metadata_numeric$Temp_median~ metadata_numeric[,i], xlab=paste(names(metadata_numeric[i])),
       ylab="Temperatur Median")
  abline(lm(metadata_numeric$Temp_median~ metadata_numeric[,i]))
  dev.off()
  
  #plot data with linear regression line with sd in ggplot (in pretty)
  #use alpha to change opacity of points
  #e.g. alpha=I(1/5) -> total opacity is reaced when 5 points overlap
  qplot(metadata_numeric$Temp_median, metadata_numeric[,i],
        method="lm", geom=c("point", "smooth"),
        ylab=paste(names(metadata_numeric[i])), xlab="Temperatur Median")
  ggsave(filename=paste("lm_smooth", names(metadata_numeric[i]), 
                        substring(list_iButton_corr_tidy[[1]][1,2], 
                        first=1, last=10), ".pdf"),
         device = "pdf")
  #plot data with no specified method for trend line
  qplot(metadata_numeric$Temp_median, metadata_numeric[,i],
        geom=c("point", "smooth"),
        ylab=paste(names(metadata_numeric[i])), xlab="Temperatur Median")
  ggsave(filename=paste("scatter_smooth", names(metadata_numeric[i]), 
                        substring(list_iButton_corr_tidy[[1]][1,2], 
                                  first=1, last=10), ".pdf"), device = "pdf")
  }
#prepare output table for results
cor_tab=data.frame(parameter=names(metadata_numeric)[-17], cor_pearson=rep(NA), cor_sig=rep(NA))
for (i in 1:length(metadata_numeric[,-17])){ #use all columns expept median temp
  #calculate pearson correlation
cor_result=cor.test(metadata_numeric$Temp_median, metadata_numeric[,i], 
                    use="na.or.complete", method="pearson")
cor_tab$cor_sig[cor_tab$parameter==names(metadata_numeric[i])]=cor_result$p.value #write significance value in table
cor_tab$cor_pearson[cor_tab$parameter==names(metadata_numeric[i])]=cor_result[["estimate"]][["cor"]] #write correlation value in table
}
write.table(cor_tab, file=paste("cor", 
            substring(list_iButton_corr_tidy[[1]][1,2], 
            first=1, last=10), ".csv"), sep=";", dec=",",
            row.names=FALSE)

######check if use="na.or.complete" is the correct option
