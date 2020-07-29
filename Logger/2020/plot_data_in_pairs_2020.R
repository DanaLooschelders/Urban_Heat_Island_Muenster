#plot in logger pairs
#set ylim 
  #for 01 August: ylim=c(10,35)
  #for September: ylim=c(5,30)
  #for 20 August: ylim=c(10,45)
  #for 14 August: ylim=c(10,35)
  #for 28 September: ylim=c(-5,30)

#plot graphs for all unique sites (not IDs)
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/paired_plots/")

for (i in unique(metadata$Standort)){
  name=paste(substr(as.character(list_iButton_corr_tidy[[1]][1,2]), 1,10),
             "day","paired_plot",i,".pdf")
  Loggers=metadata$Logger_ID[metadata$Standort==i]
  metadata_sub=metadata[metadata$Standort==i,]
 pdf(file=name, paper = "a4r", height=7, width=14)
  plot(list_iButton_corr_tidy[[as.character(metadata_sub$Logger_ID[1])]][2:3],
       type="l", col=metadata_sub$color[1], ylim=c(5,35),
       ylab="Temperature [°C]", xlab="Date", main=paste("Temperature at", i, "in 2020"),
       sub="WL = Waterlogger   WOL = Water Surface Logger   VL = Vegetation Logger   SL = Sealed Logger")
  abline(v=sun2$sunrise, col="orange")
  abline(v=sun2$sunset, col="darkred")
  for(i in 2:length(metadata_sub$Logger_ID)){
    lines(list_iButton_corr_tidy[[as.character(metadata_sub$Logger_ID[i])]][2:3],
          col=metadata_sub$color[i])
  }
  legend("topright",legend = c(metadata_sub$Standort_ID,"Sunrise", "Sunset" ), 
         fill =c(metadata_sub$color,"orange", "darkred" ))
  dev.off()
}

