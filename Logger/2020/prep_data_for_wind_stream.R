#use: all Aegidii logger, Ehrenpark, Aaseemensa VL and SL Spätzl
setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/Plots/windstream/merge")

#first: check Aegidiiloger for transect
Aeg_Logger=metadata$Logger_ID[metadata$Standort=="Aegidiistr"]
#subset normal data list
list_iButton_corr_tidy_Aegidii=list_iButton_corr_tidy[names(list_iButton_corr_tidy)%in%Aeg_Logger]
#subset 24h mean list
list_iButton_24h_Aegidii=list_iButton_24h_mms[names(list_iButton_24h_mms)%in%Aeg_Logger]
#subset day mean
list_iButton_24h_Aegidii=list_iButton_24h_mms[names(list_iButton_24h_mms)%in%Aeg_Logger]
#subset daytime
list_iButton_day_Aegidii=list_iButton_day_mms[names(list_iButton_day_mms)%in%Aeg_Logger]
#subset hourly means
list_iButton_hourly_Aegidii=list_iButton_hourly[names(list_iButton_hourly)%in%Aeg_Logger]
#subset factor list
list_iButton_factor_Aegidii=list_iButton_corr_tidy_date_factor[names(list_iButton_corr_tidy_date_factor)%in%Aeg_Logger]
#subset day list
list_iButton_Aegidii_day=list_iButton_corr_tidy_date_day[names(list_iButton_corr_tidy_date_day)%in%Aeg_Logger]
#subset night list
list_iButton_Aegidii_night=list_iButton_corr_tidy_date_night[names(list_iButton_corr_tidy_date_night)%in%Aeg_Logger]

#rename lists
for(i in 1:length(list_iButton_corr_tidy_Aegidii)){
  names(list_iButton_corr_tidy_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_corr_tidy_Aegidii)[[i]]]
  names(list_iButton_24h_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_24h_Aegidii)[[i]]]
 names(list_iButton_day_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_day_Aegidii)[[i]]]
 names(list_iButton_hourly_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_hourly_Aegidii)[[i]]]
 names(list_iButton_factor_Aegidii)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_factor_Aegidii)[[i]]]
 names(list_iButton_Aegidii_day)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_Aegidii_day)[[i]]]
 names(list_iButton_Aegidii_night)[[i]]=metadata$Standort_ID[metadata$Logger_ID==names(list_iButton_Aegidii_night)[[i]]]
  }

#reorder names 
list_iButton_corr_tidy_Aegidii <- list_iButton_corr_tidy_Aegidii[order(names(list_iButton_corr_tidy_Aegidii))]
list_iButton_24h_Aegidii <- list_iButton_24h_Aegidii[order(names(list_iButton_24h_Aegidii))]
list_iButton_day_Aegidii <- list_iButton_day_Aegidii[order(names(list_iButton_day_Aegidii))]
list_iButton_hourly_Aegidii <- list_iButton_hourly_Aegidii[order(names(list_iButton_hourly_Aegidii))]
list_iButton_factor_Aegidii <- list_iButton_factor_Aegidii[order(names(list_iButton_factor_Aegidii))]
list_iButton_Aegidii_day <- list_iButton_Aegidii_day[order(names(list_iButton_Aegidii_day))]
list_iButton_Aegidii_night <- list_iButton_Aegidii_night[order(names(list_iButton_Aegidii_night))]
