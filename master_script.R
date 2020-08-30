#Master script to automatically perform analysis for datasets
options(digits = 2)
#round to 1 or .5
x=seq(-1,3,by=.1)
x
round(x/.5)*.5

#choose correct date (data with 10 min intervall)
#data from 2020
source("~/Urban_Heat_Island_Muenster/Logger/2020/iButtons-Check_total_DL_from_07.06.2020.R")
source("~/Urban_Heat_Island_Muenster/Logger/2020/iButtons-Check_total_DL_from_03.07.2020.r")
source("~/Urban_Heat_Island_Muenster/Logger/2020/iButtons-Check_total_DL_from_17.07.2020.r")

#to map all July 2020 data together execute the following scripts
source("~/Urban_Heat_Island_Muenster/Logger/2020/iButtons-Check_total_DL_from_03.07.2020.r")
source("~/Urban_Heat_Island_Muenster/Logger/2019/start_time_correction.R") #sets all logger to same starting point
source("~/Urban_Heat_Island_Muenster/Logger/2020/subset_use_only_for_03.07_data.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/iButtons-Check_total_DL_from_17.07.2020_as_2nd_list.r")
source("~/Urban_Heat_Island_Muenster/Logger/2020/start_time_correction_for_2nd_list.R") #sets all logger to same starting point
source("~/Urban_Heat_Island_Muenster/Logger/2020/merge_july_2020_data.R")

##execute all scripts in that order for 2020 (different logger IDs)
#only for 03.07 script
source("~/Urban_Heat_Island_Muenster/Logger/2019/start_time_correction.R") #sets all logger to same starting point
source("~/Urban_Heat_Island_Muenster/Logger/2020/subset_use_only_for_03.07_data.r") 

#for all (continue here after mergering)
source("~/Urban_Heat_Island_Muenster/Logger/2020/QAQC_Logger_2020.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/plot_differences_green_blue_2020.R")
#source("~/Urban_Heat_Island_Muenster/Logger/2020/plot_water_air_difference_2020.r")
source("~/Urban_Heat_Island_Muenster/Logger/2020/plot_site_type_together_2020.r") #plot all water/Vegetation/sealed/etc logger together
source("~/Urban_Heat_Island_Muenster/Logger/2020/split_day_night_2020.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/plot_split_data_2020.r")
source("~/Urban_Heat_Island_Muenster/Logger/2020/plot_data_in_pairs_2020.R") 

#for stats/time series analysis for 2020 (different logger IDs)
source("~/Urban_Heat_Island_Muenster/Logger/2020/integrate_differences_green_blue.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/cor_site_parameters.R") #lm and cor for site chracteristics/temperatur median 
source("~/Urban_Heat_Island_Muenster/Logger/pca_site_parameters.R") #pca for site chracteristics/temperatur median 
source("~/Urban_Heat_Island_Muenster/Logger/2020/descriptive_stats_2020.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/plot_descriptive_stats_2020.r")
source("~/Urban_Heat_Island_Muenster/Logger/2020/time_series_plot_2020.R") #plot the decomposed time series
source("~/Urban_Heat_Island_Muenster/Logger/2020/time_series_significance_test_2020.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2019/Seasonal_ARIMA.R") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/water_air_split_significance_tests.r") 

#wind stream analysis 2020
source("~/Urban_Heat_Island_Muenster/Logger/2020/prep_data_for_wind_stream.R") 
source("~/Urban_Heat_Island_Muenster/supplementary_weather_data/read_in_wind_rad_data_DWD.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/plot_windstream_Data.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/sig_dif_windstream_filter_for_SW_wind.r") 
source("~/Urban_Heat_Island_Muenster/Logger/2020/sig_dif_windstream_all_values.r") 

#for mapping Logger data
source("~/Urban_Heat_Island_Muenster/Logger/leaflet_heatmap_from_02.08.r")
source("~/Urban_Heat_Island_Muenster/Logger/Heatmap.r")
source("~/Urban_Heat_Island_Muenster/Logger/spatial_interpolation_idw.r")

#supplementary weather data 
  #recent scripts
source("~/Urban_Heat_Island_Muenster/supplementary_weather_data/comp_rad_temp_diff.r")
source("~/Urban_Heat_Island_Muenster/supplementary_weather_data/DWD_data_wind_temp_2020.R")

 #older scripts
source("~/Urban_Heat_Island_Muenster/supplementary_weather_data/DWD_data_wind_temp.r")
source("~/Urban_Heat_Island_Muenster/supplementary_weather_data/comp_wind_temp_diff.r")

#netatmo data
source("~/Urban_Heat_Island_Muenster/Netatmo/prep_plot_netatmo.R")
source("~/Urban_Heat_Island_Muenster/Netatmo/merge_netatmo.R")
source("~/Urban_Heat_Island_Muenster/Netatmo/QAQC_Netatmo_level_A_B.r") #need to execute supp weather data
source("~/Urban_Heat_Island_Muenster/Netatmo/QAQC_Netatmo_level_C_D.R") #need to execute supp weather data
source("~/Urban_Heat_Island_Muenster/Netatmo/netatmo_plot_overview.R")

#source("~/Urban_Heat_Island_Muenster/Netatmo/leaflet_netatmo.R")
#source("~/Urban_Heat_Island_Muenster/Netatmo/data_from_json.r")

#for mapping Logger and netatmo
source("~/Urban_Heat_Island_Muenster/Logger_Netatmo_map.R")

#data from 2019
source("~/Urban_Heat_Island_Muenster/Logger/2019/iButtons-Check_total_DL_from_02.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/2019/iButtons-Check_total_DL_from_14.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/2019/iButtons-Check_total_DL_from_20.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/2019/iButtons-Check_total_DL_from_01.09.R")

#execute all scripts in that order for 2019
source("~/Urban_Heat_Island_Muenster/Logger/2019/start_time_correction.R") #sets all logger to same starting point
source("~/Urban_Heat_Island_Muenster/Logger/2019/QAQC_Logger.r") #corrects offset (lab test), spikes, outliers and plots histogram
source("~/Urban_Heat_Island_Muenster/Logger/2019/plot_data_in_pairs.R") #group the logger (GBS) and plot together
source("~/Urban_Heat_Island_Muenster/Logger/2019/plot_differences_green_blue.R") #plot difference between grey and green and export results to csv.file
source("~/Urban_Heat_Island_Muenster/Logger/2019/plot_water_data.R") #plot all water logger together
source("~/Urban_Heat_Island_Muenster/Logger/2019/plot_veg_sealed_data.r") #plot all vegetation/sealed logger together
source("~/Urban_Heat_Island_Muenster/Logger/2019/plot_water_air_difference.r") #plot the difference between air and water temperature
source("~/Urban_Heat_Island_Muenster/Logger/2019/split_day_night.r") #split the data into day and night (without dawn/dusk)
source("~/Urban_Heat_Island_Muenster/Logger/2019/plot_split_data.r") #plot the day and night data

#for stats/time series analysis for 2019
source("~/Urban_Heat_Island_Muenster/Logger/2019/tidy_for_linearity_test.R")
source("~/Urban_Heat_Island_Muenster/Logger/2019/time_series_plot.R") #plot the decomposed time series
source("~/Urban_Heat_Island_Muenster/Logger/2019/time_series_significance_test.R") #signficance test between logger pairs and for all grey against green logger
source("~/Urban_Heat_Island_Muenster/Logger/2019/descriptive_stats.r") #calculate mean, median and sd for 24h/day/night 
source("~/Urban_Heat_Island_Muenster/Logger/2019/plot_descriptive_stats.R") #plots with mean, meadian, standard deviation
source("~/Urban_Heat_Island_Muenster/Logger/2019/plot_site_type_together_2019.r") #plots with mean, meadian, standard deviation
source("~/Urban_Heat_Island_Muenster/Logger/2019/green_grey_merge_sig_test_2019.r") #plots with mean, meadian, standard deviation

#data with 30 min intervall, execute following block of scripts
source("~/Urban_Heat_Island_Muenster/Logger/2019/iButtons-Check_total_DL_from_24.09.r")
source("~/Urban_Heat_Island_Muenster/Logger/2019/start_time_correction_30_min.R")
source("~/Urban_Heat_Island_Muenster/Logger/2019/tidy_data_30_min.R")

RStudio.Version()
