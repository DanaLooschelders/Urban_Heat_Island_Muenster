#Master script to automatically perform analysis for datasets
#choose correct date (data with 10 min intervall)
#data from 2019
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_02.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_14.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_20.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_01.09.R")
#data from 2020
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_07.06.2020.R")
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_03.07.2020.r")

#execute all scripts in that order for 2019
source("~/Urban_Heat_Island_Muenster/Logger/start_time_correction.R") #sets all logger to same starting point
source("~/Urban_Heat_Island_Muenster/Logger/QAQC_Logger.r") #corrects offset (lab test), spikes, outliers and plots histogram
source("~/Urban_Heat_Island_Muenster/Logger/plot_data_in_pairs.R") #group the logger (GBS) and plot together
source("~/Urban_Heat_Island_Muenster/Logger/plot_differences_green_blue.R") #plot difference between grey and green and export results to csv.file
source("~/Urban_Heat_Island_Muenster/Logger/plot_water_data.R") #plot all water logger together
source("~/Urban_Heat_Island_Muenster/Logger/plot_veg_sealed_data.r") #plot all vegetation/sealed logger together
source("~/Urban_Heat_Island_Muenster/Logger/plot_water_air_difference.r") #plot the difference between air and water temperature
source("~/Urban_Heat_Island_Muenster/Logger/split_day_night.r") #split the data into day and night (without dawn/dusk)
source("~/Urban_Heat_Island_Muenster/Logger/plot_split_data.r") #plot the day and night data

##execute all scripts in that order for 2020 (different logger IDs)
source("~/Urban_Heat_Island_Muenster/Logger/start_time_correction.R") #sets all logger to same starting point
source("~/Urban_Heat_Island_Muenster/Logger/subset_use_only_for_03.07_data.r") #sets all logger to same starting point
source("~/Urban_Heat_Island_Muenster/Logger/QAQC_Logger_2020.r") #sets all logger to same starting point
source("~/Urban_Heat_Island_Muenster/Logger/plot_data_in_pairs_2020.R") #sets all logger to same starting point



#data with 30 min intervall, execute following block of scripts
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_24.09.r")
source("~/Urban_Heat_Island_Muenster/Logger/start_time_correction_30_min.R")
source("~/Urban_Heat_Island_Muenster/Logger/tidy_data_30_min.R")

#for stats (don't always execute) for 2019
source("~/Urban_Heat_Island_Muenster/Logger/tidy_for_linearity_test.R")
source("~/Urban_Heat_Island_Muenster/Logger/time_series_plot.R") #plot the decomposed time series
source("~/Urban_Heat_Island_Muenster/Logger/time_series_decomposition_correlation.R") 
source("~/Urban_Heat_Island_Muenster/Logger/time_series_significance_test.R") #signficance test between logger pairs and for all grey against green logger
source("~/Urban_Heat_Island_Muenster/Logger/descriptive_stats.r") #calculate mean, median and sd for 24h/day/night 
source("~/Urban_Heat_Island_Muenster/Logger/plot_descriptive_stats.R") #plots with mean, meadian, standard deviation
source("~/Urban_Heat_Island_Muenster/Logger/cor_site_parameters.R") #lm and cor for site chracteristics/temperatur median 
source("~/Urban_Heat_Island_Muenster/Logger/pca_site_parameters.R")

#for stats (don't always execute) for 2020 (different logger IDs)

#for mapping Logger data
source("~/Urban_Heat_Island_Muenster/Logger/leaflet_heatmap_from_02.08.r")
source("~/Urban_Heat_Island_Muenster/Logger/Heatmap.r")
source("~/Urban_Heat_Island_Muenster/Logger/spatial_interpolation_idw.r")

#supplementary weather data 
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

RStudio.Version()
