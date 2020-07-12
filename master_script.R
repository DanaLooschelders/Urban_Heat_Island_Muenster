#Master script to automatically perform analysis for datasets
#choose correct date (data with 10 min intervall)
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_02.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_14.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_20.08.R")
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_01.09.R")

#execute all scripts in that order
source("~/Urban_Heat_Island_Muenster/Logger/start_time_correction.R")
source("~/Urban_Heat_Island_Muenster/Logger/tidy_data.r")
source("~/Urban_Heat_Island_Muenster/Logger/plot_data_in_pairs.R")
source("~/Urban_Heat_Island_Muenster/Logger/calculate_differences_green_blue.R")
source("~/Urban_Heat_Island_Muenster/Logger/water_data_plot-r.R")
source("~/Urban_Heat_Island_Muenster/Logger/veg_sealed_data_plot.r")
source("~/Urban_Heat_Island_Muenster/Logger/water_air_temp_difference.r")
source("~/Urban_Heat_Island_Muenster/Logger/split_day_night.r")
source("~/Urban_Heat_Island_Muenster/Logger/plot_data.r")

#data with 30 min intervall, execute following block of scripts
source("~/Urban_Heat_Island_Muenster/Logger/iButtons-Check_total_DL_from_24.09.r")
source("~/Urban_Heat_Island_Muenster/Logger/start_time_correction_30_min.R")
source("~/Urban_Heat_Island_Muenster/Logger/tidy_data_30_min.R")

#for stats (don't always execute)
source("~/Urban_Heat_Island_Muenster/Logger/tidy_for_linearity_test.R")
source("~/Urban_Heat_Island_Muenster/Logger/time_series_plot.R")
source("~/Urban_Heat_Island_Muenster/Logger/time_series_decomposition_correlation.R")
source("~/Urban_Heat_Island_Muenster/Logger/time_series_significance_test.R")
source("~/Urban_Heat_Island_Muenster/Logger/mean_sd_logger.r")
source("~/Urban_Heat_Island_Muenster/Logger/plot_statistics.R")
source("~/Urban_Heat_Island_Muenster/Logger/linaer_reg_site_parameters_GSI.R")

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