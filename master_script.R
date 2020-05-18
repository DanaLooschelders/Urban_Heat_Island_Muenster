#Master script to automatically perform analysis for datasets
#choose correct date
source("~/Urban_Heat_Island_Muenster/iButtons-Check_total_DL_from_02.08.R")
source("~/Urban_Heat_Island_Muenster/iButtons-Check_total_DL_from_14.08.R")
source("~/Urban_Heat_Island_Muenster/iButtons-Check_total_DL_from_20.08.R")
source("~/Urban_Heat_Island_Muenster/iButtons-Check_total_DL_from_01.09.R")

#execute all scripts in that order
source("~/Urban_Heat_Island_Muenster/start_time_correction.R")
source("~/Urban_Heat_Island_Muenster/tidy_data.r")
source("~/Urban_Heat_Island_Muenster/plot_data_in_pairs.R")
source("~/Urban_Heat_Island_Muenster/calculate_differences_green_blue.R")
source("~/Urban_Heat_Island_Muenster/water_data_plot-r.R")
source("~/Urban_Heat_Island_Muenster/veg_sealed_data_plot.r")
source("~/Urban_Heat_Island_Muenster/water_air_temp_difference.r")
source("~/Urban_Heat_Island_Muenster/split_day_night.r")
source("~/Urban_Heat_Island_Muenster/plot_data.r")

#for stats (don't always execute)
source("~/Urban_Heat_Island_Muenster/mean_sd_logger.r")
source("~/Urban_Heat_Island_Muenster/plot_statistics.R")
source("~/Urban_Heat_Island_Muenster/stats_test_green_grey.R")
source("~/Urban_Heat_Island_Muenster/tidy_for_linearity_test.R")
source("~/Urban_Heat_Island_Muenster/linearity_test.R")

#supplementary weather data
source("~/Urban_Heat_Island_Muenster/DWD_data_wind.r")
source("~/Urban_Heat_Island_Muenster/comp_wind_temp_diff.r")

#netatmo data
source("~/Urban_Heat_Island_Muenster/prep_plot_netatmo_01.08.R")
source("~/Urban_Heat_Island_Muenster/prep_plot_netatmo_20.08.R")
source("~/Urban_Heat_Island_Muenster/prep_plot_netatmo_05.09.R")
source("~/Urban_Heat_Island_Muenster/merge_netatmo.R")
source("~/Urban_Heat_Island_Muenster/QAQC_Netatmo.r")

#source("~/Urban_Heat_Island_Muenster/leaflet_netatmo.R")
#source("~/Urban_Heat_Island_Muenster/data_from_json.r")

#for mapping
source("~/Urban_Heat_Island_Muenster/leaflet_heatmap_from_02.08.r")
source("~/Urban_Heat_Island_Muenster/Heatmap.r")
source("~/Urban_Heat_Island_Muenster/spatial_interpolation_idw.r")