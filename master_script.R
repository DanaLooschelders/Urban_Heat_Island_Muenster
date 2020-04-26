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
source("~/Urban_Heat_Island_Muenster/split_day_night.r")
source("~/Urban_Heat_Island_Muenster/plot_data.r")

#for stats (don't always execute)
source("~/Urban_Heat_Island_Muenster/tidy_for_linearity_test.R")
source("~/Urban_Heat_Island_Muenster/linearity_test.R")