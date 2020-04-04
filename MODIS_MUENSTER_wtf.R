
setwd("F:/data_MODIS_Muenster")
setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit/MODIS_Files/MOD11A1.006")
setwd("C:/00 Dana/Uni/Internship/Work/remote sensing/MODIS_Data/hdf_files")
library(raster)
library(sp)
library(MODIS)
library(MODISTools)
library(rgdal)
library(gdalUtils)
#MODISoptions(localArcPath='F:/data_MODIS_Muenster')
#orgStruc()
EarthdataLogin(usr="Dana_L.2020", pwd = "Bachelorarbeit2020")
period = as.Date(c("2019/08/01", "2019/09/30"))
tile = c(18,03)
getProduct("MOD21A1D")
mt_bands("MOD11A1")
d=getHdf(product='MYD11A1', begin=period[1], end=period[2],tileH=tile[1], tileV=tile[2], quiet=FALSE)
#another try
mt_subset(product="MOD11A1", band=, lat, lon, start = "2019-08-01",end = "2019-08-02")

path_modis="C:\00 Dana\Uni\6. Semester\Bachelorarbeit\MODIS_Files\MOD11A1.006"
hdf.files = list.files(path=path_modis,pattern=".hdf",recursive=T)
nFiles = length(hdf.files) # Calculate number of files to import

getwd()
#another try from paper

MODISSubsets(LoadDat = PREDICTS, Products = “MOD13Q1”, Bands = c(”250m_16_days_NDVI”, “250m_16_days_EVI”,
                                                                  “250m_16_days_pixel_reliability”), Size = c(3,3), StartDate = FALSE, TimeSeriesLength = 2)

setwd("F:/MOD13Q1.006-20200203T223634Z-001/MOD13Q1.006/2016.01.17")#new try (downloaded from site)
sds=get_subdatasets("MOD11A1.A2019213.h18v03.006.2019214085426.hdf")
sds=get_subdatasets("MOD13Q1.A2016017.h17v03.006.2016035114724.hdf")
sds=get_subdatasets("MOD11A1.A2019216.mosaic.006.2020042230807.psmccs_000501416204.LST_Day_1km.hdf")
sds=get_subdatasets("MOD13A1.A2019209.h18v03.006.2019226015445.hdf")
sds=get_subdatasets("MYD13Q1.A2016009.h17v03.006.2016027152434")
