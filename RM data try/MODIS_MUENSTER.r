
setwd("F:/data_MODIS_Muenster/MODIS_neu")
setwd("C:/00 Dana/Uni/6. Semester/Bachelorarbeit/MODIS_Files/MOD11A1.006")
setwd("C:/00 Dana/Uni/Internship/Work/remote sensing/MODIS_Data/hdf_files")
setwd("F:/data_MODIS_Muenster/MODIS_neu/muenster-regbez-latest-free.shp/")
library(raster)
library(sp)
library(MODIS)
library(MODISTools)
library(rgdal)
library(gdalUtils)

EarthdataLogin(usr="Dana_L.2020", pwd = "Bachelorarbeit2020")

tile = c(18,03)

#tif.files = list.files(path=, pattern=".tif",recursive=T)
#nFiles = length(hdf.files) # Calculate number of files to import

mt_bands(product = "MOD11") #why doesn't it work?

#shapefile muenster
shape_muenster=readOGR(dsn ="stadtgebiet.shp")
shape_de=readOGR(dsn="gadm36_DEU_0.shp")

#döööööööööööööööööööööööööööö
test3=raster("MOD11A1.A2019213.mosaic.006.2020046194822.psmcrpgs_000501417357.LST_Day_1km-LST_Day_1km.tif")
test2=raster("MOD11A1.A2019213.mosaic.006.2020046194822.psmcrpgs_000501417357.LST_Day_1km-LST_Day_1km.tif")
testa="MOD11A1.A2019213.mosaic.006.2020046194822.psmcrpgs_000501417357.LST_Day_1km-LST_Day_1km.tif"
testb="MOD11A1.A2019213.mosaic.006.2020046194822.psmcrpgs_000501417357.LST_Day_1km-LST_Day_1km.tif"
plot(test3)
plot(test2)
mosaic_rasters(gdalfile = c(testa, testb), dst_dataset = "mosaic", output_Raster = T )
mosaic=raster("mosaic")
test1=raster("MOD11A1.A2019213.mosaic.006.2020046194822.psmcrpgs_000501417357.LST_Day_1km-LST_Day_1km.tif")
test1b=raster("MOD11A1.A2019214.mosaic.006.2020046194823.psmcrpgs_000501417357.LST_Day_1km-LST_Day_1km.tif")
test1c=raster("MOD11A1.A2019215.mosaic.006.2020046194824.psmcrpgs_000501417357.LST_Day_1km-LST_Day_1km.tif")

plot(test1)
plot(test1b)
plot(test1c)

plot(shape_muenster, add=T)
testcrop=crop(test1, shape_muenster)
#apply scaling factor of 0.02
testcrop=testcrop*0.02
#convert unit to Celsius
testcrop=testcrop-273.15
testcrop["-273.15"]=NA
plot(testcrop)
plot(shape_muenster, add=T)
summary(testcrop)
#find a way to rescale the legend to a narrower temperature range
#find a way to set -273.15 values to NA

#try again with hdf
sds=get_subdatasets("MOD13A1.A2019209.h18v03.006.2019226015445.hdf")
sds[[2]]
LST=raster(sds[[2]], as.is=T)
sds=getSds("MOD13A1.A2019209.h18v03.006.2019226015445.hdf")
layer=raster(readGDAL(sds$SDS4gdal[1]))
plot(layer)

#try again with temp
sds=getSds("MOD13A1.A2019209.h18v03.006.2019226015445.hdf")
get_subdatasets("MOD13A1.A2019209.h18v03.006.2019226015445.hdf")
lst=raster(readGDAL(sds$SDS4gdal[1]))
plot(lst)
crs_muenster=crs(shape_muenster)
lst2=projectRaster(from = lst, CRSobj =shape_muenster )
