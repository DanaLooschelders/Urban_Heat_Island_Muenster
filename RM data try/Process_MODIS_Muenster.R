#MODIS surface temperature
setwd("F:/data_MODIS_Muenster/MODIS_neu")
library(raster)
library(sp)
library(MODIS)
library(MODISTools)
library(rgdal)
library(gdalUtils)
#Muenster is in MODIS tiles: h18v3 and h18v4
#product: MOD11A1

#test load MODIS .hdf files -> quite alot of clouds
sds1=getSds("MOD11A1.A2019220.h18v03.006.2019221085842.hdf")
sds2=getSds("MOD11A1.A2019220.h18v04.006.2019221085847.hdf")
#try with another file -> only clouds
sds1=getSds("MOD11A1.A2019221.h18v03.006.2019222084540.hdf")
sds2=getSds("MOD11A1.A2019221.h18v04.006.2019222084541.hdf")
#-> nearly only clouds
sds1=getSds("MOD11A1.A2019222.h18v03.006.2019223085312.hdf")
sds2=getSds("MOD11A1.A2019222.h18v04.006.2019223085304.hdf")
#-> only clouds
sds1=getSds("MOD11A1.A2019223.h18v03.006.2019224085017.hdf")
sds2=getSds("MOD11A1.A2019223.h18v04.006.2019224085005.hdf")
#->a lot of clouds
sds1=getSds("MOD11A1.A2019224.h18v03.006.2019225092419.hdf")
sds2=getSds("MOD11A1.A2019224.h18v04.006.2019225092417.hdf")
#extract layer with LST values
LST1=raster(readGDAL(sds1$SDS4gdal[1]))
LST2=raster(readGDAL(sds2$SDS4gdal[1]))
#extract layer with QC values
QC1=raster(readGDAL(sds1$SDS4gdal[2]))
QC2=raster(readGDAL(sds2$SDS4gdal[2]))
#mosiac lst files together
mosaic=mosaic(LST1, LST2, fun=mean)
plot(mosaic)
#mosaic qc files together
mosaic_qc=mosaic(QC1, QC2, fun=mean)
#get modis crs
crs_modis=crs(LST1)
#load shapefile of muenster in UTM
shape_muenster=readOGR(dsn ="stadtgebiet.shp")
shape_ms_modis=spTransform(shape_muenster, crs_modis)
#load shapefile of DE
  #shape_de=readOGR(dsn="gadm36_DEU_0.shp")
#define crs
crs_ms=crs(shape_muenster)
crs_modis=crs(LST1)
#load shapefile with buildings in muenster and convert to modis crs
  #buildings_ms=readOGR(dsn="gis_osm_buildings_a_free_1.shp")
  #buildings_ms_modis=spTransform(buildings_ms, crs_modis)
#crop lst to muenster extent
extent_ms_modis=extent(projectExtent(raster(shape_muenster),crs_modis))
extent_ms=extent(shape_muenster)
testcrop=crop(mosaic, extent_ms_modis)
plot(testcrop)
#crop qc to muenster extent
testcrop_qc=crop(mosaic_qc, extent_ms_modis)
#QC -> remove poor quality values
#scale (*0.02) and convert from Kelvon to C
  #convert unit to Celsius
testcrop=testcrop-273.15
#plot with muenster shape (convert MS shape to modis crs) and ms buildings
plot(testcrop)
plot(shape_ms_modis, add=T)

#crop to just shape of MS
testcrop_exact=mask(testcrop, shape_ms_modis)
plot(testcrop_exact, main="LST Temperature [°C] in Aug in Muenster", xlab="MODIS x coordinates", ylab="MODIS y coordniates")
plot(shape_ms_modis, add=T)
#create data.frame
coords=coordinates(testcrop)
values=getValues(testcrop)
QC=getValues(testcrop_qc)
dat=cbind(coords, values, QC)
