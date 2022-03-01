library(terra)
library(raster)
library(sp)
library(sf)
library(tidyr)
library(rgdal)
library(dplyr)
library(tidyverse)

#read the buffered state boundary
#assam <- vect("E:/cdl/maps/assam_state_buffer.shp")
#srtm_dem <- rast("E:/cdl/maps/raster/srtm_raster/srtm_filled_dem.tif")
#srtm_slope <- rast("E:/cdl/maps/raster/srtm_raster/strm_filled_slope_degrees.tif")
#gcn_global <- rast("F:/data_cities/globalcurvenumber_cn/GCN250_ARCIII_average.tif")
#drn_den_old <- rast("E:/cdl/maps/raster/gmted_drainage_density.tif")
#plot(drn_den)
#reprojecting strm_slope
#srtm_slope <- project(srtm_slope, crs(drn_den), overwrite = T)
#reprojecting strm_dem
#srtm_dem <- project(srtm_dem, crs(drn_den), overwrite = T)
#plot(srtm_slope)
#plot(assam, add=T)
#gcn_assam <- crop(gcn_global, assam)
#gcn_assam <- project(gcn_global, crs(drn_den))
#writeRaster(gcn_assam, "E:/cdl/maps/raster/3_gcn_Assam.tif", overwrite=TRUE)
#writeRaster(srtm_dem, "E:/cdl/maps/raster/1_srtm_dem.tif", overwrite=TRUE)
#writeRaster(srtm_slope, "E:/cdl/maps/raster/2_srtm_slope.tif", overwrite=TRUE)

#rasterise a assam_state_boundary buffer
assam <- vect("E:/cdl/maps/assam_state_buffer.shp")

assam_reproj <- vect("E:/cdl/maps/assam_state_buffer_reproj.shp")
district <- vect("E:/cdl/maps/assam_district_27.shp")
assam_bound <- rast("E:/cdl/maps/raster/assam_stdy_area.tif")

srtm_dem <- rast("E:/cdl/maps/raster/srtm_raster/srtm_filled_dem.tif")
srtm_slope <- rast("E:/cdl/maps/raster/srtm_raster/strm_filled_slope_degrees.tif")
drn_den <- rast("E:/cdl/maps/raster/gmted_drainage_density_without_1.tif")
#riv_dist <- rast("E:/cdl/maps/raster/dist_from_river.tif") 
riv_dist <- rast("E:/cdl/maps/raster/assam_dist_from_major_rivers.tif") 
as_soil <- rast("E:/cdl/maps/raster/assam_soil.tif") 
as_lith <- rast("E:/cdl/maps/raster/assam_lith.tif") 

gcn_global <- rast("F:/data_cities/globalcurvenumber_cn/GCN250_ARCIII_average.tif")
gcn_assam <- crop(gcn_global, assam)
#reading the rainfall data

R_15 <- rast('E:/cdl/maps/raster/rainfall_2015.tif')
R_16 <- rast('E:/cdl/maps/raster/rainfall_2016.tif')
R_17 <- rast('E:/cdl/maps/raster/rainfall_2017.tif')
R_18 <- rast('E:/cdl/maps/raster/rainfall_2018.tif')
R_19 <- rast('E:/cdl/maps/raster/rainfall_2019.tif')
R_20 <- rast('E:/cdl/maps/raster/rainfall_2020.tif')
R_21 <- rast('E:/cdl/maps/raster/rainfall_2021.tif')

rainfall <- c(R_15, R_16, R_17, R_18, R_19, R_20, R_21)
R_mean <- mean(R_15, R_16, R_17, R_18, R_19, R_20, R_21)

R_mean <- terra::project(R_mean , assam_bound, overwrite = T)
R_mean <- crop(R_mean , assam_bound, overwrite = T)
R_mean <- mask(R_mean , assam_bound, overwrite = T)


srtm_dem_prj <- terra::project(srtm_dem, assam_bound, overwrite = T)
srtm_dem_prj <- crop(srtm_dem_prj , assam_bound)
srtm_dem_prj <- mask(srtm_dem_prj , assam_bound, overwrite = T)


srtm_slope_prj <- terra::project(srtm_slope, assam_bound, overwrite = T)
srtm_slope_prj <- crop(srtm_slope_prj , assam_bound)
srtm_slope_prj <- mask(srtm_slope_prj , assam_bound, overwrite = T)


drn_den_prj <- terra::project(drn_den, assam_bound, overwrite = T)
drn_den_prj <- crop(drn_den_prj , assam_bound)
drn_den_prj <- mask(drn_den_prj , assam_bound, overwrite = T)


gcn_assam <- terra::project(gcn_assam , assam_bound, overwrite = T)
gcn_assam <- crop(gcn_assam , assam_bound, overwrite = T)
gcn_assam <- mask(gcn_assam , assam_bound, overwrite = T)


riv_dist <- terra::project(riv_dist , assam_bound, overwrite = T)
riv_dist <- crop(riv_dist , assam_bound, overwrite = T)
riv_dist <- mask(riv_dist , assam_bound, overwrite = T)

as_soil <- terra::project(as_soil , assam_bound, overwrite = T)
as_soil <- crop(as_soil , assam_bound, overwrite = T)
as_soil <- mask(as_soil , assam_bound, overwrite = T)

as_lith <- terra::project(as_lith , assam_bound, overwrite = T)
as_lith <- crop(as_lith , assam_bound, overwrite = T)
as_lith <- mask(as_lith , assam_bound, overwrite = T)

#reclassify soil
y <- classify(as_soil, cbind(id=c(56,230,254,255), v=c(5,3,0,1.5)))
unique(y[])
soil <- y
plot(soil)

#reclassify lith
y_lith <- classify(as_lith, cbind(id=c(1,2,3,4,5,6,7,8), v=c(0,5,3.5,2,3.5,2,1,2.5)))
as.matrix(y_lith)
unique(y_lith[])
lith <- y_lith
plot(lith)

#reclssify - RAIN
range(R_mean[], na.rm=T)
rain_levels <- cut(R_mean[], breaks = 5,include.lowest =T, na.rm = T)
tt <- cut(R_mean[],labels = c(1,2,3,4,5), include.lowest =T,breaks = 5, na.rm = T)
rain <- R_mean
values(rain) <- tt
rain <- rain+1
#reclassify - elevation
elev_levels <- cut(srtm_dem_prj[], breaks = 5,include.lowest =T, na.rm = T)
levels(t)
tt <- cut(srtm_dem_prj[],labels = c(5,4,3,2,1), breaks = 5, include.lowest =T,na.rm = T)
ele <- srtm_dem_prj
values(ele) <- tt
ele <- 6-(ele+1) 
writeRaster(ele, "E:/cdl/maps/raster/ele_check.tif", overwrite=TRUE)
#reclassify - slope
slope_levels <- cut(srtm_slope_prj[], breaks = 5,include.lowest =T, na.rm = T)
levels(slope_levels)

tt <- cut(srtm_slope_prj[],labels = c(5,4,3,2,1), breaks = 5,include.lowest =T, na.rm = T)
sloper <- srtm_slope_prj 
values(sloper) <- tt
sloper <- 6-(sloper+1)
#reclassify - drn_den_prj
drnden_levels <- cut(drn_den_prj[], breaks = 5,include.lowest =T, na.rm = T)
levels(drnden_levels)
tt <- cut(drn_den_prj [],labels = c(1,2,3,4,5), breaks = 5, include.lowest =T,na.rm = T)
drn_den <- drn_den_prj
values(drn_den) <- tt
drn_den <- drn_den+1

#reclassify - GCN
gcn_levels <- cut(gcn_assam[], breaks = 5,include.lowest =T, na.rm = T)
levels(gcn_levels)
tt <- cut(gcn_assam [],labels = c(5,4,3,2,1), breaks = 5,include.lowest =T, na.rm = T)
gcn <- gcn_assam
values(gcn) <- tt
gcn <- (gcn+1)#6-

#reclassify - GCN
rivdist_levels <- cut(riv_dist[], breaks = 5,include.lowest =T, na.rm = T)
levels(rivdist_levels)
tt <- cut(riv_dist[],labels = c(5,4,3,2,1), breaks = 5,include.lowest =T, na.rm = T)
rivdist <- riv_dist
values(rivdist) <- tt
rivdist <- 6-(rivdist + 1)

#flood exposure mapper
floo_vul <- .15*rain + (.20)*rivdist + .10*drn_den + (.15)*sloper + 
  (.15)*ele + (.08)*soil + (.05)*lith + (.12)*gcn
plot(floo_vul, main = "flood mapper")
writeRaster(floo_vul, "E:/cdl/maps/raster/flood_vulnerbility_opt_sdtdev.tif", overwrite=TRUE)
#unit(mm/year)
floo_vul_d <- terra::extract(floo_vul, district, fun = mean, na.rm= T, df = T)
district$floo_vul <- floo_vul_d$sum
#view(district)

lith_d <- terra::extract(lith, district, fun = mean, na.rm= T, df = T)
district$lith <- lith_d$assam_lith

soil_d <- terra::extract(soil, district, fun = mean, na.rm= T, df = T)
district$soil <- soil_d$assam_soil

#unit(mm/year)
plot(rain)
(rain[rain == 0])
rainfall_d <- terra::extract(rain, district, fun = mean, na.rm= T, df = T)
district$rain_mean <- rainfall_d$sum

#aggregating district wise
#unit (m)
dem_d <- terra::extract(ele, district, fun=mean, na.rm=TRUE, df=TRUE)
district$dem <- dem_d$srtm_filled_dem

#unit (degrees)
slope_d <- terra::extract(sloper, district, fun=mean, na.rm=TRUE, df=TRUE)
district$slope <- slope_d$strm_filled_slope_degrees

#unit(km/sqkm)
drainage_d <- terra::extract(drn_den, district, fun = mean, na.rm = T, df = T)
district$drainage <- drainage_d$gmted_drainage_density_without_1

#unit(curve number - Curve Number is a dimensionless parameter indicating 
#the runoff response characteristic of a drainage basin. 
#In the Curve Number Method, this parameter is related to land use, 
#land treatment, hydrological condition, hydrological soil group, 
#and antecedent soil moisture condition in the drainage basin.)
gcn_d <- terra::extract(gcn, district, fun = mean, na.rm = T, df = T)
district$gcn <- gcn_d$GCN250_ARCIII_average

#unit(km)
riv_d <- terra::extract(rivdist, district, fun = mean, na.rm = T, df = T)
district$river_dist <- riv_d$assam_dist_from_major_rivers

#river length per district

india_riv <- st_read("E:/cdl/maps/india_major_Rivers_wris.shp")
distr <- st_read("E:/cdl/maps/assam_district_27.shp")
india_riv <- st_transform(india_riv, crs(distr))
riv <- st_intersection(india_riv, distr)
riv$len <- st_length(riv)
pp <- riv %>% group_by(district) %>% summarise(r = sum(len))
pp$r
outfile <- "E:/cdl/maps/district_27_flood_exposure.shp"
writeVector(district, outfile, overwrite=TRUE)
write.csv(district, file = 'E:/cdl/tables/district_27_flood_exposure.csv')
