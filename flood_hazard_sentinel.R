# Install and load the necessary packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("maptools", "rgdal", "raster", "rgeos", "rasterVis", "RCurl", "devtools", "gdalUtils")
ipak(packages)
library(terra)
#install.packages("sen2r")
library(sen2r)
library(sf)
library(sp)
library(tidyverse)
library(gdalcubes)

#install.packages('rgdal', type='source')
#install.packages("rgdal", type = "source", configure.args="--with-gdal-config=/Library/Frameworks/GDAL.framework/unix/bin/gdal-config --with-proj-include=/Library/Frameworks/PROJ.framework/unix/include --with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib --with-proj-share=/Library/Frameworks/PROJ.framework/unix/share/proj --with-proj-data=/Library/Frameworks/PROJ.framework/unix/share/proj --with-data-copy=yes")
#load the rasters
folder_1 <- "G:/CDL_2/r_FLOOD_HAZARD/s2a/S2A_MSIL1C_20170521T045701_N0205_R119_T44RQQ_20170521T051027.SAFE/GRANULE/L1C_T44RQQ_A009983_20170521T051027/IMG_DATA"
bandas_1 <- list.files(folder_1)
(ruta_bandas_1 <- paste0(folder_1,"/", bandas_1))

folder_2 <- "G:/CDL_2/r_FLOOD_HAZARD/s2a_oct/S2A_MSIL1C_20171028T045911_N0206_R119_T44RQQ_20171028T084742.SAFE/GRANULE/L1C_T44RQQ_A012271_20171028T050647/IMG_DATA"
bandas_2 <- list.files(folder_2)
(ruta_bandas_2 <- paste0(folder_2,"/", bandas_2))

#read the rasters
#pre-flood
B02_may <- raster(ruta_bandas_1[2])
B03_may <- raster(ruta_bandas_1[3])
B04_may <- raster(ruta_bandas_1[4])
B08_may <- raster(ruta_bandas_1[8])
B11_may <- raster(ruta_bandas_1[11])
#raster stack with 10 m reslution images
B_may <- stack(B03_may,B08_may)
#post-flood
B02_oct <- raster(ruta_bandas_2[2])
B03_oct <- raster(ruta_bandas_2[3])
B04_oct <- raster(ruta_bandas_2[4])
B08_oct <- raster(ruta_bandas_2[8])
B11_oct <- raster(ruta_bandas_2[11])
#raster stack with 10 m reslution images
B_oct <- stack(B03_oct,B08_oct)

# Band Green (B3)
green_pre <- B_may[[1]]
green_post <- B_oct[[1]]

# Band NIR (B8)
NIR_pre <- B_may[[2]]
NIR_post <- B_oct[[2]]
#visualisation
nf <- layout(matrix(c(1,2, 3, 4), 2, 2, byrow = TRUE))  

# Indicate the 4 images that will be plotted in the  1X2 matrix
plot(green_pre, main="Green Band Pre-flood")
plot(NIR_pre, main="NIR Band Pre-flood")

plot(green_post, main="Green Band Post-flood")
plot(NIR_post, main="NIR Band Post-flood")

#calculate MNDWI
res(B03_oct)
# Resample band 3 to the spatial resolution of band 11
s2a_20170521_b3_res <- resample(B03_may, B11_may, method="bilinear") 
s2a_20171023_b3_res <- resample(B03_oct, B11_oct, method="bilinear") 
#stack with new B3 bands
B_may_resp <- stack(s2a_20170521_b3_res, B11_may)
B_oct_resp <- stack(s2a_20171023_b3_res, B11_oct)

# Band Green (B3) - 20m
green_pre_r <- B_may_resp[[1]]
green_post_r <- B_oct_resp[[1]]

# Band NIR (B8)
SWIR_pre <- B_may_resp[[2]]
SWIR_post <- B_oct_resp[[2]]

# Calculate MNDWI
mndwi_pre = (green_pre_r-SWIR_pre)/(green_pre_r+SWIR_pre)
mndwi_post = (green_post_r-SWIR_post)/(green_post_r+SWIR_post)

# Threshold: Mask out the values smaller than 0 which are supposed to be non-water surfaces
water_mndwi_pre <- calc(mndwi_pre, function(x){x[x < 0] <- NA;return(x)})
water_mndwi_post <- calc(mndwi_post, function(x){x[x < 0] <- NA;return(x)})
#visualise the results
# With 'par()' you can indicate in how many columns and rows the images have to be arranged
par(mfrow=c(2,3)) 

# Indicate the 6 images that will be plotted in the  2X3 matrix
plot(mndwi_pre, main="MNDWI Pre-flood")
plot(water_mndwi_pre, main="Water pixels MNDWI, Pre-flood")

cuts=c(-1, 0, 1)
pal= colorRampPalette(c("black", "white"))
plot(mndwi_pre, breaks=cuts, col=pal(2), main= "Water mask MNDWI, Pre-flood")

plot(mndwi_post, main="MNDWI, Post-flood")
plot(water_mndwi_post, main="Water pixels MNDWI, Post-flood")

cuts=c(-1, 0, 1)
pal= colorRampPalette(c("black", "white"))
plot(mndwi_post, breaks=cuts, col=pal(2), main= "Water mask MNDWI, Post-flood")
#read the raster files
#pre-flood
writeRaster(water_mndwi_pre, datatype="FLT4S", filename = "G:/CDL_2/r_FLOOD_HAZARD/water_mndwi_pre.tif", format = "GTiff", overwrite=TRUE)
#post-flood
writeRaster(water_mndwi_post, datatype="FLT4S", filename = "G:/CDL_2/r_FLOOD_HAZARD/water_mndwi_post.tif", format = "GTiff", overwrite=TRUE)
#post-flood full
writeRaster(mndwi_post, datatype="FLT4S", filename = "G:/CDL_2/r_FLOOD_HAZARD/mndwi_post.tif", format = "GTiff", overwrite=TRUE)
#PART 2
s2_post <- stack(B08_oct,B04_oct,B03_oct)
training <- readOGR(dsn = "E:/cdl/maps/r_FLOOD_HAZARD",layer = "lu_class")
#install.packages(c("maptools", "randomForest", "mgcv"))
library(maptools)
library(randomForest)
library(mgcv)
# Number of samples per land cover class
numsamps <- 100

# Name of the attribute that holds the integer land cover type identifyer
attName <- "id"

# Name and path of the output GeoTiff image
outImage <- "G:/CDL_2/r_FLOOD_HAZARD/classif_result.tif"

# Create a RasterBrick out of the RasterStack to speed up processing
satImage <- brick(s2_post)

# Using the vector "training":
# Assign the name "allAtt" to the data of the object "training" (polygon)
allAtt <- slot(training, "data") 
# Table with the land classes and the number of samples in each
tabAtt <- table(allAtt[[attName]]) 
# Returns the numbers of the names of the data values (i.e. 1, 2, 3, 4)
uniqueAtt <- as.numeric(names(tabAtt)) 
# Set 100 random points inside each polygon landcover class of the vector "training"
for (x in 1:length(uniqueAtt)) {
  #x<-1
  class_data <- training[training[[attName]]==uniqueAtt[x],] # take the unique values of the data
  classpts <- spsample(class_data, type="random", n=numsamps) # sample the 100 sample points on the polygons
  if (x == 1) {
    xy <- classpts
  } else {
    xy <- rbind(xy, classpts) # xy is a SpatialPoints object that has now all the 100 points randomly sampled on each of the 4 clases of polygons according to the data
  }
}

# Extract and combine classes and band values
# Overlay the SpatialPoints on the Polygons
temp <- over(x=xy, y=training) 
# Transform into a factor of four levels
response <- factor(temp[[attName]]) 
# Trainvals is a matrix of the response points and the correspondent value extracted from each band
trainvals <- cbind(response, extract(satImage, xy)) 