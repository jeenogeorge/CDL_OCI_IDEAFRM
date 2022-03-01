library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(terra) #package for raster manipulation
#read the netCDF file
nc_data <- nc_open('E:/cdl/maps/raster/rainfall/2021.nc')
nf <- terra::rast('E:/cdl/maps/raster/rainfall/2021.nc')
#plot(nf)

#crop to assam state boundary and project to study boundary

#sum daily rainfall to get annual rainfall
nf_sum <- sum(nf)
#extract total rainfall district wise for the year

#write the raster
writeRaster(nf_sum,'E:/cdl/maps/raster/rainfall_2021.tif', overwrite = T )

# Save the print(nc) dump to a text file
{
  sink('E:/cdl/maps/raster/rainfall/2015.txt')
  print(nc_data)
  sink()
}
lon <- ncvar_get(nc_data, "LONGITUDE")
lat <- ncvar_get(nc_data, "LATITUDE", verbose = F)
t <- ncvar_get(nc_data, "TIME")

head(lon) # look at the first few entries in the longitude vector
ndvi.array <- ncvar_get(nc_data, "RAINFALL") # store the data in a 3-dimensional array
dim(ndvi.array) 

fillvalue <- ncatt_get(nc_data, "RAINFALL", "_FillValue")
fillvalue

nc_close(nc_data) 

ndvi.array[ndvi.array == fillvalue$value] <- NA
ndvi.slice <- ndvi.array[, , 1] 
dim(ndvi.slice)

r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), 
            ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r <- flip(r, direction='y')
plot(r)

r_brick <- brick(ndvi.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))