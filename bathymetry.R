# Bathymetry data 

# Using tips from http://neondataskills.org/R/Raster-Data-In-R/

# load the raster, sp, and rgdal packages
library(raster)
library(sp)
library(rgdal)


# Load in bath data
# load raster in an R object called 'DEM'
DEM <- raster("bsbd-0.9.3_3035_clip.tif")

# look at the raster attributes. 
DEM

# calculate and save the min and max values of the raster to the raster object
DEM <- setMinMax(DEM)

# view raster attributes
DEM


#view coordinate reference system
DEM@crs


# view raster extent
DEM@extent

# the distribution of values in the raster
hist(DEM, main="Distribution of depth values", 
     col= "grey", 
     maxpixels=22000000)


# plot the raster
# note that this raster represents a small region of the NEON SJER field site
plot(DEM, 
     main="Digital Elevation Model, bathymetry") # add title with main

# create a plot of our raster
image(DEM)


# Extract values -----
# Tips from here: https://gis.stackexchange.com/questions/60527/how-to-extract-values-from-rasters-at-location-of-points-in-r




coordinates(dive.points) <- ~Longitude+Latitude

# Define shapefile's current CRS
projection(dive.points) <- CRS("+proj=lonlat +ellps=WGS84")

# Reproject to RasterLayer's CRS
dive.points.reprj <- spTransform(dive.points, CRS(projection(DEM)))

# Extract values
x <- extract(DEM, dive.points.reprj)

# View the distribution of this data
hist(x, breaks = 50)

plot()
