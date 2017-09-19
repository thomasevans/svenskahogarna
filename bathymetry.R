# Bathymetry data 

# Using tips from http://neondataskills.org/R/Raster-Data-In-R/

# load the raster, sp, and rgdal packages
library(raster)
library(sp)
library(rgdal)


# Load in bath data
# Credit for bathymetry data:
# Baltic Sea Hydrographic Commission, 2013, Baltic Sea Bathymetry Database version 0.9.3. Downloaded from http://data.bshc.pro/ on download date
# Data are griddent to 500 m, though source resolution may be worse than this
# In swedish waters generally quite high resolution
# Sweden: For the EEZ area a 100m × 100m grid and for the territorial waters, due to legal restrictions, a grid of 500m × 500m grid have been used. For each populated cell the average value was extracted. For areas not covered by other digital information depth figures derived from the Swedish Maritime Administrations chart database have been used.

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
     col= "grey")


# plot the raster
# note that this raster represents a small region of the NEON SJER field site
plot(DEM, 
     main="Digital Elevation Model, bathymetry") # add title with main

# create a plot of our raster
image(DEM)



# Load in the dive location data ------


# Link to DB
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

dives <- sqlQuery(gps.db, query="SELECT dives_gps_loc.device_id, dives_gps_loc.UTC_datetime, dives_gps_loc.Latitude, dives_gps_loc.Longitude, dives_gps_loc.dive_id
FROM dives_gps_loc
ORDER BY dives_gps_loc.device_id, dives_gps_loc.dive_id;")

# Extract values -----
# Tips from here: https://gis.stackexchange.com/questions/60527/how-to-extract-values-from-rasters-at-location-of-points-in-r



# Define coordinates (make Spatial points data frame)
coordinates(dives) <- ~Longitude+Latitude

# Define shapefile's current CRS
projection(dives) <- CRS("+proj=longlat +ellps=WGS84")

# Reproject to RasterLayer's CRS
dive.points.reprj <- spTransform(dives, CRS(projection(DEM)))

# Extract values
x <- extract(DEM, dive.points.reprj)

# View the distribution of this data
hist(x, breaks = 50)


# Consolidate and output data to database ------
dives.out <- dives@data
str(dives.out)

dives.out$bath_depth_m <- x

# Output to DB -------
# Check format
str(dives.out)
head(dives.out$UTC_datetime)
# Fix tz
library(lubridate)

dives.out$UTC_datetime <- force_tz(dives.out$UTC_datetime, tzone = "UTC")


# Output
sqlSave(gps.db, dives.out,
        tablename = "dives_gps_loc_bath",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(UTC_datetime = "datetime")
)

