# Mapping data

# Load in data -----
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

# Foraging trip details (for filtering) -----
trips <- sqlQuery(gps.db, query="SELECT trips.device_id, trips.trip_id, trips.n_points, trips.datetime_start, trips.datetime_end, trips.duration_s, trips.duration_h, trips.interval_s_first, trips.interval_s_last, trips.interval_s_max, trips.nest_dist_max, trips.nest_dist_first, trips.nest_dist_last, deployments.Species
FROM (trips INNER JOIN trips_dive_info ON trips.trip_id = trips_dive_info.trip_id) INNER JOIN deployments ON trips.device_id = deployments.serial_num
                  ORDER BY trips.device_id, trips.trip_id;")



# GPS locations (only those from foraging trips) ------
# Include species and individuals
points <- sqlQuery(gps.db, query="SELECT ornitela_gps.device_id, ornitela_gps.UTC_datetime, ornitela_gps.Latitude, ornitela_gps.Longitude, ornitela_gps.speed_km_h, gps_trips.nest_dist, gps_trips.trip_id, deployments.Species
FROM (ornitela_gps INNER JOIN gps_trips ON (ornitela_gps.UTC_datetime = gps_trips.UTC_datetime) AND (ornitela_gps.device_id = gps_trips.device_id)) INNER JOIN deployments ON gps_trips.device_id = deployments.serial_num
WHERE (((gps_trips.on_trip)='TRUE') AND ((gps_trips.keep_points)='TRUE'))
ORDER BY ornitela_gps.device_id, ornitela_gps.UTC_datetime;
")



# Dives (only those with locations) ------
# Include species and individuals
dives <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.datetime_start, dives.datetime_end, dives.duration_s, dives.depth_m_max, dives_trip_id.trip_id, deployments.Species, dives_gps_loc_bath_depth_dif.above_bottom_m, dives_gps_loc_bath.bath_depth_m, dives_gps_loc.device_id, dives_gps_loc.UTC_datetime, dives_gps_loc.Latitude, dives_gps_loc.Longitude, dives_gps_loc.tdff_start, dives_gps_loc.tdff_end, dives_gps_loc.tdff_abs_min
FROM deployments INNER JOIN ((((dives INNER JOIN dives_trip_id ON dives.diveevent = dives_trip_id.diveevent) INNER JOIN dives_gps_loc_bath_depth_dif ON dives_trip_id.diveevent = dives_gps_loc_bath_depth_dif.diveevent) INNER JOIN dives_gps_loc_bath ON dives_gps_loc_bath_depth_dif.diveevent = dives_gps_loc_bath.dive_id) INNER JOIN dives_gps_loc ON dives_gps_loc_bath.dive_id = dives_gps_loc.dive_id) ON deployments.serial_num = dives.device_id
WHERE (((dives_trip_id.trip_id) Is Not Null))
ORDER BY dives.device_id, dives.diveevent;")






# Bathymetry data ------
library(rgdal)

# Get into some format that it can be plot



# Good coastline data -------
load("baltic_coast_and_islands.RData")

# plot(all_coast_baltic, col = "dark grey")


# Filtering foraging trips ------
library("dplyr")

# Filter to only whole foraging trips, and exlude very short and very long movements
hist(trips$nest_dist_max, ylim = c(0,20), breaks = 100)

hist(trips$nest_dist_max,
     xlim = c(0,10), breaks = 800)
abline(v = 1, col = "red")

summary(trips$nest_dist_max>1)
# *Exclude trips with max distance <1 km & >40 km

trips2 <- filter(trips, trips$nest_dist_max>1 & trips$nest_dist_max<40)

# Retain whole tracked trips only
hist(trips2$nest_dist_first/trips2$nest_dist_max, breaks = 100)

hist(trips2$nest_dist_first, breaks = 100)

hist(trips2$nest_dist_last, breaks = 200)


# Mapping foraging trips -------
library(maps)
library(maptools)
library(mapdata)
library(dplyr)
library(RODBC)
library(sp)
library(raster)


# Helper functions -----

# Map scale
source("map.scale2.R")


# Alpha channel
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


pointsf <- filter(points, trip_id %in% trips2$trip_id)

# Clip coast data to spatial extent of data
all_coast_baltic2 <- raster::crop(all_coast_baltic,
                                 raster::extent(c(
                                   range(pointsf$Longitude)+c(-0.2,0.2),
                                   range(pointsf$Latitude)+c(-0.3,0.3))))


# Plot base map   ---------
png("trips.png", width = 6, height = 6,
    res = 600, units = "in")

plot(all_coast_baltic2, xlim = range(pointsf$Longitude),
     ylim = range(pointsf$Latitude), col= "dark grey", bg = NA,
     # main = title.text,
     border = "black",
     lwd = 0.5,
     main = "",
     lty = 1)


# Add foraging trips, drawing with alpha channel in random order
# Different colours for the two species
# Orange - guillemots #d95f02
# purple - razorbills #7570b3
cols <- c("#d95f02", "#7570b3")
col.alpha <- addalpha(cols, 0.6)


# Add axis
axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.7, cex = 0.7, padj = -1.5, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.7, cex = 0.7, padj = 0.2, hadj = 0.8)

# Outline box
box(lwd=2)



# Plot GPS data
# i <- 20
trips2plot <- trips2$trip_id
# Shuffle
trips2plot <- sample(trips2plot)
# trips2plot <- trips2plot[c(1:10)]

for(i in 1:length(trips2$trip_id)){
  
  gps.sub <- filter(pointsf, trip_id == trips2plot[i])
  col.sp <- switch(as.character(gps.sub$Species[1]),
                   u_aalge = col.alpha[1],
                   A_torda = col.alpha[2])
  # unique(gps.sub$Species)
  
  n <- length(gps.sub$Longitude)
  segments(gps.sub$Longitude[-1], gps.sub$Latitude[-1],
           gps.sub$Longitude[1:n-1], gps.sub$Latitude[1:n-1],
           col = col.sp, lty = 1, lwd = 1.5)
 
   # Also plot dives for foraging trips at the same time (to have same drawing order)
  # plot dives
  dives.sub <- filter(dives, trip_id == trips2plot[i])
  points(dives.sub$Longitude,
         dives.sub$Latitude,
         col = col.sp,
         pch = 1,
         lwd = 1,
         cex = 0.3)
  
  
}



map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.7)

dev.off()




# Bathymetry map with dives -------

# Load in bath data
# Credit for bathymetry data:
# Baltic Sea Hydrographic Commission, 2013, Baltic Sea Bathymetry Database version 0.9.3. Downloaded from http://data.bshc.pro/ on download date
# Data are griddent to 500 m, though source resolution may be worse than this
# In swedish waters generally quite high resolution
# Sweden: For the EEZ area a 100m × 100m grid and for the territorial waters, due to legal restrictions, a grid of 500m × 500m grid have been used. For each populated cell the average value was extracted. For areas not covered by other digital information depth figures derived from the Swedish Maritime Administrations chart database have been used.

# load raster in an R object called 'DEM'
bath <- raster("bsbd-0.9.3_3035_clip.tif")
extent(bath)
str(bath)


dives2 <- filter(dives, trip_id %in% trips2plot)

# Define coordinates (make Spatial points data frame)
coordinates(dives2) <- ~Longitude+Latitude

# Define shapefile's current CRS
projection(dives2) <- CRS("+proj=longlat +ellps=WGS84")

# Reproject to RasterLayer's CRS
dive.points.reprj <- spTransform(dives2, CRS(projection(bath)))



# Clip bathymetry data to spatial extent of data
bath2 <- raster::crop(bath,
                      extent(dive.points.reprj))

str(bath2)

hist(bath2@data@values)
range(bath2@data@values, na.rm = TRUE)


# 9-class YlGnBu from http://colorbrewer2.org/?type=sequential&scheme=YlGnBu&n=9
breakpoints <- -1*c(0,5,10,20,30,50,75,100,125)
colors <- rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'))
colors <- addalpha(colors, 0.7)



png("bath_dives.png", width = 6, height = 6,
    res = 600, units = "in")

# plot(volcanoR,breaks=breakpoints,)
# str(coordinates(dives))
plot(bath2,
     # xlim = range(dives$Longitude),
     # ylim = range(dives$Latitude),
     bg = NA,
     # main = title.text,
     # border = "black",
     # lwd = 0.5,
     breaks = breakpoints,
     col = colors,
     main = "",
     axes = FALSE)
# ?plot


# Reproject to RasterLayer's CRS
all_coast_baltic2.reprj <- spTransform(all_coast_baltic2, CRS(projection(bath2)))


plot(all_coast_baltic2.reprj, col= "dark grey", bg = NA,
     # main = title.text,
     border = "black",
     lwd = 0.5,
     lty = 1,
     add = TRUE)


# Add foraging trips, drawing with alpha channel in random order
# Different colours for the two species
# Orange - guillemots #d95f02
# purple - razorbills #7570b3
cols <- c("#d95f02", "black")
col.alpha <- addalpha(cols, 0.4)

# col.sp <- switch(as.character(dives$Species),
#                  u_aalge = col.alpha[1],
#                  A_torda = col.alpha[2])
col.sp <- ifelse(as.character(dives$Species) %in% c("u_aalge"), col.alpha[1], col.alpha[2])

plot(dive.points.reprj,
       col = col.sp,
       pch = 1,
       lwd = 1,
       cex = 0.2,
     add = TRUE)


# Plot land again
plot(all_coast_baltic2.reprj, col= "dark grey", bg = NA,
     # main = title.text,
     border = "black",
     lwd = 0.5,
     lty = 1,
     add = TRUE)

# Add axis
# axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.7, cex = 0.7, padj = -1.5, hadj = NA)
# axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.7, cex = 0.7, padj = 0.2, hadj = 0.8)

# Outline box
box(lwd=2)


dev.off()



# kernel thing to show overall area use --------

# Guillemots
# Set seed for repeatability
set.seed(3)

points_guillemot <- filter(pointsf, Species == "u_aalge")

points_guillemot_g <- points_guillemot %>% group_by(device_id)

# Sample 100 of each
points_guillemot_sample <- sample_n(points_guillemot_g, 100,
                                    replace = TRUE)
# Remove duplicated rows (sampling with replacement above)
points_guillemot_sample <- points_guillemot_sample[!duplicated(points_guillemot_sample),]



plot(all_coast_baltic2, col= "dark grey", bg = NA,
     # main = title.text,
     border = "black",
     lwd = 0.5,
     lty = 1)
points(points_guillemot_sample$Longitude,
       points_guillemot_sample$Latitude,
       col = "red")





# Razorbills
# Set seed for repeatability
set.seed(3)

points_razorbill <- filter(pointsf, Species == "A_torda")

points_razorbill_g <- points_razorbill %>% group_by(device_id)

# Sample 100 of each
points_razorbill_sample <- sample_n(points_razorbill_g, 100,
                                    replace = TRUE)
# Remove duplicated rows (sampling with replacement above)
points_razorbill_sample <- points_razorbill_sample[!duplicated(points_razorbill_sample),]



plot(all_coast_baltic2, col= "dark grey", bg = NA,
     # main = title.text,
     border = "black",
     lwd = 0.5,
     lty = 1)
points(points_razorbill_sample$Longitude,
       points_razorbill_sample$Latitude,
       col = "red")





# Load packaged needed for kernel density estimator calculation
library("adehabitatHR")
library("sp")

# Get in format needed for adehabitatHR
# xy <- t(rbind(coords[f,1],coords[f,2]))
xy <- cbind.data.frame(points_razorbill_sample$Longitude,
                       points_razorbill_sample$Latitude)
names(xy) <- c("x", "y")
coordinates(xy) <- c("x","y")

# # Make kernel object
# kud <- kernelUD(xy, h = 1)
# kud.lst <- list()
# h <- c(0.0001,0.001,0.01,0.1,0.4,0.5,0.8,1,2,3,5,7,10)
# for(i in 1:length(h)){
#   kud.lst[i] <- kernelUD(xy, h = h[i])
# }
# par(mfrow=c(4,5))
# for(i in 1:length(h)){
#   image(kud.lst[[i]])
# }
# 
# ?kernelUD

par(mfrow=c(1,1))
kud <- (kernelUD(xy, grid = 2000))

image(kud)
ver_95 <- getverticeshr(kud, 95)
ver_50 <- getverticeshr(kud, 50)
ver_25 <- getverticeshr(kud, 25)

# ?kernelUD

plot(all_coast_baltic2, xlim = range(pointsf$Longitude),
     ylim = range(pointsf$Latitude), col= "dark grey", bg = NA,
     # main = title.text,
     border = "black",
     lwd = 0.5,
     main = "",
     lty = 1)
plot(ver_95,
     lwd = 0.6,
     add = TRUE)
plot(ver_50,
     lwd = 0.8,
     add = TRUE)
plot(ver_25,
     lwd = 1.2,
     add = TRUE)





hist(dives$depth_m_max)

par(mfrow = c(2,1))
guil_dives <- filter(dives, Species == "u_aalge")
hist(guil_dives$depth_m_max,
     xlim = c(0,45), breaks = 20)
razo_dives <- filter(dives, Species == "A_torda")
hist(razo_dives$depth_m_max,
     xlim = c(0,45), breaks = 20)

# if(Sys.getenv("JAVA_HOME")!=""){
  Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_144")
# }
# library(rJava)

install.packages("OpenStreetMap")

library(OpenStreetMap)

## Not run:
#show some of the maps available
nm <- c("osm", "maptoolkit-topo", "bing", "stamen-toner",
        "stamen-watercolor", "esri", "esri-topo",
        "nps", "apple-iphoto", "skobbler")
par(mfrow=c(3,4))
#Korea
for(i in 1:length(nm)){
  map <- openmap(c(43.46886761482925,119.94873046875),
                 c(33.22949814144951,133.9892578125),
                 minNumTiles=3,type=nm[i])
  plot(map)
}

# install.packages("rJava")

str(map)

lats <- range(pointsf$Latitude)
longs <- range(pointsf$Longitude)

# dev.off()
map <- openmap(c(lats[2],longs[1]),
               c(lats[1],longs[2]),
               minNumTiles = 3,type="osm",
               zoom = 11)
plot(map)
# ?openmap


map.n <- openproj(map)

plot(map.n)


str(map$tiles[[1]]$projection)
plot(ver_95,
     lwd = 0.6,
     add = TRUE)
plot(ver_50,
     lwd = 0.8,
     add = TRUE)
plot(ver_25,
     lwd = 1.2,
     add = TRUE)



hist(trips$nest_dist_max, xlim = c(0,45),
     breaks = 50)









  