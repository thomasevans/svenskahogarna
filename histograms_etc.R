# Summary of depth, distance statistics


# Load in data -------
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')


# Dives with distance

dives_loc <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.datetime_start, dives.duration_s, dives.depth_m_max, dives_gps_loc.Latitude, dives_gps_loc.Longitude, deployments.nest_lat, deployments.nest_long, dives_trip_id.trip_id, deployments.Species
FROM dives_trip_id INNER JOIN ((dives INNER JOIN dives_gps_loc ON dives.diveevent = dives_gps_loc.dive_id) INNER JOIN deployments ON dives_gps_loc.device_id = deployments.serial_num) ON dives_trip_id.diveevent = dives_gps_loc.dive_id
ORDER BY dives.device_id, dives.diveevent;
")

# Dives with bathymetry

dives_depth <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.datetime_start, dives.duration_s, dives.depth_m_max, dives_gps_loc_bath.bath_depth_m, dives_trip_id.trip_id, deployments.Species
FROM deployments INNER JOIN (dives_trip_id INNER JOIN (dives INNER JOIN dives_gps_loc_bath ON dives.diveevent = dives_gps_loc_bath.dive_id) ON dives_trip_id.diveevent = dives_gps_loc_bath.dive_id) ON deployments.serial_num = dives_trip_id.device_id
                        ORDER BY dives.device_id, dives.diveevent;
")

# Dives for dive depth

dives_depth_only <- sqlQuery(gps.db, query="SELECT dives.diveevent, dives.datetime_start, dives.datetime_end, dives.duration_s, dives.depth_m_max, deployments.Species, dives_trip_id.trip_id
FROM (dives INNER JOIN deployments ON dives.device_id = deployments.serial_num) INNER JOIN dives_trip_id ON dives.diveevent = dives_trip_id.diveevent
ORDER BY dives.diveevent, dives.datetime_start;
")


# Trips with distance

trips <- sqlQuery(gps.db, query="SELECT trips.device_id, trips.trip_id, trips.datetime_start, trips.datetime_end, trips.nest_dist_max, trips.nest_dist_median, trips.nest_dist_mean, trips.nest_dist_last, deployments.Species
FROM deployments INNER JOIN trips ON deployments.serial_num = trips.device_id
                  ORDER BY trips.device_id, trips.trip_id;
")


# Calculate dive distances -----
# Function to calculate distance

# Function to calculate grand-circle distance. Copied from package 'fossil'. As this is the only function required from this package, it seems better to include it in a single script.
# Vavrek (2012) 'fossil' v0.3.7. Palaeoecological and Palaeogeographical Analysis Tools. http://matthewvavrek.com/programs-and-code/fossil/
deg.dist <- function (long1, lat1, long2, lat2, km = TRUE) 
{
  # Returns distance in km. Metres if km is false
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 40041.47/(2 * pi)
  d <- R * c
  if(km == FALSE){d <- (d*1000)}
  
  return(d)
}


# Calculate distance to nest
nest_dist <- mapply(FUN = deg.dist,
                    long1 = dives_loc$nest_long,
                    lat1 = dives_loc$nest_lat,
                    long2 = dives_loc$Longitude,
                    lat2 = dives_loc$Latitude
)

plot(nest_dist)
dives_loc$nest_dist <- nest_dist

# Filter data as required ------
library(plyr)
library(dplyr)

# Dives
# Filter out those <1 m
dives_loc_f <- filter(dives_loc, depth_m_max >1)
dives_depth_f <- filter(dives_depth, depth_m_max >1)
dives_depth_only_f <- filter(dives_depth_only, depth_m_max >1)

# *Exclude trips with max distance <1 km & >40 km
tripsf <- filter(trips, trips$nest_dist_max>1 & trips$nest_dist_max<40)


dives_loc_f <- filter(dives_loc_f, trip_id %in% tripsf$trip_id)
dives_depth_f <- filter(dives_depth_f,  trip_id %in% tripsf$trip_id)
dives_depth_only_f <- filter(dives_depth_only,trip_id %in% tripsf$trip_id)




# Plot data - series of histograms for two species -----
# Orange - guillemots #d95f02
# purple - razorbills #7570b3

# Include line indicating mean values
# Use common x scales for both guillemots and razorbills

# Trip distances ------
g <- tripsf$Species == "u_aalge"
hist(tripsf$nest_dist_max[g], breaks = 20, xlim = c(0,40),
     ylab = "Number of trips",
     xlab = "Maximum distance from nest (km)",
     main = "",
     col = "#d95f02",
     cex.lab = 1.3)
mean(tripsf$nest_dist_max[g])
sd(tripsf$nest_dist_max[g])
median(tripsf$nest_dist_max[g])


r <- tripsf$Species == "A_torda"
hist(tripsf$nest_dist_max[r], breaks = 20, xlim = c(0,40),
     ylab = "Number of trips",
     xlab = "Maximum distance from nest (km)",
     col = "#7570b3",
     main = "",
     cex.lab = 1.3)
mean(tripsf$nest_dist_max[r])
sd(tripsf$nest_dist_max[r])
median(tripsf$nest_dist_max[r])



# Dive distances ------
g <- dives_loc_f$Species == "u_aalge"
hist(dives_loc_f$nest_dist[g], breaks = 20, xlim = c(0,40),
     ylab = "Number of dives",
     xlab = "Distance from nest (km)",
     main = "",
     col = "#d95f02",
     cex.lab = 1.3)
mean(dives_loc_f$nest_dist[g])
sd(dives_loc_f$nest_dist[g])
median(dives_loc_f$nest_dist[g])
max(dives_loc_f$nest_dist[g])


r <- dives_loc_f$Species == "A_torda"
hist(dives_loc_f$nest_dist[r], breaks = 20, xlim = c(0,40),
     ylab = "Number of dives",
     xlab = "Distance from nest (km)",
     col = "#7570b3",
     main = "",
     cex.lab = 1.3)
mean(dives_loc_f$nest_dist[r])
sd(dives_loc_f$nest_dist[r])
median(dives_loc_f$nest_dist[r])
max(dives_loc_f$nest_dist[r])



# Dive bath ------
g <- dives_depth_f$Species == "u_aalge"
hist(-dives_depth_f$bath_depth_m[g], breaks = 20,
     xlim = c(0,125),
     ylab = "Number of dives",
     xlab = "Sea depth (m)",
     main = "",
     col = "#d95f02",
     cex.lab = 1.3)
mean(-dives_depth_f$bath_depth_m[g])
sd(-dives_depth_f$bath_depth_m[g])
median(-dives_depth_f$bath_depth_m[g])

# max(-dives_depth_f$bath_depth_m[r]
    # )

r <- dives_depth_f$Species == "A_torda"
hist(-dives_depth_f$bath_depth_m[r], breaks = 20,
     xlim = c(0,125),
     ylab = "Number of dives",
     xlab = "Sea depth (m)",
     main = "",
     col = "#7570b3",
     cex.lab = 1.3)
mean(-dives_depth_f$bath_depth_m[r])
sd(-dives_depth_f$bath_depth_m[r])
median(-dives_depth_f$bath_depth_m[r])




# Dive depth ------
g <- dives_depth_only_f$Species == "u_aalge"
hist(dives_depth_only_f$depth_m_max[g], breaks = 20,
     xlim = c(0,45),
     ylab = "Number of dives",
     xlab = "Dive depth (m)",
     main = "",
     col = "#d95f02",
     cex.lab = 1.3)
mean(dives_depth_only_f$depth_m_max[g])
sd(dives_depth_only_f$depth_m_max[g])
median(dives_depth_only_f$depth_m_max[g])

# max(-dives_depth_f$bath_depth_m[r]
# )

r <- dives_depth_only_f$Species == "A_torda"
hist(dives_depth_only_f$depth_m_max[r], breaks = 20,
     xlim = c(0,45),
     ylab = "Number of dives",
     xlab = "Dive depth (m)",
     main = "",
     col = "#7570b3",
     cex.lab = 1.3)
mean(dives_depth_only_f$depth_m_max[r])
sd(dives_depth_only_f$depth_m_max[r])
median(dives_depth_only_f$depth_m_max[r])






# Combined plot -----------

png("histograms.png",
    width = 12, height = 6,
    res = 300,
    units = "in")
par(mfrow = c(2,4))
# trip distance
g <- tripsf$Species == "u_aalge"
hist(tripsf$nest_dist_max[g], breaks = 20, xlim = c(0,40),
     ylab = "Number of trips",
     xlab = "Maximum distance from nest (km)",
     main = "",
     col = "#d95f02",
     cex.lab = 1.3)
legend("topright", legend=" ", title="A",
       bty='n',
       cex = 1.5)


# Dive distance
g <- dives_loc_f$Species == "u_aalge"
hist(dives_loc_f$nest_dist[g], breaks = 20, xlim = c(0,40),
     ylab = "Number of dives",
     xlab = "Distance from nest (km)",
     main = "",
     col = "#d95f02",
     cex.lab = 1.3)
legend("topright", legend=" ", title="B",
       bty='n',
       cex = 1.5)

# Dive bath
g <- dives_depth_f$Species == "u_aalge"
hist(-dives_depth_f$bath_depth_m[g], breaks = 20,
     xlim = c(0,125),
     ylab = "Number of dives",
     xlab = "Sea depth (m)",
     main = "",
     col = "#d95f02",
     cex.lab = 1.3)
legend("topright", legend=" ", title="C",
       bty='n',
       cex = 1.5)

# Dive depths
g <- dives_depth_only_f$Species == "u_aalge"
hist(dives_depth_only_f$depth_m_max[g], breaks = 20,
     xlim = c(0,45),
     ylab = "Number of dives",
     xlab = "Dive depth (m)",
     main = "",
     col = "#d95f02",
     cex.lab = 1.3)
legend("topright", legend=" ", title="D",
       bty='n',
       cex = 1.5)

# trip distance
r <- tripsf$Species == "A_torda"
hist(tripsf$nest_dist_max[r], breaks = 20, xlim = c(0,40),
     ylab = "Number of trips",
     xlab = "Maximum distance from nest (km)",
     col = "#7570b3",
     main = "",
     cex.lab = 1.3)
legend("topright", legend=" ", title="E",
       bty='n',
       cex = 1.5)

# Dive distance
r <- dives_loc_f$Species == "A_torda"
hist(dives_loc_f$nest_dist[r], breaks = 20, xlim = c(0,40),
     ylab = "Number of dives",
     xlab = "Distance from nest (km)",
     col = "#7570b3",
     main = "",
     cex.lab = 1.3)
legend("topright", legend=" ", title="F",
       bty='n',
       cex = 1.5)

# Dive bath
r <- dives_depth_f$Species == "A_torda"
hist(-dives_depth_f$bath_depth_m[r], breaks = 20,
     xlim = c(0,125),
     ylab = "Number of dives",
     xlab = "Sea depth (m)",
     main = "",
     col = "#7570b3",
     cex.lab = 1.3)
legend("topright", legend=" ", title="G",
       bty='n',
       cex = 1.5)

# Dive depths
r <- dives_depth_only_f$Species == "A_torda"
hist(dives_depth_only_f$depth_m_max[r], breaks = 20,
     xlim = c(0,45),
     ylab = "Number of dives",
     xlab = "Dive depth (m)",
     main = "",
     col = "#7570b3",
     cex.lab = 1.3)
legend("topright", legend=" ", title="H",
       bty='n',
       cex = 1.5)

dev.off()


