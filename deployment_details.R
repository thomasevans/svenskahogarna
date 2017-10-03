# Sample size summaries

# Load in data -------
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

# GPS points -----


points <- sqlQuery(gps.db, query="SELECT ornitela_gps.device_id, ornitela_gps.UTC_datetime, ornitela_gps.Latitude, ornitela_gps.Longitude, gps_trips.nest_dist, gps_trips.time_diff_s_last, gps_trips.on_trip, gps_trips.trip_id
FROM ornitela_gps INNER JOIN gps_trips ON (ornitela_gps.UTC_datetime = gps_trips.UTC_datetime) AND (ornitela_gps.device_id = gps_trips.device_id)
ORDER BY ornitela_gps.device_id, ornitela_gps.UTC_datetime;
")

# Dives -----

dives <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.datetime_start, dives.duration_s, dives.depth_m_max, dives_gps_loc_bath.bath_depth_m, dives_trip_id.trip_id
FROM (((dives INNER JOIN dives_gps_loc_bath ON dives.diveevent = dives_gps_loc_bath.dive_id) INNER JOIN dives_gps_loc_bath_depth_dif ON dives_gps_loc_bath.dive_id = dives_gps_loc_bath_depth_dif.diveevent) INNER JOIN dives_trip_id ON dives_gps_loc_bath_depth_dif.diveevent = dives_trip_id.diveevent) INNER JOIN dives_gps_loc ON dives_trip_id.diveevent = dives_gps_loc.dive_id
ORDER BY dives.device_id, dives.diveevent;
")

# Trips ----

trips <- sqlQuery(gps.db, query="SELECT trips_dive_info.device_id, trips_dive_info.trip_id, trips_dive_info.dives_n, trips_time_activity.flight_p, trips_time_activity.dive_p, trips_time_activity.other_p, trips.n_points, trips.datetime_start, trips.duration_s, trips.interval_s_max, trips.interval_s_median, trips.nest_dist_max, trips.nest_dist_first, trips.nest_dist_last
FROM (trips_dive_info INNER JOIN trips_time_activity ON trips_dive_info.trip_id = trips_time_activity.trip_id) INNER JOIN trips ON trips_time_activity.trip_id = trips.trip_id
ORDER BY trips_dive_info.device_id, trips_dive_info.trip_id;
")

# Deployments ------

deployments <- sqlQuery(gps.db, query="SELECT deployments.*
FROM deployments;
")

# Summarise each ----
library(plyr)
library(dplyr)

# Dives -----
# Filter out those <1 m
divesf <- filter(dives, depth_m_max >1)

# Summarise
dive.summary <- ddply(divesf, .(device_id),
                      summarise,
                      # N dives
                      dives_n = n(),
                      
                      # Dive depths
                      dive_max_depth = max(depth_m_max),
                      dive_mean_depth = mean(depth_m_max),
                      dive_median_depth = median(depth_m_max),
                      dive_sd_depth = sd(depth_m_max),
                      
                      # Dive depths
                      dive_max_duration = max(duration_s),
                      dive_mean_duration = mean(duration_s),
                      dive_median_duration = median(duration_s),
                      dive_sd_duration = sd(depth_m_max)
)

# Trips ------
# *Exclude trips with max distance <1 km & >40 km
tripsf <- filter(trips, trips$nest_dist_max>1 & trips$nest_dist_max<40)

# Summarise
trip.summary <- ddply(tripsf, .(device_id),
                      summarise,
                      # N trips
                      trips_n = n(),
                      
                      trips_n_pointspertrip_mean = mean(n_points),
                      trips_n_pointspertrip_median = median(n_points),
                      
                      
                      # foraging range
                      trip_range_max = max(nest_dist_max),
                      trip_range_mean = mean(nest_dist_max),
                      trip_range_median = median(nest_dist_max),
                      trip_range_sd = sd(nest_dist_max),
                      
                      # trip duration
                      trip_duration_max = max(duration_s),
                      trip_duration_mean = mean(duration_s),
                      trip_duration_median = median(duration_s),
                      trip_duration_sd = sd(duration_s),
                      
                      # Recording interval
                      trip_time_interval_meanofmedian = mean(interval_s_median),
                      trip_time_interval_meanofmax = mean(interval_s_max)
                     
)

# GPS locations -------
on_trip_points <- points$trip_id %in% tripsf$trip_id
points$on_trip_new <- on_trip_points

# Summarise
points.summary <- ddply(points, .(device_id),
                      summarise,
                      
                      # N points
                      gps_points_n = n(),
                      
                      # N points on trip
                      gps_points_trip_n = sum(on_trip_new),
                      
                      gps_points_1st = first(UTC_datetime),
                      gps_points_last = last(UTC_datetime),
                      gps_time_total = as.numeric(difftime(gps_points_last,
                                          gps_points_1st,
                                          units = "days"))
                      
)



# Combine summary tables ------
names(deployments)[3] <- "device_id"


all_summary <-  merge(deployments,
                      dive.summary
                      )
all_summary <-  merge(all_summary,
                      trip.summary
)

all_summary <-  merge(all_summary,
                      points.summary
)
# ?merge


# Output to file
write.csv(all_summary, file = "deployment_summaries.csv")

