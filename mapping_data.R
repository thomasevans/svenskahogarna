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

# Data from Openstreetmap at http://overpass-turbo.eu/#
# Querry
# "[out:json]
# [timeout:25]
# ;
# (
#   way
#   ["natural"="coastline"]
#   (58.950008233357,18.536682128906,59.86688319521,20.231323242188);
# );
# out;
# >;
# out skel qt;"


# Tips from here https://stackoverflow.com/a/30618775/1172358
ogrInfo("coastline.geojson", "OGRGeoJSON")
coast_polygons <- readOGR("coastline.geojson", "OGRGeoJSON", require_geomType="wkbPolygon")

coast_lines <- readOGR("coastline.geojson", "OGRGeoJSON", require_geomType="wkbLineString")



# Check how this looks
plot(coast_polygons, col = "black")
plot(coast_lines, col = "red", add = TRUE)
plot(coast_points, col = "blue", add = TRUE)
# ?polygon

# Mapping foraging trips -------

# Filter to only whole foraging trips, and exlude very short and very long movements
hist(trips$nest_dist_max, ylim = c(0,20), breaks = 100)


# Plot base map


# Add foraging trips, drawing with alpha channel in random order
# Different colours for the two species
# Orange - guillemots #d95f02
# purple - razorbills #7570b3

# Also plot dives for foraging trips at the same time (to have same drawing order)

