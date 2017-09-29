# Summarise trip information for dive activity and time-activity budgets

# Load in data ------
# Link to DB
# Link to dB
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

# Load trip info
trips <- sqlQuery(gps.db, query="SELECT trips.device_id, trips.trip_id, trips.datetime_start, trips.datetime_end, trips.duration_s, trips.duration_h
FROM trips
ORDER BY trips.device_id, trips.trip_id;")

# Load dive info
dives <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.duration_s, dives.depth_m_max, dives_trip_id.trip_id
FROM dives INNER JOIN dives_trip_id ON dives.diveevent = dives_trip_id.diveevent
ORDER BY dives.device_id, dives.diveevent;")

# As above but with bathymetry data too
dives2 <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.duration_s, dives.depth_m_max, dives_trip_id.trip_id, dives_gps_loc_bath.bath_depth_m
FROM dives_gps_loc_bath INNER JOIN (dives INNER JOIN dives_trip_id ON dives.diveevent = dives_trip_id.diveevent) ON dives_gps_loc_bath.dive_id = dives_trip_id.diveevent
ORDER BY dives.device_id, dives.diveevent;")



# Summarise trip info including dive data ------
library("plyr")
library("dplyr")


# hist(dives$depth_m_max, xlim = c(0,10),
     # breaks = 100)

# Use summarise to summarise dive data on trip basis
trip.summary <- ddply(dives, .(device_id, trip_id),
                      summarise,
                      
                      # n dives
                      dives_n = length(depth_m_max),
                      dives_n_over_1m = dives_n-(sum(depth_m_max<1)),
                      
                      # Depths
                      depth_m_maxi = max(depth_m_max),
                      depth_m_median = median(depth_m_max),
                      depth_m_mean = mean(depth_m_max),
                      
                      # time diving
                      time_diving_sum = sum(duration_s),
                      
                      # Mean dive duration
                      dive_duration_mean = mean(duration_s),
                      dive_duration_median = median(duration_s)
                      
                      
                      
)


# Get bathymetry summary too
# Use summarise to summarise dive data on trip basis
trip.summary2 <- ddply(dives2, .(device_id, trip_id),
                      summarise,
                      
                      # n dives
                      dives_n_bath = length(depth_m_max),
                      # dives_n_over_1m = dives_n-(sum(depth_m_max<1)),
                    
                      bath_m_mean = mean(bath_depth_m),
                      bath_m_max  = max(bath_depth_m),
                      bath_m_median = median(bath_depth_m)
)



# Combine with trip info
trip.out <- merge(trip.summary, trip.summary2)

trip.out <- filter(trip.out, !is.na(trip_id))


# Make summary table and output to DB --------
# trip.out


# Output
sqlSave(gps.db, trip.out,
        tablename = "trips_dive_info",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL
)
