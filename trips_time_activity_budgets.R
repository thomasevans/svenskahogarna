# Calculate time-activity budgets


# Read in data ------
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

# Trip summary data
trips <- sqlQuery(gps.db, query="SELECT trips.device_id, trips.trip_id, trips.duration_s, trips.flight_time_s_total, trips_dive_info.time_diving_sum
FROM trips INNER JOIN trips_dive_info ON trips.trip_id = trips_dive_info.trip_id;")


# Summarise by trip -------
# Use dplyr::summarise
library(plyr)
library(dplyr)




trip.activity <- ddply(trips, .(device_id, trip_id),
                      summarise,
                      
                      flight_p = flight_time_s_total/
                        duration_s,
                      dive_p = time_diving_sum/
                        duration_s,
                      other_p = 1-dive_p-flight_p
                      
                      
)

# Output to database -------
str(trip.activity)

# Output
sqlSave(gps.db, trip.activity,
        tablename = "trips_time_activity",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL
)


