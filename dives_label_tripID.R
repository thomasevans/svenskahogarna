# Label dives with the trip to which they belong


# Load in dive and trip data -----

# Link to dB
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')


# Read in dive data (ID, device_info_serial, start_time sufficient)
dives <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.datetime_start
FROM dives
ORDER BY dives.device_id, dives.diveevent;
")



# Read in trip data (ID, device_info_serial, start_time, end_time sufficient)
trips <- sqlQuery(gps.db, query="SELECT trips.device_id, trips.trip_id, trips.datetime_start, trips.datetime_end
FROM trips
ORDER BY trips.device_id, trips.trip_id;
")

# Check structure
str(dives)
str(trips)

head(dives$datetime_start)

head(trips$datetime_start)
head(trips$datetime_end)

# Fix tz
library(lubridate)

dives$datetime_start <- force_tz(dives$datetime_start, tzone = "UTC")
trips$datetime_start <- force_tz(trips$datetime_start, tzone = "UTC")
trips$datetime_end <- force_tz(trips$datetime_end, tzone = "UTC")


# For each dive ------
library("dplyr")

# i <- 1001

trip_ids <- rep(NA, nrow(dives))
errors <- rep(NA, nrow(dives))
for(i in 1:nrow(dives)){
  # Find trip to which they belong
  # dplyr::filter ???

  x <-    filter(trips, device_id == dives$device_id[i] &
                               datetime_start <= dives$datetime_start[i] &
                               datetime_end >= dives$datetime_start[i])
  # x[,2]
  # Record whether any dives turn out to belong to more than one trip
  # Shouldn't be true, but check.
  errors[i] <- nrow(x) > 1
  trip_ids[i] <- x[1,2]
}


# Check this
summary(errors)
# None

summary(is.na(trip_ids))
# A number not in a trip

trip_ids[1:1000]



# Summarise to table and output to DB --------
dives.out <- data.frame(diveevent = dives$diveevent,
                        device_id = dives$device_id,
                        trip_id = trip_ids)


# Output
sqlSave(gps.db, dives.out,
        tablename = "dives_trip_id",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL
)

