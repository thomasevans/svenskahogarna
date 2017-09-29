# Summarise foraging trip info from GPS data 


# Load in GPS data from database -------

library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

points <- sqlQuery(gps.db, query="SELECT gps_trips.*, ornitela_gps.Latitude, ornitela_gps.Longitude, ornitela_gps.speed_km_h
FROM gps_trips INNER JOIN ornitela_gps ON (gps_trips.device_id = ornitela_gps.device_id) AND (gps_trips.UTC_datetime = ornitela_gps.UTC_datetime)
                   ORDER BY gps_trips.device_id, gps_trips.UTC_datetime;
                   ")


# Check datetime format etc (including timezone)
str(points)
head(points$UTC_datetime)

# Fix tz
library(lubridate)

points$UTC_datetime <- force_tz(points$UTC_datetime, tzone = "UTC")




library(plyr)
library(dplyr)

# Remove points that are not on a trip
points2 <- filter(points, !is.na(trip_id))



# Inspect speed data to decide on speed threshold for flight ----
hist(points2$speed_km_h, breaks = 100,
     xlim = c(3,100), ylim = c(0,100))
abline(v=18, col = "red")
# summary(points$speed_km_h >30)

ms_speed <- points2$speed_km_h*1000/60/60
hist(ms_speed, breaks = 100, xlim = c(1,25),
     ylim = c(0,100))

5/1000*60*60

# 5 ms-1, or 18 kph looks good.


# Summarise trips ------
# Use plyr::summarise



trip.summary <- ddply(points2, .(device_id, trip_id),
                      summarise,
                      
                      # Data
                      n_points = n(),
                      
                      # Times
                      datetime_start = first(UTC_datetime),
                      datetime_end = last(UTC_datetime),
                      duration_s = as.numeric(difftime(datetime_end,
                                                       datetime_start,
                                                       units = "secs")),
                      duration_h = as.numeric(difftime(datetime_end,
                                                       datetime_start,
                                                       units = "hours")),
                      flight_points_n = sum(speed_km_h[-1]>18),
                      flight_time_s_total = sum(
                        time_diff_s_last[-1][speed_km_h[-1]>18]),
                      
                      
                      
                      
                      interval_s_first = first(time_diff_s_last),
                      interval_s_last  = last(time_diff_s_next),
                      interval_s_min = min(time_diff_s_last),
                      interval_s_max = max(time_diff_s_last),
                      interval_s_median = median(time_diff_s_last),
                      interval_s_mean = mean(time_diff_s_last),
                      
                      # Distances
                      p2pdist_sum = sum(p2p_dist_last) - first(p2p_dist_last),
                      nest_dist_max = max(nest_dist),
                      nest_dist_median = median(nest_dist),
                      nest_dist_mean = mean(nest_dist),
                      
                      nest_dist_first = first(nest_dist),
                      nest_dist_last = last(nest_dist),
                      
                      lat_most_dist = Latitude[nest_dist == nest_dist_max][1],
                      long_most_dist = Longitude[nest_dist == nest_dist_max][1]
)

str(trip.summary)

# Look at some of this data
hist(trip.summary$nest_dist_max, breaks = 50)
hist(trip.summary$duration_h/24, breaks = 50)

hist(trip.summary$flight_time_s_total/60/60,
     breaks = 100,
     xlim = c(0,2))
hist(trip.summary$flight_time_s_total/
       trip.summary$duration_s, breaks = 20,
     ylim = c(0,70))

# Output to database ------

# Check format
str(trip.summary)
head(trip.summary$datetime_end)

# Output
sqlSave(gps.db, trip.summary,
        tablename = "trips",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(datetime_start = "datetime",
                      datetime_end = "datetime")
)




