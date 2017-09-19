# Connect diving and GPs data
# Get closest GPS location for each dive (where there is one)


# Get data -------

# Link to DB
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

# Get dive data
dives <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.datetime_start, dives.datetime_end
FROM dives
                   ORDER BY dives.device_id, dives.diveevent;
                   ")

# Get GPS data
points <- sqlQuery(gps.db, query="SELECT ornitela_gps.device_id, ornitela_gps.UTC_datetime, ornitela_gps.Latitude, ornitela_gps.Longitude, ornitela_gps.speed_km_h
FROM ornitela_gps
ORDER BY ornitela_gps.device_id, ornitela_gps.UTC_datetime;
")


# Fix tz
library(lubridate)

points$UTC_datetime <- force_tz(points$UTC_datetime, tzone = "UTC")
dives$datetime_start <- force_tz(dives$datetime_start, tzone = "UTC")
dives$datetime_end <- force_tz(dives$datetime_end, tzone = "UTC")


# For each dive get GPS -----
library(dplyr)

t600s <- as.difftime(600, units = "secs")

# i <- 19

dive.points <- data.frame(matrix(vector(),0, 9,
                                 dimnames = list(c(),c("device_id",
                            "UTC_datetime",
                            "Latitude",
                            "Longitude",
                            "speed_km_h", 
                            "tdff_start",
                            "tdff_end",
                            "tdff_abs_min",
                            "dive_id"))),
                          stringsAsFactors = FALSE)

for(i in 1:nrow(dives)){
  
  # Get all GPS locations from 600 s before and after dives
  points.x <- filter(points, (device_id == dives$device_id[i]) &
                       (UTC_datetime > dives$datetime_start[i]-t600s) &
                       (UTC_datetime < dives$datetime_end[i]+t600s))
  
  if(nrow(points.x)>0){
    # Calculate time before/ after for all GPS points
    points.x$tdff_start   <- as.numeric(difftime(points.x$UTC_datetime,
                                                 dives$datetime_start[i],
                                                 units = "secs"))
    
    points.x$tdff_end   <- as.numeric(difftime(points.x$UTC_datetime,
                                               dives$datetime_end[i],
                                               units = "secs"))
    
    # Get point with minimum time difference
    points.x$tdff_abs_min <- apply((cbind.data.frame(abs(
      points.x$tdff_start), abs(points.x$tdff_end))),
      1, FUN=min)
    
    # Min point
    points.y <- points.x[which.min(points.x$tdff_abs_min),]
    
    
    # Record
    # -date_time
    # -device_id
    # -dive_id
    # -lat
    # -long
    # -tdiff (before or after with sign)
    # -velocity (instantaneous from GPS)
    
    points.y$dive_id <- dives$diveevent[i]
    # names(points.y)
    dive.points <- rbind.data.frame(dive.points, points.y)
    
  }
  
  
}

# Look at distribution of time differences
hist(dive.points$tdff_abs_min)

# Distribution of speeds
hist(dive.points$speed_km_h, xlim = c(0,50), breaks = 100)


# Output to DB -------
# Check format
str(dive.points)
head(dive.points$UTC_datetime)

# Output
sqlSave(gps.db, dive.points,
        tablename = "dives_gps_loc",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(UTC_datetime = "datetime")
)




