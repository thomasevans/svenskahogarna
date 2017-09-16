# Load in GPS data together with data about nest location
# Calculate distance to nest
# Use to work out whether on foraging trip or not
# Label foraging trips (make unique number IDs)
# Output to table in DB, fields: device, date_time, distance, trip(bool), trip_id


# Connect to DB and load in data -----

library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

points <- sqlQuery(gps.db, query="SELECT deployments.Species, deployments.Ring_number, deployments.nest_lat, deployments.nest_long, ornitela_gps.device_id, ornitela_gps.UTC_datetime, ornitela_gps.satcount, ornitela_gps.hdop, ornitela_gps.Latitude, ornitela_gps.Longitude, ornitela_gps.Altitude_m
FROM deployments INNER JOIN ornitela_gps ON deployments.serial_num = ornitela_gps.device_id
ORDER BY ornitela_gps.device_id, ornitela_gps.UTC_datetime;")

# Fix datetime time zone
library(lubridate)

points$UTC_datetime <- force_tz(points$UTC_datetime, tzone = "UTC")



# Calculate distance to nest -----
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
                    long1 = points$nest_long,
                    lat1 = points$nest_lat,
                    long2 = points$Longitude,
                    lat2 = points$Latitude
)
plot(nest_dist)

# Calculate p2p distances
p2p_dist <- mapply(FUN = deg.dist,
                    long1 = points$Longitude[-1],
                    lat1 = points$Latitude[-1],
                    long2 = points$Longitude[-nrow(points)],
                    lat2 = points$Latitude[-nrow(points)]
)

# View this
hist(p2p_dist[p2p_dist<3], breaks = 100)
p2p_dist_next <- c(p2p_dist, NA)
p2p_dist_last <- c(NA,p2p_dist)

# Time between GPS fixes ------
# Calculate this too
time_diff_s <- as.numeric(difftime(points$UTC_datetime[-1],
                                   points$UTC_datetime[-nrow(points)],
                                   units = "secs"))
time_diff_s_last <- c(NA, time_diff_s)
time_diff_s_next <- c(time_diff_s, NA)


# Do something for first (and final?) location for each device ------
# Vector labelling first locations
first_loc <- rep(FALSE, nrow(points))
first_loc[1] <- TRUE
for(i in 2:nrow(points)){
  if(points$device_id[i] != points$device_id[i-1])first_loc[i] <- TRUE
}

# Vector labelling final location
final_loc <- rep(FALSE, nrow(points))
final_loc[nrow(points)] <- TRUE
for(i in 1:(nrow(points)-1)){
  if(points$device_id[i] != points$device_id[i+1])final_loc[i] <- TRUE
}


# Use to add NAs where they should be for p2p dist, time diff etc
p2p_dist_next[final_loc] <- NA
p2p_dist_last[first_loc] <- NA
time_diff_s_last[first_loc] <- NA
time_diff_s_next[final_loc] <- NA


# Combine variables to dataframe -------
out.df <- cbind.data.frame(points$device_id,
                           points$UTC_datetime,
                           nest_dist,
                           p2p_dist_last,
                           p2p_dist_next,
                           time_diff_s_last,
                           time_diff_s_next)
# Fix names
names(out.df)[1:2] <- c("device_id", "UTC_datetime")

# out.df$UTC_datetime[1]

# Foraging trip labelling -----
# Use to work out whether on foraging trip or not

# Choose threshold
hist(nest_dist[nest_dist < 5], breaks = 100)
hist(nest_dist[nest_dist < 1], breaks = 100)
abline(v=0.3)
summary(nest_dist < 0.3)
# Is it more than 300 m from the nest?
out.df$on_trip <- nest_dist > 0.3

# Label foraging trips (make unique number IDs)
state <- FALSE
out.df$trip_id <- rep(NA, nrow(points))
trip_n <- 0

# Check first point
if(out.df$on_trip[1]){out.df$trip_id[1] <- 1
trip_n <- 1
state <- TRUE}

# For rest of points do this
for(i in 2:nrow(points)){
  if(out.df$on_trip[i]){
    if(state & (out.df$device_id[i] == out.df$device_id[i-1])){
      out.df$trip_id[i] <- trip_n}else{
        out.df$trip_id[i] <- trip_n + 1
        trip_n <- trip_n + 1
        state <- TRUE
      }
    }else{state <- FALSE}
}


# Plots to check how this looks -----

i <- 1

pdf("trips_check_300m.pdf", paper = "a4r")
n <- 1
devices <- unique(points$device_id)
for(i in 1:length(devices)){
  a <- out.df$device_id == devices[i]

  start.time <- min(out.df$UTC_datetime[a])
  end.time <- max(out.df$UTC_datetime[a])
  tot_time <- as.numeric(difftime(end.time, start.time, units = "days"))
  tot_time_round <- ceiling(tot_time)
  c <- start.time
  for(ix in 1:ceiling(tot_time_round/4)){
    day <- as.difftime(1, units = "days")
    b <- a & (out.df$UTC_datetime > c) &
      (out.df$UTC_datetime < (c + 4*day))
    # sum(b)
    if(sum(b)>0){
      
      plot(out.df$UTC_datetime[b], out.df$nest_dist[b],
           # type = "b", col = "black")
           # ylim = c(-5, 0.5),
           main = paste("Device: ", devices[i], "  start time:", c,
                        "\n plot", n),
           type = "b", col = "black",
           cex = 0.7
      )
      points(out.df$UTC_datetime[b], out.df$nest_dist[b],
             # type = "b", col = "black")
             # ylim = c(-5, 0.5),
             # main = paste("Device: ", devices[i], "  start time:", c,
                          # "\n plot", n),
             # type = "b", 
             col = as.numeric(out.df$trip_id[b]),
             cex = 0.7
        
      )
      abline(h = c(0, 0.3), lty = c(1, 2))
      abline(h = c(0.15), lty = c(2), col = "red")
      
      
      n <- n+1
      
    }
    
    c <- c + 4*day
    
  }
  
  
}
dev.off()


a <- filter(out.df, (device_id == "17966") &
         (UTC_datetime > as.POSIXct("2017-08-06 00:00", tz = "UTC"))&
         (UTC_datetime < as.POSIXct("2017-08-07 00:00", tz = "UTC"))&
         nest_dist > 20)
a[2]
str(points)

# points to exclude (bad GPS) -----
out.df$keep_points <- TRUE
out.df$keep_points[(out.df$device_id == "17966") &
                     (out.df$UTC_datetime == as.POSIXct("2017-08-06 13:20:17", tz = "UTC"))] <- FALSE


# Output to DB ------

# Output to table in DB, fields: device, date_time, distance, trip(bool), trip_id
str(out.df)

# Raw data
sqlSave(gps.db, out.df,
        tablename = "gps_trips",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(UTC_datetime = "datetime")
)
