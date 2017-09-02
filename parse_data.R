# Parse Ornitela GPS + dive sensor data

# Required packages -----
library(dplyr)

# Read in raw data file ----
rawdat <- read.csv("D:/Dropbox/Svenska_Högarna/data/17966_20170902_104000.csv",
                   header = TRUE)

# Fix data structure ----
str(rawdat)

# Drop sepparate date and time columns
rawdat <- rawdat %>% select(-one_of(c("UTC_date", "UTC_time")))

# Fix datetime structure
str(rawdat)
rawdat$UTC_datetime <- as.POSIXct(rawdat$UTC_datetime, tz = "UTC")


# Sepperate out GPS and dive data ----
gpsdat <- filter(rawdat, datatype == "GPS")
str(gpsdat)

divedat <- filter(rawdat, datatype == "SENSORS")
# Drop columns with GPS data fields
divedat <- divedat[,-c(4:13)]


# Label dive events and give correct datatime for each record ----
recs <- nrow(divedat)

# Initialisation
divedat$diveevent <- NULL
divedat$UTC_datetime_new  <- NULL
diveevent <- 0
divedatetime <- 0

# First record of first dive
divedat$diveevent[1] <- 1
divedat$UTC_datetime_new  <- divedat$UTC_datetime[1]
diveevent <- 1
divedatetime <- divedat$UTC_datetime[1]
divedatetimec <- divedat$UTC_datetime[1]
# i <- 2

for(i in 2:recs){
  if(divedat$UTC_datetime[i] == divedatetime){
    # If same block of current dive do this
    divedat$diveevent[i] <- diveevent
    divedat$UTC_datetime_new[i] <- divedatetimec + 1
    divedatetimec <- divedat$UTC_datetime_new[i]
    
  } else if((divedat$UTC_datetime[i] == (divedat$UTC_datetime[i-1]+4))|
            (divedat$UTC_datetime[i] == (divedat$UTC_datetime[i-1]+3))){
    # If new block of current dive do this
    divedat$diveevent[i] <- diveevent
    divedat$UTC_datetime_new[i] <- divedatetimec + 1
    divedatetimec <- divedat$UTC_datetime_new[i]
    divedatetime <- divedat$UTC_datetime[i]
  } else {
    # If new dive event do this
    diveevent <- diveevent+1
    divedatetime <- divedat$UTC_datetime[i]
    divedatetimec <- divedat$UTC_datetime[i]
    divedat$diveevent[i] <- diveevent
    divedat$UTC_datetime_new[i] <- divedatetimec
    
  }
}

# Plotting data
a <- c(1800:2200)
a <- divedat$diveevent == 549
plot(divedat$UTC_datetime_new[a], -divedat$depth_m[a],
     # type = "b", col = "black")
     type = "b", col = as.numeric(divedat$diveevent[a]))

# Drop extra column
divedat <- divedat %>% select(-one_of(c("X")))


# Tidy GPS DF -----
str(gpsdat)
gpsdat <- gpsdat %>% select(-one_of(c("depth_m","X")))


