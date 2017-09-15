# Parse Ornitela GPS + dive sensor data
# 
# filename <- paste(
#   "D:/Dropbox/Svenska_Högarna/data/",
#   "17955_20170903_082900.csv", sep = "")
# device.name <- "17955"
# Wrap in function
parse.file <- function(filename, device.name){
  
  # Required packages -----
  library(dplyr)
  
  # Read in raw data file ----
  rawdat <- read.csv(filename,
                     header = TRUE)
  
  
  
  # # Look for duplicates
  # x <- duplicated.data.frame(rawdat)
  # summary(x)
  # rawdat.x <- rawdat[x,]
  # 
  # rawdat2 <- rawdat[-c(27888:29943),]
  # x <- duplicated.data.frame(rawdat2)
  # summary(x)
  # rawdat.x <- rawdat2[x,]
  # all.equal(rawdat2,rawdat3)
  
  
  # Remove duplicated lines
  rawdat <- rawdat[!duplicated.data.frame(rawdat),]
  # summary(duplicated.data.frame(rawdat))
  
  # plot(rawdat2$UTC_datetime)

  # If no data
  if(nrow(rawdat) == 0){
    out.list <- list()
    
    out.list[[1]] <- NULL
    out.list[[2]] <- NULL
    out.list[[3]] <- NULL
    out.list[[4]] <- NULL
    details <- cbind.data.frame(device = device.name,
                                alldata = 0,
                                gpsdata = 0,
                                divedata = 0)
    out.list[[5]] <- details
    
  }else{
    # Fix data structure ----
    # str(rawdat)
    
    # Drop sepparate date and time columns
    rawdat <- rawdat %>% select(-one_of(c("UTC_date", "UTC_time")))
    
    # Fix datetime structure
    # str(rawdat)
    rawdat$UTC_datetime <- as.POSIXct(rawdat$UTC_datetime, tz = "UTC")
    
    
    # Remove nonsense data -------
    rawdat.original <- rawdat
    f <-  (rawdat$depth_m<50) &  (rawdat$Latitude != 0)
    # summary(f)
    f[is.na(f)] <- TRUE
    rawdat <- rawdat[f,]
    
    
    # Sepperate out GPS and dive data ----
    gpsdat <- filter(rawdat, datatype == "GPS")
    # str(gpsdat)
    
    divedat <- filter(rawdat, datatype == "SENSORS")
    # Drop columns with GPS data fields
    divedat <- divedat[,-c(4:13)]
    
    
    # Label dive events and give correct datatime for each record ----
    recs <- nrow(divedat)
    
    # # Number timeblocks (records sharing the same date-time stamps)
    # divedat$timeblock <- NULL
    # divedat$block_rec <- NULL
    # r <- 1
    # rec <- 1
    # divedat$timeblock[1] <- r
    # divedat$block_rec[1] <- rec
    # # Number records within timeblocks
    # for(i in 2:nrow(divedat)){
    #   if(divedat$UTC_datetime[i] == divedat$UTC_datetime[i-1]){
    #     divedat$timeblock[i] <- r
    #     divedat$block_rec[i] <- rec
    #     rec <- rec +1
    #     } else{
    #     r <- r + 1
    #     rec <- 1
    #     divedat$timeblock[i] <- r
    #     divedat$block_rec[i] <- rec
    #     rec <- rec +1
    #     }
    #   }
    
    
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
                (divedat$UTC_datetime[i] == (divedat$UTC_datetime[i-1]+3))|
                (divedat$UTC_datetime[i] == (divedat$UTC_datetime_new[i-1]))|
                (divedat$UTC_datetime[i] == (divedat$UTC_datetime_new[i-2]))|
                 (divedat$UTC_datetime[i] == (divedat$UTC_datetime_new[i-1]+
                                              as.difftime(1, units = "secs")))|
                 (divedat$UTC_datetime[i] == (divedat$UTC_datetime_new[i-1]+
                                              as.difftime(2, units = "secs")))|
                  ((as.numeric(difftime(divedat$UTC_datetime[i], (divedat$UTC_datetime_new[i-1]))) < 15) &
                   (divedat$depth_m[i] > 5) &
                   (abs(divedat$depth_m[i]-divedat$depth_m[i-1]) < 2)
                  )|
                  ((as.numeric(difftime(divedat$UTC_datetime[i], (divedat$UTC_datetime_new[i-1]))) < 10) &
                   (divedat$depth_m[i] > 1.5) &
                   (abs(divedat$depth_m[i]-divedat$depth_m[i-1]) < 0.5)
                  )
                  
                  
                  ){
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
    
    # hist(divedat$block_rec, breaks = 50)
    
    # # Plotting data
    # a <- c(3000:3300)
    # # a <- divedat$diveevent == 549
    # a <- c(650:900)
    # plot(divedat$UTC_datetime_new[a], -divedat$depth_m[a],
    #      # type = "b", col = "black")
    #      type = "b", col = as.numeric(divedat$diveevent[a]))

    # divedat$delta_depth <- c(0, divedat$depth_m[-1]-divedat$depth_m[-nrow(divedat)])
    
    # Drop extra column
    divedat <- divedat %>% select(-one_of(c("X")))
    
    
    # Tidy GPS DF -----
    # str(gpsdat)
    gpsdat <- gpsdat %>% select(-one_of(c("depth_m","X")))
    
    
    out.list <- list()
    
    out.list[[1]] <- rawdat
    out.list[[2]] <- gpsdat
    out.list[[3]] <- divedat
    details <- cbind.data.frame(device = device.name,
                                alldata = nrow(rawdat),
                                gpsdata = nrow(gpsdat),
                                divedata = nrow(divedat))
    out.list[[4]] <- rawdat.original
    out.list[[5]] <- details
  }
  
  
  return(out.list)
}




