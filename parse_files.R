# Wrapper to parse all ornitela files from a directory



# First list files from directory matching some file naming criteria
files <- list.files(path = 
                      "D:/Dropbox/Svenska_Högarna/data/",
                    pattern = "..csv",
                    all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE)


# View what file are found
files

# Vector of device IDs
fun.wrap <- function(x){
  # strsplit(x, split = ".txt" )[[1]][1]
  substr(x, 1, 5)
}

# What devices are represented?
devices <- sapply(X = files, FUN = fun.wrap)
names(devices) <- NULL




# Parse all files
n <- length(files)

parse.list <- list()
# i <- 1
x <- NULL
for(i in 1:n){
  x <- parse.file(paste(
    "D:/Dropbox/Svenska_Högarna/data/",
    files[i], sep = ""), devices[i])
  # x <- cbind(x,devices[i],files[i])
  parse.list[[i]] <- x
}

# str(parse.list[[1]][[1]])
# str(parse.list[[1]][[2]])
# str(parse.list[[1]][[3]])
# str(parse.list[[1]][[4]])


# Recombine data ----
# Combine all raw data to single data frame
rawdata.df <- parse.list[[1]][[1]]
for(i in 2:22){
  if(!is.null(parse.list[[i]][[1]])){
    rawdata.df <- rbind.data.frame(rawdata.df, parse.list[[i]][[1]])
    }
}


# Combine all GPS data to single data frame
GPS.df <- parse.list[[1]][[2]]
for(i in 2:22){
  if(!is.null(parse.list[[i]][[2]])){
    GPS.df <- rbind.data.frame(GPS.df, parse.list[[i]][[2]])
  }
}


# Combine all dive data to single data frame
dives.df <- parse.list[[1]][[3]]
for(i in 2:22){
  if(!is.null(parse.list[[i]][[3]])){
    dives.df <- rbind.data.frame(dives.df, parse.list[[i]][[3]])
  }
}

# Combine all info data to single data frame
info.df <- parse.list[[1]][[4]]
for(i in 2:22){
  if(!is.null(parse.list[[i]][[4]])){
    info.df <- rbind.data.frame(info.df, parse.list[[i]][[4]])
  }
}



# Plotting data
a <- c(700700:700900)
# a <- dives.df$diveevent == 549
plot(dives.df$UTC_datetime_new[a], -dives.df$depth_m[a],
     # type = "b", col = "black")
     type = "b", col = as.numeric(dives.df$diveevent[a]))
# files


# Make unique dive IDs -----
diven <- 0
for(i in 1:22){
  # add max number of previous device
  if(sum(dives.df$device_id == devices[i])>0){
    dives.df$diveevent[dives.df$device_id == devices[i]] <- 
      dives.df$diveevent[dives.df$device_id == devices[i]] + diven
    diven <- max(dives.df$diveevent[dives.df$device_id == devices[i]])
    
  }
 
}

max(dives.df$diveevent)


# Output tables to database ------
names(info.df)[1] <- "device_id"
info.df$device_id <- as.integer(as.character(info.df$device_id))


# Link to DB
# Write to database

library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

str(rawdata.df)

# Raw data
sqlSave(gps.db, rawdata.df,
        tablename = "ornitela_raw",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(UTC_datetime = "datetime")
)

# GPS data
str(GPS.df)
sqlSave(gps.db, GPS.df,
        tablename = "ornitela_gps",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(UTC_datetime = "datetime")
)


# Dive data
str(dives.df)
sqlSave(gps.db, dives.df,
        tablename = "ornitela_dives",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(UTC_datetime = "datetime",
                      UTC_datetime_new = "datetime")
)

# Deployment data
str(info.df)
sqlSave(gps.db, info.df,
        tablename = "ornitela_data_recorded",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL
)


# x <- duplicated.data.frame(rawdata.df)
# summary(x)
# x2 <- rawdata.df[x,]
# 
# 
# x <- rawdata.df$mag_x == 931
# summary(x)
# x2 <- rawdata.df[x,]
# 
# rawdata.df.x <- rawdata.df[(duplicated(rawdata.df) | duplicated(rawdata.df, fromLast = TRUE)), ]
# summary(as.factor(rawdata.df.x[,1]))
# 
# 
# 
# hist(rawdata.df$depth_m)
# rawdata.df.x <- rawdata.df[(!is.na(rawdata.df$depth_m) & (rawdata.df$depth_m<50)),]
# hist(rawdata.df.x$depth_m)
# 
# 
# # Exclude depths >50 m
# # No dives over this, only false values
# summary(rawdata.df$Latitude == 0)
# summary(rawdata.df$Longitude == 0)
# 
# f <-  (rawdata.df$depth_m<50) &  (rawdata.df$Latitude != 0)
# summary(f)
# f[is.na(f)] <- TRUE
# rawdata.df.f <- rawdata.df[f,]
# 
# 
# x <- duplicated.data.frame(rawdata.df.f)
# summary(x)
# x2 <- rawdata.df.f[x,]
# summary(as.factor(x2$device_id))
# 
# files
