# Calculations on dive depth and bathymetry data

# Read in data ------


# Link to DB
library("RODBC")

gps.db <- odbcConnectAccess2007('D:/Dropbox/Svenska_Högarna/db/svenskahogarna.accdb')

# Get dive data
dives <- sqlQuery(gps.db, query="SELECT dives.device_id, dives.diveevent, dives.depth_m_max, dives_gps_loc_bath.bath_depth_m, dives_gps_loc.speed_km_h, dives_gps_loc.tdff_abs_min
FROM dives_gps_loc INNER JOIN (dives INNER JOIN dives_gps_loc_bath ON dives.diveevent = dives_gps_loc_bath.dive_id) ON dives_gps_loc.dive_id = dives_gps_loc_bath.dive_id
                  ORDER BY dives.device_id, dives.diveevent;
                   ")



# Calculate differences between dive depth and bath depth ------

# Height above sea bottom
# As number
dives$above_bottom_m <- -dives$bath_depth_m-dives$depth_m_max
hist(dives$above_bottom_m)

# As a proportion
dives$above_bottom_prop <- dives$depth_m_max/(-dives$bath_depth_m)
hist(dives$above_bottom_prop[dives$above_bottom_prop > -2 &
                               dives$above_bottom_prop < 2])

# Make some plots of this ------
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

bath_depth <- -dives$bath_depth_m
plot(-dives$depth_m_max~bath_depth, col = add.alpha("black", 0.1),
     xlim = c(0,100))
polygon(c(0,300,300), c(0,-300,0), col = "light blue")
polygon(c(0,0,300), c(0,-300,-300), col = "light yellow")
points(-dives$depth_m_max~bath_depth, col = add.alpha("black", 0.1))
box()
# abline(a = 0, b = -1, col = "red", lwd = 2, lty = 2)

plot(-dives$above_bottom_prop~bath_depth, col = add.alpha("black", 0.1),
     xlim = c(0,100), ylim = c(-2,0.5))



# ?abline
# dev.off()

# Summarise and output to DB ------
dives.out <- dives[,c(1,2,7,8)]

str(dives.out)

# Output
sqlSave(gps.db, dives.out,
        tablename = "dives_gps_loc_bath_depth_dif",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL
)

