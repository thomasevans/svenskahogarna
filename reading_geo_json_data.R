install.packages("geojsonio")


# POssibly required packages
library(rgdal)
library(geojsonio)
library(sp)
library(maps)

# Read in geojson file of coastline data, read as a spatial object
# coastline <- geojson_read("coastline.geojson", what="sp")

# Tips from here https://stackoverflow.com/a/30618775/1172358
library(rgdal)
coast_polygons <- readOGR("coastline.geojson", "OGRGeoJSON", require_geomType="wkbPolygon")

