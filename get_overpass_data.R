# Get coastline data from Overpass from openstreetmap

# Install package to get data
devtools::install_github("hrbrmstr/overpass")

# Run package
library("overpass")

# Save querry
# To interactively make query see http://overpass-turbo.eu/
coastline_q <- '[out:json]
[timeout:25]
;
(
  way
  ["natural"="coastline"]
  (58.950008233357,18.636682128906,59.86688319521,20.231323242188);
);
  out;
  >;
  out skel qt;'
  
  coastline <- overpass_query(coastline_q)
  
  # Fails with warning about available memory, not sure how to resolve this
  
  
  str(coastline)
  
  gg <- ggplot()
  gg <- gg + geom_path(data=fortify(frb), 
                       aes(x=long, y=lat, group=group),
                       color="black", size=0.25)
  gg <- gg + coord_quickmap()
  gg <- gg + ggthemes::theme_map()
  gg
  
  