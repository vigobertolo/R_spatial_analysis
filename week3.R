library(tidyverse)
library(sf)
path.counties <- "./County_Boundaries.shp"
path.stations <- "./Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
data.counties <- sf::read_sf(path.counties)
data.stations <- sf::read_sf(path.stations)
data.counties
glimpse(data.counties)
del.counties <- data.counties %>% dplyr::filter(STATEFP10 == 10)
sf::st_crs(data.counties)
sf::st_crs(data.stations)
sf::st_crs(data.counties) == sf::st_crs(data.stations)
1+1
2+2
