library(osmdata)
library(tidyverse)
library(leaflet)



geodesic_distance_meters <- function(long1, lat1, long2, lat2) {
  # Computes the geodesic distance between two coordinate vectors.
  # geodesic_distance(-99.113231, 19.296341, -99.110213, 19.295771)
  # geodesic_distance(-99.110712, 19.295860, -99.113153, 19.296328)
  # geodesic_distance(c(-99.113231,-99.110712),
  #                   c(19.296341, 19.295860),
  #                   c(-99.110213, -99.113153),
  #                   c(19.295771, 19.296328))
  n1 <- length(long1)
  n2 <- length(long2)
  n3 <- length(lat1)
  n4 <- length(lat2)
  bool <- n1 != n2 | n1 != n3 | n1 != n4 | n2 != n3 | n2 != n4 | n3 != n4
  if(bool) stop("Las entradas no tienen la misma longitud")
  else{
    long1 <- long1*pi/180
    lat1 <- lat1*pi/180
    long2 <- long2*pi/180
    lat2 <- lat2*pi/180
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    c <- 2 * asin(pmin(sqrt(a), rep(1, n1)))
    d = 6371 * c # 6371 is Earth's mean radius in Km
    return(1000*d) # Distance in meters
  }
}


osmdf <- opq("leuven belgium") %>%
  add_osm_feature (key = "highway", value = "!primary") %>%
  add_osm_feature(key = "surface", 
                  value = c("sett", "unhewn_cobblestone", "cobblestone")) %>%
  osmdata_sf()



roads <- osmdf$osm_lines

roads %>%
  leaflet() %>% 
  addTiles() %>% 
  addPolylines()


test_road <- roads %>% filter(osm_id == 3328699)
test_road$geometry


test_road_coords = as.matrix(test_road$geometry$`3328699`) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble()


geodesic_distance_meters(
  test_road_coords$lon[1], 
  test_road_coords$lat[1], 
  test_road_coords$lon[nrow(test_road_coords)], 
  test_road_coords$lat[nrow(test_road_coords)])






distances = tibble(dist = rep(NA_real_, nrow(roads)))

for(i in 1:nrow(roads)){
  
  if(i %% 10 == 1) print(i)
  
  road_i <- roads[i,]
  
  road_i_coords = as.matrix(road_i$geometry[[1]]) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  
  dist_i = geodesic_distance_meters(
    road_i_coords$lon[1], 
    road_i_coords$lat[1], 
    road_i_coords$lon[nrow(road_i_coords)], 
    road_i_coords$lat[nrow(road_i_coords)])
  
  distances$dist[i] = dist_i
}


distances2 = distances %>% 
  mutate(osm_id = roads$osm_id,
         name = roads$name)






roads %>%
  filter(distances2$dist >= 200) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(opacity = 1)







