
library(osmdata)
library(tidyverse)
library(leaflet)
library(elevatr)
library(sp)



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












# compute distances

distances = data.frame(osm_id = roads$osm_id, name = roads$name) %>% 
  mutate(
    dist = NA_real_,
    lon_init = NA_real_,
    lat_init = NA_real_,
    lon_end = NA_real_,
    lat_end = NA_real_
  )


for(i in 1:nrow(roads)){
  
  if(i %% 100 == 1) print(i)
  
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
  distances$lon_init[i] = road_i_coords$lon[1] 
  distances$lat_init[i] = road_i_coords$lat[1]
  distances$lon_end[i] = road_i_coords$lon[nrow(road_i_coords)] 
  distances$lat_end[i] = road_i_coords$lat[nrow(road_i_coords)]
  
}





# compute elevations


# I think I did this part overly complicated, but it works in the end
coords_elev = distances %>% 
  select(osm_id, starts_with("lon")) %>% 
  pivot_longer(cols = lon_init:lon_end, names_to = "lon", values_to = "value_lon") %>% 
  inner_join(
    distances %>% 
      select(osm_id, starts_with("lat")) %>% 
      pivot_longer(cols = lat_init:lat_end, names_to = "lat", values_to = "value_lat")
  ) %>% 
  filter(substring(lon, 4) == substring(lat, 4))


elevations_SpatialPointsDataFrame = get_elev_point(
  as.data.frame(coords_elev %>% select(value_lon, value_lat)), 
  prj = '+proj=longlat +datum=WGS84', src = "aws")


elevations = coords_elev %>% 
  mutate(elevation = elevations_SpatialPointsDataFrame$elevation) %>% 
  group_by(osm_id) %>% 
  summarize(min_elev = min(elevation), max_elev = max(elevation))



# merge distances and elevations


distances_elevations = distances %>% 
  full_join(elevations) %>% 
  mutate(gradient = (max_elev - min_elev)/dist)


# 50 steepest cobbled ascents of more than 100 meters
distances_elevations %>% 
  filter(dist > 100) %>% 
  arrange(desc(gradient)) %>% 
  head(50)

top_50_more_300 = distances_elevations %>% 
  filter(dist > 300) %>% 
  arrange(desc(gradient)) %>% 
  head(50) %>% 
  pull(osm_id)


roads %>%
  filter(osm_id %in% top_50_more_300) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(opacity = 1)










# I think I did this part overly complicated, but it works in the end
cobbles_sub = distances_elevations %>% 
  filter(osm_id %in% top_50_more_300) %>% 
  select(osm_id, starts_with("lon")) %>% 
  pivot_longer(cols = lon_init:lon_end, names_to = "lon", values_to = "value_lon") %>% 
  inner_join(
    distances_elevations %>% 
      filter(osm_id %in% top_50_more_300) %>% 
      select(osm_id, starts_with("lat")) %>% 
      pivot_longer(cols = lat_init:lat_end, names_to = "lat", values_to = "value_lat")
  ) %>% 
  filter(substring(lon, 4) == substring(lat, 4))

afl_venue_coords <- cobbles_sub %>% 
  select(longitude = value_lon, latitude = value_lat)

distance_matrix <- as.matrix(
  distm(afl_venue_coords, fun = distHaversine)
)/1000 #convert metres to kilometres

rownames(distance_matrix) <- cobbles_sub$osm_id
colnames(distance_matrix) <- cobbles_sub$osm_id




library(TSP)


aaa = TSP(distance_matrix)
solution = solve_TSP(aaa)
solution

# permutation vector
as.integer(solution)


cobbles_sub[as.integer(solution),] %>% 
  select(osm_id, value_lon, value_lat) %>% 
  as.data.frame() %>% 
  pgirmess::writeGPX(., filename = "~/Desktop/tsp_2.gpx")


