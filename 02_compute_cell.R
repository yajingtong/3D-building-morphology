library(sf)
library(dplyr)
library(purrr)

b_city <- readRDS("outputs/all_cities_buildings.rds")
 
centers <- readRDS("data/centers.rds")

buffer_dist<- 50
b_buffered <- lyon %>%
  st_buffer(dist = buffer_dist)


#lyon <- b_city  %>% filter(city == "Lyon")

# Parameters

cell_size  <- 500  #grid resolution in meters
max_rings <- 100
ring_width <- 250
radius_max <- ring_width * max_rings    
target_crs <- 32632 # UTM 32N

# Create Empty Grid for One City

create_empty_grid <- function(city_name, center_geom) {
  
  message("Creating grid for ", city_name)
  
  # 1. Buffer around city center
  city_circle <- st_buffer(center_geom, radius_max)
  
  #  Grid generation
  grid <- st_make_grid(city_circle, cellsize = c(cell_size, cell_size), what = "polygons")
  
  # Convert to sf
  grid_sf <- st_sf(id = seq_along(grid), geometry = grid)
  
  #  Clip grid to circular city extent
  grid_crop <- st_intersection(grid_sf, city_circle)
  
  #   Compute centroid coordinates
  cent <- st_coordinates(st_centroid(grid_crop))
  grid_crop$X <- cent[,1]
  grid_crop$Y <- cent[,2]
  
  #  Compute distance to CBD
  center_xy <- st_coordinates(center_geom)
  grid_crop$distCBD <- sqrt((grid_crop$X - center_xy[1])^2 +(grid_crop$Y - center_xy[2])^2)
  
  #  Add city name
  grid_crop$city <- city_name
  
  return(grid_crop)
}


city_list <- map(1:nrow(centers), function(i) {
  city_name <- centers$Name[i]          # create grid based on center
  center_geom <- centers[i, ]
  create_empty_grid(city_name, center_geom)
})

# Combine all and save
all_city_grids <- do.call(rbind, city_list)
saveRDS(all_city_grids, "all_city_empty_grids_500m.rds")
st_write(all_city_grids,  "all_city_grids_500m.gpkg",delete_dsn = TRUE)


# List of city names
cities <- unique(all_city_grids$city)


## spatial join

spjoin_city_buildings <- function(city_name, grid_city, bpts_city) {
  
  message("Processing (spatial join): ", city_name)
  
  #  No buildings for this city
  if (nrow(bpts_city) == 0) {
    message(" → No buildings for ", city_name)
    grid_city$G <- NA
    grid_city$X <- 0
    grid_city$H <- NA
    return(grid_city)
  }

  # SPATIAL JOIN
  joined <- st_join(grid_city, bpts_city, join = st_intersects)
  

  # AGGREGATE

  aggregated <- joined %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(
        G = sum(gi, na.rm = TRUE), #G total floor area (m²)
        N = n(),                    # number of buildings 
        H = mean(hi, na.rm = TRUE), # average height(m)
        B = mean(bi, na.rm = TRUE), # average building footprint (m²)
        V = mean(vi, na.rm = TRUE), # average building volume (m³)
      .groups = "drop")

  grid_city <- left_join(grid_city, aggregated, by = "id")
  
  return(grid_city)
}


all_city_spjoined <- map_df(cities, function(city_name) {
  
  # filter grid and buildings
  grid_city <- all_city_grids %>% filter(city == !!city_name)
  bpts_city <- building_points %>% filter(city == !!city_name)
  
  spjoin_city_buildings(city_name, grid_city, bpts_city)
})



# Create dissolved buffers
b_buffered <- b_city %>%
  st_buffer(dist = buffer_dist) %>%
  st_union() %>%
  st_sf(geometry = .)

# Intersect buffer with rings
buffers_in_rings <- st_intersection(all_city_grids, b_buffered)


cell_stats_500 <- all_city_spjoined %>%
  mutate(
    ri = ceiling(distCBD/250),
    FAR = G/250000)
    # FAR = G/(cell_size*cell_size))



city_center <- centers %>% filter(Name == "Paris")

city_center <- st_transform(city_center, st_crs(b_city))
centroids <- st_centroid(Paris)
Paris$distances <- as.numeric(st_distance(centroids, city_center))
distances <- as.numeric(b_city$distance)

all_b <- all_b %>%
  mutate(
    #ri = ceiling(distances/ring_width),
    FARi = round(gi/(50*50),digits = 5))





library(sf)
library(dplyr)
library(purrr)

