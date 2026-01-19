library(sf)
library(dplyr)
library(purrr)


# Paths

building_files <- list.files(
  "data/building/fr",
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

centers <- st_read("centers_fr.shp", quiet = TRUE)
target_crs <- 32632
centers <- st_transform(centers, target_crs)


# Precompute bbox centroids for all building tiles

tile_info <- map(building_files, function(f) {
  
  # Read header quickly (only bbox is needed)
  tmp <- st_read(f, quiet = TRUE)
  
  # Bounding box → geometry
  bb <- st_as_sfc(st_bbox(tmp), crs = st_crs(tmp))
  bb <- st_transform(bb, target_crs)
  
  list(
    file = f,
    centroid = st_centroid(bb)
  )
})



# Loop through each city center


all_cities_buildings <- map_dfr(1:nrow(centers), function(i) {
  
  city_name <- centers$Name[i]
  city_geom <- centers[i, ]
  
  message("\n=== Processing city: ", city_name, " ===")
  

  # 1) Compute distance to all tile centroids

  dists <- sapply(tile_info, function(ti) {
    st_distance(city_geom, ti$centroid)
  })
  
  nearest_idx <- which.min(dists)
  nearest_file <- tile_info[[nearest_idx]]$file
  
  message("Nearest building tile: ", basename(nearest_file),
          " (", round(as.numeric(dists[nearest_idx]), 0), " m )")
  

  # 2) Load the  building tile

  b <- st_read(nearest_file, quiet = TRUE, options = "PROMOTE_TO_MULTI=YES")
  
  if (st_crs(b) != st_crs(target_crs)) {
    b <- st_transform(b, target_crs)
  }
  
  #  validity fix
  if (any(!st_is_valid(b))) {
    b <- st_make_valid(b)
  }
  
  message("Attributes found: ", paste(names(b), collapse = ", "))
  

  # 3) Crop buildings within radius (25 km)

  max_dist <- 250 * 100  # 25 km
  city_buffer <- st_buffer(city_geom, max_dist)
  
  idx <- st_intersects(b, city_buffer) %>% lengths() > 0
  b_city <- b[idx, ]
  
  b_city$city <- city_name
  
  message("→ ", nrow(b_city), " buildings kept for ", city_name)
  
  b_city
})




library(sf)
library(dplyr)
library(purrr)

# Read city centers
centers <- st_read("centers_fr.shp", quiet = TRUE)
target_crs <- 32632
centers <- st_transform(centers, target_crs)

# List all building tiles
building_files <- list.files(
  "data/building/fr",
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

# Precompute bbox for each tile (fast)
tile_info <- map(building_files, function(f) {
  tmp <- st_read(f, quiet = TRUE)   # only reads header + bbox
  bb  <- st_as_sfc(st_bbox(tmp), crs = st_crs(tmp))
  bb  <- st_transform(bb, target_crs)
  
  list(
    file = f,
    bbox = bb
  )
})


max_dist <- 25000   # 25 km radius

all_cities_buildings <- map_dfr(1:nrow(centers), function(i) {
  
  city_name <- centers$Name[i]
  city_geom <- centers[i, ]
  
  message("\n=== Processing: ", city_name, " ===")
  
  # City buffer
  city_buffer <- st_buffer(city_geom, max_dist)
  

  # 1) Find *all* building tiles overlapping the buffer

  overlaps <- sapply(tile_info, function(ti)
    lengths(st_intersects(ti$bbox, city_buffer)) > 0
  )
  
  tile_subset <- tile_info[overlaps]
  
  message("Found tiles: ", length(tile_subset))
  
  # Stop if none found
  if (length(tile_subset) == 0) {
    warning("No building tiles found for ", city_name)
    return(NULL)
  }
  

  # 2) Load, crop, and combine all matching tiles

  b_city <- map_dfr(tile_subset, function(ti) {
    
    f <- ti$file
    message("  → Loading tile: ", basename(f))
    
    b <- st_read(f, quiet = TRUE, options = "PROMOTE_TO_MULTI=YES")
    
    # CRS align
    if (st_crs(b) != st_crs(target_crs)) {
      b <- st_transform(b, target_crs)
    }
    
    # Fix invalid geometries (if needed)
    if (any(!st_is_valid(b))) {
      b <- st_make_valid(b)
    }
    
    # Crop using intersects (fast)
    idx <- st_intersects(b, city_buffer) %>% lengths() > 0
    b_crop <- b[idx, ]
    
    b_crop
  })
  
  b_city$city <- city_name
  
  message(" → Total buildings kept for ", city_name, ": ", nrow(b_city))
  
  b_city
})

