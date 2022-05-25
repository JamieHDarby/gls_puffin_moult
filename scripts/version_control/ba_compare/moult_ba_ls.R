moult.ba.ls <- list()

# Create spatial points data frame
moult.spdf <- moult_df %>%
  # Filter to only moult locations
  filter(prim_moult > 0) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled,
         area.1 = ifelse(area == "Ireland", "Ireland", "Not Ireland")) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, area.1, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

# Bhattycharrya's affinity for moult areas
ba.moult <- kerneloverlap(moult.spdf[, 4],
                          method = "BA")

moult.ba.ls[[1]] <- ba.moult[2,1]

moult.spdf <- moult_df %>%
  # Filter to only moult locations
  filter(prim_moult > 0) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled,
         area.1 = ifelse(area == "Iceland", "Iceland", "Not Iceland")) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, area.1, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

# Bhattycharrya's affinity for moult areas
ba.moult <- kerneloverlap(moult.spdf[, 4],
                          method = "BA")

moult.ba.ls[[2]] <- ba.moult[2,1]

moult.spdf <- moult_df %>%
  # Filter to only moult locations
  filter(prim_moult > 0) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled,
         area.1 = ifelse(area == "Faroe Islands", "Faroe Islands", "Not Faroe Islands")) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, area.1, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

# Bhattycharrya's affinity for moult areas
ba.moult <- kerneloverlap(moult.spdf[, 4],
                          method = "BA")

moult.ba.ls[[3]] <- ba.moult[2,1]

moult.spdf <- moult_df %>%
  # Filter to only moult locations
  filter(prim_moult > 0) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled,
         area.1 = ifelse(area == "Norwegian Sea", "Norwegian Sea", "Not Norwegian Sea")) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, area.1, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

# Bhattycharrya's affinity for moult areas
ba.moult <- kerneloverlap(moult.spdf[, 4],
                          method = "BA")

moult.ba.ls[[4]] <- ba.moult[2,1]

moult.spdf <- moult_df %>%
  # Filter to only moult locations
  filter(prim_moult > 0) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled,
         area.1 = ifelse(area == "Barents Sea", "Barents Sea", "Not Barents Sea")) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, area.1, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

# Bhattycharrya's affinity for moult areas
ba.moult <- kerneloverlap(moult.spdf[, 4],
                          method = "BA")

moult.ba.ls[[5]] <- ba.moult[2,1]

moult.spdf <- moult_df %>%
  # Filter to only moult locations
  filter(prim_moult > 0) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled,
         area.1 = ifelse(area == "UK", "UK", "Not UK")) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, area.1, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

# Bhattycharrya's affinity for moult areas
ba.moult <- kerneloverlap(moult.spdf[, 4],
                          method = "BA")

moult.ba.ls[[6]] <- ba.moult[2,1]

unlist(moult.ba.ls)
