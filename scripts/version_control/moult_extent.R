# Load data and libraries in
load(file = "data/cleaned/moult_df.RData")
library(adehabitatHR)
library(dplyr)
library(sp)
library(raster)

# Create spatial points data frame
moult.spdf <- moult_df %>%
  # Filter to only moult locations
  filter(prim_moult > 0) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(moult.spdf[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_moult <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(moult.spdf[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_moult_all <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_moult)
raster::area(sdf_poly_moult_all)

# Non-moult 1 -------------------------------------------------------------

# Create spatial points data frame
nonmoult.spdf1 <- moult_df %>%
  # Filter to only moult locations
  filter(julian < -90) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(nonmoult.spdf1[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_nonmoult.1 <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(nonmoult.spdf1[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_nonmoult_all.1 <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_nonmoult.1)
raster::area(sdf_poly_nonmoult_all.1)

# Non-moult 2 -------------------------------------------------------------

# Create spatial points data frame
nonmoult.spdf2 <- moult_df %>%
  # Filter to only moult locations
  filter(julian < -60 & julian > -90) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(nonmoult.spdf2[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_nonmoult.2 <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(nonmoult.spdf2[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_nonmoult_all.2 <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_nonmoult.2)
raster::area(sdf_poly_nonmoult_all.2)


# Non-moult 3 -------------------------------------------------------------

# Create spatial points data frame
nonmoult.spdf3 <- moult_df %>%
  # Filter to only moult locations
  filter(julian < -30 & julian > -60) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(nonmoult.spdf3[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_nonmoult.3 <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(nonmoult.spdf3[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_nonmoult_all.3 <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_nonmoult.3)
raster::area(sdf_poly_nonmoult_all.3)

# Non-moult 4 -------------------------------------------------------------

# Create spatial points data frame
nonmoult.spdf4 <- moult_df %>%
  # Filter to only moult locations
  filter(julian < 0 & julian > -30) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(nonmoult.spdf4[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_nonmoult.4 <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(nonmoult.spdf4[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_nonmoult_all.4 <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_nonmoult.4)
raster::area(sdf_poly_nonmoult_all.4)

# Non-moult 5 -------------------------------------------------------------

# Create spatial points data frame
nonmoult.spdf5 <- moult_df %>%
  # Filter to only moult locations
  filter(julian < 30 & julian > 0) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(nonmoult.spdf5[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_nonmoult.5 <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(nonmoult.spdf5[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_nonmoult_all.5 <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_nonmoult.5)
raster::area(sdf_poly_nonmoult_all.5)

# Non-moult 5 -------------------------------------------------------------

# Create spatial points data frame
nonmoult.spdf6 <- moult_df %>%
  # Filter to only moult locations
  filter(julian < 60 & julian > 30) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(nonmoult.spdf6[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_nonmoult.6 <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(nonmoult.spdf6[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_nonmoult_all.6 <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_nonmoult.6)
raster::area(sdf_poly_nonmoult_all.6)

# Non-moult 5 -------------------------------------------------------------

# Create spatial points data frame
nonmoult.spdf7 <- moult_df %>%
  # Filter to only moult locations
  filter(julian < 90 & julian > 60) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(nonmoult.spdf7[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_nonmoult.7 <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(nonmoult.spdf7[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_nonmoult_all.7 <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_nonmoult.7)
raster::area(sdf_poly_nonmoult_all.7)


# Non-moult 5 -------------------------------------------------------------

# Create spatial points data frame
nonmoult.spdf8 <- moult_df %>%
  # Filter to only moult locations
  filter(julian > 90) %>%
  # Rename lat_filled to lat
  mutate(lat = lat_filled, 
         all = 1) %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat, all) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))

ud <- kernelUD(nonmoult.spdf8[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- names(ud)[i]
})

# Reduce list and transform to lat lon
sdf_poly_nonmoult.8 <- Reduce(rbind, ud) %>%
  spTransform(., CRS("+init=epsg:4326")) 

ud.all <- kernelUD(nonmoult.spdf8[, 6]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# Reduce list and transform to lat lon
sdf_poly_nonmoult_all.8 <- Reduce(rbind, ud.all) %>%
  spTransform(., CRS("+init=epsg:4326")) 

raster::area(sdf_poly_nonmoult.8)
raster::area(sdf_poly_nonmoult_all.8)


# Combine everything ------------------------------------------------------


  Moult <- c(raster::area(sdf_poly_moult), 
             raster::area(sdf_poly_moult_all))/1e+09
  nonmoult.1 <- c(raster::area(sdf_poly_nonmoult.1), 
             raster::area(sdf_poly_nonmoult_all.1))/1e+09
  nonmoult.2 <- c(raster::area(sdf_poly_nonmoult.2), 
                  raster::area(sdf_poly_nonmoult_all.2))/1e+09
  nonmoult.3 <- c(raster::area(sdf_poly_nonmoult.3), 
                  raster::area(sdf_poly_nonmoult_all.3))/1e+09
  nonmoult.4 <- c(raster::area(sdf_poly_nonmoult.4), 
                  raster::area(sdf_poly_nonmoult_all.4))/1e+09
  nonmoult.5 <- c(raster::area(sdf_poly_nonmoult.5), 
                  raster::area(sdf_poly_nonmoult_all.5))/1e+09
  nonmoult.6 <- c(raster::area(sdf_poly_nonmoult.6), 
                  raster::area(sdf_poly_nonmoult_all.6))/1e+09
  nonmoult.7 <- c(raster::area(sdf_poly_nonmoult.7), 
                  raster::area(sdf_poly_nonmoult_all.7))/1e+09
  nonmoult.8 <- c(raster::area(sdf_poly_nonmoult.8), 
                  raster::area(sdf_poly_nonmoult_all.8))/1e+09

areas.df <- data.frame(Moult, nonmoult.1, nonmoult.2,
                       nonmoult.3, nonmoult.4, nonmoult.5,
                       nonmoult.6, nonmoult.7, nonmoult.8)

areas.df$area <- c("Barents Sea", "Faroe Islands", "Iceland",
                   "Ireland", "Norway", "UK", "All")

area_df <- areas.df %>%
  tidyr::gather(timing, expanse, Moult:nonmoult.8)

library(wesanderson)

nonmoult_area <- 
  area_df %>%
  filter(area != "All" & timing != "Moult") %>%
  mutate(num = as.numeric(as.factor(timing)),
         split = ifelse(area == "All", "All Colonies", "By Area")) %>%
  ggplot() +
  scale_fill_manual(values = wes_palette(n = 6, name = "IsleofDogs1", type = "continuous")) +
  scale_y_continuous(limits = c(0, 15000)) +
  scale_x_continuous(breaks = c(1:8),
                     labels = c("Sep", "Oct", "Nov", "Dec",
                                "Jan", "Feb", "Mar", "Apr")) +
  geom_area(aes(x = num, y = expanse, fill = area),
            position = "stack", alpha = 0.8) +
  labs(fill = "Area", x = "Month", y = "")

moult_area <- 
  area_df %>%
  filter(area != "All" & timing == "Moult") %>%
  ggplot() +
  scale_fill_manual(values = wes_palette(n = 6, name = "IsleofDogs1", type = "continuous")) +
  scale_y_continuous(limits = c(0, 15000)) +
  theme(legend.position = "none") +
  geom_col(aes(y = expanse, x = timing, fill = area),
            position = "stack", alpha = 0.9) +
  labs(fill = "Area", x = "", y = "Expanse (75% UD, 1000km2)")

moult_area_plot <- cowplot::plot_grid(moult_area, nonmoult_area, nrow = 1, rel_widths = c(1,5))

ggsave(moult_area_plot, filename = "plots/moult_area.png", dpi = 500, width = 8, height = 3, units = "in")

nonmoult_area_all <- 
  area_df %>%
  filter(timing != "Moult") %>%
  mutate(num = as.numeric(as.factor(timing)),
         split = ifelse(area == "All", "All", "By Area")) %>%
  ggplot() +
  scale_fill_brewer(palette = "Dark2") +
  # scale_y_continuous(limits = c(0, 8000)) +
  scale_x_continuous(breaks = c(1:8),
                     labels = c("Sep", "Oct", "Nov", "Dec",
                                "Jan", "Feb", "Mar", "Apr")) +
  geom_area(aes(x = num, y = expanse, fill = area),
            alpha = 0.6) +
  labs(fill = "Area", x = "Month", y = "") %>%
  facet_wrap(facets = ~split, nrow = 2)

moult_area_all <- 
  area_df %>%
  filter(timing == "Moult") %>%
  mutate(split = ifelse(area == "All", "All", "By Area")) %>%
  ggplot() +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 8000)) +
  theme(legend.position = "none") +
  geom_col(aes(y = expanse, x = timing, fill = area),
           alpha = 0.6) +
  labs(fill = "Area", x = "", y = "Expanse (75% UD, 1000km2)") %>%
  facet_wrap(facets = ~split)

cowplot::plot_grid(moult_area_all, nonmoult_area_all,
                   nrow = 1, rel_widths = c(1,5))
