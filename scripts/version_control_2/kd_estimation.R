
# Load data and libraries in
load(file = "data/cleaned/moult_df.RData")
library(adehabitatHR)
library(dplyr)
library(sp)

# moult_df <- moult_df3

# Create spatial points data frame
moult.spdf <- meta_df %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"))


# Bhattycharrya's affinity for moult areas
ba.moult <- kerneloverlap(moult.spdf[, 3],
                           method = "BA")
# Take a look at above
ba.moult

ud.moult <- kernelUD(moult.spdf[, 3]) %>%
  lapply(., function(x){try(getverticeshr(x, 75))})

# changing each polygons id to the area name for rbind call
sapply(1:length(ud.moult), function(i) {
  row.names(ud.moult[[i]]) <<- names(ud.moult)[i]
})

# Reduce list and transform to lat lon
sdf_poly <- Reduce(rbind, ud.moult) %>%
  spTransform(., CRS("+init=epsg:4326")) 

g.moult <- ggplot(sdf_poly, aes(x = long,
                    y = lat,
                    fill = id,
                    group = group)) +
  labs(fill = "Colony\nLocation") +
  geom_polygon(alpha = .4) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0.01) +
  ggthemes::scale_fill_gdocs() +
  coord_map(projection = "azequidistant",
            xlim = c(-50, 15),
            ylim = c(35, 82)) +
  theme_void()

g.moult

ggsave(g.moult, filename = "moult_dist.png", dpi = 500, width = 8, height = 6)

# Rasterised KDEs ---------------------------------------------------------

library(spatialEco)

# KDE for Irish colonies
kde.ireland <- meta_df %>%
  # Filter to only moult locations
  filter(area == "Ireland") %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
  # KDE estimation
  sp.kde(., bw = 1000000, nr = 1000, nc = 1000,
         newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))

# KDE for Norwegian colonies
kde.norway <- meta_df %>%
  # Filter to only moult locations
  filter(area == "Norwegian Sea") %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
  # KDE estimation
  sp.kde(., bw = 1000000, nr = 1000, nc = 1000,
         newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))

# KDE for Faroe Island
kde.faroes <- meta_df %>%
  # Filter to only moult locations
  filter(area == "Faroe Islands") %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
  # KDE estimation
  sp.kde(., bw = 1000000, nr = 1000, nc = 1000,
         newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))

# KDE for UK
kde.uk <- meta_df %>%
  # Filter to only moult locations
  filter(area == "UK") %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
  # KDE estimation
  sp.kde(., bw = 500000, nr = 1000, nc = 1000,
         newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))

# KDE for Iceland
kde.iceland <- meta_df %>%
  # Filter to only moult locations
  filter(area == "Iceland") %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
  # KDE estimation
  sp.kde(., bw = 1000000, nr = 1000, nc = 1000,
         newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))

# KDE for Barents Sea colonies
kde.barents <- meta_df %>%
  # Filter to only moult locations
  filter(area == "Barents Sea") %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
  # KDE estimation
  sp.kde(., bw = 1000000, nr = 1000, nc = 1000,
         newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))

# KDE for Barents Sea colonies
kde.canada <- meta_df %>%
  # Filter to only moult locations
  filter(area == "Canada") %>%
  # Keep useful variables
  dplyr::select(id, colony, area, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
  # KDE estimation
  sp.kde(., bw = 1000000, nr = 1000, nc = 1000,
         newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))
   
# Stack all of the above raster objects 
kde.stack <- 
  raster::stack(kde.barents, kde.canada, kde.faroes, kde.iceland,
                kde.ireland, kde.norway, kde.uk)

# Give te hem their proper names
names(kde.stack) <- c("Barents Sea", "Canada", "Faroe Islands",
                      "Iceland", "Ireland", "Norwegian Sea", "UK")

# Quick plot of all of the above
plot(kde.stack)

raster::layerStats(kde.stack, stat = "pearson")

# Rasterised KDEs for initial migration -----------------------------------

# # KDE for Irish colonies
# kde.ireland.premoult <- moult_df %>%
#   # Filter to only moult locations
#   filter(julian < -90 & prim_moult == 0 & area == "Ireland") %>%
#   # Rename lat_filled to lat
#   mutate(lat = lat_filled) %>%
#   # Keep useful variables
#   dplyr::select(id, colony, area, lon, lat) %>%
#   # Transform to spatial points data frame
#   SpatialPointsDataFrame(
#     coords = .[c("lon", "lat")],
#     proj4string = CRS("+init=epsg:4326"),
#     data = .) %>%
#   # Transform to azemuthal projection in metres for KD estimation
#   spTransform(., CRS(
#     "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
#   # KDE estimation
#   sp.kde(., bw = 500000, nr = 1000, nc = 1000,
#          newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))
# 
# # KDE for Norwegian colonies
# kde.norway.premoult <- moult_df %>%
#   # Filter to only moult locations
#   filter(julian < -90 & prim_moult == 0 & area == "Norway") %>%
#   # Rename lat_filled to lat
#   mutate(lat = lat_filled) %>%
#   # Keep useful variables
#   dplyr::select(id, colony, area, lon, lat) %>%
#   # Transform to spatial points data frame
#   SpatialPointsDataFrame(
#     coords = .[c("lon", "lat")],
#     proj4string = CRS("+init=epsg:4326"),
#     data = .) %>%
#   # Transform to azemuthal projection in metres for KD estimation
#   spTransform(., CRS(
#     "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
#   # KDE estimation
#   sp.kde(., bw = 500000, nr = 1000, nc = 1000,
#          newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))
# 
# # KDE for Faroe Island
# kde.faroes.premoult <- moult_df %>%
#   # Filter to only moult locations
#   filter(julian < -90 & prim_moult == 0 & area == "Faroe Islands") %>%
#   # Rename lat_filled to lat
#   mutate(lat = lat_filled) %>%
#   # Keep useful variables
#   dplyr::select(id, colony, area, lon, lat) %>%
#   # Transform to spatial points data frame
#   SpatialPointsDataFrame(
#     coords = .[c("lon", "lat")],
#     proj4string = CRS("+init=epsg:4326"),
#     data = .) %>%
#   # Transform to azemuthal projection in metres for KD estimation
#   spTransform(., CRS(
#     "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
#   # KDE estimation
#   sp.kde(., bw = 500000, nr = 1000, nc = 1000,
#          newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))
# 
# # KDE for UK
# kde.uk.premoult <- moult_df %>%
#   # Filter to only moult locations
#   filter(julian < -90 & prim_moult == 0 & area == "UK") %>%
#   # Rename lat_filled to lat
#   mutate(lat = lat_filled) %>%
#   # Keep useful variables
#   dplyr::select(id, colony, area, lon, lat) %>%
#   # Transform to spatial points data frame
#   SpatialPointsDataFrame(
#     coords = .[c("lon", "lat")],
#     proj4string = CRS("+init=epsg:4326"),
#     data = .) %>%
#   # Transform to azemuthal projection in metres for KD estimation
#   spTransform(., CRS(
#     "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
#   # KDE estimation
#   sp.kde(., bw = 500000, nr = 1000, nc = 1000,
#          newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))
# 
# # KDE for Iceland
# kde.iceland.premoult <- moult_df %>%
#   # Filter to only moult locations
#   filter(julian < -90 & prim_moult == 0 & area == "Iceland") %>%
#   # Rename lat_filled to lat
#   mutate(lat = lat_filled) %>%
#   # Keep useful variables
#   dplyr::select(id, colony, area, lon, lat) %>%
#   # Transform to spatial points data frame
#   SpatialPointsDataFrame(
#     coords = .[c("lon", "lat")],
#     proj4string = CRS("+init=epsg:4326"),
#     data = .) %>%
#   # Transform to azemuthal projection in metres for KD estimation
#   spTransform(., CRS(
#     "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
#   # KDE estimation
#   sp.kde(., bw = 500000, nr = 1000, nc = 1000,
#          newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))
# 
# # KDE for Barents Sea colonies
# kde.barents.premoult <- moult_df %>%
#   # Filter to only moult locations
#   filter(julian < -90 & prim_moult == 0 & area == "Barents Sea") %>%
#   # Rename lat_filled to lat
#   mutate(lat = lat_filled) %>%
#   # Keep useful variables
#   dplyr::select(id, colony, area, lon, lat) %>%
#   # Transform to spatial points data frame
#   SpatialPointsDataFrame(
#     coords = .[c("lon", "lat")],
#     proj4string = CRS("+init=epsg:4326"),
#     data = .) %>%
#   # Transform to azemuthal projection in metres for KD estimation
#   spTransform(., CRS(
#     "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m")) %>%
#   # KDE estimation
#   sp.kde(., bw = 500000, nr = 1000, nc = 1000,
#          newdata = c(-6e+06, 3e+06, 3e+06, 10e+06))
# 
# # Stack all of the above raster objects 
# kde.stack.premoult <- 
#   raster::stack(kde.barents.premoult, kde.faroes.premoult,
#                 kde.iceland.premoult, kde.ireland.premoult,
#                 kde.norway.premoult, kde.uk.premoult)
# 
# # Give te hem their proper names
# names(kde.stack.premoult) <- names(ud.moult)
# 
# # Quick plot of all of the above
# plot(kde.stack.premoult)
# 
# # Pearson correlation
# raster::layerStats(kde.stack.premoult, stat = "pearson")
# 
# # Compare Ireland moult vs non-moult
# layerStats(
#   stack(kde.ireland, kde.ireland.premoult),
#   stat = "pearson")
# plot(stack(kde.ireland, kde.ireland.premoult))
# 
# # Compare Barents moult vs non-moult
# layerStats(
#   stack(kde.barents, kde.barents.premoult),
#   stat = "pearson")
# plot(stack(kde.barents, kde.barents.premoult))
# 
# # Compare Iceland moult vs non-moult
# layerStats(
#   stack(kde.iceland, kde.iceland.premoult),
#   stat = "pearson")
# plot(stack(kde.iceland, kde.iceland.premoult))
# 
# # Compare Faroes moult vs non-moult
# layerStats(
#   stack(kde.faroes, kde.faroes.premoult),
#   stat = "pearson")
# plot(stack(kde.faroes, kde.faroes.premoult))
# 
# # Compare UK moult vs non-moult
# layerStats(
#   stack(kde.uk, kde.uk.premoult),
#   stat = "pearson")
# plot(stack(kde.uk, kde.uk.premoult))
# 
# # Compare Norway moult vs non-moult
# layerStats(
#   stack(kde.norway, kde.norway.premoult),
#   stat = "pearson")
# plot(stack(kde.norway, kde.norway.premoult))


plotRGB(brick(kde.faroes, kde.ireland, kde.iceland), scale = 2e-12)


plotRGB(brick(kde.norway, kde.barents, kde.canada), scale = 2e-12)

kde.stack.wgs <- projectRaster(kde.stack, crs = crs("+init=epsg:4326"))


kde.df <- as.data.frame(rasterToPoints(kde.stack.wgs))

ggplot() +
  theme(panel.background = element_rect(fill = "white")) +
  coord_cartesian(xlim = c(-80, 40),
            ylim = c(35, 80)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0.1) +
  # geom_raster(data = kde.df, aes(x = x, y = y, fill = "Ireland", alpha = Ireland)) +
  geom_raster(data = kde.df, aes(x = x, y = y, fill = "Iceland", alpha = Iceland)) +
  # geom_raster(data = kde.df, aes(x = x, y = y, fill = "Canada", alpha = Canada)) +
  # geom_raster(data = kde.df, aes(x = x, y = y, fill = "Faroes", alpha = Faroe.Islands)) +
  # geom_raster(data = kde.df, aes(x = x, y = y, fill = "UK", alpha = UK)) +
  geom_raster(data = kde.df, aes(x = x, y = y, fill = "Barents", alpha =  Barents.Sea)) +
  geom_raster(data = kde.df, aes(x = x, y = y, fill = "Norway", alpha = Norwegian.Sea)) +
  scale_alpha_continuous(range = c(0, 1), trans = "sqrt")
