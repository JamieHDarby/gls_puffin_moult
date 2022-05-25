
storm_rstr <- raster("data/shapes/winter_raster_latlon_augsep.tif")

plot(storm_rstr)

sp <- SpatialPoints(coords = moult_df3[, c("lon", "lat_filled")])

moult_df3$storms <- raster::extract(storm_rstr, sp)

ggplot(meta_df) +
  geom_density(aes(x = storm, fill = area), alpha = 0.6)

plot(sp, add = T)
