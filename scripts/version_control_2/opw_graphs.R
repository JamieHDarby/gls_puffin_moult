# plot.iceland <-
ggplot(meta_df %>% filter(area == "Norwegian Sea")) +
  coord_map(projection = "azequidistant",
            xlim = c(-30, 30),
            ylim = c(60, 80)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#545454',
               fill = '#545454',
               size = 0) +
  geom_point(aes(x = lon, y = lat), colour = "black", size = 6, alpha = 0.8) +
  stat_density2d(aes(x = lon, y = lat,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 h = c(4, 4),
                 show.legend = F) +
  scale_fill_viridis_c(option = "A") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Norwegian Sea colonies")

  
# plot.iceland <-
ggplot(meta_df %>% filter(area == "Iceland")) +
  coord_map(projection = "azequidistant",
            xlim = c(-60, -10),
            ylim = c(50, 70)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#545454',
               fill = '#545454',
               size = 0) +
  geom_point(aes(x = lon, y = lat), colour = "black", size = 6, alpha = 0.8) +
  stat_density2d(aes(x = lon, y = lat,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 h = c(5, 5),
                 show.legend = F) +
  scale_fill_viridis_c(option = "A") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Icelandic colonies")

ggplot(meta_df %>% filter(area == "UK")) +
  coord_map(projection = "azequidistant",
            xlim = c(-20, 10),
            ylim = c(50, 65)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#545454',
               fill = '#545454',
               size = 0) +
  geom_point(aes(x = lon, y = lat), colour = "black", size = 6, alpha = 0.8) +
  stat_density2d(aes(x = lon, y = lat,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 h = c(2, 2),
                 show.legend = F) +
  scale_fill_viridis_c(option = "A") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Icelandic colonies")
  
# plot.iceland <-
ggplot(meta_df %>% filter(area == "Barents Sea")) +
  coord_map(projection = "azequidistant",
            xlim = c(-10, 50),
            ylim = c(60, 80)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#545454',
               fill = '#545454',
               size = 0) +
  geom_point(aes(x = lon, y = lat), colour = "black", size = 6, alpha = 0.8) +
  stat_density2d(aes(x = lon, y = lat,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 h = c(4, 4),
                 show.legend = F) +
  scale_fill_viridis_c(option = "A") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Barents Sea colonies")
  
  p.all <- plot_grid(plot.ireland.1, plot.ireland.2, plot.ireland.3, plot.ireland.4,
            nrow = 2)

  ggsave(filename = "allplot.png", plot = p.all, dpi = 500, width = 8, height = 6, units = "in")  
  