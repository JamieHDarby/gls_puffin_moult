
library(dplyr)
library(ggplot2)
library(cowplot)

# Heatmaps by area --------------------------------------------------------

# Plot of norwegian birds' moulting areas
plot.norway <- 
  ggplot(moult_df %>% filter(moult > 0 & area == "Norwegian Sea")) +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 25),
            ylim = c(35, 80)) +
  scale_fill_viridis_b(option = "C") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 show.legend = F) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             shape = 23, colour = "black", fill = "dark red", size = 6)

# Plot of icelandic birds' moulting areas
plot.iceland <- 
  ggplot(moult_df %>% filter(moult > 0 & area == "Iceland")) +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 25),
            ylim = c(35, 80)) +
  scale_fill_viridis_b(option = "C") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 show.legend = F) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             shape = 23, colour = "black", fill = "dark red", size = 6)

# Plot of irish birds' moulting areas
plot.ireland <- 
  ggplot(moult_df %>% filter(moult > 0 & area == "Ireland")) +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 25),
            ylim = c(35, 80)) +
  scale_fill_viridis_b(option = "C") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     fill = ..level..),
                 geom = "polygon",
                 n = 1000,
                 show.legend = F) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             shape = 23, colour = "black", fill = "dark red", size = 6)

# Plot of uk birds' moulting areas
plot.uk <- 
  ggplot(moult_df %>% filter(moult > 0 & area == "UK")) +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 25),
            ylim = c(35, 80)) +
  scale_fill_viridis_b(option = "C") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     fill = ..level..),
                 geom = "polygon",
                 n = 1000,
                 show.legend = F) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             shape = 23, colour = "black", fill = "dark red", size = 6)

# Plot of faroe birds' moulting areas
plot.faroes <- 
  ggplot(moult_df %>% filter(moult > 0 & area == "Faroe Islands")) +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 25),
            ylim = c(35, 80)) +
  scale_fill_viridis_b(option = "C") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 show.legend = F) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             shape = 23, colour = "black", fill = "dark red", size = 6)

# Plot of barents sea birds' moulting areas
plot.barents <-
  ggplot(moult_df %>% filter(moult > 0 & area == "Barents Sea")) +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 25),
            ylim = c(35, 80)) +
  scale_fill_viridis_b(option = "C") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 show.legend = F) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             shape = 23, colour = "black", fill = "dark red", size = 6)

# Put all of these together
plot_grid(plot.barents, plot.norway, plot.iceland,
          plot.ireland, plot.faroes, plot.uk, nrow = 2)

mycolours = c(brewer.pal(name = "Dark2", n = 7),
              brewer.pal(name = "Set3", n = 7))

density_plot1 <-
  moult_df3 %>%
  filter(prim_moult > 0 &
           area %in% c("Ireland", "UK", "Faroe Islands")) %>%
  ggplot() +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 15),
            ylim = c(35, 82)) +
  scale_fill_brewer(palette = "Dark2") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     alpha = ..level.., fill = colony, group = colony),
                 geom = "polygon",
                 contour_var = "ndensity",
                 n = 500,
                 show.legend = F) +
  labs(fill = "Colony") +
  scale_alpha_continuous(range = c(0, 0.8)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha("white", 0))) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0.01) +
  geom_point(aes(x = col_lon, y = col_lat, fill = colony),
             shape = 23, colour = "black", size = 4)

ggsave(density_plot1, filename = "plots/density_plot1.png", dpi = 500, width = 5, height = 5)

density_plot2 <-
  moult_df3 %>%
  filter(prim_moult > 0 &
           area == "Iceland") %>%
  ggplot() +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 15),
            ylim = c(35, 82)) +
  scale_fill_brewer(palette = "Dark2") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     alpha = ..level.., fill = colony, group = colony),
                 geom = "polygon",
                 contour_var = "ndensity",
                 n = 500,
                 show.legend = F) +
  labs(fill = "Colony") +
  scale_alpha_continuous(range = c(0, 0.8)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha("white", 0))) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0.01) +
  geom_point(aes(x = col_lon, y = col_lat, fill = colony),
             shape = 23, colour = "black", size = 4)

ggsave(density_plot2, filename = "plots/density_plot2.png", dpi = 500, width = 5, height = 5)

density_plot3 <-
  moult_df3 %>%
  filter(prim_moult > 0 &
           area == "Barents Sea") %>%
  ggplot() +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 15),
            ylim = c(35, 82)) +
  scale_fill_brewer(palette = "Dark2") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     alpha = ..level.., fill = colony, group = colony),
                 geom = "polygon",
                 contour_var = "ndensity",
                 n = 500,
                 show.legend = F) +
  labs(fill = "Colony") +
  scale_alpha_continuous(range = c(0, 0.8)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha("white", 0))) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0.01) +
  geom_point(aes(x = col_lon, y = col_lat, fill = colony),
             shape = 23, colour = "black", size = 4)

density_plot3

ggsave(density_plot3, filename = "plots/density_plot3.png", dpi = 500, width = 5, height = 5)

density_plot4 <-
  moult_df3 %>%
  filter(prim_moult > 0 &
           area == "Norwegian Sea") %>%
  ggplot() +
  coord_map(projection = "azequidistant",
            xlim = c(-40, 15),
            ylim = c(35, 82)) +
  scale_fill_brewer(palette = "Dark2") +
  stat_density2d(aes(x = lon, y = lat_filled,
                     alpha = ..level.., fill = colony, group = colony),
                 geom = "polygon",
                 contour_var = "ndensity",
                 n = 500,
                 show.legend = F) +
  labs(fill = "Colony") +
  scale_alpha_continuous(range = c(0, 0.8)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha("white", 0))) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0.01) +
  geom_point(aes(x = col_lon, y = col_lat, fill = colony),
             shape = 23, colour = "black", size = 4)

ggsave(density_plot4, filename = "plots/density_plot4.png", dpi = 500, width = 5, height = 5)
