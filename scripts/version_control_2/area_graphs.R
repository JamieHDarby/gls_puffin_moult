
# Plot the areas used by moulting puffins ---------------------------------

# Simple plot of puffins from colonies with a low sample size, Atlantic colonies
plot.atlantic <-
  ggplot(meta_df %>% filter(area %in% c("Canada", "Faroe Islands", "Ireland"))) +
  coord_map(projection = "azequidistant",
            xlim = c(-70, 10),
            ylim = c(35, 60)) +
  scale_color_brewer(palette = "Dark2") +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#545454',
               fill = '#545454',
               size = 0.01,
               alpha = 1) +
  geom_point(aes(x = lon,
                 y = lat,
                 colour = factor(colony,
                                 levels = c("Skellig Michael", "Faroe Islands", "Witless Bay"))),
             size = 8, alpha = 0.7) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "e. Ireland, Canada and Faroe Islands (3 colonies, n = 9)", colour = "")

# Norwegian Sea puffin heatmap
plot.norway <-
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
  geom_point(aes(x = lon, y = lat), colour = "#400000", size = 4, alpha = 0.8) +
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
  labs(title = "d. Norwegian Sea (3 colonies, n = 68)")

# Icelandic puffin heatmap
plot.iceland <-
  ggplot(meta_df %>% filter(area == "Iceland")) +
  coord_map(projection = "azequidistant",
            xlim = c(-60, -10),
            ylim = c(45, 70)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#545454',
               fill = '#545454',
               size = 0) +
  geom_point(aes(x = lon, y = lat), colour = "#400000", size = 4, alpha = 0.8) +
  stat_density2d(aes(x = lon, y = lat,
                     fill = ..level..),
                 geom = "polygon",
                 n = 500,
                 h = c(6, 6),
                 show.legend = F) +
  scale_fill_viridis_c(option = "A") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "a. Iceland (3 colonies, n = 24)")

# UK puffin heatmap
plot.uk <- 
  ggplot(meta_df %>% filter(area == "UK")) +
  coord_map(projection = "azequidistant",
            xlim = c(-25, 10),
            ylim = c(50, 65)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#545454',
               fill = '#545454',
               size = 0) +
  geom_point(aes(x = lon, y = lat), colour = "#400000", size = 4, alpha = 0.8) +
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
  labs(title = "b. UK (1 colony, n = 20)")

# Barents Sea puffin heatmap
plot.barent <-
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
  geom_point(aes(x = lon, y = lat), colour = "#400000", size = 4, alpha = 0.8) +
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
  labs(title = "c. Barents Sea (2 colonies, n = 25)")

# Gather all of these plots into one
p.all <- plot_grid(plot_grid(plot.iceland, plot.uk,
                             plot.barent, plot.norway,
                             nrow = 2),
                   plot.atlantic,
                   nrow = 2,
                   rel_heights = c(1, 0.55))

# Take a look
p.all

ggsave(filename = "plots/space_plot.png", plot = p.all, dpi = 500, width = 8, height = 10, units = "in")  

