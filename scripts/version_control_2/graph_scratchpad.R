library(ggplot2)
library(wesanderson)

 moult_df3 %>%
  filter(prim_moult > 0) %>%
  ggplot() +
  scale_color_viridis_c() +
  geom_point(aes(x = julian, y = id_year, size = moult, colour = moult)) +
  facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y")
 
 timing.plot.rev2 <-  moult_df3 %>% 
    mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
  ggplot() +
  scale_fill_manual(values = wes_palette(n = 16, name = "Zissou1", type = "continuous")) +
  geom_histogram(aes(x = julian, fill = as.factor(colony), weight = prim_moult/15),
                 position = "stack", alpha = 0.9, binwidth = 15) +
  labs(x = "Month", fill = "", y = "") +
  scale_x_continuous(breaks = c(-150, -120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
                     limits = c(-160, 120)) +
    scale_y_continuous(breaks = scales::breaks_pretty(3)) +
  # theme(axis.text.y = element_blank(),
  # axis.ticks.y = element_blank()) +
  facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y")

  ggsave(timing.plot.rev2, filename = "plots/timing_plot_rev2.png", dpi = 500, width = 6, height = 8)
  

timing.plot7 <-
  moult_df3 %>%
    filter(area == "Barents Sea") %>%
  mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
  ggplot() +
  scale_fill_manual(values = wes_palette(n = 4, name = "Zissou1", type = "discrete")) +
  geom_histogram(aes(x = julian, fill = as.factor(colony), weight = prim_moult/15),
               position = "stack", alpha = 0.6, binwidth = 15) +
  labs(x = "Month", fill = "", y = "") +
  scale_x_continuous(breaks = c(-152, -121, -91, -60, -30, 0, 32, 60, 91),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
                     limits = c(-150, 140)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  # theme(axis.text.y = element_blank(),
        # axis.ticks.y = element_blank()) +
  facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y") +
  theme(legend.justification = c("right", "top"),
        legend.position = c(1, 1.3),
        legend.box.just = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.8, "line"))

timing.plot1 <-
  timing.plot7 %+% (moult_df3 %>%
                      mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
                      filter(area == "Ireland" & prim_moult > 0)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(1, 1.4)) +
  labs(x = "")

timing.plot2 <-
  timing.plot1 %+% (moult_df3 %>%
                      mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
                     filter(area == "UK" & prim_moult > 0))
timing.plot3 <-
  timing.plot1 %+% (moult_df3 %>%
                      mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
                     filter(area == "Faroe Islands" & prim_moult > 0))
timing.plot4 <-
  timing.plot1 %+% (moult_df3 %>%
                      mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
                     filter(area == "Iceland" & prim_moult > 0))
timing.plot5 <-
  timing.plot1 %+% (moult_df3 %>%
                      mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
                     filter(area == "Norwegian Sea" & prim_moult > 0))
timing.plot6 <-
  timing.plot1 %+% (moult_df3 %>%
                      mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
                      filter(area == "Canada" & prim_moult > 0)) +
  scale_y_continuous(breaks = c(0, 1, 2))

timing.plot.full = cowplot::plot_grid(
  timing.plot1, timing.plot2, timing.plot3,
  timing.plot4, timing.plot5, timing.plot6, timing.plot7,
  ncol = 1, rel_heights = c(1,1,1,1,1,1,1.2)
)

timing.plot.full

ggsave(timing.plot.full, filename = "plots/timing_plot.png", dpi = 500, width = 8, height = 8)

moult_df3 %>%
  filter(prim_moult > 0) %>%
  ggplot() +
  geom_density(aes(x = col_dist_smth, fill = as.factor(colony)), position = "stack") +
  facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y")

# moult_df3 %>% 
#   filter(prim_moult > 0) %>%
#   ggplot() +
#   geom_density(aes(x = lon, fill = as.factor(colony)), position = "stack") +
#   geom_point(aes(x = col_lon, y = 0, colour = colony)) +
#   facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y")
# 
# moult_df3 %>% 
#   filter(moult > 0 & moult < 50) %>%
#   ggplot() +
#   geom_density(aes(x = lat_filled, fill = as.factor(area))) +
#   facet_wrap(~ as.factor(area), ncol = 1)

moult_df3 %>% 
  filter(!is.na(sex) & sex != "unknown" & moult > 0 & moult < 50 & year < 20) %>%
  ggplot() +
  geom_density(aes(x = julian, fill = as.factor(sex)), alpha = 0.5) +
  facet_wrap(~ as.factor(colony), ncol = 1)

moult_df3 %>% 
  filter(colony == "Papey" & moult > 0 & moult < 50 & year < 20) %>%
  ggplot() +
  geom_density(aes(x = julian, fill = as.factor(colony)), position = "stack") +
  facet_wrap(~ as.factor(year), ncol = 1)

moult_df3 %>% 
  filter(prim_moult > 0) %>%
  split(., .$id_year) %>%
  lapply(., FUN = function(x){x[1,]}) %>%
  do.call(rbind, .) %>%
  ggplot() +
  geom_density(aes(x = prim_moult, fill = as.factor(colony)), position = "stack") +
  facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y")

moult_df3 %>% filter(prim_moult > 0) %>%
ggplot() +
  geom_point(aes(y = lat_filled, x = julian, colour = prim_moult))# +


moult_df3 %>% filter(prim_moult > 0) %>%
ggplot() +
  geom_violin(aes(x = as.factor(year), y = julian, fill = as.factor(year))) +
facet_wrap(~ as.factor(colony), nrow = 1)

moult_df3 %>% filter(prim_moult > 0 & year > 1) %>%
  mutate(year = as.factor(year)) %>%
  ggplot() +
  coord_map(projection = "azequidistant",
            xlim = c(-70, 40),
            ylim = c(30, 80)) +
  geom_point(aes(x = lon, y = lat_filled, colour = area)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0.1) +
  geom_point(aes(x = col_lon, y = col_lat, fill = area),
             shape = 23, colour = "black", size = 6) +
  facet_wrap(~ year)

ggplot(moult_df3 %>% filter(prim_moult > 0)) +
  coord_map(projection = "azequidistant",
            xlim = c(-50, 40),
            ylim = c(35, 80)) +
  geom_point(aes(x = lon, y = lat_filled, colour = area)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0.1) +
  geom_point(aes(x = col_lon, y = col_lat, fill = area),
             shape = 23, colour = "black", size = 6)

ggplot(moult_df3 %>% filter(prim_moult > 0)) +
  coord_map(projection = "azequidistant",
            xlim = c(-50, 40),
            ylim = c(35, 80)) +
  geom_point(aes(x = lon, y = lat_filled, colour = colony)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0.1) +
  geom_point(aes(x = col_lon, y = col_lat, fill = colony),
             shape = 23, colour = "black", size = 6)

ggplot(data = moult_df3 %>% filter(prim_moult > 0)) +
  geom_violin(aes(x = as.factor(moult), y = col_dist_smth, fill = as.factor(moult)))# +
# facet_wrap(facets = ~colony)

ggplot(moult_df3 %>% filter(prim_moult > 0), aes(x = lon, y = lat_filled)) +
  coord_cartesian(xlim = c(-60, 60),
            ylim = c(35, 80)) +
  geom_hex(bins = 60) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
