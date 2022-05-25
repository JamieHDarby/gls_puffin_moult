
devtools::install_github("mastoffel/rptR", build_vignettes = TRUE)

require(rptR)

meta_df2$start_julian <- meta_df2$start_julian + 151

mod.rpt.lat.col <- rpt(lat ~ (1|colony), grname = c("colony"), data = mod_df)
mod.rpt.lon.col <- rpt(lon ~ (1|colony), grname = c("colony"), data = mod_df)
mod.rpt.time.col <- rpt(mid_julian ~ (1|colony), grname = "colony", data = mod_df)

meta_df2_restricted <- split(meta_df2, meta_df2$id) %>%
  lapply(function(x){if(nrow(x) < 2){x <- NULL}; x}) %>%
  do.call(rbind, .)

mod.rpt.lat.id <- rpt(lat ~ (1|id), grname = c("id"), data = mod_df_restricted)
mod.rpt.lon.id <- rpt(lon ~ (1|id), grname = c("id"), data = mod_df_restricted)
mod.rpt.time.id <- rpt(start_julian ~ (1|id), grname = "id", datatype = "Poisson", data = mod_df_restricted)

plot(mod.rpt.lat.id, grname = "id", type = "boot", cex.main = 0.8)
plot(mod.rpt.lon.id, grname = "id", type = "boot", cex.main = 0.8)
plot(mod.rpt.time.col, grname = "colony", type = "boot")

timing.plot.old <-
meta_df %>% 
  ggplot() +
  scale_fill_manual(values = wes_palette(n = 12, name = "Zissou1", type = "continuous")) +
  geom_histogram(aes(x = mid_julian, fill = as.factor(colony)),
                 position = "stack", alpha = 1, binwidth = 15) +
  scale_y_continuous(breaks = scales::breaks_pretty(3)) +
  labs(x = "Month", fill = "", y = "") +
  scale_x_continuous(breaks = c(-150, -120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
                     limits = c(-160, 120)) +
  # theme(axis.text.y = element_blank(),
  # axis.ticks.y = element_blank()) +
  facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y")

ggsave(timing.plot.old, filename = "plots/timing_plot_old.png", dpi = 500, width = 6, height = 8)

mod_ls <- split(mod_df, mod_df$area)

date_variation <- 
  mod_ls %>%
  lapply(function(x){sd(x$mid_julian)}) %>%
  do.call(rbind, .)

storm_average <- 
  mod_ls %>%
  lapply(function(x){mean(x$storm)}) %>%
  do.call(rbind, .)

lat_variation <- 
  mod_ls %>%
  lapply(function(x){sd(x$lat)}) %>%
  do.call(rbind, .)

lon_variation <- 
  mod_ls %>%
  lapply(function(x){sd(x$lon)}) %>%
  do.call(rbind, .)

summary(lm(date_variation ~ storm_average))

summary(lm(data = meta_df, start_julian ~ storm))


meta_df <- split(moult_df3, moult_df3$id_year) %>%
  lapply(., function(x){
    
    nsd_total <- sum(x$col_dist_smth ^ 2, na.rm = T) / (nrow(x) * 10000)
    
    x <- x %>% filter(prim_moult > 0)
    
    id <- x$id[1]
    id_year <- x$id_year[1]
    colony <- x$colony[1]
    area <- x$area[1]
    year <- x$year[1]
    logger <- x$logger[1]
    lat <- mean(x$lat_filled)
    lon <- mean(x$lon)
    duration <- max(x$prim_moult)
    start_date <- x$date[1]
    end_date <- x$date[nrow(x)]
    start_julian <- x$julian[1]
    end_julian <- x$julian[nrow(x)]
    mid_julian <- median(x$julian)
    col_dist_smth <- x$col_dist_smth[1]
    col_lat <- x$col_lat[1]
    col_lon <- x$col_lon[1]
    time_dry <- mean(x$flight, na.rm = T)
    
    out <- data.frame(id, id_year, colony, area,
                      year, logger, lat, lon,
                      duration, start_date, end_date,
                      start_julian, end_julian, mid_julian,
                      col_dist_smth, nsd_total,
                      col_lat, col_lon, time_dry)
    
    out
  }) %>%
  
  do.call(rbind, .) %>%
  
  mutate(id = as.factor(id),
         colony = as.factor(colony),
         area = as.factor(area),
         nsd_total = nsd_total ^ (1/2))

nsd_mod <- gam(data = meta_df,
               formula = duration ~
                 s(nsd_total, bs = "ts", k = 5) +
                 s(id, bs = "re"))

summary(nsd_mod)

plot(nsd_mod)

meta_df2 <- split(meta_df, meta_df$id) %>%
  lapply(function(x){if(nrow(x) < 2){x <- NULL}; x}) %>%
  do.call(rbind, .)

moult.timing.trend <- 
  ggplot(meta_df2) +
  geom_point(aes(x = (year + 2000),
                 y = mid_julian,
                 colour = factor(area, levels = c("Barents Sea",
                                                  "Iceland",
                                                  "Norwegian Sea",
                                                  "UK",
                                                  "Faroe Islands"))),
             alpha = 0.8) +
  geom_path(aes(x = (year + 2000), y = mid_julian,
                colour = factor(area, levels = c("Barents Sea",
                                                 "Iceland",
                                                 "Norwegian Sea",
                                                 "UK",
                                                 "Faroe Islands")),
                group = id), alpha = 0.8) +
  scale_y_continuous(breaks = c(-150, -120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
                     limits = c(-150, 110)) +
  labs(x = "Year (at start of non-breeding season)",
       y = "Moult timing (midpoint of inferred moult)",
       colour = "Colony\nArea") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

ggsave(moult.timing.trend, filename = "plots/moult_timings_id.png", width = 8, height = 5, dpi = 500)


table(meta_df2$colony)

date_range <- 
  split(meta_df2, meta_df2$id) %>%
  lapply(function(x){
    for(i in 2:nrow(x)){
      date_change <- x$mid_julian[i] - x$mid_julian[i - 1]
      id <- x$id[1]
      year <- x$year[i]
      area <- as.character(x$area[1])
      y <- data.frame(date_change, id, year, area)
      return(y)
    }}) %>%
  do.call(rbind, .) %>%
  mutate(area = ifelse(area == "Norwegian Sea", "Norwegian Sea (n = 18)", area),
         area = ifelse(area == "Iceland", "Iceland (n = 6)", area),
         area = ifelse(area == "Barents Sea", "Barents Sea (n = 4)", area),
         area = ifelse(area == "UK", "UK (n = 3)", area))

table(date_range$area)

date.range.plot <- 
  date_range  %>%
  ggplot() +
  scale_fill_brewer(palette = "Dark2") +
  geom_density(aes(x = date_change, fill = area), alpha = 0.6) +
  facet_wrap(facets = ~area, ncol = 1) +
  labs(x = "Change in date of midpoint of moult (days)", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

moult.change.plots <- plot_grid(moult.timing.trend, date.range.plot, nrow = 2)

ggsave(moult.change.plots, filename = "plots/moult_change_plots.png", width = 8, height = 8, dpi = 500)

t.test(meta_df$duration[which(meta_df$mid_julian < -10)],
       meta_df$duration[which(meta_df$mid_julian > -10)])

quantile(meta_df$duration)

ggplot(meta_df) + geom_density(aes(x = duration, fill = "blue"), position = "stack")

col_df <- data.frame(unique(meta_df$colony),
           unique(meta_df$col_lon),
           unique(meta_df$col_lat))

  names(col_df) <- c("colony", "col_lon", "col_lat")
  
  col_plot <-
  ggplot(col_df) +
  coord_map(projection = "azequidistant",
            xlim = c(-60, 30),
            ylim = c(50, 75)) +
    geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0.01,
               alpha = 0.7) +
    geom_point(aes(x = col_lon,
                   y = col_lat),
               shape = 19,
               size = 5,
               alpha = 1) +
    ggrepel::geom_label_repel(aes(x = col_lon,
                                  y = col_lat,
                                  label = colony),
                              point.padding = 0.5,
                              label.padding = 0.5,
                              box.padding = 0.5) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(colour = "")

ggsave(space_plot, filename = "plots/space_plot.png", width = 10, height = 15, dpi = 500)
