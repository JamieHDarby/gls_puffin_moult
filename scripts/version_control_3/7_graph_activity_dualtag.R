
# Make a subset of activity data for plotting
act_example <- do.call(rbind, act.ls.dt) %>%
  mutate(time = format(.$date_time, "%H:%M:%S")) %>%
  mutate(time = as_hms(.$time),
         day_night = ifelse(sun.angle > -9, "day", "night"),
         sun.angle.night = ifelse(day_night == "day", 0, sun.angle))

# Breaks for graph y axis
day_breaks <- c(as_hms("00:00:00"),
                as_hms("06:00:00"),
                as_hms("12:00:00"),
                as_hms("18:00:00"),
                as_hms("24:00:00"))

# Plot adjusted activity data timeseries
plot_1A <- 
  ggplot(act_example %>% filter(individ_id == "BTO_EJ47625L")) +
  scale_fill_viridis_c(option = "A") +
  geom_tile(aes(x = date, y = time,
                fill = std_conductivity),
            height = 600, width = 1) +
  scale_y_time(breaks = day_breaks) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(fill = "Proportion\ntime wet\nraw data",
       y = NULL, x = NULL, title = "1A") +
  theme_minimal()


# Plot adjusted activity data timeseries
plot_1B <- 
  ggplot(act_example %>% filter(individ_id == "BTO_EJ47625R")) +
  scale_fill_viridis_c(option = "A") +
  geom_tile(aes(x = date, y = time,
                fill = std_conductivity),
            height = 600, width = 1) +
  scale_y_time(breaks = day_breaks) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(fill = "Proportion\ntime wet\nraw data",
       y = NULL, x = NULL, title = "1B") +
  theme_minimal()

# Plot leg tucks identified
plot_2 <- 
  ggplot(act_example %>% filter(individ_id == "BTO_EJ47625L")) +
  geom_tile(aes(x = date, y = time, alpha = leg_tuck, fill = "Left"),
            height = 600) +
  geom_tile(aes(x = date, y = time, alpha = leg_tuck2, fill = "Right"),
            height = 600) +
  scale_alpha_continuous(range = c(0, 0.8), guide = "none") +
  scale_y_time(breaks = day_breaks) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(fill = "Leg",
       y = NULL, x = NULL, title = "2") +
  theme_minimal()

# Plot adjusted activity data timeseries
plot_3A <- 
  ggplot(act_example %>% filter(individ_id == "BTO_EJ47625L")) +
  scale_fill_viridis_c(option = "A") +
  geom_tile(aes(x = date, y = time,
                fill = std_conductivity_ad),
            height = 600, width = 1) +
  scale_y_time(breaks = day_breaks) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(fill = "Proportion\ntime wet\nadjusted\nfor tuck",
       y = NULL, x = NULL, title = "3A") +
  theme_minimal()

# Plot adjusted activity data timeseries
plot_3B <- 
  ggplot(act_example %>% filter(individ_id == "BTO_EJ47625R")) +
  scale_fill_viridis_c(option = "A") +
  geom_tile(aes(x = date, y = time,
                fill = std_conductivity_ad),
            height = 600, width = 1) +
  scale_y_time(breaks = day_breaks) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(fill = "Proportion\ntime wet\nadjusted\nfor tuck",
       y = NULL, x = NULL, title = "3B") +
  theme_minimal()

# Plot adjusted activity data timeseries
plot_4 <- 
  ggplot(act_example %>% filter(individ_id == "BTO_EJ47625L")) +
  scale_fill_viridis_c(option = "D", trans = "reverse") +
  geom_tile(aes(x = date, y = time,
                fill = act_final,
                alpha = sun.angle.night),
            height = 600, width = 1) +
  scale_alpha_continuous(guide = "none") +
  scale_y_time(breaks = day_breaks) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(fill = "Proportion\ntime wet\nfully adjusted,\nboth legs",
       y = NULL, x = NULL, title = "4") +
  theme_minimal()

process_plot <- 
  plot_grid(plot_grid(plot_1A, plot_1B, nrow = 1),
          plot_2,
          plot_grid(plot_3A, plot_3B, nrow = 1),
          plot_4,
          nrow = 4)

ggsave(process_plot, filename = "plots/process_plot.png",
       dpi = 500, height = 12, width = 10)
