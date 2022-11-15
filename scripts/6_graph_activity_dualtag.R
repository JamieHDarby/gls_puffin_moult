
# Load in this dataframe
load(file = "data/cleaned/act_ls_dt.RData")

# Make a subset of activity data for plotting
act_example <- bind_rows(act.ls.dt) %>%
  mutate(time = format(.$date_time, "%H:%M:%S")) %>%
  mutate(time = as_hms(.$time),
         day_night = ifelse(sun.angle > -6, "day", "night"),
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
       y = NULL, x = NULL, title = "A") +
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
       y = NULL, x = NULL, title = "B") +
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
       y = NULL, x = NULL, title = "C") +
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
       y = NULL, x = NULL, title = "D") +
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
       y = NULL, x = NULL, title = "E") +
  theme_minimal()

# Plot adjusted activity data timeseries
plot_4 <-
  ggplot(act_example %>% filter(individ_id == "BTO_EJ47625L")) +
  scale_fill_viridis_c(option = "D", trans = "reverse", values = c(0, 0.05, 0.15, 0.4, 0.6, 1)) +
  geom_tile(aes(x = date, y = time,
                fill = act_final,
                alpha = sun.angle.night),
            height = 600, width = 1) +
  scale_alpha_continuous(guide = "none") +
  scale_y_time(breaks = day_breaks) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(fill = "Proportion\ntime wet\nfully adjusted,\nboth legs",
       y = NULL, x = NULL, title = "F") +
  theme_minimal()

# Subset a dataframe of moults to retain left legs only
graph_df <- moult_df %>%
  filter(!grepl("R", id)) %>%
  mutate(id = gsub('.{1}$', '', id)) %>%
  mutate(ID = substr(id, 5, 11))

# Plot out the results

plot_5 <-
  ggplot(graph_df %>% filter(ID == "EJ47625")) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, size = 8)) +
  scale_alpha_continuous(range = c(0, 1), breaks = c(0, 1), guide = "none") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  # scale_y_continuous(trans = "sqrt") +
  geom_point(aes(x = date, y = -0.1,
                 alpha = prim_moult,
                 colour = "First moult\ndetected"), size = 5, shape = 15) +
  geom_point(aes(x = date, y = -0.1,
                 alpha = sec_moult,
                 colour = "Second moult\ndetected"), size = 5, shape = 15) +
  geom_line(aes(x = date, y = sqrt(flight)), colour = "dark green") +
  geom_line(aes(x = date, y = sqrt(rm_flight)), colour = "red") +
  labs(x = "Month", y = "Proportion\nof time dry", colour = NULL, title = "G") +
  geom_hline(aes(yintercept = 0)) +
  scale_y_continuous(breaks = c(0, 0.224, 0.447, 0.671),
                     labels = c("0", "0.05", "0.2", "0.45"))

# Compile process plot
process_plot <- 
  plot_grid(plot_grid(plot_1A, plot_1B, nrow = 1),
          plot_2,
          plot_grid(plot_3A, plot_3B, nrow = 1),
          plot_4,
          plot_5,
          nrow = 5)

process_plot

ggsave(process_plot, filename = "plots/process_plot.png",
       dpi = 500, height = 12, width = 10)
