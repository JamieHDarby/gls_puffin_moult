
graph_df <- split(moult_df3, moult_df3$id)

# Example finding graph
ind <- c(7)

do.call(rbind, graph_df[ind]) %>%
  mutate(migration = ifelse(point_dist > 100, 0.3, 0)) %>%
  ggplot() +
  theme(legend.position = "none") +
  scale_alpha_continuous(range = c(0, 1)) +
  scale_x_continuous(breaks = c(-120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) +
  geom_point(aes(x = julian, y = -0.05,
                 alpha = moult, size = moult), colour = "dark green") +
  geom_point(aes(x = julian, y = -0.05,
                 alpha = prim_moult, size = prim_moult), colour = "dark blue") +
  geom_line(aes(x = julian, y = flight), colour = "green") +
  geom_line(aes(x = julian, y = rm_flight), colour = "dark green") +
  # geom_line(aes(x = julian, y = ars), colour = "blue") +
  # geom_line(aes(x = julian, y = tuck), colour = "red") +
  # geom_line(aes(x = julian, y = (ars)), colour = "red") +
  # geom_line(aes(x = julian, y = long_fl), colour = "blue") +
  # geom_line(aes(x = julian, y = fixes/300), colour = "black") +
  # geom_area(aes(x = julian, y = (migration)), fill = "dark red", alpha = 0.5) +
  labs(x = "Month", y = "Proportion of time flying",
       title = do.call(rbind, graph_df[ind])$colony[1],
       caption = do.call(rbind, graph_df[ind])$logger[1]) +
  facet_wrap(~ paste(id, year, sep = "_"), ncol =  1)

# timing.plot.old <- 
moult_df3 %>% 
  filter(moult > 0) %>%
  ggplot() +
  scale_fill_manual(values = wes_palette(n = 16, name = "Zissou1", type = "continuous")) +
  geom_histogram(aes(x = julian, fill = as.factor(colony), weight = 1/(length(unique(id))) * 1/15),
                 position = "stack", alpha = 0.8, binwidth = 15) +
  labs(x = "Month", fill = "", y = "") +
  scale_x_continuous(breaks = c(-150, -120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
                     limits = c(-160, 120)) +
  # theme(axis.text.y = element_blank(),
  # axis.ticks.y = element_blank()) +
  facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y")
