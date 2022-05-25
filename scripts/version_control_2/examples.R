
library(ggplot2)
library(cowplot)

# Split moult dataframe into list of graphable dataframes
graph_df <- split(moult_df3, moult_df3$id)

# Example finding graph
ind <- c(3)

do.call(rbind, graph_df[ind]) %>%
  mutate(migration = ifelse(point_dist > 100, 0.3, 0)) %>%
  ggplot() +
  theme(legend.position = "none") +
  scale_alpha_continuous(range = c(0, 1)) +
  scale_x_continuous(breaks = c(-120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) +
  # geom_point(aes(x = julian, y = -0.05,
  #                alpha = moult, size = moult), colour = "dark green") +
  geom_point(aes(x = julian, y = -0.05,
  alpha = prim_moult, size = prim_moult), colour = "dark blue") +
  geom_line(aes(x = julian, y = flight), colour = "green") +
  geom_line(aes(x = julian, y = rm_flight), colour = "blue") +
  # geom_line(aes(x = julian, y = (tucking + flight)), colour = "red") +
  labs(x = "Month", y = "Proportion of time flying",
       title = do.call(rbind, graph_df[ind])$colony[1],
       caption = do.call(rbind, graph_df[ind])$logger[1]) +
  facet_wrap(~ paste(id, year, sep = "_"), ncol =  1)


ex_1 <- do.call(rbind, graph_df[129]) %>%
  mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
  mutate(migration = ifelse(point_dist > 100, 0.6, 0)) %>%
  ggplot() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-0.2, 0.7), breaks = c(0, 0.2, 0.4, 0.6)) +
  scale_x_continuous(breaks = c(-151, -120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) +
  scale_alpha_continuous(range = c(0, 1)) +
  geom_point(aes(x = julian, alpha = prim_moult, y = -0.1), size = 4, colour = "dark blue") +
  geom_line(aes(x = julian, y = flight), colour = "green") +
  geom_line(aes(x = julian, y = rm_flight), colour = "blue") +
  labs(x = "Month", y = "Proportion of time dry",
       title = paste("Example 1", do.call(rbind, graph_df[129])$colony[1], sep = " - ")) +
  facet_wrap(~ paste(id, (year + 2000), sep = " - "), ncol =  1)

ex_2 <- do.call(rbind, graph_df[5]) %>%
  mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
  mutate(migration = ifelse(point_dist > 100, 0.6, 0)) %>%
  ggplot() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-0.2, 0.7), breaks = c(0, 0.2, 0.4, 0.6)) +
  scale_x_continuous(breaks = c(-151, -120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) +
  scale_alpha_continuous(range = c(0, 1)) +
  geom_point(aes(x = julian, alpha = prim_moult, y = -0.1), size = 4, colour = "dark blue") +
  geom_line(aes(x = julian, y = flight), colour = "green") +
  geom_line(aes(x = julian, y = rm_flight), colour = "blue") +
  labs(x = "Month", y = "Proportion of time dry",
       title = paste("Example 2", do.call(rbind, graph_df[5])$colony[1], sep = " - ")) +
  facet_wrap(~ paste(id, (year + 2000), sep = " - "), ncol =  1)

ex_3 <- do.call(rbind, graph_df[34]) %>%
  mutate(prim_moult = ifelse(prim_moult > 0, 1, 0)) %>%
  mutate(migration = ifelse(point_dist > 100, 0.6, 0)) %>%
  ggplot() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-0.2, 0.7), breaks = c(0, 0.2, 0.4, 0.6)) +
  scale_x_continuous(breaks = c(-151, -120, -90, -60, -30, 0, 30, 60, 90),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) +
  scale_alpha_continuous(range = c(0, 1)) +
  geom_point(aes(x = julian, alpha = prim_moult, y = -0.1), size = 4, colour = "dark blue") +
  geom_line(aes(x = julian, y = flight), colour = "green") +
  geom_line(aes(x = julian, y = rm_flight), colour = "blue") +
  labs(x = "Month", y = "Proportion of time dry",
       title = paste("Example 3", do.call(rbind, graph_df[26])$colony[1], sep = " - ")) +
  facet_wrap(~ paste(id, (year + 2000), sep = " - "), ncol =  1)

# require(cowplot)

ex_plot <- plot_grid(ex_1, plot_grid(ex_2, ex_3, nrow = 2), nrow = 1)

ex_plot

ggsave(ex_plot, filename = "plots/example_plot.png", dpi = 500, height = 8, width = 8)
