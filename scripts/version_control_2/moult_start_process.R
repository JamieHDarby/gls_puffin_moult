
moult_start_df <- filter(.data = moult_df3, prim_moult > 0) %>%
  split(., .$bout_id) %>%
  lapply(., function(x){x[1,]}) %>%
  do.call(rbind, .)

moult_start_df %>%
  ggplot() +
  geom_density(aes(x = julian, fill = as.factor(colony)), position = "stack", alpha = 0.6) +
  facet_wrap(~ as.factor(area), ncol = 1, scales = "free_y")

test_df <- moult_start_df %>%
  split(., .$colony) %>%
  lapply(., function(x){
    col <- (x$colony[1])
    area <- (x$area[1])
    sd_day <- (sd(x$julian, na.rm = T))
    dist <- (mean(x$col_dist_smth, na.rm = T))
    duration <- mean(x$prim_moult, na.rm = T)
    out <- data.frame(col, sd_day, dist, duration, area)}) %>%
  do.call(rbind, .)

test_df

summary(lm(data = test_df, formula = duration ~ dist))

plot(test_df)

ggplot(test_df) +
  geom_point(aes(x = duration, y = dist, fill = col), shape = 21, size = 6) +
  geom_line(aes(x = duration, y = dist, colour = area))

summary(lm(data = test_df %>% filter(area != "UK"), formula = duration ~ dist))

ggplot(moult_start_df) +
  geom_density(aes(x = prim_moult, fill = 1), alpha = 0.4)

summary(lm(data = moult_start_df, formula = prim_moult ~ col_dist_smth + julian))

