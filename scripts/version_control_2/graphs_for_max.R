
require(ggplot2)
require(lubridate)

example <- act.ls3[[30]] %>%
  mutate(light_diff = light.pred - std_light_ad,
         time = format(.$date_time, "%H:%M:%S")) %>%
  mutate(light_diff = ifelse(light_diff < 0, 0, light_diff),
         time = as.POSIXct(.$time, format = "%H:%M:%S")) %>%
  mutate(time = as.numeric(.$time)) %>%
  filter(sun.angle > -10)

ggplot(example) +
  scale_fill_viridis_c(option = "A") +
  geom_tile(aes(x = date, y = time, fill = std_light_ad), height = 1000)

ggplot(example) +
  scale_fill_viridis_c(option = "A") +
  geom_tile(aes(x = date, y = time, fill = std_conductivity), height = 1000)

ggplot(example) +
  scale_fill_viridis_c(option = "A") +
  geom_tile(aes(x = date, y = time, fill = light.pred), height = 1000)

ggplot(example) +
  scale_fill_viridis_c(option = "D") +
  geom_tile(aes(x = date, y = time, fill = light_diff), height = 1000)

# install.packages("TSA")
require(TSA)

unique_julian <- unique(example$julian)

results <- rep(0, length(unique_julian))

for(i in 1:length(unique_julian)){
example_test <- example %>% filter(julian == unique_julian[i])

results[i] <- (arimax(
  x = example_test$std_conductivity,
  xreg = example_test[, c("sun.angle", "std_light_ad")]))$aic
}

results <- unlist(results)

ggplot() +
  geom_point(aes(x = 1:length(results), y = results), size = 3) +
  geom_smooth(aes(x = 1:length(results), y = results))


unique_julian <- unique(example$julian)

results <- rep(0, length(unique_julian))

for(i in 1:length(unique_julian)){
  example_test <- example %>% filter(julian == unique_julian[i])
  
  change <- rep(0, nrow(example_test))
  
  for(j in 2:nrow(example_test)){
    change[j] <- abs(example_test$std_conductivity[j] -
                       example_test$std_conductivity[j - 1])
  }
  
  results[i] <- sum(change)/nrow(example_test)
 
}

ggplot() +
  geom_point(aes(x = 1:length(results), y = results), size = 3) +
  geom_smooth(aes(x = 1:length(results), y = results))

