library(ggplot2)

combo.ls2[[100]] %>%
  ggplot() + geom_density(aes(x = std_light, fill = logger))


combo.ls2[[100]] %>%
  ggplot() + geom_density(aes(x = act, fill = logger))

do.call(rbind, combo.ls2[1:200]) %>% filter(sun.angle > -6) %>%
  ggplot() + geom_density(aes(x = act, fill = logger), alpha = 0.5) +
  facet_wrap(facets = ~logger, ncol = 3)

do.call(rbind, combo.ls2[200:400]) %>% filter(sun.angle > -6) %>%
  ggplot() + geom_density(aes(x = std_light, fill = colony), alpha = 0.5) +
  facet_wrap(facets = ~logger, ncol = 1)

test4 <- unique(moult_df4$id_year)
test2 <- unique(moult_df2$id_year)
test <- unique(moult_df$id_year)

test
test[test %in% test2]
test[test %in% test4]
test[!test %in% test2]
test[!test %in% test4]
test4

length(test[!test %in% test2])

test <- do.call(rbind, combo.ls2)
test2 <- test[which(test$logger %in% c("mk4083", "mk4093", "mk18")),]
test3 <- test[which(!test$logger %in% c("mk4083", "mk4093", "mk18")),]
