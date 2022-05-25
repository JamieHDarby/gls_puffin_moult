
test <- lgh.ls2[[300]]

ggplot(test) +
  geom_point(aes(x = sun.angle, y = std_light), colour = "blue", alpha = 0.1) +
  geom_point(aes(x = sun.angle.unsm, y = std_light_ad), colour = "red", alpha = 0.1)
ggplot(lgh.ls2[[102]]) + geom_point(aes(x = sun.angle.unsm, y = std_light))
ggplot(test) +
  geom_density(aes(x = std_light_ad), fill = "red", alpha = 0.5)
ggplot(test) +
  geom_density(aes(x = sun.angle), fill = "blue", alpha = 0.5) 
ggplot(lgh.ls2[[102]]) + geom_density(aes(x = std_light), fill = "red", alpha = 0.5)
ggplot(lgh.ls2[[102]]) + geom_density(aes(x = sun.angle), fill = "blue", alpha = 0.5)

sum(lgh.ls2[[102]]$std_light > 0.5) / nrow(lgh.ls2[[102]])
sum(test$std_light_ad > 0.5) / nrow(test)

lgh.ls2[[300]]$logger[1]
test$logger[1]
test$colony[1]

lgh.ls2[[300]]$individ_id[1]
act.ls2[[300]]$individ_id[1]
lgh.ls2[[400]]$individ_id[1]
act.ls2[[400]]$individ_id[1]

library(mgcv)

mod_data <- do.call(rbind, lgh.ls2) %>%
  .[sample(c(1:nrow(.)), 100000), ] %>%
  mutate(logger_basic = ifelse(logger %in% c("f100", "c65", "w65"), "MT", "LT")) %>%
  mutate(logger = as.factor(logger), logger_basic = as.factor(logger_basic))

light_mod <- gam(data = mod_data,
           family = "quasibinomial",
           gamma = 2,
           formula = std_light_ad ~
             s(sun.angle.unsm, bs = "ts", by = logger_basic, k = 4))

summary(light_mod)

plot(light_mod)

save(light_mod, file = "data/cleaned/light_model.RData")
load(file = "data/cleaned/light_model.RData")

combo.df <- do.call(rbind, combo.ls) %>%
  mutate(logger_basic = ifelse(logger %in% c("f100", "c65", "w65"), "MT", "LT")) %>%
  mutate(logger = as.factor(logger), logger_basic = as.factor(logger_basic))

lgh.df <- do.call(rbind, lgh.ls2) %>%
  mutate(logger_basic = ifelse(logger %in% c("f100", "c65", "w65"), "MT", "LT")) %>%
  mutate(logger = as.factor(logger), logger_basic = as.factor(logger_basic))

act.df <- do.call(rbind, act.ls2) %>%
  mutate(logger_basic = ifelse(logger %in% c("f100", "c65", "w65"), "MT", "LT")) %>%
  mutate(logger = as.factor(logger), logger_basic = as.factor(logger_basic))

system.time(combo.df$light.pred <- predict.gam(light_mod, combo.df))
system.time(act.df$light.pred <- predict.gam(light_mod, act.df))
system.time(lgh.df$light.pred <- predict.gam(light_mod, lgh.df))

combo.df$light.pred[which(combo.df$light.pred > 1)] <- 1
combo.df$light.pred[which(combo.df$light.pred < 0)] <- 0
act.df$light.pred[which(act.df$light.pred > 1)] <- 1
act.df$light.pred[which(act.df$light.pred < 0)] <- 0
lgh.df$light.pred[which(lgh.df$light.pred > 1)] <- 1
lgh.df$light.pred[which(lgh.df$light.pred < 0)] <- 0

save(combo.df, file = "data/cleaned/combo_df.RData")
save(lgh.df, file = "data/cleaned/lgh_df.RData")
save(act.df, file = "data/cleaned/act_df.RData")
load(file = "data/cleaned/combo_df.RData")
load(file = "data/cleaned/lgh_df.RData")
load(file = "data/cleaned/act_df.RData")
