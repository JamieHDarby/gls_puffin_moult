
meta_df <- split(moult_df3, moult_df3$id_year) %>%
  lapply(., function(x){
    
    x <- x %>% filter(prim_moult > 0)
    
    id <- x$id[1]
    id_year <- x$id_year[1]
    colony <- x$colony[1]
    area <- x$area[1]
    year <- x$year[1]
    logger <- x$logger[1]
    lat <- x$lat_filled[1]
    lon <- x$lon[1]
    duration <- max(x$prim_moult)
    start_date <- x$date[1]
    end_date <- x$date[nrow(x)]
    start_julian <- x$julian[1]
    end_julian <- x$julian[nrow(x)]
    mid_julian <- median(x$julian)
    col_dist_smth <- x$col_dist_smth[1]
    col_lat <- x$col_lat[1]
    col_lon <- x$col_lon[1]
    nao <- x$noa[1]
    nao_winter <- x$noa_winter[1]
    nao_postb <- x$noa_postb[1]
    time_dry <- mean(x$flight, na.rm = T)
    
    out <- data.frame(id, id_year, colony, area,
                      year, logger, lat, lon,
                      duration, start_date, end_date,
                      start_julian, end_julian, mid_julian,
                      col_dist_smth,
                      col_lat, col_lon, time_dry,
                      nao, nao_winter, nao_postb)
    
    out
  }) %>%
  
  do.call(rbind, .)

require(ggplot2)

mod_df %>%
ggplot(aes(x = year, y = mid_julian, colour = colony, group = id)) + 
  geom_point() + 
  geom_path()

meta_df %>%
  ggplot(aes(x = year, y = start_julian, fill = year)) +
  geom_violin() +
  geom_smooth(aes(x = year, y = start_julian), inherit.aes = F, formula = "y ~ x")

meta_df2 <- meta_df %>% 
  mutate(colony = as.factor(colony),
         area = as.factor(area),
         id_year = id_year,
         id = as.factor(id),
         strategy = as.factor(ifelse(start_julian < -75, "e", "l")),
         duration = duration - 15)# %>%
  # filter(year > 13)

require(mgcv)
require(nlme)
require(mgcViz)

mod <- gamm(data = meta_df2,
                 formula = start_julian ~
              s(colony, bs = "re") +
              s(lat, bs = "ts", k = 5),
            correlation = corAR1(form = ~ year | id))

summary(mod$gam)
# summary(mod$lme)
plot(getViz(mod$gam))

concurvity(mod$gam)

strat_mod <- gamm(data = mod_df,
            formula = start_julian ~
              s(colony, bs = "re") +
              te(lon, lat, bs = "ts", k = 5) +
              s(year, bs = "ts", k = 5, by = strategy) +
              s(nao_winter, bs = "ts", k = 5, by = strategy),
            correlation = corAR1(form = ~ year | id))

summary(strat_mod$gam)
# summary(strat_mod$lme)
plot(strat_mod$gam)

bin_mod <- gamm(data = mod_df,
            formula = strategy ~
              s(colony, bs = "re") +
              te(lon, lat, bs = "ts", k = 5) +
              s(year, bs = "ts", k = 5) +
              s(nao_winter, bs = "ts", k = 5),
            family = binomial(link = "logit"),
            correlation = corAR1(form = ~ year | id),
            niterPQL = 50)

summary(bin_mod$gam)
plot(bin_mod$gam)

dur_mod <- gamm(data = mod_df,
                formula = duration ~
                  s(colony, bs = "re") +
                  te(lon, lat, bs = "ts", k = 5) +
                  s(year, bs = "ts", k = 5) +
                  s(nao_winter, bs = "ts", k = 5),
                family = "nb",
                correlation = corAR1(form = ~ year | id))

summary(dur_mod$gam)
plot(dur_mod$gam)

lat_mod <- gamm(data = mod_df,
                formula = lat ~
                  s(colony, bs = "re") +
                  # s(lon, bs = "ts", k = 5) +
                  s(year, bs = "ts", k = 5),# +
                  # s(nao_winter, bs = "ts", k = 5),
                # family = "nb",
                correlation = corAR1(form = ~ year | id))

summary(lat_mod$gam)
plot(lat_mod$gam)

