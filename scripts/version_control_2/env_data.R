
# Write csv out of puffin moult dataframe, with 12:00 written in for all times
moult_df3$time <- hms("12:00:00")
moult_df3$date_time <- ymd_hms(paste(moult_df3$date, moult_df3$time))

write.csv(moult_df3, file = "data/cleaned/moult_df3_pre_mb.csv")

puffin_env <- read.csv(file = "data/env/puffin_mb.csv")

sum(puffin_env$Longitude != moult_df3$Longtiude)

moult_df3[, c("Wave", "WindU", "WindV")] <- puffin_env[, c("Wave", "WindU", "WindV")]

moult_df3$WindSpeed <- sqrt((moult_df3$WindU^2) + (moult_df3$WindV^2))

ggplot(moult_df3) + geom_density(aes(x = Wave))
