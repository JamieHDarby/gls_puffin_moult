
# Prep input list, discard empty entries and split by session id
act.ls.moulttest <- do.call(rbind, act.ls.dt) %>%
  # Create timing variables
  mutate(year = as.integer(format((as_date(date) - 182), "%y")),
         julian = as.integer(format(date, "%j")),
         id = individ_id,
         id_year = paste(id, year, sep = "_")) %>%
  # Get rid of summer dates
  filter(sun.angle > -9) %>%
  # Split by id year
  split(., .$id_year)

# Create empty list
moult.ls <- list()
# And indicator variable
indicator <- rep("fine", length(act.ls.moulttest))

# Loop through index of files
for(i in 1:length(act.ls.moulttest)){
  
  df <- act.ls.moulttest[[i]] %>%
    mutate(act = std_conductivity_ad)
  
  # Collate daily data from the above
  days.df <- df %>%
    # Split by day
    split(.$date) %>%
    # Create a function to collect relevant data
    lapply(., function(x) {
      
      # Flying data per day
      flight <- (sum(1 - x$act_final) / nrow(x))
      
      flight_simp <- (sum(1 - x$std_conductivity) / nrow(x))
      flight_ad_simp <- (sum(1 - x$std_conductivity_ad) / nrow(x))
      
      # Tucking data per day
      tuck <- (sum(x$leg_tuck) / nrow(x))
      
      tuck_total <- sum(x$leg_tuck + x$leg_tuck2) / nrow(x)
      
      vars <- c("date", "julian", "year",
                "id_year", "lat", "lon", "id",
                "colony", "col_lat", "col_lon",
                "logger")
      
      # Get number of fixes in the day
      fixes <- nrow(x)
      
      # Create data frame
      out <- data.frame(fixes, flight, flight_simp,
                        flight_ad_simp, tuck, tuck_total)
      
      # Transfer over specified variables
      out[, vars] <- x[1, vars]
      
      # Return dataframe
      out
    }) %>%
    
    # Combine into dataframe
    do.call(rbind, .) %>%
    
    # INterpolate latitudes around equinoxes
    lat_filler(., lat.var = "lat")
  
  # Centre julian day around 0
  days.df$julian[which(days.df$julian > 150)] <-
    days.df$julian[which(days.df$julian > 150)] - 365
  
  # Get rolling mean of flight per day
  days.df$rm_flight <- days.df$flight
  # Get rolling mean of longitude
  days.df$rm_lon <- days.df$lon
  # Get rolling mean of latitude
  days.df$rm_lat <- days.df$lat_filled
  
  for(j in 3:(nrow(days.df) - 2)){
    days.df$rm_flight[j] <- mean(days.df$flight[(j - 2):(j + 2)])
    days.df$rm_lon[j] <- mean(days.df$lon[(j - 2):(j + 2)])
    days.df$rm_lat[j] <- mean(days.df$lat_filled[(j - 2):(j + 2)])}
  
  # Smoothed distance between points, only on longitudinal plane of first point
  days.df$point_dist <- 0
  for(j in 2:nrow(days.df)){
    days.df$point_dist[j] <- (
      pointDistance(p1 <- c(days.df$rm_lon[j], days.df$rm_lat[j]),
                    p2 <- c(days.df$rm_lon[j - 1], days.df$rm_lat[j]),
                    lonlat = T) / 1000)}
  
  # Smoothed distance to colony
  days.df$col_dist_smth <- (
    pointDistance(p1 <- data.frame(days.df$rm_lon, days.df$rm_lat),
                  p2 <- c(days.df$col_lon[1], days.df$col_lat[1]),
                  lonlat = T) / 1000)
  
  # Apply lowPoint function to each element of the list
  days.df <- lowPoint(days.df,
                      dist.lim = 1000,
                      min.group = 30,
                      t = 30,
                      prop.group = 0.2,
                      countdown = 1,
                      min.rate = 0.01)
  
  days.df$rm_flight2 <-
    ifelse(days.df$prim_moult > 0, 1, days.df$rm_flight)
  
  # Apply lowPoint function to each element of the list
  days.df <- lowPoint(days.df,
                      par = "rm_flight2",
                      prim.var = "sec_moult",
                      dist.lim = 1000,
                      min.group = 30,
                      t = 30,
                      prop.group = 0.2,
                      countdown = 1,
                      min.rate = 0.01)
  
  limits <- 
    seq(min(days.df$julian[which(days.df$prim_moult > 0)]) - 60,
        max(days.df$julian[which(days.df$prim_moult > 0)]) + 60)
  
  if(
    length(
      which(
        days.df[which(days.df$sec_moult > 0), "julian"] %in% limits
      )
    ) > 0){
    days.df$sec_moult <- 0
  }
  
  # Number the moult bouts in order of app
  days.df$moult_bout <- 0
  
  # Indicator to give moult bouts unique identifier
  ind <- 0
  
  # Loop through dataframe and recognise unique potential moult periods
  for(k in 2:nrow(days.df)){
    
    if(days.df$moult[k - 1] == 0 &
       days.df$moult[k] != 0){
      ind <- ind + 1
      days.df$moult_bout[k] <- ind
    }
    
    if(days.df$moult[k - 1] != 0 &
       days.df$moult[k] != 0){
      days.df$moult_bout[k] <- ind
    }
  }
  
  # Boole describing whether or not bird is in "moult" state
  days.df$moult_bool <- ifelse(days.df$moult == 0, 0, 1)
  
  # Put dataframe into list index
  moult.ls[[i]] <- days.df
  
  # Monitor progress
  print(paste(i, days.df$colony[1], indicator[i], sep = ":    "))
}

# rbind list into overall dataframe
moult_df <- do.call(rbind, moult.ls) %>%
  
  # Create bout id and year id variables
  mutate(bout_id = ifelse(moult > 0,
                          paste(id, year, moult_bout, sep = "_"),
                          0))

# Plot out the results
moult_plot <- 
  ggplot(moult_df) +
  theme(legend.position = "none") +
  scale_alpha_continuous(range = c(0, 1), breaks = c(0, 1)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  geom_point(aes(x = date, y = -0.05,
                 alpha = prim_moult),
             colour = "dark blue", size = 5, shape = 15) +
  geom_point(aes(x = date, y = -0.05,
                 alpha = sec_moult),
             colour = "dark red", size = 5, shape = 15) +
  geom_line(aes(x = date, y = flight), colour = "green") +
  geom_line(aes(x = date, y = rm_flight), colour = "blue") +
  labs(x = "Month", y = "Proportion of time dry") +
  facet_wrap(~ id, ncol =  2)

ggsave(moult_plot, filename = "plots/moult_plot.png",
       dpi = 500, height = 10, width = 10)

# Create a dataframe without April to explore the tucking behaviour
tuck_test <- moult_df %>%
  mutate(month = as.numeric(format(date, format = "%m")),
         leg = ifelse(grepl("R", id), "Right", "Left"),
         id = gsub('.{1}$', '', id)) %>%
  filter(month != 4)

# Metrics for proportion of time per day spent tucking/in flight
summary(tuck_test$flight)
summary(tuck_test$tuck)
summary(tuck_test$tuck_total)

# How many days did the bird spend less than 1% in flight
sum(tuck_test$flight < 0.01) / nrow(tuck_test)

# Plot out tucking over time by leg and by ID
tuck_plot <- ggplot(tuck_test) +
  geom_histogram(aes(x = date,
                     weight = (tuck * 10),
                     fill = leg),
                 position = "stack",
                 binwidth = 10) +
  theme_minimal() +
  labs(fill = "Leg", x = "Date", y = "Proportion of time tucking (%)") +
  facet_wrap(facets = ~id)

# Save off the tuck plot
ggsave(tuck_plot,
       filename = "plots/tuck_plot.png",
       dpi = 500, height = 6, width = 8)  

# Calculate flight metrics during moult
tuck_test %>% filter(prim_moult > 0 | sec_moult > 0) %>%
  select(flight, flight_simp, flight_ad_simp) %>% summary()

# Calculate flight metrics outside moult
tuck_test %>% filter(prim_moult == 0 & sec_moult == 0) %>%
  select(flight, flight_simp, flight_ad_simp) %>% summary()

# Get start/end dates of moult, as well as duration 
moult_df %>% filter(prim_moult > 0) %>%
  select(date, id) %>% split(., .$id) %>%
  lapply(., function(x) summary(x))

# Get start/end dates of second inferred moult, as well as duration
moult_df %>% filter(sec_moult > 0) %>%
  select(date, id) %>% split(., .$id) %>%
  lapply(., function(x) summary(x))

# Find average lat/lon of moult bouts
moult_meta <- split(moult_df, moult_df$id) %>%
  lapply(., function(x){
    x$moult <- x$prim_moult + x$sec_moult
    
    lat1 <- mean(x$lat_filled[which(x$moult > 0 & x$julian < 0)])
    lon1 <- mean(x$lon[which(x$moult > 0 & x$julian < 0)])
    
    lat2 <- mean(x$lat_filled[which(x$moult > 0 & x$julian > 0)])
    lon2 <- mean(x$lon[which(x$moult > 0 & x$julian > 0)])
    
    ID <- gsub('.{1}$', '', x$id[1])
    id <- x$id[1]
    out <- data.frame(id, ID, lon1, lat1, lon2, lat2)
    out
  }) %>%
  do.call(rbind, .)

# Plot of moulting areas
space_moult <- 
  ggplot(moult_df %>% mutate(ID = gsub('.{1}$', '', id))) +
  coord_map(projection = "azequidistant",
            xlim = c(-35, 5),
            ylim = c(35, 80)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0) +
    geom_path(aes(x = lon, y = lat_filled, colour = id)) +
    geom_point(data = moult_meta, 
              aes(x = lon1, y = lat1, fill = "Autumn Moult"),
              shape = 23, size = 8, alpha = 0.8) +
    geom_point(data = moult_meta, 
               aes(x = lon2, y = lat2, fill = "Spring Moult"),
               shape = 23, size = 8, alpha = 0.8) +
  geom_point(aes(x = col_lon, y = col_lat),
             shape = 21, colour = "black", fill = "dark red", size = 4) +
    facet_wrap(facets = ~ID, nrow = 2) +
    labs(fill = "", colour = "Logger ID", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Save it off
ggsave(space_moult, filename = "plots/space_plot.png",
       dpi = 500, height = 10, width = 10)
