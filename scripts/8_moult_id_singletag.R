
# Load in required data ---------------------------------------------------

# Load in this dataframe
load(file = "data/cleaned/act_ls_dt.RData")

# Prep input list, discard empty entries and split by session id
act.ls.moulttest <- bind_rows(act.ls.dt) %>%
  # Create timing variables
  mutate(year = as.integer(format((as_date(date) - 182), "%y")),
         julian = as.integer(format(date, "%j")),
         id = individ_id,
         id_year = paste(id, year, sep = "_")) %>%
  # Get rid of summer dates
  filter(sun.angle > -3) %>%
  # Split by id year
  split(., .$id_year)

# Run moult identification process ----------------------------------------

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
    # Create a function to collect relevent data
    lapply(., function(x) {
      
      # Flying data per day
      flight <- (sum(1 - x$act) / nrow(x))

      vars <- c("date", "julian", "year",
                "id_year", "lat", "lon", "id",
                "colony", "col_lat", "col_lon",
                "logger")
      
      # Get numbver oif fixes in the day
      fixes <- nrow(x)
      
      # Create data frame
      out <- data.frame(fixes, flight)
      
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
                           countdown = 2,
                           min.rate = 0.01)
  
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
  
  if(sum(days.df$prim_moult > 0) == 0){indicator[i] <- "no moult detected"}
  
  # Monitor progress
  print(paste(i, days.df$colony[1], indicator[i], sep = ":    "))
}

# rbind list into overall dataframe
moult_df <- bind_rows(moult.ls) %>%
  
  # Create bout id and year id variables
  mutate(bout_id = ifelse(moult > 0,
    paste(id, year, moult_bout, sep = "_"),
    0))

# Graph some outputs ------------------------------------------------------

# Plot out the results
ggplot(moult_df) +
  theme(legend.position = "none") +
  scale_alpha_continuous(range = c(0, 1)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  geom_point(aes(x = date, y = -0.05,
                 alpha = prim_moult, size = prim_moult), colour = "dark blue") +
  geom_line(aes(x = date, y = flight), colour = "green") +
  geom_line(aes(x = date, y = rm_flight), colour = "blue") +
  labs(x = "Month", y = "Proportion of time flying") +
  facet_wrap(~ id, ncol =  2)

