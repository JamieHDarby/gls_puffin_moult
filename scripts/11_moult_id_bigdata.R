# Load in act list
load("data/cleaned/act_ls_post.RData")

# Prep input list, discard empty entries and split by session id
act.ls.moulttest <- bind_rows(act.ls) %>%
  # Create timing variables
  mutate(year = as.integer(format((as_date(date) - 182), "%y")),
         julian = as.integer(format(date, "%j")),
         id = individ_id,
         id_year = paste(id, year, sep = "_")) %>%
  # Split by id year
  split(., .$id_year)

# Create empty list
moult.ls <- list()
# And indicator variable
indicator <- rep("fine", length(act.ls.moulttest))

# Loop through index of files
for(i in 1:length(act.ls.moulttest)){
  
  df <- act.ls.moulttest[[i]] %>%
    mutate(act = std_conductivity_ad2)
  
  # Collate daily data from the above
  days.df <- df %>%
    # Split by day
    split(.$date) %>%
    # Create a function to collect relevent data
    lapply(., function(x) {
      
      # Get rid of night fixes
      y <- x[which(x$sun.angle > -3), ]
      
      # Get numbver of fixes in the day
      fixes <- nrow(y)
      
      # Flying data per day
      flight <- (sum(1 - y$act) / nrow(y))
      
      # Tucking data per day
      tuck <- (sum(y$leg_tuck) / nrow(y))
      
      flight[which(is.na(flight))] <- 0
      tuck[which(is.na(tuck))] <- 0
      
      vars <- c("date", "julian", "year",
                "id_year", "lat", "lon", "id",
                "colony", "col_lat", "col_lon",
                "logger_model")
      
      # Create data frame
      out <- data.frame(fixes, flight, tuck)
      
      # Transfer over specified variables
      out[, vars] <- x[1, vars]
      
      # Return dataframe
      out
    }) %>%
    
    # Combine into dataframe
    bind_rows(.) %>%
    
    # INterpolate latitudes around equinoxes
    lat_filler(., lat.var = "lat")
  
  # Centre julian day around 0
  days.df$julian[which(days.df$julian > 150)] <-
    days.df$julian[which(days.df$julian > 150)] - 365
  
  if(nrow(days.df) > 200){
    
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
                      prop.group = 0.001,
                      countdown = 2,
                      min.rate = 0.01)
  }else{
    days.df$prim_moult <- 0
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
  
  if(sum(days.df$prim_moult > 0) == 0){indicator[i] <- "no moult detected"}
  
  if(sum(days.df$moult > 0) > (2 * max(days.df$prim_moult))){
    indicator[i] <- "too many flightless periods"}
  
  if(sum(days.df$flight < 0.02) > (nrow(days.df) * 0.5)){
    indicator[i] <- "too many flightless periods"}
  
  if(sum(days.df$flight > 0.1) > (nrow(days.df) * 0.75)){
    indicator[i] <- "flight overestimated"}
  
  if(max(days.df$prim_moult) > 70){indicator[i] <- "moult detection error"}
  
  if(sum(days.df$fixes < 40) > 10){indicator[i] <- "too few fixes"}
  
  if(nrow(days.df) < 200){indicator[i] <- "too few days"}
  
  # Monitor progress
  print(paste(i, days.df$colony[1], indicator[i], sep = ":    "))
}

# rbind list into overall dataframe
moult_df <- bind_rows(moult.ls[which(indicator == "fine")]) %>%
  # Create bout id and year id variables
  mutate(bout_id = ifelse(moult > 0,
                          paste(id, year, moult_bout, sep = "_"),
                          0))

require(forcats)

# Graph timing plot
graph_df <- 
  moult_df %>%
  mutate(id = fct_reorder(id, julian, .fun = 'min'),
         id = fct_reorder(id, colony, .fun = 'min'),
         month = as.numeric(format(date, format = "%m")))

# Insert dummy year so all individuals can use a shared x-axis
year(graph_df$date[which(graph_df$month < 6)]) <- 2021
year(graph_df$date[which(graph_df$month > 6)]) <- 2020

# Plot out the results
ggplot(graph_df) +
  theme(legend.position = "none") +
  scale_alpha_continuous(range = c(0, 1), breaks = c(0, 1)) +
  scale_size_continuous(range = c(3, 3)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_point(aes(x = date, y = -0.15,
                 alpha = prim_moult, size = prim_moult),
             shape = 15, colour = "dark blue") +
  geom_line(aes(x = date, y = flight), colour = "green") +
  geom_line(aes(x = date, y = rm_flight), colour = "blue") +
  labs(x = "Month", y = "Proportion of time flying") +
  facet_wrap(~ paste(id_year, colony), ncol =  4)

# Plot out moult timings
ggplot(graph_df %>% filter(prim_moult > 0)) +
  geom_point(aes(y = id, x = date, size = prim_moult, colour = colony), shape = 15) +
  scale_size_continuous(range = c(3, 3), breaks = c(1, 20)) +
  scale_colour_brewer(palette = "Dark2")
  scale_x_date(date_labels = "%b",
               limits = c(ymd("2020-9-01"), ymd("2021-05-01")),
               date_breaks = "1 month") +
  labs(x = "Time of year", y = "Bird ID", colour = "Colony")

table(indicator)
