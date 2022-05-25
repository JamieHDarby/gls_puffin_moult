
# Load in act list
load("data/cleaned/act_ls_sm2.RData")
act.ls.sm1 <- act.ls %>%
  lapply(., function(x){
    x <- x %>%
      dplyr::select(-logger)
    x
  })

# Load in act list
load("data/cleaned/act_ls_post.RData")
act.ls.sm2 <- act.ls %>%
  lapply(., function(x){
    
    x <- x %>%
      dplyr::select(-logger_model)
    
    if(x$colony[1] != "Skellig Michael"){
      x <- NULL
    }
    x
  })

# Combine the two lists
act.ls.sm.full <- c(act.ls.sm1, act.ls.sm2) %>%
  .[!sapply(., is.null)]

# Get rid of these variables
rm(act.ls, act.ls.sm1, act.ls.sm2)

# Prep input list, discard empty entries and split by session id
act.ls.moulttest <- bind_rows(act.ls.sm.full) %>%
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
    mutate(act = std_conductivity_ad)
  
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
                "colony", "col_lat", "col_lon")
      
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
                        min.group = 25,
                        t = 25,
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
  
  if(sum(days.df$fixes < 40) > 10){indicator[i] <- "too few fixes"}
  
  if(nrow(days.df) < 200){indicator[i] <- "too few days"}
  
  # Monitor progress
  print(paste(i, days.df$colony[1], indicator[i], sep = ":    "))
}

# rbind list into overall dataframe
moult_df_sm <- bind_rows(moult.ls[which(indicator == "fine")]) %>%
  # Create bout id and year id variables
  mutate(bout_id = ifelse(moult > 0,
                          paste(id, year, moult_bout, sep = "_"),
                          0))

# Graph timing plot
graph_df <- 
  moult_df_sm %>%
  mutate(id = fct_reorder(id, julian, .fun = 'min'),
         ID = substr(id, 5, 11),
         month = as.numeric(format(date, format = "%m")))

# Insert dummy year so all individuals can use a shared x-axis
year(graph_df$date[which(graph_df$month < 6)]) <- 2021
year(graph_df$date[which(graph_df$month > 6)]) <- 2020

# Plot out the results
skellig_moult_plot <-
  ggplot(graph_df) +
  theme(legend.position = "none") +
  scale_alpha_continuous(range = c(0, 1), breaks = c(0, 1)) +
  scale_size_continuous(range = c(3, 3)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  geom_point(data = (graph_df %>% filter(prim_moult > 0)),
                     aes(x = date, y = -0.05),
             shape = 15, colour = "dark blue", size = 3) +
  geom_line(aes(x = date, y = flight), colour = "green") +
  geom_line(aes(x = date, y = rm_flight), colour = "blue") +
  labs(x = "Month", y = "Proportion of time dry") +
  facet_wrap(~id, ncol = 1, strip.position = "top") +
  theme_minimal() +
  theme(legend.position = "none")

# Find average lat/lon of moult bouts
moult_meta <- split(moult_df_sm, moult_df_sm$id) %>%
  lapply(., function(x){
    y <- x[which(x$prim_moult > 0), ]
    
    lat1 <- mean(y$lat_filled)
    lon1 <- mean(y$lon)
    
    date <- y$date[1]
    duration <- nrow(y)
    ID <- y$id[1]
    out <- data.frame(ID, lon1, lat1, date, duration)
    out
  }) %>%
  do.call(rbind, .)

# Plot of moulting areas
space_moult <- 
  ggplot(moult_df_sm %>% mutate(ID = id)) +
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
  geom_path(aes(x = lon, y = lat_filled, colour = id), size = 0.8) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  scale_fill_brewer(palette = "Dark2") +
  geom_point(data = moult_meta, 
             aes(x = lon1, y = lat1, fill = ID),
             shape = 23, size = 8, alpha = 0.9) +
  geom_point(aes(x = col_lon, y = col_lat),
             shape = 21, colour = "black", fill = "dark red", size = 4) +
  labs(fill = "Moult\nLocaion", colour = "", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(strip.text = element_blank(),
        legend.text = element_blank()) +
  facet_wrap(facets = ~ID, ncol = 1)

# Combine plots of flight time series and moult locations
skellig_combo_plot <- 
  plot_grid(skellig_moult_plot, space_moult, nrow = 1)

# Save off this plot
ggsave(skellig_combo_plot, filename = "plots/skellig_moult_plot.png",
       dpi = 500, height = 8, width = 8)

# rbind list into overall dataframe
moult_df_sm <- bind_rows(moult.ls[which(indicator != "too few days")]) %>%
  # Create bout id and year id variables
  mutate(bout_id = ifelse(moult > 0,
                          paste(id, year, moult_bout, sep = "_"),
                          0))

# Graph timing plot
graph_df <- bind_rows(moult.ls) %>%
  # Create bout id and year id variables
  mutate(bout_id = ifelse(moult > 0,
                          paste(id, year, moult_bout, sep = "_"),
                          0)) %>%
  mutate(id = fct_reorder(id, julian, .fun = 'min'),
         id = fct_reorder(id, colony, .fun = 'min'),
         month = as.numeric(format(date, format = "%m")),
         year = as.character(year + 2000)) %>%
  filter(!month %in% c(5:7), year != 2019) 

# Insert dummy year so all individuals can use a shared x-axis
year(graph_df$date[which(graph_df$month < 6)]) <- 2021
year(graph_df$date[which(graph_df$month > 6)]) <- 2020

# Create dataframe to plot out tuck proportions by year
graph_df <- split(graph_df, graph_df$year) %>%
  lapply(., function(x){
    
    date <- unique(x$date)
    
    tuck <- rep(0, length(date))
    
    for(i in 1:length(date)){
      tuck[i] <- mean(x$tuck[which(x$date == date[i])], na.rm = T)
    }
    
    df <- data.frame(date, tuck)
    
    df$year <- x$year[1]
    
    df
  }) %>%
  bind_rows(.)

# Plot out the results
skellig_tuck_plot <-
  ggplot(graph_df) +
  theme(legend.position = "none") +
    scale_fill_brewer(palette = "Pastel1") +
  scale_size_continuous(range = c(3, 3)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  geom_histogram(aes(x = date, weight = (tuck) * (100 / 15), fill = year),
                 alpha = 0.8, binwidth = 15) +
  labs(x = "Month", y = "Time tucking equipped leg (%)", fill = "Year") +
  theme_minimal() +
  facet_wrap(facets = ~year)

# Save off this plot
ggsave(skellig_tuck_plot, filename = "plots/skellig_tuck_plot.png",
       dpi = 500, height = 4, width = 8)

# Plot out failed moult identifications for supplementary materials -------

# Graph timing plot
graph_df <- 
  bind_rows(moult.ls[which(indicator != "fine")]) %>%
  mutate(id = fct_reorder(id, julian, .fun = 'min'),
         ID = substr(id, 5, 11),
         month = as.numeric(format(date, format = "%m")))

# Insert dummy year so all individuals can use a shared x-axis
year(graph_df$date[which(graph_df$month < 6)]) <- 2021
year(graph_df$date[which(graph_df$month > 6)]) <- 2020

# Plot out the results
skellig_failed_moult_plot <-
  ggplot(graph_df %>% filter(ID %in% c("EW67601", "EX52006",
                                       "EW66639", "EW67605",
                                       "EW66645", "EW67608",
                                       "EX52011", "EX52003"))) +
  theme(legend.position = "none") +
  scale_alpha_continuous(range = c(0, 1), breaks = c(0, 1)) +
  scale_size_continuous(range = c(3, 3)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_line(aes(x = date, y = flight), colour = "green") +
  geom_line(aes(x = date, y = rm_flight), colour = "blue") +
  labs(x = "Month", y = "Proportion of time dry") +
  facet_wrap(~ID, ncol = 2, strip.position = "top") +
  theme_minimal() +
  theme(legend.position = "none")

# Take a look
skellig_failed_moult_plot

# Save off this plot
ggsave(skellig_failed_moult_plot, filename = "plots/failed_moult_plot.png",
       dpi = 500, height = 8, width = 6)
