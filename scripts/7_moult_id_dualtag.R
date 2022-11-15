
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
      
      flight_cat_simp <- (sum(x$std_conductivity < 0.05) / nrow(x))
      flight_cat_ad <- (sum(x$std_conductivity_ad < 0.05) / nrow(x))
      flight_cat_full <- (sum(x$act_final < 0.05) / nrow(x))
      
      rest_cat_simp <- (sum(x$std_conductivity > 0.95) / nrow(x))
      rest_cat_ad <- (sum(x$std_conductivity_ad > 0.95) / nrow(x))
      rest_cat_full <- (sum(x$act_final > 0.95) / nrow(x))
      
      ars_cat_simp <- 1 - (flight_cat_simp + rest_cat_simp)
      ars_cat_ad <- 1 - (flight_cat_ad + rest_cat_ad)
      ars_cat_full <- 1 - (flight_cat_full + rest_cat_full)
      
      # Tucking data per day
      tuck <- (sum(x$leg_tuck) / nrow(x))
      
      tuck_total <- sum(x$leg_tuck + x$leg_tuck2) / nrow(x)
      
      x$tuck_both <- ifelse(x$leg_tuck == 1 & x$leg_tuck2 == 1, 1, 0)
      
      tuck_both <- sum(x$tuck_both) / nrow(x)
      
      vars <- c("date", "julian", "year",
                "id_year", "lat", "lon", "id",
                "colony", "col_lat", "col_lon",
                "logger")
      
      # Get number of fixes in the day
      fixes <- nrow(x)
      
      # Create data frame
      out <- data.frame(fixes, flight, flight_simp,
                        flight_ad_simp, tuck, tuck_total, tuck_both,
                        flight_cat_simp, flight_cat_ad, flight_cat_full,
                        ars_cat_simp, ars_cat_ad, ars_cat_full,
                        rest_cat_simp, rest_cat_ad, rest_cat_full)
      
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
moult_df <- bind_rows(moult.ls) %>%
  
  # Create bout id and year id variables
  mutate(bout_id = ifelse(moult > 0,
                          paste(id, year, moult_bout, sep = "_"),
                          0))

# Subset a dataframe of moults to retain left legs only
graph_df <- moult_df %>%
  filter(!grepl("R", id)) %>%
  mutate(id = gsub('.{1}$', '', id)) %>%
  mutate(ID = substr(id, 5, 11))

# Plot out the results
moult_plot <- 
  ggplot(graph_df) +
  # theme_minimal() +
  theme(legend.position = "none") +
  scale_alpha_continuous(range = c(0, 1), breaks = c(0, 1)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  # scale_y_continuous(trans = "sqrt") +
  geom_point(aes(x = date, y = -0.05,
                 alpha = prim_moult,
                 colour = "First moult\ndetected"), size = 5, shape = 15) +
  geom_point(aes(x = date, y = -0.05,
                 alpha = sec_moult,
                 colour = "Second moult\ndetected"), size = 5, shape = 15) +
  geom_line(aes(x = date, y = sqrt(flight)), colour = "dark green") +
  geom_line(aes(x = date, y = sqrt(rm_flight)), colour = "red") +
  labs(x = "Month", y = "Proportion of time dry") +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~ ID, ncol =  1) +
  scale_y_continuous(breaks = c(0, 0.224, 0.447, 0.671),
                     labels = c("0", "0.05", "0.2", "0.45"))

# Save off the graph
ggsave(moult_plot, filename = "plots/moult_plot.png",
       dpi = 500, height = 10, width = 6)

# Create a dataframe without April to explore the tucking behaviour
tuck_test <- moult_df %>%
  mutate(month = as.numeric(format(date, format = "%m")),
         leg = ifelse(grepl("R", id), "Right", "Left"),
         id = gsub('.{1}$', '', id),
         ID = substr(id, 5, 11)) %>%
  filter(month != 4)

# Metrics for proportion of time per day spent tucking/in flight
summary(tuck_test$flight)
summary(tuck_test$tuck)
summary(tuck_test$tuck_total)
sd(tuck_test$tuck_total)
sd(tuck_test$flight)

summary(tuck_test$flight_simp)
sd(tuck_test$flight_simp)
summary(tuck_test$flight_ad_simp)
sd(tuck_test$flight_ad_simp)

meta_df <- tuck_test %>%
  split(., .$id) %>%
  lapply(., FUN = function(x){
    flight <- mean(x$flight)
    flight_simp <- mean(x$flight_simp)
    flight_ad_simp <- mean(x$flight_ad_simp)
    flight_cat_simp <- mean(x$flight_cat_simp)
    flight_cat_ad <- mean(x$flight_cat_ad)
    flight_cat_full <- mean(x$flight_cat_full)
    ars_cat_simp <- mean(x$ars_cat_simp)
    ars_cat_ad <- mean(x$ars_cat_ad)
    ars_cat_full <- mean(x$ars_cat_full)
    rest_cat_simp <- mean(x$rest_cat_simp)
    rest_cat_ad <- mean(x$rest_cat_ad)
    rest_cat_full <- mean(x$rest_cat_full)
    
    data.frame(flight, flight_simp, flight_ad_simp,
               flight_cat_simp, flight_cat_ad, flight_cat_full,
               ars_cat_simp, ars_cat_ad, ars_cat_full,
               rest_cat_simp, rest_cat_ad, rest_cat_full)
  }) %>%
  bind_rows(.)

summary(meta_df$flight)
sd(meta_df$flight)
summary(meta_df$flight_simp)
sd(meta_df$flight_simp)
summary(meta_df$flight_ad_simp)
sd(meta_df$flight_ad_simp)
summary(meta_df$flight_cat_simp)
sd(meta_df$flight_cat_simp)
summary(meta_df$flight_cat_ad)
sd(meta_df$flight_cat_ad)
summary(meta_df$flight_cat_full)
sd(meta_df$flight_cat_full)

# How many days did the bird spend less than 1% in flight
sum(tuck_test$flight < 0.01) / nrow(tuck_test)

# Correct for including both legs 
tuck_test <- tuck_test %>%
  mutate(tuck = ifelse(leg == "Right", tuck + (tuck_both/2), tuck - (tuck_both/2)))

# Plot out tucking over time by leg and by ID
tuck_plot <- ggplot(tuck_test) +
  geom_histogram(aes(x = date,
                     weight = (tuck * 10),
                     fill = leg),
                 position = "stack",
                 binwidth = 10) +
  geom_histogram(aes(x = date,
                     weight = (tuck_both * 5),
                     fill = "Both"),
                 position = "stack",
                 binwidth = 10) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  labs(fill = "Leg", x = "Date", y = "Time spent tucking (%)") +
  facet_wrap(facets = ~ID)

# Save off the tuck plot
ggsave(tuck_plot,
       filename = "plots/tuck_plot.png",
       dpi = 500, height = 6, width = 8)  

# Calculate flight metrics during moult
tuck_test %>% filter(prim_moult > 0 | sec_moult > 0) %>%
  dplyr::select(flight, flight_simp, flight_ad_simp) %>% summary()

# Calculate flight metrics outside moult
tuck_test %>% filter(prim_moult == 0 & sec_moult == 0) %>%
  dplyr::select(flight, flight_simp, flight_ad_simp) %>% summary()

# Get start/end dates of moult, as well as duration 
moult_df %>% filter(prim_moult > 0) %>%
  dplyr::select(date, id) %>% split(., .$id) %>%
  lapply(., function(x) summary(x))

# Get start/end dates of second inferred moult, as well as duration
moult_df %>% filter(sec_moult > 0) %>%
  dplyr::select(date, id) %>% split(., .$id) %>%
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
    ID <- substr(ID, 5, 11)
    id <- x$id[1]
    out <- data.frame(id, ID, lon1, lat1, lon2, lat2)
    out
  }) %>%
  do.call(rbind, .) %>%
  filter(!grepl("R", id))
  

# Plot of moulting areas
space_moult <- 
  ggplot(moult_df %>%
           mutate(ID = substr(gsub('.{1}$', '', id), 5, 11),
                  logger_id = substr(id, 5, 11),
                  logger_id = ifelse(grepl("R", id),
                                     paste(logger_id, "Right", sep = " "),
                                     paste(logger_id, "Left", sep = " ")))) +
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
    geom_path(aes(x = lon, y = lat_filled,
                  colour = logger_id)) +
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

space_moult

# Save it off
ggsave(space_moult, filename = "plots/space_plot.png",
       dpi = 500, height = 10, width = 10)

# Create a list of individuals for plotting time spent tucking over time
tuck_plot_ls <- bind_rows(moult.ls) %>%
  mutate(ID = substring(id, 5, 11),
         leg = ifelse(grepl("R", id), "Right", "Left")) %>%
  split(., .$ID)

# Create namespace for plot list
tuck_moult_plot <- list()

# Populate this list with graphs of proportion of time moulting
# Split by individual and facet wrapped by leg
for(i in c(1:4)){
  
  if(i %in% c(1,3)){
    tuck_moult_plot[[i]] <- 
      ggplot(tuck_plot_ls[[i]]) +
      geom_ribbon(aes(x = ymd(date), ymin = 0, ymax = tuck), fill = "dark red") +
      scale_x_date(date_breaks = "2 months", date_labels = "%b") +
      geom_ribbon(
        data = tuck_plot_ls[[i]] %>% filter(prim_moult > 1),
        aes(x = ymd(date), ymin = 1, ymax = tuck), alpha = 0.4) +
      geom_ribbon(
        data = tuck_plot_ls[[i]] %>% filter(sec_moult > 1),
        aes(x = ymd(date), ymin = 1, ymax = tuck), alpha = 0.4) +
      labs(title = tuck_plot_ls[[i]]$ID[1],
           x = NULL, y = "Proportion of time tucking") +
      facet_wrap(facets = ~leg)
  }
  else{
    
    tuck_moult_plot[[i]] <- 
      ggplot(tuck_plot_ls[[i]]) +
      geom_ribbon(aes(x = ymd(date), ymin = 0, ymax = tuck), fill = "dark red") +
      scale_x_date(date_breaks = "2 months", date_labels = "%b") +
      geom_ribbon(
        data = tuck_plot_ls[[i]] %>% filter(prim_moult > 1),
        aes(x = ymd(date), ymin = 1, ymax = tuck), alpha = 0.4) +
      labs(title = tuck_plot_ls[[i]]$ID[1],
           x = NULL, y = "Proportion of time tucking") +
      facet_wrap(facets = ~leg)}}

# Make a grid of these plots
tuck_moult_plot <- cowplot::plot_grid(plotlist = tuck_moult_plot, nrow = 4)

# And save it off
ggsave(tuck_moult_plot, filename = "plots/tuck_moult_plot.png",
       width = 6, height = 8, dpi = 500)

