

# Create empty list
moult.ls <- list()

# Prep input list, discard empty entries and split by session id
combo.ls3 <- combo.df %>%
  # Create timing variables
  mutate(year = as.integer(format((as_date(date) - 182), "%y")),
         julian = as.integer(format(date, "%j")),
         id = individ_id,
         id_year = paste(id, year, sep = "_")) %>%
  # Keep only mk type loggers
  filter(!logger %in% c("f100", "c65", "w65")) %>%
  # Get rid of summer dates
  .[!as.integer(format(.$date, "%j")) %in% 110:214, ] %>%
  # Split by id year
  split(., .$id_year)

indicator <- rep("fine", length(combo.ls3))

# Loop through index of files
for(i in 1:length(combo.ls3)){
  
  df <- combo.ls3[[i]]
  
  if(class(df) == "data.frame"){

    df$light.min <- df$light.pred - 0.2
    
  # Infer flying behaviour
  df$flying <- ifelse((df$std_light_ad > df$light.min) & (df$act < 0.05), 1, 0)

  # Collate daily data from the above
  days.df <- df %>%
    # Get rid of night time values
    filter(light.min > 0) %>%
    # Split by day
    split(.$date) %>%
    # Create a function to collect relevent data
    lapply(., function(x) {
      # Wet data per day
      rest <- (sum(x$act > 0.9) / nrow(x))
      flight_simp <- (sum(x$act < 0.1) / nrow(x))
      # Flying data per day
      flight <- (sum(x$flying == 1) / nrow(x))
      # Date
      date <- x$date[1]
      julian <- x$julian[1]
      year <- x$year[1]
      id_year <- x$id_year[1]
      # Location of points
      lat <- x$lat[1]
      lat_eq <- x$lat_eq[1]
      lon <- x$lon[1]
      # ID, colony, distance to colony and sex
      id <- x$individ_id[1]
      colony <- x$colony[1]
      col_dist <- x$col_dist[1]
      col_lat <- x$col_lat[1]
      col_lon <- x$col_lon[1]
      sex <- x$sex[1]
      logger <- x$logger[1]
      fixes <- nrow(x)
      # Combine data
      out <- data.frame(id, fixes, colony, col_dist, col_lat, col_lon,
                        sex, date, flight, rest, flight_simp, date, julian, year, id_year,
                        lat_eq, lat, lon, logger)
    }) %>%
    # Combine into dataframe
    do.call(rbind, .) %>%
    # INterpolate latitudes around equinoxes
    lat_filler(.)

  # Centre julian day around 0
  days.df$julian[which(days.df$julian > 150)] <-
    days.df$julian[which(days.df$julian > 150)] - 365
  
  days.df <- days.df[!as.integer(format(days.df$date, "%j")) %in% 110:214, ]
  
  if(nrow(days.df) > 200){
    
    # Get rolling mean of flight per day
    days.df$rm_flight <- days.df$flight
    # Get rolling mean of simple flight per day
    days.df$rm_flight_simp <- days.df$flight_simp
    # Get rolling mean of longitude
    days.df$rm_lon <- days.df$lon
    # Get rolling mean of latitude
    days.df$rm_lat <- days.df$lat_filled
    
    for(j in 3:(nrow(days.df) - 2)){
      days.df$rm_flight[j] <- mean(days.df$flight[(j - 2):(j + 2)])
      days.df$rm_flight_simp[j] <- mean(days.df$flight_simp[(j - 2):(j + 2)])
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

    # Create new dataframe with split variable
  days_split <- days.df %>% mutate(split = 0)
    # Populate the split variable based on time difference from initial date
    for(j in 2:nrow(days_split)){
      days_split$split[j] <-
        ifelse((
          abs(
            as.numeric(
              difftime(time1 = days.df$date[1],
                         time2 = days.df$date[j],
                         units = "days"))) > 365), 
          ifelse((
            abs(
              as.numeric(
                difftime(time1 = days.df$date[1],
                         time2 = days.df$date[j],
                         units = "days"))) > 730),
            2, 1),
          0)}
    
    # Split data
  days_split <- split(days.df, as.factor(days_split$split)) %>%
    .[sapply(., nrow) > 200]
    
    if(length(days_split) > 0){
    
    # Apply lowPoint function to each element of the list
      days_split <- lapply(days_split,
                           lowPoint,
                           min.group = 15,
                           t = 15,
                           prop.group = 0.2,
                           min.rate = 0.1)
    
    # Apply lowPoint function to each element of the list
    days_split <- 
      lapply(days_split,
             function(x){
               # Number the moult bouts in order of app
               x$moult_bout <- 0
               
               ind <- 0
               
               for(k in 2:nrow(x)){
                 
                 if(x$moult[k - 1] == 0 &
                    x$moult[k] != 0){
                   ind <- ind + 1
                   x$moult_bout[k] <- ind
                 }
                 
                 if(x$moult[k - 1] != 0 &
                    x$moult[k] != 0){
                   x$moult_bout[k] <- ind
                 }
               }
             x
             })
    
    # Recombine into dataframe
    days.df <- do.call(rbind, days_split)
    
    # Boole describing whether or not bird is in "moult" state
    days.df$moult_bool <- ifelse(days.df$moult == 0, 0, 1)
  
  # Put dataframe into list index
  moult.ls[[i]] <- days.df
  
  # If data doesn't meet requirements, write NULL into list element to discard
    }else{moult.ls[[i]] <- NULL; indicator[i] <- "missing days"}
  }else{moult.ls[[i]] <- NULL; indicator[i] <- "too few daylight fixes 1"}
  }else{moult.ls[[i]] <- NULL; indicator[i] <- "null frame"}
  
  # If too many consecutive days have been identified as moult, discard the unit
  if(max(days.df$moult) > 60 & indicator[i] == "fine"){
    moult.ls[[i]] <- NULL; indicator[i] <- "failed method"}
  if(sum(days.df$rm_flight > 0.5) > 50 & indicator[i] == "fine"){
    moult.ls[[i]] <- NULL;  indicator[i] <- "failed gls"}
  if(sum(days.df$moult_bool) > 100 & indicator[i] == "fine"){
    moult.ls[[i]] <- NULL;  indicator[i] <- "failed method"}
  if(sum(days.df$fixes < 10) > 5 & indicator[i] == "fine"){
    moult.ls[[i]] <- NULL;  indicator[i] <- "too few daylight fixes 2"}
  if(max(days.df$prim_moult) < 15 & indicator[i] == "fine"){
    moult.ls[[i]] <- NULL;  indicator[i] <- "moult too short"}
  
  # Monitor progress
  print(paste(i, days.df$colony[1], indicator[i], sep = ":    "))
}

 moult.ls <- moult.ls %>%
  .[sapply(., class) == "data.frame"] %>%
  discard(is.null)

# rbind list into overall dataframe
moult_df <- do.call(rbind, moult.ls) %>%
  # Create bout id and year id variables
  mutate(bout_id = ifelse(moult > 0,
    paste(id, year, moult_bout, sep = "_"),
    0),
    id_year = paste(id, year, sep = "_"))

{# Append colony variable to moult dataframe
moult_df$area <- "Barents Sea"
moult_df$area[which(moult_df$colony == "Skellig Michael")] <- "Ireland"
moult_df$area[which(moult_df$colony == "Isle of May")] <- "UK"
moult_df$area[which(moult_df$colony %in%
                      c("Grimsey", "Papey", "Holmahals"))] <- "Iceland"
moult_df$area[which(moult_df$colony == "Faroe Islands")] <- "Faroe Islands"
moult_df$area[which(moult_df$colony %in%
                      c("Anda", "Sklinna", "RÃ¸st", "Runde and Ã…lesund"))] <- "Norwegian Sea"

moult_df$colony[which(moult_df$colony == "RÃ¸st")] <- "Røst"
moult_df$colony[which(moult_df$colony == "Runde and Ã…lesund")] <- "Runde & Ålesund"
moult_df$colony[which(moult_df$colony == "BjÃ¸rnÃ¸ya")] <- "Bjørnøya"
moult_df$colony[which(moult_df$colony == "HornÃ¸ya")] <- "Hornøya"
moult_df$colony[which(moult_df$colony == "HjelmsÃ¸ya")] <- "Hjelmsøya"
moult_df$colony[which(moult_df$colony == "Runde & Ålesund")] <- "Runde"
}
# Save off the processed data
save(moult_df, file = "data/cleaned/moult_df.RData")
save(moult.ls, file = "data/cleaned/moult_ls.RData")

load(file = "data/cleaned/moult_df.RData")
load(file = "data/cleaned/moult_ls.RData")

