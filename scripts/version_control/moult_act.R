# Create empty list
moult_ls <- list()

# Loop through index of files
for(i in 1:length(file)){
  
  # Read in the raw lig files
  GLS_raw <- ligTrans(paste(file[i], ".lig", sep = "")) %>%
    mutate(Date = lubridate::ymd_hms(datetime)) %>%
    rename(Light = light) %>%
    filter(!is.na(Date))
  
  # Read in the raw act file
  GLS_act <- TwGeos::readAct(paste(file[i], ".act", sep = "")) %>%
    mutate(Date = lubridate::ymd_hms(Date)) %>%
    filter(!is.na(Date))
  

  # Get locations -----------------------------------------------------------
  twl <- 
    TwGeos::findTwilights(GLS_raw[c("Date", "Light")],
                        threshold = 20,
                        include = GLS_raw$Date[1:nrow(GLS_raw)],
                        dark.min = 240)
  
  # Remove April to end August dates
  twl <- twl[!as.integer(format(twl$Twilight, "%m")) %in% 4:8, ]
  
  # Find the zenith using Hill-Ekstrom method
  zenith <- findHEZenith(twl,
                         tol = 0.12,
                         range = c(1, nrow(twl)))
  
  # Ship out to geolight
  twl.gl <- export2GeoLight(twl)
  
  # Calculate coords using geolight
  crd <- coord(
    twl = twl.gl,
    degElevation = 90 - zenith,
    note = F)
  
  # Collate datestamp and lat lon info and keep only unique dates
  track <- data.frame(twl.gl$tFirst, crd) %>%
    mutate(date = as_date(twl.gl.tFirst)) %>%
    select(- twl.gl.tFirst) %>%
    .[!duplicated(.$date),]
  
  # Sort out bad Latitude estimates
  for(j in 2:nrow(track)){
    if(is.na(track$lat[j])){
      track$lat[j] <- track$lat[j - 1]
    }
  }
  
  # Make sure time series conclude at the same point
  GLS_raw <- GLS_raw[which(GLS_raw$Date <= max(GLS_act$Date)),]
  
  # Create index variable
  ind <- 1:nrow(GLS_raw)
  
  # Remove even indices
  ind <- ind[which(ind %% 2 != 0)]
  
  # Combine light and activity data
  GLS_combo <- cbind(GLS_raw[ind,], GLS_act %>% select(-Date)) %>%
    mutate(date = as_date(Date)) %>%
    split(., .$date) %>%
    lapply(., function(x){
      x$lat <- track$lat[which.closest(track$date, x$date[1])]
      x$lon <- track$lon[which.closest(track$date, x$date[1])]
      x
    }) %>%
    do.call(rbind, .)
  
  # Get sunangle for all points
  GLS_combo$sunangle <- sunAngle(t = GLS_combo$Date,
                         longitude = GLS_combo$lon,
                         latitude = GLS_combo$lat)$altitude
  
  # Infer resting behaviour from light/activity data
  GLS_combo$light_act <- ifelse(GLS_combo$Light < 10 & GLS_combo$Activity < 180, 1, 0)
  GLS_combo$light_act[which(GLS_combo$Activity > 180)] <- 1
  
  # Infer flying behaviour
  GLS_combo$flying <- ifelse(GLS_combo$Light > 5 & GLS_combo$Activity < 20, 1, 0)
  
  # Collate daily data from the above
  GLS_days <- GLS_combo %>%
    # Get rid of night time values
    filter(sunangle > -12) %>%
    # Split by day
    split(.$Day) %>%
    # Create a function to collect relevent data
    lapply(., function(x) {
      # Resting points per day
      l_act <- (sum(x$light_act == 1) / nrow(x))
      # Wet data per day
      act <- (sum(x$Activity > 180) / nrow(x))
      # Flying data per day
      flight <- (sum(x$flying == 1) / nrow(x))
      # Date
      date <- x$Date[1]
      # Day (numeric)
      day <- x$Day[1]
      # Location of points
      lat <- x$lat[1]
      lon <- x$lon[1]
      # Combine data
      df <- data.frame(l_act, act, date, day, flight, lat, lon)
    }) %>%
    # Combine into dataframe
    do.call(rbind, .)
  
  # Get rolling mean of flight per day
  GLS_days$rm_flight <- GLS_days$flight
  for(j in 3:(nrow(GLS_days) - 2)){
    GLS_days$rm_flight[j] <- mean(GLS_days$flight[(j - 2):(j + 2)])
  }

  # Remove April to end August dates
  GLS_days <- GLS_days[!as.integer(format(GLS_days$date, "%m")) %in% 4:8, ]
  
    # Create new dataframe with split variable
    GLS_days_split <- GLS_days %>% mutate(split = 0)
    # Populate the split variable based on time difference from initial date
    for(j in 2:nrow(GLS_days_split)){
      GLS_days_split$split[j] <-
        ifelse((
          abs(
            as.numeric(
              difftime(time1 = GLS_days$date[1],
                         time2 = GLS_days$date[j],
                         units = "days"))) > 365), 
          ifelse((
            abs(
              as.numeric(
                difftime(time1 = GLS_days$date[1],
                         time2 = GLS_days$date[j],
                         units = "days"))) > 730),
            2, 1),
          0)}
    
    # Split data
    GLS_days_split <- split(GLS_days, as.factor(GLS_days_split$split)) %>%
    .[sapply(., nrow) > 180]
    
    if(length(GLS_days_split) > 0){# Recombine this df
    GLS_days <- do.call(rbind, GLS_days_split)
    
    # Apply lowPoint function to each element of the list
    GLS_days$moult <- unlist(lapply(GLS_days_split, lowPoint, min.group = 10, t = 10))
    
    # Pick julian day out of the date variable
    GLS_days$julian <- as.integer(format(GLS_days$date, "%j"))
    
    # Isolate year out of the date variable
    GLS_days$year <- as.integer(format((as_date(GLS_days$date) - 182), "%y"))
    
    # Centre julian day around 0
    GLS_days$julian[which(GLS_days$julian > 150)] <-
      GLS_days$julian[which(GLS_days$julian > 150)] - 365
  
    # populate id field
  GLS_days$id <- id[i]
  
  # Put dataframe into list index
  moult_ls[[i]] <- GLS_days}else{moult_ls[[i]] <- NULL}
  
  # Monitor progress
  print(i)
}

# rbind list into overall dataframe
moult_df <- do.call(rbind, moult_ls)
  
moult_df %>%
  filter(moult > 5 & date > ymd("2010/07/01") & date < ymd("2013/07/01")) %>%
  ggplot() + 
  theme_classic() +
  scale_size_continuous(range = c(1, 5), limits = c(1, max(df$moult))) +
  geom_point(aes(x = julian, y = id, size = moult, colour = id)) +
  facet_wrap(~ as.factor(year), nrow = 3, scales = "free_y")

moult_df %>%
  filter(moult > 0) %>%
  ggplot() +
  scale_color_viridis_c() +
  geom_point(aes(x = julian, y = id, size = moult, colour = moult))

moult_df %>% 
  filter(moult > 0 & year < 15) %>%
  ggplot() +
  geom_density(aes(x = julian, fill = as.factor(year))) +
  facet_wrap(~ as.factor(year), nrow = 3)

do.call(rbind, moult_ls[30:34]) %>%
ggplot() +
  scale_color_gradient(low = "light blue", high = "dark blue") +
  geom_point(aes(x = julian, y = -0.1, colour = moult, alpha = moult, size = moult)) +
  geom_line(aes(x = julian, y = rm_flight), colour = "blue") +
  geom_line(aes(x = julian, y = flight), colour = "green") +
  facet_wrap(~ paste(id, year, sep = "_"), nrow = 4, ncol =  2)


moult_year <- split(moult_df, as.factor(moult_df$year))

t.test(moult_year$`10`$julian, moult_year$`12`$julian)

Eve Merrall