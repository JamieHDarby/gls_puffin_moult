
# Additional preparation for raw geolocator processing --------------------

# Import packages required for geolocator processing
if(!require(data.table)) install.packages(
  "data.table")
if(!require(devtools)) install.packages(
  "devtools")
if(!require(maptools)) install.packages(
  "maptools")
if(!require(GeoLight)) install_github(
  "SLisovski/GeoLight")
if(!require(GeoLocTools)) install_github(
  "SLisovski/GeoLocTools")

# Load geolocation packages
setupGeolocation()

# Load in simple land shapefile for early data exploration
data("wrld_simpl")

# gls_fun -----------------------------------------------------------------

# Function to semi-automate geolcator location processing
gls_fun <- function(filepath, col.lon, col.lat, colony, manual.zenith = F){
  
  # Bounds of the plots generated
  xlim <- c(-60, 20)
  ylim <-  c(30, 80)
  
  # Read in the light data
  GLSRaw <- ligTrans(filepath)
  
  # Set names for raw data
  names(GLSRaw) <- c("Date", "Light")
  
  # Sort out NAs
  GLSRaw <- GLSRaw[which(!is.na(GLSRaw$Date)), ]
  
  # Log transform light data
  GLSRaw$Light  <-
    (log(GLSRaw$Light + 0.0001) +
       abs(min(log(GLSRaw$Light + 0.0001))))
  
  # Make sure plotting window is setup
  par(mar=c(1,1,1,1))
  
  # Launches UI for selecting data to process
  GLSpretwl <- preprocessLight(
    GLSRaw, 
    threshold = 2.5,
    offset = 12, 
    lmax = 20,
    gr.Device = "x11")
  
  # Removes outliers
  GLStwl <- twilightEdit(
    twilights = GLSpretwl,
    offset = 12,
    window = 4,
    outlier.mins = 15,
    stationary.mins = 30,
    plot = F)
  
  # Gets rid of outliers
  GLStwl <- subset(GLStwl, !Deleted)
  
  # Creates time series plot of raw light data and superimposes twilights
  lightImage(tagdata = GLSRaw,
             offset = 12,
             zlim = c(0, 20))

  tsimagePoints(GLStwl$Twilight,
                offset = 12,
                pch = 16,
                cex = 1.2,
                col = ifelse(GLStwl$Deleted,
                             "grey20",
                             ifelse(GLStwl$Rise,
                                    "firebrick",
                                    "cornflowerblue")))
  
  # If the zenith is to be set using the Hills-Ekstrom calibration
  if(manual.zenith == F){GLSzenith <-
    findHEZenith(GLStwl,
                 tol = 0.15,
                 range = c(1,nrow(GLStwl)))}
  else{GLSzenith <- manual.zenith}
  
  # Create a path using the zenith and twilights
  path <- thresholdPath(
    twilight = GLStwl$Twilight,
    rise = GLStwl$Rise,
    zenith = GLSzenith,
    tol = 0.07)
  
  # Make sure plot window is setup, probably not required, but anyway
  par(mfrow = c(1,1))
  
  # Plots out initial track path
  {
    plot(path$x, type = "n",
         xlab = "",
         ylab = "",
         xlim = xlim,
         ylim = ylim)
    plot(wrld_simpl,
         col = "grey95",
         add = T)
    points(path$x,
           pch = 19,
           col = "cornflowerblue",
           type = "o")
    points(col.lon,
           col.lat,
           pch = 16,
           cex = 2.5,
           col = "firebrick")
    box()
  }
  
  # Extract pasth from outputs above
  GLSPrePath <- as.data.frame(path$x)
  
  # Add time variable
  GLSPrePath$time <- path$time
  
  # Extract julian date from time
  GLSPrePath$julian <- as.numeric(
    format(GLSPrePath$time, format = "%j"))
  
  # Extract year from time
  GLSPrePath$year <- as.numeric(
    format(GLSPrePath$time, format = "%y"))
  
  # Define equinoxes
  equinox1 <- c(234:296)
  equinox2 <- c(50:112)
  
  # Create placeholder for the LOESS smoothed latitude
  GLSPrePath$latloess <- GLSPrePath$lat
  
  # Split path out by year
  year_list <- split(GLSPrePath, GLSPrePath$year)
  
  # Loop through years in the path and smooth around equinoxes
  # There's a quick check to make sure that at least 10 positions fall
  # within the equinox
  for(i in 1:length(year_list)){
    
    ind <- which(year_list[[i]]$julian %in% equinox1)
    
    if(length(ind) > 10){
      LatLoess <- loess(
        formula = year_list[[i]]$lat[ind] ~ c(1:length(ind)),
        span = 0.3)
      
      year_list[[i]]$latloess[ind] <- LatLoess$fitted}
    
    ind <- which(year_list[[i]]$julian %in% equinox2)
    
    if(length(ind) > 10){
      LatLoess <- loess(
        formula = year_list[[i]]$lat[ind] ~ c(1:length(ind)),
        span = 0.3)
      
      year_list[[i]]$latloess[ind] <- LatLoess$fitted}
  }
  
  # Reformat this as a dataframe
  GLSPath <- do.call(rbind, year_list)
  
  # Clean out some temporary variables
  rm(LatLoess, ind, i, year_list)
  
  # Change plot window parameters
  par(mfrow = c(4,1))
  
  # Plot of unsmoothed latitudes
  plot(path$time,
       path$x[,2],
       type = "n",
       ylab = "Latitude",
       xlab = "",
       yaxt = "n",
       ylim = ylim)
  axis(2, las = 2)
  lines(path$time,
        path$x[,2],
        lwd = 2)
  
  # Plot of smoothed latitudes
  plot(GLSPath$time,
       GLSPath$latloess,
       type = "n",
       ylab = "Latitude",
       xlab = "",
       yaxt = "n",
       ylim = ylim)
  axis(2, las = 2)
  lines(GLSPath$time,
        GLSPath$latloess,
        lwd = 2)
  
  # Single plot window
  par(mfrow = c(1,1))
  
  # Plot smoothed path
  {
    plot(GLSPath[, c(1,6)], type = "n",
         xlab = "",
         ylab = "",
         xlim = xlim,
         ylim = ylim)
    plot(wrld_simpl,
         col = "grey95",
         add = T)
    points(GLSPath[, c(1,6)],
           pch = 19,
           col = "cornflowerblue",
           type = "o")
    points(col.lon,
           col.lat,
           pch = 18,
           cex = 1.5,
           col = "firebrick")
    box()
  }
  
  # Define position dataframe
  pos <- GLSPath[1:(nrow(GLSPath) - 1), ] %>%
    # Extract first twilight of each day
    mutate(tfirst = time)
  
  # Extract second twilight of each day
  pos$tsecond <- GLStwl$Twilight[2:nrow(GLStwl)]
  
  # Average time of position, between two twilights
  pos$date_time <-
    as.POSIXct((as.numeric(pos$tfirst) + as.numeric(pos$tsecond)) / 2,
               origin = '1970-01-01')
  
  # Rise True or Rise False 
  pos$Rise <- GLStwl$Rise[1:(nrow(GLStwl) - 1)]
  
  # Twilight type
  pos$type <- ifelse(pos$Rise == T, 1, 2)
  
  # Colony locations
  pos$col.lon <- col.lon
  pos$col.lat <- col.lat
  
  # Zenith used to obtain locations
  pos$GLSzenith <- GLSzenith
  
  # Return dataframe
  pos
}

