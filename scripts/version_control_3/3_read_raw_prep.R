
if(!require(data.table)) install.packages(
  "data.table")

if(!require(devtools)) install.packages(
  "devtools")

if(!require(maptools)) install.packages(
  "maptools")

if(!require(GeoLocTools)) install_github(
  "SLisovski/GeoLocTools")

setupGeolocation()

data("wrld_simpl")

gls_fun <- function(filepath, col.lon, col.lat, colony){
  
  xlim <- c(-60, 20)
  
  ylim <-  c(30, 80)
  
  GLSRaw <- ligTrans(filepath)
  
  # Set names in raw file
  names(GLSRaw) <- c("Date", "Light")
  
  # Log transform light data
  GLSRaw$Light  <-
    (log(GLSRaw$Light + 0.0001) +
       abs(min(log(GLSRaw$Light + 0.0001))))
  
  GLSpretwl <- preprocessLight(
    GLSRaw, 
    threshold = 2.5,
    offset = 12, 
    lmax = 20,
    gr.Device = "x11")
  
  GLStwl <- twilightEdit(
    twilights = GLSpretwl,
    offset = 12,
    window = 4,
    outlier.mins = 15,
    stationary.mins = 30,
    plot = TRUE)
  
  GLStwl <- subset(GLStwl, !Deleted)
  
  
  lightImage(tagdata = GLSRaw,
             offset = 12,     
             zlim = c(0, 4))
  
  tsimagePoints(GLStwl$Twilight,
                offset = 12,
                pch = 16,
                cex = 1.2,
                col = ifelse(GLStwl$Deleted,
                             "grey20",
                             ifelse(GLStwl$Rise,
                                    "firebrick",
                                    "cornflowerblue")))
  
  GLSzenith <-
    findHEZenith(GLStwl,
                 tol = 0.15,
                 range = c(1,nrow(GLStwl)))
  
  
  path <- thresholdPath(
    twilight = GLStwl$Twilight,
    rise = GLStwl$Rise,
    zenith = GLSzenith,
    tol = 0.07)
  
  par(mfrow = c(1,1))
  
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
  
  GLSPrePath <- path$x
  
  GLSPrePath <-  as.data.frame(GLSPrePath)
  
  GLSPrePath$time <- path$time
  
  GLSPrePath$julian <- as.numeric(
    format(GLSPrePath$time, format = "%j"))
  
  GLSPrePath$year <- as.numeric(
    format(GLSPrePath$time, format = "%y"))
  
  equinox1 <- c(234:296)
  
  equinox2 <- c(50:112)
  
  GLSPrePath$latloess <- GLSPrePath$lat
  
  year_list <- split(GLSPrePath, GLSPrePath$year)
  
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
  
  GLSPath <- do.call(rbind, year_list)
  
  rm(LatLoess, ind, i, year_list)
  
  par(mfrow = c(4,1))
  
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
        lwd = 2)                                     #Plot of unsmoothed Latitudes
  
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
        lwd = 2)                                     #Plot of smoothed Latitudes for comparison
  
  par(mfrow = c(1,1))                                #Single pane
  
  {
    plot(GLSPath[, c(1,6)], type = "n",
         xlab = "",
         ylab = "",
         xlim = xlim,
         ylim = ylim)                                #Plot setup, lims
    plot(wrld_simpl,
         col = "grey95",
         add = T)                                    #Land
    points(GLSPath[, c(1,6)],
           pch = 19,
           col = "cornflowerblue",
           type = "o")                               #Smoothed path
    points(col.lon,
           col.lat,
           pch = 18,
           cex = 1.5,
           col = "firebrick")                        #Colony blob
    box()                                            #Border
  }
  
  pos <- GLSPath[1:(nrow(GLSPath) - 1), ] %>%
    mutate(tfirst = time)
  
  pos$tsecond <- GLStwl$Twilight[2:nrow(GLStwl)]
  
  pos$date_time <-
    as.POSIXct((as.numeric(pos$tfirst) + as.numeric(pos$tsecond)) / 2,
               origin = '1970-01-01')
  
  pos$Rise <- GLStwl$Rise[1:(nrow(GLStwl) - 1)]
  
  pos$type <- ifelse(pos$Rise == T, 1, 2)
  
  pos
}

