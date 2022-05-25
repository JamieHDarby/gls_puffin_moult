
# Set up environment ------------------------------------------------------

# Load in land shapefile for graphs and packages
load(file = "data/shapes/land_df_wgs.RData")
require(zoo)
require(lubridate)
require(purrr)
require(raster)
require(dplyr)
require(ggplot2)
require(oce)
require(birk)
require(stringr)
require(future.apply)
require(cowplot)
require(hms)
require(forcats)
require(adehabitatLT)

# lowPoint ----------------------------------------------------------------

# Workhorse function for finding a consistently low but non-zero section of
# a time series. In this case corresponding to little or no flight,
# thought to be consistent with moult
lowPoint <- function(df,
                     par = "rm_flight",
                     t = 21,
                     min.group = 21,
                     dist.par = "point_dist",
                     dist.lim = 100,
                     prop.group = 0.5,
                     min.rate = 0.1,
                     countdown = 50,
                     prim.var = "prim_moult")
{
  group <- rep(0, nrow(df))
  
  for(i in 1:(min.rate * 1000))
  {
    index <- which(df[par] < (i/1000))
    
    for(j in 2:nrow(df))
    {
      group[j] <- ifelse(((df[j, par] < (i/1000)) &
                            (df[j, dist.par] < dist.lim)),
                         (1 + (group[j - 1])),
                         0)
      
      group[j:(j - (group[j] - 1))] <- group[j]
    }
    
    if((length(index) > t) & (max(group) >= min.group)){countdown <- countdown - 1}
    if(countdown == 0){break}
  }
  
  moult <- rep(0, nrow(df))
  
  moult[index] <- group[index]
  
  moult[which(moult < (min.group))] <- 0
  
  df$moult <- moult
  
  df[, prim.var] <- moult
  
  df[which(df[, prim.var] < max(df[, prim.var])), prim.var] <- 0
  
  df
}

# lat_filler --------------------------------------------------------------

# Function to linearly interpolate latitudes filtered out because of high
# inaccuracy around the equinoxes
lat_filler <-
  function(df, lat.var = "lat_eq", lat.var.out = "lat_filled") {
    
    handshake <- F
    
    df[lat.var.out] <- df[lat.var]
    ind <- 0
    for(i in 1:nrow(df)){
      
      if(is.na(df[i, lat.var.out])){
        ind <- ind + 1
        for(j in i:nrow(df)){
          
          if(!is.na(df[j, lat.var.out])){
            
            if(i == 1){
              
              df[i:(j - 1), lat.var.out] <- df[j, lat.var.out]
              
              break
            }
            else
            {  
              df[(i:(j - 1)), lat.var.out] <-
                seq(from = df[(i - 1), lat.var.out],
                    to = df[j, lat.var.out],
                    length = (j - i))
              
              break
            }
          }
          else
          {
            if(j == nrow(df)){
              df[(i:nrow(df)), lat.var.out] <-
                df[(i - 1), lat.var.out]
            break}
          }
        }
      }
    }
    df
  }

# var_append --------------------------------------------------------------

# Append variables from a location dataframe to a raw light or
# activity dataframe
var_append <- function(x,
                       pos.df,
                       vars = c("colony", "col_lat",
                                "col_lon", "logger"),
                       lon.var = "lon",
                       lat.var = "lat_filled"){
  
  pos.df = pos.df[which(pos.df$individ_id == x$individ_id[1]), ]
  
  x[, vars] <- pos.df[1, vars]
  
  x <- split(x, x$date) %>%
    lapply(., function(x){
      ind <- which.closest(pos.df$date, x$date[1])
      
      x$lat <-
        pos.df[ind, lat.var]
      
      x$lon <-
        pos.df[ind, lon.var]
      
      x$sun.angle <- sunAngle(t = x$date_time, 
                              longitude = x$lon,
                              latitude = x$lat)$altitude
      x
    }) %>%
    
    do.call(rbind, .)
  
  x
}
