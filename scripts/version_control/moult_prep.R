
# load(file = "data/cleaned/combo_ls2.RData")
# load(file = "data/cleaned/act_ls2.RData")
# load(file = "data/cleaned/lgh_ls2.RData")
# load(file = "data/cleaned/combo_df.RData")
# load(file = "data/cleaned/lgh_df.RData")
# load(file = "data/cleaned/act_df.RData")
load(file = "data/cleaned/pos_ls.RData")
load(file = "data/shapes/land_df_wgs.RData")
library(zoo)
library(lubridate)
library(oce)
library(birk)
library(purrr)
library(raster)
library(dplyr)

lowPoint <- function(df,
                     par = "rm_flight",
                     t = 21,
                     min.group = 21,
                     dist.par = "point_dist",
                     dist.lim = 100,
                     prop.group = 0.5,
                     min.rate = 0.1)
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
    
    if((length(index) > t) & (max(group) >= min.group)){break}
  }
  
  moult <- rep(0, nrow(df))
  
  moult[index] <- group[index]
  
  moult[which(moult < (prop.group * max(moult)))] <- 0
  
  df$moult <- moult
  
  df$prim_moult <- moult
  
  df$prim_moult[which(df$prim_moult != max(df$prim_moult))] <- 0
  
  df
}


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
            if(j == nrow(df)){df[(i:nrow(df)), lat.var.out] <- df[(i - 1), lat.var.out]
            
            break}
          }
        }
      }
    }
    df
  }
