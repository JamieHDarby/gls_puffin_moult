
for(i in 1:length(lgh.ls)){
  lgh.ls[[i]]$colony <- pos.ls[[i]]$colony[1]
  lgh.ls[[i]]$sex <- pos.ls[[i]]$sex[1]
  lgh.ls[[i]]$col_lat <- pos.ls[[i]]$col_lat[1]
  lgh.ls[[i]]$col_lon <- pos.ls[[i]]$col_lon[1]
  lgh.ls[[i]]$logger <- pos.ls[[i]]$logger_model[1]
  act.ls[[i]]$colony <- pos.ls[[i]]$colony[1]
  act.ls[[i]]$sex <- pos.ls[[i]]$sex[1]
  act.ls[[i]]$col_lat <- pos.ls[[i]]$col_lat[1]
  act.ls[[i]]$col_lon <- pos.ls[[i]]$col_lon[1]
  act.ls[[i]]$logger <- pos.ls[[i]]$logger_model[1]
}

lgh.ls2 <- list()

system.time(
  for(i in 1:length(lgh.ls)){
    lgh.ls2[[i]] <- 
      split(lgh.ls[[i]], lgh.ls[[i]]$date) %>%
      lapply(., function(x){
        ind <- which.closest(pos.ls[[i]]$date, x$date[1])
        
        x$lat <-
          pos.ls[[i]]$lat_filled[ind]
        x$lon <-
          pos.ls[[i]]$lon_smooth2[ind]
        x$col_dist <-
          pos.ls[[i]]$disttocol_s2[ind]
        
        x$lat_raw <-
          pos.ls[[i]]$lat_raw[ind]
        x$lon_raw <-
          pos.ls[[i]]$lon_raw[ind]
        
        x$sun.angle <- sunAngle(t = x$date_time, 
                                longitude = x$lon,
                                latitude = x$lat)$altitude
        
        x$sun.angle.unsm <- sunAngle(t = x$date_time, 
                                     longitude = x$lon_raw,
                                     latitude = x$lat_raw)$altitude
        x
      }) %>%
      do.call(rbind, .)
    print(i)
  }
)

act.ls2 <- list()

system.time(
  for(i in 1:length(act.ls)){
    act.ls2[[i]] <- 
      split(act.ls[[i]], act.ls[[i]]$date) %>%
      lapply(., function(x){
        ind <- which.closest(pos.ls[[i]]$date, x$date[1])
        
        x$lat <-
          pos.ls[[i]]$lat_smooth2[ind]
        x$lat_eq <-
          pos.ls[[i]]$lat_smooth2_eqfilt3[ind]
        x$lon <-
          pos.ls[[i]]$lon_smooth2[ind]
        x$col_dist <-
          pos.ls[[i]]$disttocol_s2[ind]
        
        x$lat_raw <-
          pos.ls[[i]]$lat_raw[ind]
        x$lon_raw <-
          pos.ls[[i]]$lon_raw[ind]
        
        x$sun.angle <- sunAngle(t = x$date_time, 
                                longitude = x$lon,
                                latitude = x$lat)$altitude
        
        x$sun.angle.unsm <- sunAngle(t = x$date_time, 
                                     longitude = x$lon_raw,
                                     latitude = x$lat_raw)$altitude
        
        x
      }) %>%
      do.call(rbind, .)
    print(i)
  }
)

for(i in 1:length(lgh.ls2)){
  if(lgh.ls2[[i]]$logger %in% c("f100", "c65", "w65")){
    
    lgh.ls2[[i]]$std_light_ad <-
      lgh.ls2[[i]]$std_light - min(lgh.ls2[[i]]$std_light)
    
    lgh.ls2[[i]]$std_light_ad <- 
      ifelse(lgh.ls2[[i]]$std_light_ad > 0.1, 0.1, lgh.ls2[[i]]$std_light_ad)
    
    lgh.ls2[[i]]$std_light_ad <-
      lgh.ls2[[i]]$std_light_ad * 10
  }else{
    lgh.ls2[[i]]$std_light_ad <- lgh.ls2[[i]]$std_light
  }
  print(i)
}

save(pos.ls, file = "data/cleaned/pos_ls.RData")

combo.ls <- list()

for(i in 1:length(act.ls)){
  
  act.ls2[[i]] <- arrange(act.ls2[[i]], date_time)
  lgh.ls2[[i]] <- arrange(lgh.ls2[[i]], date_time)
  
  date_list <- list(act.ls2[[i]]$date_time, lgh.ls2[[i]]$date_time)
  common_dates = Reduce(intersect, date_list)
  indices <- lapply(date_list, function(x) which(x %in% common_dates))
  
  act.ls2[[i]]$std_light_ad <- NA
  lgh.ls2[[i]]$act <- NA
  act.ls2[[i]]$light_match <- F
  lgh.ls2[[i]]$act_match <- F
  
  while(length(indices[[1]]) != length(indices[[2]])){
    for(j in 1:length(indices[[1]])){
      if(act.ls2[[i]]$date_time[indices[[1]][j]] !=
         lgh.ls2[[i]]$date_time[indices[[2]][j]]){
        indices[[1]] <- indices[[1]][-j]; print(j); break
      }
    }
  }
  
  act.ls2[[i]]$std_light_ad[indices[[1]]] <-
    lgh.ls2[[i]]$std_light_ad[indices[[2]]]
  
  lgh.ls2[[i]]$act[indices[[2]]] <-
    act.ls2[[i]]$std_conductivity[indices[[1]]]
  
  act.ls2[[i]]$light_match[indices[[1]]] <- T
  
  lgh.ls2[[i]]$act_match[indices[[2]]] <- T
  
  combo.ls[[i]] <- lgh.ls2[[i]][indices[[2]],]
  
  combo.ls[[i]]$act <-
    act.ls2[[i]]$std_conductivity[indices[[1]]]
  
  # print(paste(i, (length(indices[[1]]) == length(indices[[2]]))))
  print(paste(i, all.equal(act.ls2[[i]]$date_time[indices[[1]]],
                           lgh.ls2[[i]]$date_time[indices[[2]]])))
  print(length(indices[[1]])); print(length(indices[[2]]))
}

save(act.ls2, file = "data/cleaned/act_ls2.RData")

save(lgh.ls2, file = "data/cleaned/lgh_ls2.RData")

save(combo.ls, file = "data/cleaned/combo_ls2.RData")

load(file = "data/cleaned/act_ls2.RData")
load(file = "data/cleaned/lgh_ls2.RData")
load(file = "data/cleaned/combo_ls2.RData")
load(file = "data/cleaned/pos_ls.RData")
