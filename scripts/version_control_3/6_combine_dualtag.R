
# Create double tagged list
act.ls.dt <- act.ls

# Write over leg tucks from one leg to the activity data of the other
# Cycle through the datasets to append tucks to activity data
for(i in c(1, 3, 5, 7)){
  act.ls.dt[[i]] <- tuck_transfer(lgh = lgh.ls[[i + 1]],
                               act = act.ls[[i]],
                               tuck_var = "leg_tuck2",
                               ad_act = "std_conductivity_ad2")
  
  act.ls.dt[[i + 1]] <- tuck_transfer(lgh = lgh.ls[[i]],
                               act = act.ls[[i + 1]],
                               tuck_var = "leg_tuck2",
                               ad_act = "std_conductivity_ad2")
  
  print(i)
}

# Create a function to transfer tucks from light to activity data
act2_transfer <- function(act_keeper, act_doner){
  require(xts)
  require(zoo)
  
  # Create xts object from tucks
  xts_act1 <- xts(x = act_keeper$std_conductivity_ad2,
                  order.by = act_keeper$date_time)
  
  # Create xts object from activitiy data
  xts_act2 <- xts(x = act_doner$std_conductivity_ad2,
                  order.by = act_doner$date_time)
  
  # Merge these two, initially keeping all time points
  xts_merge <- merge.xts(xts_act1, xts_act2, all = T, fill = na.locf)
  
  # Merge this merged xts with the activity xts
  # This time, keep only the values at the keeper activity timestamps
  xts_merge <- merge.xts(xts_act1, xts_merge, all = F, fill = na.locf)
  
  # Put the new act variable back into the original activity dataframe
  act_keeper$std_conductivity_ad_full <- data.frame(xts_merge)$xts_act2
  
  # Return the activity dataframe
  act_keeper
}

# Cycle through the datasets to append additional activity data
for(i in c(1, 3, 5, 7)){
  act.ls.dt[[i]] <- act2_transfer(act_doner = act.ls.dt[[i + 1]],
                               act_keeper = act.ls.dt[[i]])
  
  act.ls.dt[[i + 1]] <- act2_transfer(act_doner = act.ls.dt[[i]],
                                   act_keeper = act.ls.dt[[i + 1]])
  
  print(i)
}

# Loop to 
act.ls.dt <- lapply(act.ls.dt, function(x){
  x$act_final <- x$std_conductivity_ad2
  
  x$act_final <- ifelse(x$std_conductivity_ad_full > x$act_final,
                        x$std_conductivity_ad_full, x$act_final)
  
  x
})

# Save this off so it can just be loaded directly next time
save(act.ls.dt, file = "data/cleaned/act_ls_dt.RData")
load(file = "data/cleaned/act_ls_dt.RData")
