
# Create data frames from lists of geolocator outputs ---------------------

load(file = "data/cleaned/pos_ls_sm.RData")
load(file = "data/cleaned/act_ls_sm.RData")
load(file = "data/cleaned/lgh_ls_sm.RData")

# Combine light data into a dataframe, get rid of summer fixes
lgh_df <- do.call(rbind, lgh.ls.sm) %>%
  mutate(month = as.numeric(format(date_time, format = "%m"))) %>%
  filter(!month %in% c(5:7))

# Combine activity data into a dataframe, get rid of summer fixes
act_df <- do.call(rbind, act.ls.sm) %>%
  mutate(month = as.numeric(format(date_time, format = "%m"))) %>%
  filter(!month %in% c(5:7))

# Model the light level according to solar angle --------------------------

# Subsample light dataframe into 100,000 data points at random
lgh_mod_data <- 
  lgh_df[sample(c(1:nrow(lgh_df)), 100000), ]

# Run the model predicting 
require(mgcv)
lgh_mod <- gam(data = lgh_mod_data,
                 family = "binomial",
                 gamma = 1.2,
                 formula = std_light ~
                   s(sun.angle, bs = "ts", k = 10))

# Summarise this model
summary(lgh_mod)

# Plot out the model to make sure nothing too strange is happening
plot(lgh_mod)

# Predict this model over the observed solar angles in the data
lgh_df$light.pred <- predict.gam(lgh_mod, lgh_df)
lgh_df$light.pred <- ifelse(lgh_df$light.pred > 1, 1, lgh_df$light.pred)
lgh_df$light.pred <- ifelse(lgh_df$light.pred < 0, 0, lgh_df$light.pred)

# Identify leg tucks using light predictions ------------------------------

# Calculate the difference between the observed and expected light
lgh_df$light_diff <- lgh_df$light.pred - lgh_df$std_light

# Standard deviation on the difference between observed and expected light
sd(lgh_df$light_diff)

# Define a tuck as a point with lower than expected light (X2sd)
lgh_df$leg_tuck <- ifelse(lgh_df$light_diff > 0.5, 1, 0)

# Migrate leg tucks to activity dataframe ---------------------------------

# Split act and lgh dfs into lists by ID
lgh.ls <- split(lgh_df, lgh_df$individ_id)
act.ls <- split(act_df, act_df$individ_id)

# Create a function to transfer tucks from light to activity data
tuck_transfer <- function(lgh, act,
                          tuck_var = "leg_tuck",
                          raw_act = "std_conductivity",
                          ad_act = "std_conductivity_ad2"){
  require(xts)
  require(zoo)
  
  # Create alternative tuck variable for the light data
  lgh$leg_tuck_buffer <- lgh$leg_tuck 
  
  # Expand tucks by one fix either side of those identified using light levels
  for(i in 2:(nrow(lgh) - 1)){
    if(lgh$leg_tuck[i - 1] == 0 &
       lgh$leg_tuck[i] == 1){
      lgh$leg_tuck_buffer[i - 1] <- 1}
    
    if(lgh$leg_tuck[i] == 1 &
       lgh$leg_tuck[i + 1] == 0){
      lgh$leg_tuck_buffer[i + 1] <- 1}
  }
  
  # Create xts object from tucks
  xts_tuck <- xts(x = lgh$leg_tuck_buffer, order.by = lgh$date_time)
  
  # Create xts object from activitiy data
  xts_act <- xts(x = act[, raw_act], order.by = act$date_time)
  
  # Merge these two, initially keeping all time points
  xts_merge <- merge.xts(xts_act, xts_tuck, all = T, fill = na.locf)
  
  # Merge this merged xts with the activity xts
  # This time, keep only the values at the activity timestamps
  xts_merge <- merge.xts(xts_act, xts_merge, all = F, fill = na.locf)
  
  # Put the tuck variable back into the original activity dataframe
  act[, tuck_var] <- data.frame(xts_merge)$xts_tuck
  
  # Adjust the activity data, informed by the tuck variable
  act[, ad_act] <-
    if_else(act[, tuck_var] == 1,
            1, act[, raw_act])
  
  # Return the activity dataframe
  act
}

# Cycle through the datasets to append tucks to activity data
for(i in 1:length(act.ls)){
  act.ls[[i]] <- tuck_transfer(lgh = lgh.ls[[i]],
                            act = act.ls[[i]],
                            ad_act = "std_conductivity_ad")
  print(i)
}

# Save for later
save(act.ls, file = "data/cleaned/act_ls_sm2.RData")

