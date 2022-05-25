
# Load in data lists ------------------------------------------------------

load(file = "data/cleaned/pos_ls_pre.RData")

load(file = "data/cleaned/lgh_ls_pre.RData")

load(file = "data/cleaned/act_ls_pre.RData")

# Standardise the scaled light variables ----------------------------------

for(i in 1:length(lgh.ls)){
  
  lgh.ls[[i]]$std_light_ad <- rep(NA, nrow(lgh.ls[[i]]))
  
  if(lgh.ls[[i]]$logger_model[1] %in% c("f100", "c65", "w65")){
    
    lgh.ls[[i]]$std_light_ad <-
      lgh.ls[[i]]$std_light - min(lgh.ls[[i]]$std_light)
    
    lgh.ls[[i]]$std_light_ad <- 
      ifelse(lgh.ls[[i]]$std_light_ad > 0.1, 0.1, lgh.ls[[i]]$std_light_ad)
    
    lgh.ls[[i]]$std_light_ad <-
      lgh.ls[[i]]$std_light_ad * 10
  }else{
    lgh.ls[[i]]$std_light_ad <- lgh.ls[[i]]$std_light
  }
  print(i)
}

# Create data frames from lists of geolocator outputs ---------------------

# Combine light data into a dataframe, get rid of summer fixes
lgh_df <- bind_rows(lgh.ls) %>%
  mutate(month = as.numeric(format(date_time, format = "%m"))) %>%
  filter(!month %in% c(5:7)) %>%
  mutate(logger = as.factor(
    ifelse(logger_model %in% c("f100", "c65", "w65"),
           "lt", "mk")))

# Model the light level according to solar angle --------------------------

# Subsample light dataframe into 100,000 data points at random
lgh_mod_data <- 
  lgh_df[sample(c(1:nrow(lgh_df)), 100000), ]

# Run the model predicting 
require(mgcv)
lgh_mod <- gam(data = lgh_mod_data,
               family = "binomial",
               gamma = 1.2,
               formula = std_light_ad ~
                 s(sun.angle, bs = "ts", k = 10, by = logger))

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
lgh_df$light_diff <- lgh_df$light.pred - lgh_df$std_light_ad

# Standard deviation on the difference between observed and expected light
sd(lgh_df$light_diff)

# Define a tuck as a point with lower than expected light (X2sd)
lgh_df$leg_tuck <- ifelse(lgh_df$light_diff > 0.5, 1, 0)

# Migrate leg tucks to activity dataframe ---------------------------------

# Split act and lgh dfs into lists by ID
lgh.ls <- split(lgh_df, lgh_df$individ_id)

# Save this off
save(lgh.ls, file = "data/cleaned/lgh_ls_post.RData")

# Combine activity data into a dataframe, get rid of summer fixes
act.ls <- lapply(act.ls, function(x){
  x <- x %>%
  mutate(month = as.numeric(format(date_time, format = "%m"))) %>%
  filter(!month %in% c(5:7))
  return(x)
})

# Load this if not loaded
load("data/cleaned/lgh_ls_post.RData")

# Cycle through the datasets to append tucks to activity data
for(i in 1:length(act.ls)){
  act.ls[[i]] <- tuck_transfer(lgh = lgh.ls[[i]],
                               act = act.ls[[i]],
                               ad_act = "std_conductivity_ad")
  print(i)
}

# Save this off
save(act.ls, file = "data/cleaned/act_ls_post.RData")
