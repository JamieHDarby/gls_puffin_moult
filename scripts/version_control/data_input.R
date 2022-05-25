
require(dplyr)
require(lubridate)
require(ggplot2)
require(oce)
require(birk)
require(stringr)

pos.data <- read.csv("data/MoultStrategies_Puffin_posdata_JamieD.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))

# Read in extra RDS data and align with csv data
act.data1 <- readRDS("data/activity_BT_puffins_SkelligMichael.rds") %>%
  mutate(date_time = ymd_hms(date_time),
         conductivity = as.integer(conductivity),
         std_conductivity = as.numeric(std_conductivity)) %>%
  rename(filename = file_name) %>%
  split(., .$session_id) %>%
  lapply(., function(x){x$individ_id <- 
    pos.data$individ_id[
      which(pos.data$session_id == x$session_id[1])[1]]
  x
  }) %>%
  do.call(rbind, .)

# Read in activity CSV
act.data2 <- read.csv("data/MoultStrategies_Puffin_activitydata_JamieD.csv") %>%
  mutate(date_time = ymd_hms(date_time))

# Combine the two above files
act.data <- rbind(act.data1, act.data2) %>%
  .[order(.$individ_id), ]

lgh.data1 <- readRDS("data/light_BT_puffins_SkelligMichael.rds") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time),
         raw_light = as.numeric(raw_light),
         std_light = as.numeric(std_light),
         filename = gsub(".lig", "", file_name),
         filename = gsub("Ske", "Skellig", filename)) %>%
  select(!file_name) %>%
  split(., .$filename) %>%
  lapply(., function(x){x[c("session_id", "individ_id")] <- 
    act.data1[which(act.data1$filename == x$filename[1])[1],
      c("session_id", "individ_id")]
  x
  }) %>%
  do.call(rbind, .)

lgh.data2 <- read.csv("data/MoultStrategies_Puffin_lightdata_JamieD.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))

# Combine the two above files
lgh.data <- rbind(lgh.data1, lgh.data2) %>%
  filter(individ_id %in% act.data$individ_id) %>%
  .[order(.$individ_id), ]

pos.data <- pos.data %>%
  filter(individ_id %in% act.data$individ_id) %>%
  .[order(.$individ_id), ]

# List activity dataframe by individual
act.ls <- split(act.data, act.data$individ_id)

lgh.ls <- split(lgh.data, lgh.data$individ_id)

pos.ls <- split(pos.data, pos.data$individ_id)

for(i in 1:400){
  print(nrow(act.ls[[i]]) / nrow(lgh.ls[[i]]))
}

combo.ls <- list()

for(i in 1:400){
  combo.ls[[i]] <-
    lgh.ls[[i]][
      which(lgh.ls[[i]]$date_time %in% act.ls[[i]]$date_time),]
  
  combo.ls[[i]]$act <- 
    act.ls[[i]]$std_conductivity[
      which(combo.ls[[i]]$date_time %in% act.ls[[i]]$date_time)]
  
  print(i)
}

combo.ls2 <- list()

system.time(
  for(i in 1:400){
  combo.ls2[[i]] <- 
    split(combo.ls[[i]], combo.ls[[i]]$date) %>%
    lapply(., function(x){
      x$lat <- pos.ls[[i]]$lat_smooth2[which.closest(pos.ls[[i]]$date, x$date[1])]
      x$lon <- pos.ls[[i]]$lon_smooth2[which.closest(pos.ls[[i]]$date, x$date[1])]
      x$sun.angle <- sunAngle(t = x$date_time, 
                              longitude = x$lon,
                              latitude = x$lat)$altitude
      x
        }) %>%
    do.call(rbind, .)
    print(i)
  }
)

for(i in 1:400){
  combo.ls2[[i]]$colony <- pos.ls[[i]]$colony[1]
  combo.ls2[[i]]$sex <- pos.ls[[i]]$sex[1]
  combo.ls2[[i]]$col_dist <- pos.ls[[i]]$disttocol_s2[1]
  }

save(combo.ls2, file = "data/cleaned/combo_ls2.RData")

lgh.ls2 <- list()

system.time(
  for(i in 1:400){
    lgh.ls2[[i]] <- 
      split(lgh.ls[[i]], lgh.ls[[i]]$date) %>%
      lapply(., function(x){
        x$lat <- pos.ls[[i]]$lat_smooth2[which.closest(pos.ls[[i]]$date, x$date[1])]
        x$lon <- pos.ls[[i]]$lon_smooth2[which.closest(pos.ls[[i]]$date, x$date[1])]
        x$sun.angle <- sunAngle(t = x$date_time, 
                                longitude = x$lon,
                                latitude = x$lat)$altitude
        x
      }) %>%
      do.call(rbind, .)
    print(i)
  }
)


for(i in 1:400){
  lgh.ls2[[i]]$colony <- pos.ls[[i]]$colony[1]
  lgh.ls2[[i]]$sex <- pos.ls[[i]]$sex[1]
  combo.ls2[[i]]$col_dist <- pos.ls[[i]]$disttocol_s2[1]
}

save(lgh.ls2, file = "data/cleaned/lgh_ls2.RData")

save(pos.ls, file = "data/cleaned/pos_ls.RData")

