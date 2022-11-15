
# Read in processed position data
pos.data1 <- read.csv("data/raw/MoultStrategies_Puffin_posdata_JamieD.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time),
         date_time_tz = date_time)
pos.data2 <- read.csv("data/raw/MoultStrategies_Puffin_posdata_JamieD_2019-2020.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))
pos.data3 <- read.csv("data/raw/MoultStrategies_Puffin_posdata_WitlessBay_2013-15.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))
pos.data <- rbind(pos.data1, pos.data2[, -1], pos.data3[, -1]) %>%
  .[order(.$individ_id), ]

rm(pos.data1, pos.data2, pos.data3)

# Read in extra RDS data and align with csv data
act.data1 <- readRDS("data/raw/activity_BT_puffins_SkelligMichael.rds") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time),
         conductivity = as.integer(conductivity),
         std_conductivity = as.numeric(std_conductivity)) %>%
  rename(filename = file_name) %>%
  split(., .$session_id) %>%
  lapply(., function(x){x$individ_id <- 
    pos.data$individ_id[
      which(pos.data$session_id == x$session_id[1])[1]]
  x
  }) %>%
  bind_rows()

# Read in activity CSV
act.data2 <- read.csv(
  "data/raw/MoultStrategies_Puffin_activitydata_JamieD.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))
act.data3 <- read.csv(
  "data/raw/MoultStrategies_Puffin_activitydata_JamieD_2019-2020.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))
act.data4 <- read.csv(
  "data/raw/MoultStrategies_Puffin_activitydata_WitlessBay_2013-15.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))

# Combine the four above files
act.data <- rbind(act.data1, act.data2, act.data3[, -1], act.data4[, -1]) %>%
  .[order(.$individ_id), ] %>%
  select(-filename, -session_id)

rm(act.data2, act.data3, act.data4)

lgh.data1 <- readRDS("data/raw/light_BT_puffins_SkelligMichael.rds") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time),
         raw_light = as.numeric(raw_light),
         std_light = as.numeric(std_light),
         filename = gsub(".lig", "", file_name),
         filename = gsub("Ske", "Skellig", filename)) %>%
  dplyr::select(!file_name) %>%
  split(., .$filename) %>%
  lapply(., function(x){x[c("session_id", "individ_id")] <- 
    act.data1[which(act.data1$filename == x$filename[1])[1],
      c("session_id", "individ_id")]
  x
  }) %>%
  bind_rows(.)

# Read in CSVs
lgh.data2 <- read.csv(
  "data/raw/MoultStrategies_Puffin_lightdata_JamieD.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))
lgh.data3 <- read.csv(
  "data/raw/MoultStrategies_Puffin_lightdata_JamieD_2019-2020.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))
lgh.data4 <- read.csv(
  "data/raw/MoultStrategies_Puffin_lightdata_WitlessBay_2013-15.csv") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time))

# Combine the two above files
lgh.data <- rbind(lgh.data1, lgh.data2, lgh.data3[, -1], lgh.data4[, -1]) %>%
  filter(individ_id %in% act.data$individ_id) %>%
  .[order(.$individ_id), ] %>%
  select(-clipped, -filename, -session_id)

rm(act.data1, lgh.data1, lgh.data2, lgh.data3, lgh.data4)

pos.data <- pos.data %>%
  filter(individ_id %in% lgh.data$individ_id) %>%
  .[order(.$individ_id), ]

act.data <- act.data %>%
  filter(individ_id %in% lgh.data$individ_id)

pos.data <- split(pos.data, pos.data$individ_id) %>%
  lapply(., lat_filler,
         lat.var = "lat_smooth2_eqfilt3") %>%
  bind_rows()

# List activity dataframe by individual
pos.ls <- split(pos.data, pos.data$individ_id)

# List activity dataframe by individual
act.ls <- split(act.data, act.data$individ_id)

# List light dataframe by individual
lgh.ls <- split(lgh.data, lgh.data$individ_id)

# Get rid of row names, drastically reduces variable size
lgh.ls <- lapply(lgh.ls,
                 FUN = function(x){
                   attributes(x)$row.names <- (1:length(x$date_time)); x})
act.ls <- lapply(act.ls,
                 FUN = function(x){
                   attributes(x)$row.names <- (1:length(x$date_time)); x})

# Multicore to speed things up
plan(multisession)

# Append location etc to activity and light data
system.time(
  act.ls <-
    future_lapply(act.ls,
           FUN = var_append,
                  pos.df = pos.data,
                  lon.var = "lon_smooth2",
           vars = c("colony", "col_lat",
                    "col_lon", "logger_model")))

system.time(
  lgh.ls <-
    future_lapply(lgh.ls,
                  FUN = var_append,
                  pos.df = pos.data,
                  lon.var = "lon_smooth2",
                  vars = c("colony", "col_lat",
                           "col_lon", "logger_model")))

# Go back to single thread
plan(sequential)

# Save everything to be loaded fresh the next time
save(pos.ls, file = "data/cleaned/pos_ls_pre.RData")

save(lgh.ls, file = "data/cleaned/lgh_ls_pre.RData")

save(act.ls, file = "data/cleaned/act_ls_pre.RData")

rm(list = ls())
gc()

load(file = "data/cleaned/pos_ls_pre.RData")

load(file = "data/cleaned/lgh_ls_pre.RData")

load(file = "data/cleaned/act_ls_pre.RData")
