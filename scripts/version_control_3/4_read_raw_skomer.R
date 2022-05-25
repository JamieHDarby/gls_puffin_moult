
pos.ls.sk <- list()
act.ls.sk <- list()
lgh.ls.sk <- list()

# 1 - EJ47625L ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/AF_raw_data/sk_puff_ej47625_LL_b442_gl17090_20110626.lig",
  colony = "Skomer",
  col.lon = -5.3129,
  col.lat = 51.7356) %>%
  mutate(logger = "gl17090_mk4083",
         logger_id = "gl17090",
         logger_model = "mk4083",
         individ_id = "BTO_EJ47625L",
         colony = "Skomer",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/AF_raw_data/sk_puff_ej47625_LL_b442_gl17090_20110626.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EJ47625L") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/AF_raw_data/sk_puff_ej47625_LL_b442_gl17090_20110626.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EJ47625L") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sk[[1]] <- pos
act.ls.sk[[1]] <- act
lgh.ls.sk[[1]] <- lig

# 2 - EJ47625R ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/AF_raw_data/sk_puff_ej47625_RL_b442_gl17101_20110626.lig",
  colony = "Skomer",
  col.lon = -5.3129,
  col.lat = 51.7356) %>%
  mutate(logger = "gl17101_mk4083",
         logger_id = "gl17101",
         logger_model = "mk4083",
         individ_id = "BTO_EJ47625R",
         colony = "Skomer",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/AF_raw_data/sk_puff_ej47625_RL_b442_gl17101_20110626.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EJ47625R") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/AF_raw_data/sk_puff_ej47625_RL_b442_gl17101_20110626.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EJ47625R") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sk[[2]] <- pos
act.ls.sk[[2]] <- act
lgh.ls.sk[[2]] <- lig

# 3 - EL60569L ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/AF_raw_data/sk_puff_el60569_LL_female_gl17080_b422_20110414.lig",
  colony = "Skomer",
  col.lon = -5.3129,
  col.lat = 51.7356) %>%
  mutate(logger = "gl17080_mk4083",
         logger_id = "gl17080",
         logger_model = "mk4083",
         individ_id = "BTO_EL60569L",
         colony = "Skomer",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/AF_raw_data/sk_puff_el60569_LL_female_gl17080_b422_20110414.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EL60569L") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/AF_raw_data/sk_puff_el60569_LL_female_gl17080_b422_20110414.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EL60569L") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sk[[3]] <- pos
act.ls.sk[[3]] <- act
lgh.ls.sk[[3]] <- lig

# 4 - EL60569R ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/AF_raw_data/sk_puff_el60569_RL_female_gl17104_b422_20110414.lig",
  colony = "Skomer",
  col.lon = -5.3129,
  col.lat = 51.7356) %>%
  mutate(logger = "gl17104_mk4083",
         logger_id = "gl17104",
         logger_model = "mk4083",
         individ_id = "BTO_EL60569R",
         colony = "Skomer",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/AF_raw_data/sk_puff_el60569_RL_female_gl17104_b422_20110414.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EL60569R") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/AF_raw_data/sk_puff_el60569_RL_female_gl17104_b422_20110414.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EL60569R") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sk[[4]] <- pos
act.ls.sk[[4]] <- act
lgh.ls.sk[[4]] <- lig

# 5 - EL60573L ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/AF_raw_data/sk_puff_el60573_LL_gl17100_b442_20110630.lig",
  colony = "Skomer",
  col.lon = -5.3129,
  col.lat = 51.7356) %>%
  mutate(logger = "gl17100_mk4083",
         logger_id = "gl17100",
         logger_model = "mk4083",
         individ_id = "BTO_EL60573L",
         colony = "Skomer",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/AF_raw_data/sk_puff_el60573_LL_gl17100_b442_20110630.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EL60573L") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/AF_raw_data/sk_puff_el60573_LL_gl17100_b442_20110630.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EL60573L") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sk[[5]] <- pos
act.ls.sk[[5]] <- act
lgh.ls.sk[[5]] <- lig

# 6 - EL60573R ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/AF_raw_data/sk_puff_el60573_RL_gl17089_b442_20110630.lig",
  colony = "Skomer",
  col.lon = -5.3129,
  col.lat = 51.7356) %>%
  mutate(logger = "gl17089_mk4083",
         logger_id = "gl17089",
         logger_model = "mk4083",
         individ_id = "BTO_EL60573R",
         colony = "Skomer",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/AF_raw_data/sk_puff_el60573_RL_gl17089_b442_20110630.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EL60573R") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/AF_raw_data/sk_puff_el60573_RL_gl17089_b442_20110630.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         clipped = FALSE,
         individ_id = "BTO_EL60573R") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sk[[6]] <- pos
act.ls.sk[[6]] <- act
lgh.ls.sk[[6]] <- lig

# 7 - EL60648L ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/AF_raw_data/sk_puff_el60648_LL_gl17106_b441_20110416.lig",
  colony = "Skomer",
  col.lon = -5.3129,
  col.lat = 51.7356) %>%
  mutate(logger = "gl17106_mk4083",
         logger_id = "gl17106",
         logger_model = "mk4083",
         individ_id = "BTO_EL60648L",
         colony = "Skomer",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/AF_raw_data/sk_puff_el60648_LL_gl17106_b441_20110416.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EL60648L") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/AF_raw_data/sk_puff_el60648_LL_gl17106_b441_20110416.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         clipped = FALSE,
         individ_id = "BTO_EL60648L") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sk[[7]] <- pos
act.ls.sk[[7]] <- act
lgh.ls.sk[[7]] <- lig

# 8 - EL60648R ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/AF_raw_data/sk_puff_el60648_RL_gl3782_b441_20110416.lig",
  colony = "Skomer",
  col.lon = -5.3129,
  col.lat = 51.7356) %>%
  mutate(logger = "gl3782_mk4083",
         logger_id = "gl3782",
         logger_model = "mk4083",
         individ_id = "BTO_EL60648R",
         colony = "Skomer",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/AF_raw_data/sk_puff_el60648_RL_gl3782_b441_20110416.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EL60648R") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/AF_raw_data/sk_puff_el60648_RL_gl3782_b441_20110416.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EL60648R") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sk[[8]] <- pos
act.ls.sk[[8]] <- act
lgh.ls.sk[[8]] <- lig

# Save off results --------------------------------------------------------

save(pos.ls.sk, file = "data/cleaned/pos_ls_sk.RData")
save(act.ls.sk, file = "data/cleaned/act_ls_sk.RData")
save(lgh.ls.sk, file = "data/cleaned/lgh_ls_sk.RData")

load(file = "data/cleaned/pos_ls_sk.RData")
load(file = "data/cleaned/act_ls_sk.RData")
load(file = "data/cleaned/lgh_ls_sk.RData")
