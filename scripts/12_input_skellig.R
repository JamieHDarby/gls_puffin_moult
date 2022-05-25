
# Create empty lists to house the GLS data

pos.ls.sm <- list()
act.ls.sm <- list()
lgh.ls.sm <- list()

# 1 - BTO_EW67615 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C3149_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c3149_mk4083",
         logger_id = "c3149",
         logger_model = "mk4083",
         individ_id = "BTO_EW67615",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C3149_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67615") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C3149_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EW67615") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[1]] <- pos
act.ls.sm[[1]] <- act
lgh.ls.sm[[1]] <- lig

# 2 - BTO_EW67607 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C3150_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c3150_mk4083",
         logger_id = "c3150",
         logger_model = "mk4083",
         individ_id = "BTO_EW67607",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C3150_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67607") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C3150_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EW67607") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[2]] <- pos
act.ls.sm[[2]] <- act
lgh.ls.sm[[2]] <- lig

# 3 - BTO_EW67609 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C3156_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c3156_mk4083",
         logger_id = "c3156",
         logger_model = "mk4083",
         individ_id = "BTO_EW67609",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C3156_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67609") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C3156_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EW67609") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[3]] <- pos
act.ls.sm[[3]] <- act
lgh.ls.sm[[3]] <- lig

# 4 - BTO_EW67608 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C3157_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c3157_mk4083",
         logger_id = "c3157",
         logger_model = "mk4083",
         individ_id = "BTO_EW67608",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C3157_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67608") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C3157_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EW67608") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[4]] <- pos
act.ls.sm[[4]] <- act
lgh.ls.sm[[4]] <- lig

# 5 - BTO_EW67606 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C4702_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c4702_mk4083",
         logger_id = "c4702",
         logger_model = "mk4083",
         individ_id = "BTO_EW67606",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C4702_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67606") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C4702_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EW67606") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[5]] <- pos
act.ls.sm[[5]] <- act
lgh.ls.sm[[5]] <- lig

# 6 - BTO_EW67604 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C4848_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702,
  manual.zenith = 95) %>%
  mutate(logger = "c4848_mk4083",
         logger_id = "c4848",
         logger_model = "mk4083",
         individ_id = "BTO_EW67604",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C4848_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67604") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C4848_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         clipped = FALSE,
         individ_id = "BTO_EW67604") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[6]] <- pos
act.ls.sm[[6]] <- act
lgh.ls.sm[[6]] <- lig

# 7 - BTO_EW67603 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C4851_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c4851_mk4083",
         logger_id = "c4851",
         logger_model = "mk4083",
         individ_id = "BTO_EW67603",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C4851_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67603") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C4851_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         clipped = FALSE,
         individ_id = "BTO_EW67603") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[7]] <- pos
act.ls.sm[[7]] <- act
lgh.ls.sm[[7]] <- lig

# 8 - BTO_EW67605 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C4852_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c4852_mk4083",
         logger_id = "c4852",
         logger_model = "mk4083",
         individ_id = "BTO_EW67605",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C4852_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67605") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C4852_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EW67605") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[8]] <- pos
act.ls.sm[[8]] <- act
lgh.ls.sm[[8]] <- lig


# 9 - BTO_EW67601 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C4853_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c4853_mk4083",
         logger_id = "c4853",
         logger_model = "mk4083",
         individ_id = "BTO_EW67601",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C4853_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EW67601") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C4853_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EW67601") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[9]] <- pos
act.ls.sm[[9]] <- act
lgh.ls.sm[[9]] <- lig


# 10 - BTO_EX52010 ------------------------------------------------------------

pos <- gls_fun(
  filepath =
    "data/raw/UCC_raw_data/C4854_000.lig",
  colony = "Skellig Michael",
  col.lon = -10.5385,
  col.lat = 51.7702) %>%
  mutate(logger = "c4854_mk4083",
         logger_id = "c4854",
         logger_model = "mk4083",
         individ_id = "BTO_EX52010",
         colony = "Skellig Michael",
         col_lon = col.lon,
         col_lat = col.lat,
         sun = 90 - GLSzenith,
         date = as.Date(date_time),
         latloess_eq = ifelse(julian %in% c(244:286, 60:102), NA, latloess)) %>%
  lat_filler(., lat.var = "latloess_eq")

act <-
  readAct(file = "data/raw/UCC_raw_data/C4854_000.act") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         conductivity = Activity,
         std_conductivity = Activity/max(Activity, na.rm = T),
         individ_id = "BTO_EX52010") %>%
  dplyr::select(individ_id, date_time,
                conductivity, std_conductivity, date) %>%
  var_append(., pos.df = pos)

lig <-
  readLig(file = "data/raw/UCC_raw_data/C4854_000.lig") %>%
  filter(!is.na(Date)) %>%
  mutate(date_time = ymd_hms(Date),
         date = as_date(Date),
         raw_light = Light,
         std_light = Light/max(Light, na.rm = T),
         individ_id = "BTO_EX52010") %>%
  dplyr::select(individ_id, date_time,
                raw_light, std_light, date) %>%
  var_append(., pos.df = pos)

pos.ls.sm[[10]] <- pos
act.ls.sm[[10]] <- act
lgh.ls.sm[[10]] <- lig

# Save off results --------------------------------------------------------

save(pos.ls.sm, file = "data/cleaned/pos_ls_sm.RData")
save(act.ls.sm, file = "data/cleaned/act_ls_sm.RData")
save(lgh.ls.sm, file = "data/cleaned/lgh_ls_sm.RData")

