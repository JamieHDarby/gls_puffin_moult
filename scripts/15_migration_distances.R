
# Load in this dataframe
load(file = "data/cleaned/pos_ls_sk.RData")

# Calculate NSD score for each puffin#

# EJ47625 -----------------------------------------------------------------

# Choose list element
  df <- pos.ls.sk[[1]]
  
  # Calculate distance from the colony
  df$col_dist <- pointDistance(p1 = df[c("lon", "lat_filled")],
                               p2 = df[1, c("col_lon", "col_lat")],
                               lonlat = T)
  
  # Caluculate numeric month
  df$month <- as.numeric(format(df$time, format = "%m"))
  
  # Filter out any breeding months
  df <- df %>% filter(!month %in% c(4, 5, 6, 7)) %>%
    # Caluclate NSD
    mutate(nsd = (col_dist / 1000) ^ 2)
  
  # Lavielle segmentation based on Amerlineau et al 2021
  lav_res <- lavielle(x = df$nsd, Lmin = 5, Kmax = 15, plotit = T)
  
  # Print bird name
  print(df$individ_id[1])
  
  # Choose best segmentation based on graph shape
chooseseg(lav_res)

# Extract out segment indices
find_path_res <- findpath(lav_res, 4)

# Print out metrics describing each segment
for(i in 1:length(find_path_res)){
  # Subset the dataframe into segment of interest
  x <- df[find_path_res[[i]][1]:find_path_res[[i]][2], ]
  
  # Print start date, end date, time difference and mean distance
  print(x$date[1])
  print(x$date[nrow(x)])
  print(x$date[nrow(x)] - x$date[1])
  print(mean(x$col_dist / 1000))
}

# Process is the same for all subsequent birds

# EL60569 -----------------------------------------------------------------

df <- pos.ls.sk[[3]]

df$col_dist <- pointDistance(p1 = df[c("lon", "lat_filled")],
                             p2 = df[1, c("col_lon", "col_lat")],
                             lonlat = T)

df$month <- as.numeric(format(df$time, format = "%m"))

df <- df %>% filter(!month %in% c(4, 5, 6, 7)) %>%
  mutate(nsd = (col_dist / 1000) ^ 2)

lav_res <- lavielle(x = df$nsd, Lmin = 5, Kmax = 15, plotit = T)

print(df$individ_id[1])

chooseseg(lav_res)
find_path_res <- findpath(lav_res, 4)

for(i in 1:length(find_path_res)){
  x <- df[find_path_res[[i]][1]:find_path_res[[i]][2], ]
  
  print(x$date[1])
  print(x$date[nrow(x)])
  print(x$date[nrow(x)] - x$date[1])
  print(mean(x$col_dist / 1000))
}

# EL60573 -----------------------------------------------------------------

df <- pos.ls.sk[[5]]

df$col_dist <- pointDistance(p1 = df[c("lon", "lat_filled")],
                             p2 = df[1, c("col_lon", "col_lat")],
                             lonlat = T)

df$month <- as.numeric(format(df$time, format = "%m"))

df <- df %>% filter(!month %in% c(4, 5, 6, 7)) %>%
  mutate(nsd = (col_dist / 1000) ^ 2)

lav_res <- lavielle(x = df$nsd, Lmin = 5, Kmax = 15, plotit = T)

print(df$individ_id[1])

chooseseg(lav_res)
find_path_res <- findpath(lav_res, 4)

for(i in 1:length(find_path_res)){
  x <- df[find_path_res[[i]][1]:find_path_res[[i]][2], ]
  
  print(x$date[1])
  print(x$date[nrow(x)])
  print(x$date[nrow(x)] - x$date[1])
  print(mean(x$col_dist / 1000))
}

# EL60648 -----------------------------------------------------------------

df <- pos.ls.sk[[7]]

df$col_dist <- pointDistance(p1 = df[c("lon", "lat_filled")],
                             p2 = df[1, c("col_lon", "col_lat")],
                             lonlat = T)

df$month <- as.numeric(format(df$time, format = "%m"))

df <- df %>% filter(!month %in% c(4, 5, 6, 7)) %>%
  mutate(nsd = (col_dist / 1000) ^ 2)

lav_res <- lavielle(x = df$nsd, Lmin = 5, Kmax = 15, plotit = T)

print(df$individ_id[1])

chooseseg(lav_res)
find_path_res <- findpath(lav_res, 3)

for(i in 1:length(find_path_res)){
  x <- df[find_path_res[[i]][1]:find_path_res[[i]][2], ]
  
  print(x$date[1])
  print(x$date[nrow(x)])
  print(x$date[nrow(x)] - x$date[1])
  print(mean(x$col_dist / 1000))
}

# Skellig Michael birds ---------------------------------------------------

load("data/cleaned/pos_ls_sm.RData")

# EW67607 -----------------------------------------------------------------

df <- pos.ls.sm[[2]]

df$col_dist <- pointDistance(p1 = df[c("lon", "lat_filled")],
                             p2 = df[1, c("col_lon", "col_lat")],
                             lonlat = T)

df$month <- as.numeric(format(df$time, format = "%m"))

df <- df %>% filter(!month %in% c(4, 5, 6, 7)) %>%
  mutate(nsd = (col_dist / 1000) ^ 2)

lav_res <- lavielle(x = df$nsd, Lmin = 5, Kmax = 15, plotit = T)

print(df$individ_id[1])

chooseseg(lav_res)
find_path_res <- findpath(lav_res, 4)

for(i in 1:length(find_path_res)){
  x <- df[find_path_res[[i]][1]:find_path_res[[i]][2], ]
  
  print(x$date[1])
  print(x$date[nrow(x)])
  print(x$date[nrow(x)] - x$date[1])
  print(mean(x$col_dist / 1000))
}

# EW67604 -----------------------------------------------------------------

df <- pos.ls.sm[[6]]

df$col_dist <- pointDistance(p1 = df[c("lon", "lat_filled")],
                             p2 = df[1, c("col_lon", "col_lat")],
                             lonlat = T)

df$month <- as.numeric(format(df$time, format = "%m"))

df <- df %>% filter(!month %in% c(4, 5, 6, 7)) %>%
  mutate(nsd = (col_dist / 1000) ^ 2)

lav_res <- lavielle(x = df$nsd, Lmin = 5, Kmax = 15, plotit = T)

print(df$individ_id[1])

chooseseg(lav_res)
find_path_res <- findpath(lav_res, 4)

for(i in 1:length(find_path_res)){
  x <- df[find_path_res[[i]][1]:find_path_res[[i]][2], ]
  
  print(x$date[1])
  print(x$date[nrow(x)])
  print(x$date[nrow(x)] - x$date[1])
  print(mean(x$col_dist / 1000))
}

# EW67603 -----------------------------------------------------------------

df <- pos.ls.sm[[7]]

df$col_dist <- pointDistance(p1 = df[c("lon", "lat_filled")],
                             p2 = df[1, c("col_lon", "col_lat")],
                             lonlat = T)

df$month <- as.numeric(format(df$time, format = "%m"))

df <- df %>% filter(!month %in% c(4, 5, 6, 7)) %>%
  mutate(nsd = (col_dist / 1000) ^ 2)

lav_res <- lavielle(x = df$nsd, Lmin = 5, Kmax = 15, plotit = T)

print(df$individ_id[1])

chooseseg(lav_res)
find_path_res <- findpath(lav_res, 2)

for(i in 1:length(find_path_res)){
  x <- df[find_path_res[[i]][1]:find_path_res[[i]][2], ]
  
  print(x$date[1])
  print(x$date[nrow(x)])
  print(x$date[nrow(x)] - x$date[1])
  print(mean(x$col_dist / 1000))
}
