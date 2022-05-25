
library(zoo)
library(lubridate)
library(oce)
library(birk)
library(purrr)
library(raster)
library(dplyr)
library(ggplot2)
library(mgcv)

moult_df3 <- moult_df3 %>%
  mutate(storm1 = NA,
         storm2 = NA,
         storm3 = NA,
         storm4 = NA,
         month = as.numeric(format(date, "%m")))

for(i in c(1:4, 8:12)){
  for(c in 1:4){
    rstr <- 
      raster(paste("data/storm_process/NBmoycyclone_classe",
                   c, "_mois", i, ".tif", sep = "")) %>%
      projectRaster(., crs = CRS("+init=epsg:4326"))
    
    sp <- SpatialPoints(
      coords = moult_df3[which(moult_df3$month == i), c("lon", "lat_filled")])
    
    var <- paste("storm", c, sep = "")
    
    moult_df3[which(moult_df3$month == i), var] <- raster::extract(rstr, sp)
  }
}

storm_rstr <- raster("data/shapes/winter_raster_latlon.tif")

plot(storm_rstr)

sp <- SpatialPoints(coords = moult_df3[, c("lon", "lat_filled")])

moult_df3$storms <- raster::extract(storm_rstr, sp)

moult_df3$year <- as.numeric(format((moult_df3$date - 182), format = "%y"))

save(moult_df3, file = "data/cleaned/moult_df3_storms.RData")
load("data/cleaned/moult_df3_storms.RData")

ggplot(meta_df) +
  geom_density(aes(x = storms, fill = area), alpha = 0.6)

plot(sp, add = T)


mod_df <- moult_df3 %>%
  mutate(bin_moult = ifelse(prim_moult > 0, 1, 0),
         area = as.factor(area),
         id = as.factor(id),
         colony = as.factor(colony),
         storm1_trans = sqrt(storm1),
         storm2_trans = sqrt(storm2),
         storm3_trans = sqrt(storm3),
         storm4_trans = (storm4)^(0.5),
         storm_all = (storm1 +
           (storm2 * 2) +
           (storm3 * 3) +
           (storm4 * 4))) %>%
  filter(!is.na(storm4))

mod_df$arstart <- T
for(i in 2:nrow(mod_df)){
  if(mod_df$id_year[i] == mod_df$id_year[i - 1]){
    mod_df$arstart[i] <- F
  }else{
    mod_df$arstart[i] <- T
  }
}

mod_df_res <- split(mod_df, mod_df$id_year) %>%
  
  lapply(function(x){
    ind <- rep(T, nrow(x))
    
    for(i in 2:nrow(x)){
      if(x$bin_moult[i] == 0 & x$bin_moult[i - 1]){
        ind[i:nrow(x)] <- F
        break
      }
    }
    x <- x[ind, ]
    
    x
  }) %>%
  
  # lapply(function(x){if(nrow(x) < 120){x <- NULL}; x}) %>%
  
  do.call(rbind, .)

# require(mgcv)

mod_1_bin <- bam(data = mod_df,
             formula = bin_moult ~
               # s(storm1_trans, bs = "ts", k = 5) +
               # s(storm2_trans, bs = "ts", k = 5) +
               # s(storm3_trans, bs = "ts", k = 5) +
               s(storm4_trans, bs = "ts", k = 5) +
               # s(sqrt(Wave), bs = "ts", k = 5) +
               # s(sqrt(WindSpeed), bs = "ts", k = 5) +
               # s(storm_all, bs = "ts", k = 5) +
               s(lon, lat_filled, bs = "ts", k = 5) +
               s(colony, id, bs = "re") +
               s(id, bs = "re"),
             # na.action = "na.fail",
             family = binomial(link = "logit"),
             method = "fREML",
             select = T,
             discrete = T,
             rho = 0.90,
             AR.start = arstart)

summary(mod_1_bin)
# acf(residuals(mod_1_bin))[1]
plot(mod_1_bin)

# MuMIn::dredge(mod_1_bin)

concurvity(mod_1_bin)

require(mgcViz)

viz_mod <- getViz(mod_1_bin)

var_plot1 <- plot(sm(viz_mod, 1)) +
  l_fitLine(linetype = 1) +
  l_ciPoly(fill = "#162c4d", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  ylab(label = "Probability of Moult Behaviour") +
  xlab(label = "Category 4 cyclones per month per 250km grid square") +
  labs(caption = "Estimated degrees of freedom = 1.3\nChi-squared value = 27.6\np-value < 0.001") +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5), labels = c("0", "0.25", "1", "2.25"))

var_plot2 <- plot(sm(viz_mod, 2)) +
  l_fitRaster() +
  l_fitContour() +
  ylab(label = "Probability of Moult Behaviour\n\nLatitude") +
  xlab(label = "Longitude") +
  labs(title = "",
       caption = "Estimated degrees of freedom = 13.2\nChi-squared value = 177.8\np-value < 0.001") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "left")

var_combo <- gridPrint(var_plot1, var_plot2, nrow = 2, heights = c(1, 1.5))
  
ggsave(var_combo, filename = "plots/var_plot.png", width = 8, height = 10, dpi = 500)
