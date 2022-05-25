require(mgcViz)

viz_mod <- getViz(mod$gam)

plot.gamViz(viz_mod)

viz_strat_mod <- getViz(strat_mod$gam)

plot.gamViz(viz_strat_mod)

viz_bin_mod <- getViz(bin_mod$gam)

plot.gamViz(viz_bin_mod)

viz_lat_mod <- getViz(lat_mod$gam)

plot.gamViz(viz_lat_mod)
