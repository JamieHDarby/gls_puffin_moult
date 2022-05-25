moult_df_mk <- moult_df %>%
  filter(!id_year %in% moult_df2$id_year)

moult_df_cwf <- moult_df4 %>%
  filter(!id_year %in% moult_df2$id_year)

moult_df_all <- rbind(moult_df2, moult_df_cwf, moult_df_mk)

length(unique(moult_df_all$id))

lgh_df <- do.call(rbind, lgh.ls2)

ggplot(lgh_df) +
  geom_density(aes(x = std_light, fill = logger)) +
  scale_y_continuous(trans = "sqrt") +
  facet_wrap(facets = ~logger, scales = "free_y")

