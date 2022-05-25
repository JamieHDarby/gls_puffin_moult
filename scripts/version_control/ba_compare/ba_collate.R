

ba.df <-
  data.frame(moult = do.call(rbind, moult.ba.ls),
             "X1" = do.call(rbind, nonmoult.ba.ls.1),
             "X2" = do.call(rbind, nonmoult.ba.ls.2),
             "X3" = do.call(rbind, nonmoult.ba.ls.3),
             "X4" = do.call(rbind, nonmoult.ba.ls.4),
             "X5" = do.call(rbind, nonmoult.ba.ls.5),
             "X6" = do.call(rbind, nonmoult.ba.ls.6),
             "X7" = do.call(rbind, nonmoult.ba.ls.7),
             "X8" = do.call(rbind, nonmoult.ba.ls.8))

ba.df$area <- c("Ireland", "Iceland", "Faroe Islands", "Norwegian Sea", "Barents Sea", "UK")

ba.df <- ba.df %>%
  tidyr::pivot_longer(cols = c("moult", "X1", "X2",
                               "X3", "X4", "X5",
                               "X6", "X7", "X8"),
                      names_to = "Month", values_to = "Overlap") %>%
  as.data.frame()

nonmoult_overlap <- 
  ba.df %>%
  filter(Month != "moult") %>%
  mutate(num = as.numeric(as.factor(Month))) %>%
         # split = ifelse(area == "All", "All Colonies", "By Area")) %>%
  ggplot() +
  scale_fill_manual(values = wes_palette(n = 6, name = "IsleofDogs1", type = "continuous")) +
  # scale_y_continuous(limits = c(0, 15000)) +
  scale_x_continuous(breaks = c(1:8),
                     labels = c("Sep", "Oct", "Nov", "Dec",
                                "Jan", "Feb", "Mar", "Apr")) +
  geom_area(aes(x = num, y = Overlap, fill = area),
            position = "stack", alpha = 0.8) +
  labs(fill = "Area", x = "Month", y = "")

moult_overlap <- 
  ba.df %>%
  filter(Month == "moult") %>%
  mutate(num = as.numeric(as.factor(Month))) %>%
  # split = ifelse(area == "All", "All Colonies", "By Area")) %>%
  ggplot() +
  scale_fill_manual(values = wes_palette(n = 6, name = "IsleofDogs1", type = "continuous")) +
  # scale_y_continuous(limits = c(0, 15000)) +
  geom_col(aes(x = Month, y = Overlap, fill = area),
            position = "stack", alpha = 0.8) +
  labs(fill = "Area", x = "Moult", y = "")
