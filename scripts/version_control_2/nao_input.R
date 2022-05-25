noa_dat <- read.csv(file = "data/env/noa_ind.csv") %>%
  rename(yyyy = ï..yyyy) %>%
  mutate(nao = ifelse(NAO == -99.90, NA, NAO),
         false_year = ifelse(mm > 6, yyyy, yyyy - 1))

moult_df3 <- moult_df3 %>%
  mutate(true_year = year(date),
         false_year = year(date - 182),
         month = month(date))

moult_df3$nao <- NA
moult_df3$nao_winter <- NA
moult_df3$nao_postb <- NA

for(i in 1:nrow(moult_df3)){
moult_df3$noa[i] <-
  noa_dat$nao[which(noa_dat$yyyy == moult_df3$true_year[i] &
                      noa_dat$mm == moult_df3$month[i])]

moult_df3$noa_winter[i] <-
  noa_dat$nao[which(noa_dat$false_year == moult_df3$false_year[i] &
                      (noa_dat$mm < 5 |
                      noa_dat$mm > 7))] %>%
  mean(na.rm = T)

moult_df3$noa_postb[i] <-
  noa_dat$nao[which(noa_dat$false_year == moult_df3$false_year[i] &
                      noa_dat$mm < 10 &
                         noa_dat$mm > 7)] %>%
  mean(na.rm = T)
}

save(moult_df3, file = "data/cleaned/moult_df3_noa.RData")
load(file = "data/cleaned/moult_df3_noa.RData")
