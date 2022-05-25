
test <- do.call(rbind, pos.ls)

# Pick julian day out of the date variable
test$julian <- as.integer(format(test$date, "%j"))

# Isolate year out of the date variable, offset by -1/2
test$year <- as.integer(format((as_date(test$date) - 182), "%y"))

# Centre julian day around 0
test$julian[which(test$julian > 150)] <-
  test$julian[which(test$julian > 150)] - 365

# Remove April to end August dates
test <- test[!as.integer(format(test$date, "%m")) %in% 5:7, ]

test$id_year <- paste(test$individ_id, test$year, sep = "_")

test2 <- split(moult_df3, moult_df3$id_year) %>%
  .[(which(sapply(., nrow) > 100))] %>%
  do.call(rbind, .)

test.ls <- split(test, test$colony)

length(unique(test$individ_id))
length(unique(test$id_year))

do.call(rbind)
test.ls[[19]]$colony[1]
length(unique(test.ls[[19]]$individ_id))
length(unique(test.ls[[19]]$id_year))

test.combo.rost <- do.call(rbind,test.ls[c(13,14)])
length(unique(test.combo.rost$individ_id))
length(unique(test.combo.rost$id_year))

test.combo.hjelm <- do.call(rbind,test.ls[c(5,6)])
length(unique(test.combo.hjelm$individ_id))
length(unique(test.combo.hjelm$id_year))

test.combo.horn <- do.call(rbind,test.ls[c(8,9)])
length(unique(test.combo.horn$individ_id))
length(unique(test.combo.horn$id_year))

length(unique(moult_df3$id))
length(unique(moult_df3$id_year))

test <- split(moult_df3, moult_df3$colony)

length(unique(test$Anda$id))
length(unique(test$Anda$id_year))
length(unique(test$Bjørnøya$id))
length(unique(test$Bjørnøya$id_year))
length(unique(test$`Faroe Islands`$id))
length(unique(test$`Faroe Islands`$id_year))
length(unique(test$Grimsey$id))
length(unique(test$Grimsey$id_year))
length(unique(test$Hjelmsøya$id))
length(unique(test$Hjelmsøya$id_year))
length(unique(test$Holmahals$id))
length(unique(test$Holmahals$id_year))
length(unique(test$Hornøya$id))
length(unique(test$Hornøya$id_year))
length(unique(test$`Isle of May`$id))
length(unique(test$`Isle of May`$id_year))
length(unique(test$`Little Saltee`$id))
length(unique(test$`Little Saltee`$id_year))
length(unique(test$Papey$id))
length(unique(test$Papey$id_year))
length(unique(test$Røst$id))
length(unique(test$Røst$id_year))
length(unique(test$Runde$id))
length(unique(test$Runde$id_year))
length(unique(test$`Seven Islands`$id))
length(unique(test$`Seven Islands`$id_year))
length(unique(test$`Skellig Michael`$id))
length(unique(test$`Skellig Michael`$id_year))
length(unique(test$Sklinna$id))
length(unique(test$Sklinna$id_year))
length(unique(test$`Witless Bay`$id))
length(unique(test$`Witless Bay`$id_year))

