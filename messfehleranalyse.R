

# Checkpoint-Stats nach type

table(checkpoint_stats_27_02$type)
table(checkpoint_stats_28_02$type)

# NAs zu 0 machen

zaehlung_lvs_check_27_02[is.na(zaehlung_lvs_check_27_02)] <- 0
zaehlung_lvs_check_28_02[is.na(zaehlung_lvs_check_28_02)] <- 0

# Zählungen zu numerischen Vektoren machen

zaehlung_lvs_check_27_02[, 2:5] <- as.numeric(unlist(zaehlung_lvs_check_27_02[, 2:5]))
zaehlung_lvs_check_28_02[, 2:5] <- as.numeric(unlist(zaehlung_lvs_check_28_02[, 2:5]))

# restliche NAs entfernen

zaehlung_lvs_check_27_02 <- na.omit(zaehlung_lvs_check_27_02)
zaehlung_lvs_check_28_02 <- na.omit(zaehlung_lvs_check_28_02)

# Hilfstabellen mit kürzerem Namen

z_27 <- mutate(zaehlung_lvs_check_27_02, erfasst = Erfasst_SG + Erfasst_aK, 
               nicht_erfasst = Nicht_erfasst_SG + Nicht_erfasst_aK,
               gesamt = erfasst + nicht_erfasst)
z_28 <- mutate(zaehlung_lvs_check_28_02, erfasst = Erfasst_SG + Erfasst_aK, 
               nicht_erfasst = Nicht_erfasst_SG + Nicht_erfasst_aK,
               gesamt = erfasst + nicht_erfasst)



summen_28 <- apply(z_28[,-1], 2, sum)
summen_27 <- apply(z_27[,-1], 2, sum)

sums_28 <- as.data.frame(cbind(names(summen_28), as.vector(summen_28)))
names(sums_28) <- c("type", "sum")

ggplot(sums_28, aes(x = type, y = sum)) +
  geom_bar(stat = "identity")



ggplot(data = z_28, aes(x = time)) +
  geom_col(aes(y = erfasst), color = "green", position = "dodge", stat = "identity") +
    geom_col(aes(y = nicht_erfasst), color = "red", position = "dodge", stat = "identity")

ggplot(data = z_28, aes(x = time, y = nicht_erfasst)) +
  geom_bar(stat = "identity")

?geom_bar
