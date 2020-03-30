

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
               SG_gesamt = Erfasst_SG + Nicht_erfasst_SG,
               aK_gesamt = Erfasst_aK + Nicht_erfasst_aK,
               gesamt = erfasst + nicht_erfasst)
z_28 <- mutate(zaehlung_lvs_check_28_02, erfasst = Erfasst_SG + Erfasst_aK, 
               nicht_erfasst = Nicht_erfasst_SG + Nicht_erfasst_aK,
               SG_gesamt = Erfasst_SG + Nicht_erfasst_SG,
               aK_gesamt = Erfasst_aK + Nicht_erfasst_aK,
               gesamt = erfasst + nicht_erfasst)



## Summen der einzelnen Reihen

summen_27 <- apply(z_27[,-1], 2, sum)
summen_28 <- apply(z_28[,-1], 2, sum)

# als separate Tabelle (für den 28.)

sums_27 <- data.frame(cbind(names(summen_27), as.numeric(summen_27)))
names(sums_27) <- c("type", "sum")
sums_27$sum <- as.numeric(as.character(sums_27$sum))

sums_28 <- data.frame(cbind(names(summen_28), as.numeric(summen_28)))
names(sums_28) <- c("type", "sum")
sums_28$sum <- as.numeric(as.character(sums_28$sum))

# alle Summen geplottet

ggplot(sums_27, aes(x = type, y = sum)) +
  geom_bar(stat = "identity")

ggplot(sums_28, aes(x = type, y = sum)) +
  geom_bar(stat = "identity")



## Erfassungen und Nicht-Erfassungen Gesamt nebeinanderstellen nach Uhrzeit

z_27_Erfassung <- z_27 %>% gather(key = "Erfassung", value = "Anzahl", c(erfasst, nicht_erfasst))

ggplot(data = z_27_Erfassung, aes(x = time, y = Anzahl, fill = Erfassung)) +
  geom_bar(stat = "identity", position = "dodge")

z_28_Erfassung <- z_28 %>% gather(key = "Erfassung", value = "Anzahl", c(erfasst, nicht_erfasst))

ggplot(data = z_28_Erfassung, aes(x = time, y = Anzahl, fill = Erfassung)) +
  geom_bar(stat = "identity", position = "dodge")
  # + scale_x_time(limits = c(as.POSIXct("11:00:00", tz = "UTC"), as.POSIXct("17:30:00", tz = "UTC")))

## Skitourengänger und andere Kontakte Gesamt nach Uhrzeit

ggplot(data = z_27, aes(x = time)) +
  geom_bar(aes(y = SG_gesamt), stat = "identity", position = "dodge", fill = "blue") +
  geom_bar(aes(y = aK_gesamt), stat = "identity", fill = "green", position = "dodge")


