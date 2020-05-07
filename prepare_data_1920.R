

## Daten aus 19/20 einlesen

library(readxl)
library(tidyverse)
library(gridExtra)

# dazu aus den beiden .txt-Files rauskopiert und in Exceldatei eingefügt

# welcher war Nord, welcher Süd?

data_1920_01 <- read_excel("Daten/Daten_1920.xlsx", sheet = "1920_01", 
                           col_names = c("type", "date", "time"))
data_1920_02 <- read_excel("Daten/Daten_1920.xlsx", sheet = "1920_02", 
                           col_names = c("type", "date", "time"))

data_1920 <- rbind(data_1920_01, data_1920_02)

# Reihen, die nicht Beacon oder Infrared enthalten, rauslöschen

all_checkpoint_stats_1920 <- subset(data_1920,
                               type %in% c("Beacon", "Infrared"))

# Zeit und Datum in passendes Foramt umwandeln

all_checkpoint_stats_1920$date <- as.POSIXct(all_checkpoint_stats_1920$date, 
                                             format = "%d %B %Y", tz = "UTC")
all_checkpoint_stats_1920$time <- as.POSIXct(all_checkpoint_stats_1920$time, 
                                             format = "%d %B %Y", tz = "UTC")


# manuelle Zählung einlesen

zaehlung_lvs_check_27_02 <- read_excel("Daten/Zaehlung_LVS-check.xlsx", 
                                       sheet = 1, skip = 1,
                                       col_names = c("time", "Erfasst_SG", 
                                                     "Nicht_erfasst_SG", 
                                                     "Erfasst_aK", 
                                                     "Nicht_erfasst_aK"))
zaehlung_lvs_check_28_02 <- read_excel("Daten/Zaehlung_LVS-check.xlsx", 
                                       sheet = 2, skip = 1,
                                       col_names = c("time", "Erfasst_SG", 
                                                     "Nicht_erfasst_SG", 
                                                     "Erfasst_aK", 
                                                     "Nicht_erfasst_aK"))

# Checkpoint-Daten an den Tagen der manuellen Zählung

checkpoint_stats_27_02 <- filter(all_checkpoint_stats_1920, 
                                 date == as.POSIXct("2020-02-27", tz = "UTC"))
checkpoint_stats_28_02 <- filter(all_checkpoint_stats_1920, 
                                 date == as.POSIXct("2020-02-28", tz = "UTC"))


## Checkpointmessungen im Zeitraum der manuellen Messungen filtern

ckpt_27 <- subset(checkpoint_stats_27_02, 
                  time >= as.POSIXct('1899-12-31 10:30', tz = "UTC") &
                    time <= as.POSIXct('1899-12-31 12:58', tz = "UTC"))

ckpt_28 <- rbind(subset(checkpoint_stats_28_02, 
                        time >= as.POSIXct('1899-12-31 10:38', tz = "UTC") &
                          time <= as.POSIXct('1899-12-31 12:27', tz = "UTC")),
                 subset(checkpoint_stats_28_02, 
                        time >= as.POSIXct('1899-12-31 14:39', tz = "UTC") &
                          time <= as.POSIXct('1899-12-31 16:51', tz = "UTC")))

ckpt_beide <- rbind(ckpt_27, ckpt_28)





# Checkpoint-Stats nach type

table(ckpt_27$type)
table(ckpt_28$type)

# NAs zu 0 machen

zaehlung_lvs_check_27_02[is.na(zaehlung_lvs_check_27_02)] <- 0
zaehlung_lvs_check_28_02[is.na(zaehlung_lvs_check_28_02)] <- 0

# Zählungen zu numerischen Vektoren machen

zaehlung_lvs_check_27_02[, 2:5] <- 
  as.numeric(unlist(zaehlung_lvs_check_27_02[, 2:5]))
zaehlung_lvs_check_28_02[, 2:5] <- 
  as.numeric(unlist(zaehlung_lvs_check_28_02[, 2:5]))

# restliche NAs entfernen

zaehlung_lvs_check_27_02 <- na.omit(zaehlung_lvs_check_27_02)
zaehlung_lvs_check_28_02 <- na.omit(zaehlung_lvs_check_28_02)

# Hilfstabellen mit kürzerem Namen

zlg_27 <- mutate(zaehlung_lvs_check_27_02, erfasst = Erfasst_SG + Erfasst_aK, 
               nicht_erfasst = Nicht_erfasst_SG + Nicht_erfasst_aK,
               SG_gesamt = Erfasst_SG + Nicht_erfasst_SG,
               aK_gesamt = Erfasst_aK + Nicht_erfasst_aK,
               gesamt = erfasst + nicht_erfasst)
zlg_28 <- mutate(zaehlung_lvs_check_28_02, erfasst = Erfasst_SG + Erfasst_aK, 
               nicht_erfasst = Nicht_erfasst_SG + Nicht_erfasst_aK,
               SG_gesamt = Erfasst_SG + Nicht_erfasst_SG,
               aK_gesamt = Erfasst_aK + Nicht_erfasst_aK,
               gesamt = erfasst + nicht_erfasst)

zlg_28_01 <- filter(zlg_28, time < as.POSIXct('1899-12-31 12:27', tz = "UTC"))

zlg_28_02 <- filter(zlg_28, time > as.POSIXct('1899-12-31 14:39', tz = "UTC") &
                            time < as.POSIXct('1899-12-31 16:51', tz = "UTC"))

# Zusammenlegung beider Tage zu einem Datensatz

zlg_beide <- rbind(mutate(zlg_27, date = "27.02."), 
                   mutate(zlg_28, date = "28.02."))


# Zählungen des Checkpoints nach Minute

zlg_ckpt_27 <- ckpt_27 %>% group_by(time) %>%
                summarise(erfasst = n())

zlg_ckpt_28 <- ckpt_28 %>% group_by(time) %>%
                summarise(erfasst = n())

zlg_ckpt_beide <- rbind(mutate(zlg_ckpt_27, date = "27.02."), 
                   mutate(zlg_ckpt_28, date = "28.02."))


# Zeitlimit der manuellen Messungen für die Plots später

time_limits <- as.POSIXct(strptime(c("1899-12-31 10:00:00","1899-12-31 17:00:00"), 
                                   format = "%Y-%m-%d %H:%M", tz = "UTC"))



## Summen der einzelnen Reihen in der manuellen Zählung: 
# SG, aK, erfasst, nicht erfasst

(summen_27 <- apply(zlg_27[,-1], 2, sum))
(summen_28 <- apply(zlg_28[,-1], 2, sum))

# Summen als separate Tabelle

sums_27 <- data.frame(cbind(names(summen_27), as.numeric(summen_27)))
names(sums_27) <- c("type", "sum")
sums_27$sum <- as.numeric(as.character(sums_27$sum))

sums_28 <- data.frame(cbind(names(summen_28), as.numeric(summen_28)))
names(sums_28) <- c("type", "sum")
sums_28$sum <- as.numeric(as.character(sums_28$sum))



# gruppiert in 3-Minuten-Intervalle

zlg_27_breaks_3min <- mutate(zlg_27, breaks = cut(zlg_27$time, 
                                                  breaks = "3 min"))
zlg_27_grouped_3min <- zlg_27_breaks_3min %>% group_by(breaks) %>% 
  summarise(
    erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
  )

zlg_28_breaks_3min <- mutate(zlg_28, breaks = cut(zlg_28$time, 
                                                  breaks = "3 min"))
zlg_28_grouped_3min <- zlg_28_breaks_3min %>% group_by(breaks) %>% 
  summarise(
    erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
  )

zlg_28_01_breaks_3min <- mutate(zlg_28_01, breaks = cut(zlg_28_01$time, 
                                                        breaks = "3 min"))
zlg_28_01_grouped_3min <- zlg_28_01_breaks_3min %>% group_by(breaks) %>% 
  summarise(
    erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
  )

zlg_28_02_breaks_3min <- mutate(zlg_28_02, breaks = cut(zlg_28_02$time, 
                                                        breaks = "3 min"))
zlg_28_02_grouped_3min <- zlg_28_02_breaks_3min %>% group_by(breaks) %>% 
  summarise(
    erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
  )


zlg_beide_breaks_3min <- mutate(zlg_beide, breaks = cut(zlg_beide$time, 
                                                        breaks = "3 min"))
zlg_beide_grouped_3min <- zlg_beide_breaks_3min %>% group_by(breaks) %>% 
  summarise(
    erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
  )


zlg_27_grouped_3min_erf <- gather(zlg_27_grouped_3min, 
                                  key = "erfassung", value = "sum", 
                                  erfasst, nicht_erfasst)

zlg_28_01_grouped_3min_erf <- gather(zlg_28_01_grouped_3min, 
                                     key = "erfassung", value = "sum", 
                                     erfasst, nicht_erfasst)

zlg_28_02_grouped_3min_erf <- gather(zlg_28_02_grouped_3min, 
                                     key = "erfassung", value = "sum", 
                                     erfasst, nicht_erfasst)






## alle manuell eingetragenen Überschätzungswerte aus den Studentendaten raus:

# neuer, bereinigter Datensatz

zlg_beide_bereinigt <- zlg_beide

zlg_ckpt_beide_bereinigt <- zlg_ckpt_beide


# spielende Kinder aus manueller Zählung entfernen
# Zeilen mit Kinder auswählen und Werte auf Null setzen

zlg_beide_bereinigt[135:140, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[c(33,34), -c(1,3)] <- 0

# Hund entfernen

zlg_beide_bereinigt[213, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[61, -c(1,3)] <- 0

# "Gerät erkennt erst "LVS erst nicht erkannt, dann doch"
# -> passt nicht zu den Daten vom Checkpoint


# Gerät piepst drei mal bei zwei Personen

zlg_beide_bereinigt[448, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[84, -c(1,3)] <- 2


# Gerät piepst durchgängig, da eine Person sich Schuhe auszieht

zlg_beide_bereinigt[482:484, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[94:97, -c(1,3)] <- 0

# Gerät piepst zwei mal, Person macht vor Gerät Foto

zlg_beide_bereinigt[488, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[98, -c(1,3)] <- 0



## Über gesamten Zeitraum, bereinigter Datensatz: 

# Passenden Dataframe mit Summen für Plot erstellen

zlg_beide_bereinigt_summen <- 
  data.frame(type = colnames(zlg_beide_bereinigt)[-c(1,11)], 
             sum = apply(zlg_beide_bereinigt[,-c(1,11)], 2, sum), 
             row.names = NULL)
zlg_beide_bereinigt_summen$type <- as.character(zlg_beide_bereinigt_summen$type)

zlg_beide_bereinigt_summen <- rbind(zlg_beide_bereinigt_summen, 
          c("checkpoint", as.numeric(sum(zlg_ckpt_beide_bereinigt$erfasst))))

zlg_beide_bereinigt_summen$type <- as.factor(zlg_beide_bereinigt_summen$type)
zlg_beide_bereinigt_summen$sum <- as.numeric(zlg_beide_bereinigt_summen$sum)

zlg_beide_bereinigt_summen



# Erfassung je Gruppe berechnen (für kompletten Datensatz)

erfasst_je_gruppe <- zlg_beide %>% group_by(gesamt) %>% 
  summarise(anzahl = sum(erfasst), 
            mittel = mean(erfasst),
            häufigkeit = n()) %>% 
  mutate(type = "erfasst")

nicht_erfasst_je_gruppe <- zlg_beide %>% group_by(gesamt) %>% 
  summarise(anzahl = sum(nicht_erfasst),
            mittel = mean(nicht_erfasst),
            häufigkeit = n()) %>% 
  mutate(type = "nicht_erfasst")

erfassung_je_gruppe <- rbind(erfasst_je_gruppe, nicht_erfasst_je_gruppe) %>%
  rename(grösse = gesamt)

erfassung_je_gruppe


# Dataframe mit Summen für alle Daten erstellen

zlg_beide_summen <- data.frame(type = colnames(zlg_beide)[-c(1,11)], 
                               sum = apply(zlg_beide[,-c(1,11)], 2, sum), 
                               row.names = NULL)
zlg_beide_summen$type <- as.character(zlg_beide_summen$type)

zlg_beide_summen <- rbind(zlg_beide_summen, 
                          c("checkpoint", as.numeric(sum(zlg_ckpt_beide$erfasst))))

zlg_beide_summen$type <- as.factor(zlg_beide_summen$type)
zlg_beide_summen$sum <- as.numeric(zlg_beide_summen$sum)

zlg_beide_summen




