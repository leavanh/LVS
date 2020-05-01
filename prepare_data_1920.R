

## Daten aus 19/20 einlesen

library(readxl)
library(tidyverse)

# dazu aus den beiden .txt-Files rauskopiert und in Exceldatei eingefügt

# welcher war Nord, welcher Süd?

data_1920_01 <- read_excel("Daten/Daten_1920.xlsx", sheet = "1920_01", col_names = c("type", "date", "time"))
data_1920_02 <- read_excel("Daten/Daten_1920.xlsx", sheet = "1920_02", col_names = c("type", "date", "time"))

data_1920 <- rbind(data_1920_01, data_1920_02)

# Reihen, die nicht Beacon oder Infrared enthalten, rauslöschen

all_checkpoint_stats_1920 <- subset(data_1920,
                               type %in% c("Beacon", "Infrared"))

# Zeit und Datum in passendes Foramt umwandeln

all_checkpoint_stats_1920$date <- as.POSIXct(all_checkpoint_stats_1920$date, format = "%d %B %Y", tz = "UTC")
all_checkpoint_stats_1920$time <- as.POSIXct(all_checkpoint_stats_1920$time, format = "%d %B %Y", tz = "UTC")


# manuelle Zählung einlesen

zaehlung_lvs_check_27_02 <- read_excel("Daten/Zaehlung_LVS-check.xlsx", sheet = 1, skip = 1,
                                       col_names = c("time", "Erfasst_SG", "Nicht_erfasst_SG", "Erfasst_aK", "Nicht_erfasst_aK"))
zaehlung_lvs_check_28_02 <- read_excel("Daten/Zaehlung_LVS-check.xlsx", sheet = 2, skip = 1,
                                       col_names = c("time", "Erfasst_SG", "Nicht_erfasst_SG", "Erfasst_aK", "Nicht_erfasst_aK"))

# Checkpoint-Daten an den Tagen der manuellen Zählung

checkpoint_stats_27_02 <- filter(all_checkpoint_stats_1920, date == as.POSIXct("2020-02-27", tz = "UTC"))
checkpoint_stats_28_02 <- filter(all_checkpoint_stats_1920, date == as.POSIXct("2020-02-28", tz = "UTC"))


## Checkpointmessungen im Zeitraum der manuellen Messungen filtern

ckpt_27 <- subset(checkpoint_stats_27_02, time >= as.POSIXct('1899-12-31 10:30', tz = "UTC") &
                    time <= as.POSIXct('1899-12-31 12:58', tz = "UTC"))

ckpt_28 <- rbind(subset(checkpoint_stats_28_02, time >= as.POSIXct('1899-12-31 10:38', tz = "UTC") &
                          time <= as.POSIXct('1899-12-31 12:27', tz = "UTC")),
                 subset(checkpoint_stats_28_02, time >= as.POSIXct('1899-12-31 14:39', tz = "UTC") &
                          time <= as.POSIXct('1899-12-31 16:51', tz = "UTC")))


# Checkpoint-Stats nach type

table(ckpt_27$type)
table(ckpt_28$type)

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

