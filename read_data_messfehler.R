

## Daten aus 19/20 einlesen

# Zuerst die Checkpointmessungen aus den .txt-Dateien in Exceldatei namens 
# "Date_1920.xlsx" kopiert

# Checkpointmessungen aus der Exceldatei einlesen

all_checkpoint_stats_1920 <- 
  rbind(read_excel("Daten/Daten_1920.xlsx", sheet = "1920_01", 
                   col_names = c("type", "date", "time")), 
        read_excel("Daten/Daten_1920.xlsx", sheet = "1920_02", 
                   col_names = c("type", "date", "time")))

# Reihen, die nicht Beacon oder Infrared enthalten, löschen

all_checkpoint_stats_1920 <- subset(all_checkpoint_stats_1920,
                               type %in% c("Beacon", "Infrared"))

# Zeit und Datum in passendes Format umwandeln

all_checkpoint_stats_1920$date <- as.POSIXct(all_checkpoint_stats_1920$date, 
                                             format = "%d %B %Y", tz = "GMT")
all_checkpoint_stats_1920$time <- as.POSIXct(all_checkpoint_stats_1920$time, 
                                             format = "%d %B %Y", tz = "GMT")


# manuelle Zählung einlesen

zaehlung_lvs_check_27 <- read_excel("Daten/Zaehlung_LVS-check.xlsx", 
                                       sheet = 1, skip = 1,
                                       col_names = c("time", "erfasst_SG", 
                                                     "nicht_erfasst_SG", 
                                                     "erfasst_aK", 
                                                     "nicht_erfasst_aK"))
zaehlung_lvs_check_28 <- read_excel("Daten/Zaehlung_LVS-check.xlsx", 
                                       sheet = 2, skip = 1,
                                       col_names = c("time", "erfasst_SG", 
                                                     "nicht_erfasst_SG", 
                                                     "erfasst_aK", 
                                                     "nicht_erfasst_aK"))

# Checkpointmessungen filtern zu den Tagen der manuellen Zählung

checkpoint_stats_27 <- filter(all_checkpoint_stats_1920, 
                                 date == as.POSIXct("2020-02-27", tz = "GMT"))
checkpoint_stats_28 <- filter(all_checkpoint_stats_1920, 
                                 date == as.POSIXct("2020-02-28", tz = "GMT"))


## Checkpointmessungen filtern zu den Zeiträumen der manuellen Messungen

checkpoint_stats_27 <- subset(checkpoint_stats_27, 
                  time >= as.POSIXct('1899-12-31 10:30', tz = "GMT") &
                    time <= as.POSIXct('1899-12-31 12:58', tz = "GMT"))

checkpoint_stats_28 <- rbind(subset(checkpoint_stats_28, 
                        time >= as.POSIXct('1899-12-31 10:38', tz = "GMT") &
                          time <= as.POSIXct('1899-12-31 12:27', tz = "GMT")),
                 subset(checkpoint_stats_28, 
                        time >= as.POSIXct('1899-12-31 14:39', tz = "GMT") &
                          time <= as.POSIXct('1899-12-31 16:51', tz = "GMT")))

# Checkpointmessungen von beiden Tagen zusammenlegen

checkpoint_stats <- rbind(checkpoint_stats_27, checkpoint_stats_28)


# NAs zu 0 machen

zaehlung_lvs_check_27[is.na(zaehlung_lvs_check_27)] <- as.character(0)
zaehlung_lvs_check_28[is.na(zaehlung_lvs_check_28)] <- as.character(0)

# Zählungen zu numerischen Vektoren machen
# (Warnmeldung wird im nächsten Schritt gelöst)

zaehlung_lvs_check_27[,2:5] <- apply(zaehlung_lvs_check_27[,2:5], c(2), as.numeric)
zaehlung_lvs_check_28[,2:5] <- apply(zaehlung_lvs_check_28[,2:5], c(2), as.numeric)

# restliche NAs entfernen

zaehlung_lvs_check_27 <- na.omit(zaehlung_lvs_check_27)
zaehlung_lvs_check_28 <- na.omit(zaehlung_lvs_check_28)

# zusätzliche Variablen berechnen & Namen der Tabellen zur Vereinfachung 
# abkürzen

zlg_27 <- mutate(zaehlung_lvs_check_27, erfasst = erfasst_SG + erfasst_aK, 
               nicht_erfasst = nicht_erfasst_SG + nicht_erfasst_aK,
               SG_gesamt = erfasst_SG + nicht_erfasst_SG,
               aK_gesamt = erfasst_aK + nicht_erfasst_aK,
               gesamt = erfasst + nicht_erfasst)
zlg_28 <- mutate(zaehlung_lvs_check_28, erfasst = erfasst_SG + erfasst_aK, 
               nicht_erfasst = nicht_erfasst_SG + nicht_erfasst_aK,
               SG_gesamt = erfasst_SG + nicht_erfasst_SG,
               aK_gesamt = erfasst_aK + nicht_erfasst_aK,
               gesamt = erfasst + nicht_erfasst)

# Aufteilen der Zählung am 28.02. in die zwei unterschiedlichen Zeiträume

zlg_28_01 <- filter(zlg_28, time < as.POSIXct('1899-12-31 12:27', tz = "GMT"))

zlg_28_02 <- filter(zlg_28, time > as.POSIXct('1899-12-31 14:39', tz = "GMT") &
                            time < as.POSIXct('1899-12-31 16:51', tz = "GMT"))

# Zusammenlegung beider Tage

zlg_beide <- rbind(mutate(zlg_27, date = as.POSIXct("2020-02-27 00:00", 
                                                    tz = "GMT")), 
                   mutate(zlg_28, date = as.POSIXct("2020-02-28 00:00", 
                                                    tz = "GMT")))


# Zählungen des Checkpoints nach Minute gruppieren (für die Gruppenanalyse)

checkpoint_stats_grouped <- checkpoint_stats %>% group_by(time) %>%
                              summarise(erfasst = n())

# Zeitlimit der manuellen Messungen für die Plots später

time_limits <- as.POSIXct(strptime(c("1899-12-31 10:00:00",
                                     "1899-12-31 17:00:00"), 
                                   format = "%Y-%m-%d %H:%M", tz = "GMT"))



# Die Reihen der Tabelle der manuellen Zählugen aufsummieren und als eigene
# Ergebnistabelle speichern

zlg_beide_sums <- data.frame(type = colnames(zlg_beide)[-c(1,11)], 
                             sum = apply(zlg_beide[,-c(1,11)], 2, sum), 
                             row.names = NULL)
zlg_beide_sums$type <- as.character(zlg_beide_sums$type)
zlg_beide_sums$sum <- as.numeric(zlg_beide_sums$sum)

zlg_beide_sums <- rbind(zlg_beide_sums, 
                        c("checkpoint", 
                          as.numeric(sum(checkpoint_stats_grouped$erfasst))))

zlg_beide_sums$type <- as.factor(zlg_beide_sums$type)
zlg_beide_sums$sum <- as.numeric(zlg_beide_sums$sum)

# Die vorherige Ergebnistabelle noch weiter unterteilen

zlg_beide_sums <- 
  mutate(zlg_beide_sums, 
         erfassung = as.factor(c("erfasst", "nicht_erfasst", "erfasst", 
                                 "nicht_erfasst", "erfasst", "nicht_erfasst", 
                                 "beide", "beide", "beide", "beide")),
         kontakt = c("SG", "SG", "aK", "aK", "beide", "beide", "SG", "aK",
                     "beide", "beide"))


# Manuelle Zählungen je Zeitraum gruppiert in 3 Minuten für übersichtlichere
# Darstellung im Plot später

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

# Erfassung je Gruppengrösse berechnen

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







