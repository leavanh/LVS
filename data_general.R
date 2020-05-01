## Diese Datei erzeugt data_general
# Dem Szenario 1 (20% Unterschätzung) folgend werden Messungen generiert

## Messungen erzeugen

n_messungen <- 0.25 * nrow(data_noNA) # Anzahl der zu erzeugenden Messungen
# diese Zahl wird abgerundet (in der Schleife)

time_intervall <- interval( # alle möglichen Uhrzeiten
  as.POSIXct("1899-12-31 04:00:00", tz = "MESZ"),
  as.POSIXct("1900-01-01 03:59:59", tz = "MESZ")
)

all_dates <- date_data_noNA$date # alle möglichen Tage

# neuen data.frame erschaffen (am Ende Erste Zeile löschen)

neue_messungen <- data.frame(id = NA, lvs = FALSE, position = NA,
                     time = as.POSIXct("1899-12-31 00:00:00", tz = "MESZ"), 
                     date = as.POSIXct("1899-12-31", tz = "MESZ"))

for(i in 1:n_messungen) { # neue Messungen generieren
  messung_time <- as.POSIXct(sample(time_intervall, 1), tz = "MESZ", 
                             origin = "1899-12-31 04:00:00") # erzeugen
  messung_time <- as.POSIXct( # alle Sekunden auf 0
    trunc(messung_time, units = "mins"))
  messung_date <- sample(all_dates, 1)
  messung <- data.frame(id = NA, lvs = FALSE, position = NA, time = messung_time,
               date = messung_date)
  neue_messungen <- rbind(neue_messungen, messung)
  i = i + 1
}

neue_messungen <- neue_messungen[-1,] # erste Zeile löschen

## Messungen hinzufügen

data_general <- neue_messungen %>%
  full_join(date_data_noNA, by = "date") %>%
  mutate(lvs_true_min = 0,
         lvs_false_min = 0,
         count_people_min = 0,
         ratio_min = 0) %>%
  rbind(data_noNA)

# neue Summen berechnen

data_general <- data_general %>%
  group_by(date) %>%
  mutate(lvs_true = sum(lvs == TRUE), # Anzahl Beaconmessung
         lvs_false = sum(lvs == FALSE), # Anzahl Infrarotmessungen
         count_people = lvs_true + lvs_false, # Anzahl Leute insg.
         ratio = lvs_true/(count_people)) %>%
  ungroup() %>%
  group_by(date, hour(time), minute(time)) %>%
  # neu berechnen
  mutate(lvs_true_min = sum(lvs == TRUE), # Anzahl mit LVS
         lvs_false_min = sum(lvs == FALSE), # Anzahl ohne LVS
         count_people_min = lvs_true_min + lvs_false_min, # Anzahl
         # Leute insg.
         ratio_min = lvs_true_min/(count_people_min)) %>% # Ratio
  ungroup()


data_general <- select(data_general, - c("hour(time)", "minute(time)")) # unnötige Variablen

date_data_general <- distinct(subset(data_general, 
                              select = -c(lvs, time, position, id, lvs_true_min,
                                          lvs_false_min, count_people_min,
                                          ratio_min)))

min_data_general <- distinct(subset(data_general, 
                             select = -c(lvs, position, id)))
