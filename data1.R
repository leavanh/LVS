## Diese Datei erzeugt data1
# Dem Szenario 1 (20% Unterschätzung) folgend werden Messungen generiert

## Messungen erzeugen

n_messungen <- 0.25 * nrow(data_noNA) # Anzahl der zu erzeugenden Messungen

time_intervall <- interval( # alle möglichen Uhrzeiten
  as.POSIXct("1899-12-31 04:00:00", tz = "UTC"),
  as.POSIXct("1900-01-01 03:59:59", tz = "UTC")
)

all_dates <- date_data_noNA$date # alle möglichen Tage

# neuen data.frame erschaffen (am Ende Erste Zeile löschen)

neue_messungen <- data.frame(id = NA, lvs = FALSE, position = NA,
                     time = as.POSIXct("1899-12-31 00:00:00", tz = "UTC"), 
                     date = as.POSIXct("1899-12-31", tz = "UTC"))

for(i in 1:n_messungen) { # neue Messungen generieren
  messung_time <- as.POSIXct(sample(time_intervall, 1), tz = "UTC", 
                             origin = "1899-12-31 04:00:00")
  messung_date <- sample(all_dates, 1)
  messung <- data.frame(id = NA, lvs = FALSE, position = NA, time = messung_time,
               date = messung_date)
  neue_messungen <- rbind(neue_messungen, messung)
  i = i + 1
}

neue_messungen <- neue_messungen[-1,] # erste Zeile löschen

## Messungen hinzufügen

data1 <- neue_messungen %>%
  full_join(date_data_noNA, by = "date") %>%
  mutate(lvs_true_min = 0,
         lvs_false_min = 0,
         count_people_min = 0,
         ratio_min = 0) %>%
  rbind(data_noNA)

# neue Summen berechnen

#PROBLEME berechnen und minuten runden

data1 <- data1 %>%
  group_by(date, min(time)) %>%
  # neu berechnen
  mutate(lvs_true_min = sum(lvs == TRUE), # Anzahl mit LVS
         lvs_false_min = sum(lvs == FALSE), # Anzahl ohne LVS
         count_people_min = lvs_true_min + lvs_false_min, # Anzahl
         # Leute insg.
         ratio_min = lvs_true_min/(count_people_min)) %>% # Ratio
  ungroup() %>%
  group_by(date) %>%
  mutate(lvs_true = sum(lvs == TRUE), # Anzahl Beaconmessung
         lvs_false = sum(lvs == FALSE), # Anzahl Infrarotmessungen
         count_people = lvs_true + lvs_false, # Anzahl Leute insg.
         ratio = lvs_true/(count_people)) %>%
  ungroup()

data1 <- select(data1, - "min(time)") # unnötige Variable löschen

date_data1 <- distinct(subset(data1, 
                              select = -c(lvs, time, position, id, lvs_true_min,
                                          lvs_false_min, count_people_min,
                                          ratio_min)))

min_data1 <- distinct(subset(data1, 
                             select = -c(lvs, position, id)))
