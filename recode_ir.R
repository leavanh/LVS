## Data.frame LVS = TRUE erstellen

# enthält alle Messungen von Personen mit LVS-Gerät und soll nachher mit 
# anderem data.frame (Personen ohne Gerät) zusammengeführt werden

lvs_true_data <- data %>%
                  # nur die Beacon (also LVS) Fälle
                  subset(type == "Beacon") %>%
                  # unnötige Spalten löschen
                  select(date, time) %>%
                  # neue Spalte hinzufügen
                  mutate(lvs = TRUE)

## Data.frame LVS = FALSE erstellen

# enthält anfangs alle Infrarotmessungen

lvs_false_data <- data %>%
                    # nur die Infrarot Fälle
                  subset(type == "Infrared") %>%
                  # unnötige Spalten löschen
                  select(date, time) %>%
                  # neue Spalte hinzufügen
                  mutate(lvs = FALSE) %>%
                  # id hinzufügen um später einfach löschen zu können
                  rowid_to_column("ID")

# Falls eine Beaconmessung zugeordnet werden kann, soll die Infrarotmessung
# entfernt werden

## Schleife

# läuft alle Reihen von lvs_true_data ab und prüft, ob eine korrespondierende
# Infrarotmessung in lvs_false_data vorliegt. Falls ja, wird diese gelöscht.

for (i in 1:nrow(lvs_true_data)) {
  # Zeit der Beobachtung erhalten
  time_i <- lvs_true_data[[i, "time"]]
  # Datum der Beobachtung erhalten
  date_i <- lvs_true_data[[i, "date"]]
  # Zeitintervall definieren (5 Minuten drumherum)
  interval_i <- interval(time_i - minutes(5), time_i + minutes(5))
  # die passenden Infrarotmessungen raussuchen
  infrared_i <- subset(lvs_false_data,
                       time %within% interval_i & date == date_i)
  # die Infrarotmessung finden, die am nächsten an der Beaconmessung ist
  # falls es keine Infrarotmessung gibt die ID auf 0 setzen
  if(nrow(infrared_i) = 0) {
    id_i <- 0
  } else {
    id_i <- infrared_i[which.min(abs(time_i - infrared_i$time)),]$ID
  }
  # falls es keine Infrarotmessung gibt die ID auf 0 setzen
  # aus lvs_false_data entfernen
  lvs_false_data <- lvs_false_data[-id_i,]
  i <- i + 1
}


