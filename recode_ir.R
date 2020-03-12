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
                  mutate(lvs = FALSE)

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
  infrared_i
}


