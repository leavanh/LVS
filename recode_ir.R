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

