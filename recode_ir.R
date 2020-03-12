# Diese R-Datei erstellt die RDS-Datei lvs_data.RDS

# zueinander gehörende Beacon- und Infrarotmessungen werden erkannt
# es wird ein Datensatz erstellt, der nicht mehr die Art der Messung angibt,
# sondern ob eine Person mit lvs-Gerät registriert wurde

## Pakete laden

if (!require("lubridate")) install.packages("lubridate")
library("lubridate")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")

## Data.frame LVS = TRUE erstellen

# enthält alle Messungen von Personen mit LVS-Gerät und soll nachher mit 
# anderem data.frame (Personen ohne Gerät) zusammengeführt werden

lvs_true_data <- data %>%
  # nur die Beacon (also LVS) Fälle
  subset(type == "Beacon") %>%
  # unnötige Spalten löschen
  select(id, date, time, position) %>%
  # neue Spalte hinzufügen
  mutate(lvs = TRUE)

## Data.frame LVS = FALSE erstellen

# enthält anfangs alle Infrarotmessungen

lvs_false_data <- data %>%
  # nur die Infrarot Fälle
  subset(type == "Infrared") %>%
  # unnötige Spalten löschen
  select(id, date, time, position) %>%
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
  # Position der Beobachtung erhalten
  position_i <- lvs_true_data[[i, "position"]]
  # Zeitintervall definieren (5 Minuten drumherum)
  interval_i <- interval(time_i - minutes(5), time_i + minutes(5))
  # die passenden Infrarotmessungen raussuchen
  infrared_i <- subset(lvs_false_data,
                       time %within% interval_i & 
                         date == date_i & 
                         position == position_i)
  # prüfen, ob überhaupt Infrarotmessungen vorhanden sind
  if(nrow(infrared_i) != 0){
    # die Infrarotmessung finden, die am nächsten an der Beaconmessung ist
    id_i <- infrared_i[which.min(abs(time_i - infrared_i$time)),]$id
    # aus lvs_false_data entfernen
    lvs_false_data <- lvs_false_data[-id_i,]
  }
  # mit der nächsten Beaconmessung weitermachen
  i <- i + 1
}

## Zu einem Datensatz zusammenführen

lvs_data <- lvs_false_data %>%
              # mit lvs_true_data verbinden
              rbind(lvs_true_data) %>%
              # mit date_data zu einem großen Datensatz verbinden
              full_join(date_data, by = "date")
## als RDS speichern

saveRDS(lvs_data, file = "Daten/lvs_data.RDS")
