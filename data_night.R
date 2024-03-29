## Diese Datei erzeugt data_night
# Dem Szenario 3 (Wildtiere) folgend werden Messungen gelöscht

## Pakete laden

library("tidyverse")
library("lubridate")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")
min_data <- readRDS(file = "Daten/min_data.RDS")

# NAs (fehlende Werte) rauswerfen

date_data_noNA <- date_data[!is.na(date_data$count_people),]
data_noNA <- data[!is.na(data$lvs),]
min_data_noNA <- min_data[!is.na(min_data$count_people_min),]

# Da die Daten zufällig erzeugt werden benutzen wir einen seed
# so kommt bei jedem Durchlauf das selbe Ergebnis raus

set.seed(42)


## data_night erzeugen (ohne NAs da wir diese Tage eh nicht brauchen)

data_night <- data_noNA

## Messungen löschen

perc <- 0.5 # Prozent die gelöscht werden sollen

# welche kommen in Frage?

time_intervall <- interval( # In diesem Intervall ist nie die Sonne zu sehen
  as.POSIXct("1899-12-31 05:29:00", tz = "GMT"),
  as.POSIXct("1899-12-31 18:58:00", tz = "GMT")
)

nacht_messungen <- subset(data_night, 
                          !(time %within% time_intervall) # nachts
                          & lvs == FALSE) # nur Infrarotmessungen
delete_rows <- sample(nrow(nacht_messungen),
                      size = perc * nrow(nacht_messungen)) # rundet immer ab!
delete_ids <- nacht_messungen[delete_rows,]$id

data_night <- subset(data_night, !(id %in% delete_ids)) # löschen



# neue Summen berechnen

data_night <- data_night %>%
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
  

data_night <- select(data_night, - c("hour(time)", "minute(time)")) # unnötige Variablen

date_data_night <- distinct(subset(data_night, 
                             select = -c(lvs, time, position, id, lvs_true_min,
                                         lvs_false_min, count_people_min,
                                         ratio_min, cloud_cover)))

min_data_night <- distinct(subset(data_night, 
                            select = -c(lvs, position, id)))

# abspeichern

saveRDS(data_night, file = "Daten/data_night.RDS")
saveRDS(min_data_night, file = "Daten/min_data_night.RDS")
saveRDS(date_data_night, file = "Daten/date_data_night.RDS")   