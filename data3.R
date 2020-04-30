## Diese Datei erzeugt data3
# Dem Szenario 3 (Wildtiere) folgend werden Messungen gelöscht

## data3 erzeugen (ohne NAs da wir diese Tage eh nicht brauchen)

data3 <- data_noNA

## Messungen löschen

perc <- 0.5 # Prozent die gelöscht werden sollen

# welche kommen in Frage?

time_intervall <- interval(
  as.POSIXct("1900-01-01 01:00:00", tz = "UTC"),
  as.POSIXct("1900-01-01 03:59:59", tz = "UTC")
)
nacht_messungen <- subset(data3, time %within% time_intervall # nachts
                          & lvs == FALSE) # nur Infrarotmessungen
delete_rows <- sample(nrow(nacht_messungen),
                      size = perc * nrow(nacht_messungen)) # rundet immer ab!
delete_ids <- nacht_messungen[delete_rows,]$id

data3 <- subset(data3, !(id %in% delete_ids)) # löschen



# neue Summen berechnen

data3 <- data3 %>%
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
  

data3 <- select(data3, - c("hour(time)", "minute(time)")) # unnötige Variablen

date_data3 <- distinct(subset(data3, 
                             select = -c(lvs, time, position, id, lvs_true_min,
                                         lvs_false_min, count_people_min,
                                         ratio_min)))

min_data3 <- distinct(subset(data3, 
                            select = -c(lvs, position, id)))
