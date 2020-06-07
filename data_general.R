
## Diese Datei erzeugt data_general
# Dem Szenario 1 (20% Unterschätzung) folgend werden Messungen generiert
# Die Funktion nimmt als Argument den Anteil an Checkpoint-Messungen, die neu
# generiert werden sollen

# wir brauchen später cloud_cover

cloud_cover <- readRDS(file = "Daten/cloud_cover.RDS")


data_general_function <- function(Anteil_neu_messungen) {

## Messungen erzeugen

# Anzahl der zu erzeugenden Messungen
# diese Zahl wird abgerundet (in der Schleife)
  
n_messungen <- Anteil_neu_messungen * nrow(data_noNA)


time_intervall <- interval( # alle möglichen Uhrzeiten
  as.POSIXct("1899-12-31 04:00:00", tz = "GMT"),
  as.POSIXct("1900-01-01 03:59:59", tz = "GMT")
)

all_dates <- date_data_noNA$date # alle möglichen Tage

# neuen data.frame erschaffen (am Ende Erste Zeile löschen)

neue_messungen <- data.frame(id = NA, lvs = FALSE, position = NA,
                     time = as.POSIXct("1899-12-31 00:00:00", tz = "GMT"), 
                     date = as.POSIXct("1899-12-31", tz = "GMT"))

for(i in 1:n_messungen) { # neue Messungen generieren
  messung_time <- as.POSIXct(sample(time_intervall, 1), tz = "GMT", 
                             origin = "1899-12-31 04:00:00") # erzeugen
  messung_time <- as.POSIXct( # alle Sekunden auf 0
    trunc(messung_time, units = "mins"))
  messung_date <- sample(all_dates, 1)
  messung <- data.frame(id = NA, lvs = FALSE, position = NA, time = messung_time,
               date = messung_date)
  neue_messungen <- rbind(neue_messungen, messung)
}

neue_messungen <- neue_messungen[-1,] # erste Zeile löschen

## Messungen hinzufügen

# Allgemeine Variablen hinzufügen

data_general <- neue_messungen %>%
  full_join(date_data_noNA, by = c("date")) %>%
  mutate(lvs_true_min = 0,
         lvs_false_min = 0,
         count_people_min = 0,
         ratio_min = 0)

# cloud_cover hinzufügen

for(i in 1:nrow(data_general)) {
  date <- data_general[[i, "date"]]
  hour <- hour(data_general[[i, "time"]])
  cloud_cover_i <- cloud_cover[cloud_cover$date == date & 
                                 hour(cloud_cover$time) == hour,][1,] %>%
    pull("cloud_cover")
  data_general[i, "cloud_cover"] <- cloud_cover_i
}

# zu den "alten" Daten hinzufügen

data_general <- rbind(data_general, data_noNA)

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
                                          ratio_min, cloud_cover)))

min_data_general <- distinct(subset(data_general, 
                             select = -c(lvs, position, id)))

data_list <- list(date_data = date_data_general,
                  data = min_data_general
)

return(data_list)

}
