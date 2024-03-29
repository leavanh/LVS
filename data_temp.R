## Diese Datei erzeugt data_temp
# Bei niedrigen Temperaturen werden Messungen generiert

## Pakete laden

library("tidyverse")
library("lubridate")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")
min_data <- readRDS(file = "Daten/min_data.RDS")
cloud_cover <- readRDS(file = "Daten/cloud_cover.RDS")

# NAs (fehlende Werte) rauswerfen

date_data_noNA <- date_data[!is.na(date_data$count_people),]
data_noNA <- data[!is.na(data$lvs),]
min_data_noNA <- min_data[!is.na(min_data$count_people_min),]

# Da die Daten zufällig erzeugt werden benutzen wir einen seed
# so kommt bei jedem Durchlauf das selbe Ergebnis raus

set.seed(42)


## data_temp erzeugen (ohne NAs da wir diese Tage eh nicht brauchen)

data_temp <- data_noNA

## Messungen erzeugen

time_intervall <- interval( # alle möglichen Uhrzeiten
  as.POSIXct("1899-12-31 04:00:00", tz = "GMT"),
  as.POSIXct("1900-01-01 03:59:59", tz = "GMT")
)

# Funktion die je nach Temperatur die Prozentzahl der hinzuzufügenden
# Messungen ausrechnet (für jeden Tag)
# ab 22° werden die Prozente hier negativ (Vorsicht!)

perc_by_date <- 0.266 - 0.012 * date_data_noNA$temperature
n_by_date <- data.frame(
  count_new = round(date_data_noNA$count_people * perc_by_date), # rundet
  date = date_data_noNA$date)

# neuen data.frame erschaffen (am Ende Erste Zeile löschen)

neue_messungen <- data.frame(id = NA, lvs = FALSE, position = NA,
                             time = as.POSIXct("1899-12-31 00:00:00", tz = "GMT"), 
                             date = as.POSIXct("1899-12-31", tz = "GMT"))

for(i in 1:nrow(date_data_noNA)) {
  for(j in 1:n_by_date$count_new[i]) { # neue Messungen generieren
    messung_time <- as.POSIXct(sample(time_intervall, 1), tz = "GMT", 
                               origin = "1899-12-31 04:00:00") # erzeugen
    messung_time <- as.POSIXct( # alle Sekunden auf 0
      trunc(messung_time, units = "mins"))
    messung_date <- n_by_date$date[i]
    messung <- data.frame(id = NA, lvs = FALSE, position = NA, 
                          time = messung_time,
                          date = messung_date)
    neue_messungen <- rbind(neue_messungen, messung)
  }
}

neue_messungen <- neue_messungen[-1,] # erste Zeile löschen

## Messungen hinzufügen

# Allgemeine Variablen hinzufügen

data_temp <- neue_messungen %>%
  full_join(date_data_noNA, by = c("date")) %>%
  mutate(lvs_true_min = 0,
         lvs_false_min = 0,
         count_people_min = 0,
         ratio_min = 0)

# cloud_cover hinzufügen

for(i in 1:nrow(data_temp)) {
  date <- data_temp[[i, "date"]]
  hour <- hour(data_temp[[i, "time"]])
  cloud_cover_i <- cloud_cover[cloud_cover$date == date & 
                                 hour(cloud_cover$time) == hour,][1,] %>%
    pull("cloud_cover")
  data_temp[i, "cloud_cover"] <- cloud_cover_i
}

# zu den "alten" Daten hinzufügen

data_temp <- rbind(data_temp, data_noNA)

# neue Summen berechnen

data_temp <- data_temp %>%
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


data_temp <- select(data_temp, - c("hour(time)", "minute(time)")) # unnötige Variablen

date_data_temp <- distinct(subset(data_temp, 
                              select = -c(lvs, time, position, id, lvs_true_min,
                                          lvs_false_min, count_people_min,
                                          ratio_min, cloud_cover)))

min_data_temp <- distinct(subset(data_temp, 
                             select = -c(lvs, position, id)))

# abspeichern

saveRDS(data_temp, file = "Daten/data_temp.RDS")
saveRDS(min_data_temp, file = "Daten/min_data_temp.RDS")
saveRDS(date_data_temp, file = "Daten/date_data_temp.RDS")  