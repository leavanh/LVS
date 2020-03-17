# Diese R-Datei erstellt die RDS-Dateien time_data.RDS und time_date_data.RDS

# Messungen zwischen 0 und 5 am Morgen sollen dem vorherigen Tag zugeordnet
# werden

# Zum Glück haben wir an Tagen an denen keine Messungen vom Tag vorher 
# vorliegen (21.12.18 und 25.12.18) keine Messungen in dem kritischen
# Intervall

## Pakete laden

if (!require("lubridate")) install.packages("lubridate")
library("lubridate")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

## Daten laden

lvs_data <- readRDS(file = "Daten/lvs_data.RDS")
lvs_date_data <- readRDS(file = "Daten/lvs_date_data.RDS")

## neuer Datensatz

time_data <- lvs_data

## Schleife

# Zeit von 0 ohne 5 soll umkodiert werden

time_interval <- interval(
  as.POSIXct("1899-12-31 00:00:00", tz = "UTC"),
  as.POSIXct("1899-12-31 04:59:59", tz = "UTC")
)

for(i in 1:nrow(time_data)) {
  # Zeit der Beobachtung 
  time_i <- time_data[[i, "time"]]
  # Datum der Beobachtung
  date_i <- time_data[[i, "date"]]
  # prüfen, ob Zeit von 0 bis 5 Uhr ist und umkodiert werden muss
  if(time_i %within% time_interval) {
    # Datum des Tags davor
    date_new <- date_i - days(1)
    # Reihe der Beobachtung herausfinden
    row_i <- which(lvs_date_data$date == date_new)
    # neue Werte zuweisen
    time_data[[i, "date"]] <- lvs_date_data[[row_i, "date"]]
    time_data[[i, "day"]] <- lvs_date_data[[row_i, "day"]]
    time_data[[i, "ratio"]] <- lvs_date_data[[row_i, "ratio"]]
    time_data[[i, "snowhight"]] <- lvs_date_data[[row_i, "snowhight"]]
    time_data[[i, "temperature"]] <- lvs_date_data[[row_i, "temperature"]]
    time_data[[i, "solar_radiation"]] <- lvs_date_data[[row_i,
                                                        "solar_radiation"]]
    time_data[[i, "avalanche_report_down"]] <- lvs_date_data[[
                                                row_i, "avalanche_report_down"]]
    time_data[[i, "avalanche_report_top"]] <- lvs_date_data[[
                                                row_i, "avalanche_report_top"]]
    time_data[[i, "avalanche_report_border"]] <- lvs_date_data[[
                                                  row_i,
                                                  "avalanche_report_border"]]
    time_data[[i, "avalanche_report_comment"]] <- lvs_date_data[[
                                                    row_i, 
                                                    "avalanche_report_comment"]]
    time_data[[i, "day_weekday"]] <- lvs_date_data[[row_i, "day_weekday"]]
    time_data[[i, "day_weekend"]] <- lvs_date_data[[row_i, "day_weekend"]]
    time_data[[i, "holiday"]] <- lvs_date_data[[row_i, "holiday"]]
    time_data[[i, "sunrise"]] <- lvs_date_data[[row_i, "sunrise"]]
    time_data[[i, "sunset"]] <- lvs_date_data[[row_i, "sunset"]]
    time_data[[i, "day_length"]] <- lvs_date_data[[row_i, "day_length"]]
    time_data[[i, "avalanche_report"]] <- lvs_date_data[[row_i,
                                                         "avalanche_report"]]
    time_data[[i, "lvs_true"]] <- lvs_date_data[[row_i, "lvs_true"]]
    time_data[[i, "lvs_false"]] <- lvs_date_data[[row_i, "lvs_false"]]
    time_data[[i, "count_people"]] <- lvs_date_data[[row_i, "count_people"]]
  }
  # mit der nächsten Messung weitermachen
  i <- i + 1
}

## Variablen neu berechnen

time_data <- group_by(time_data, date) %>%
  # neu berechnen
  mutate(lvs_true = sum(lvs == TRUE), # Anzahl mit LVS
         lvs_false = sum(lvs == FALSE), # Anzahl ohne LVS
         count_people = lvs_true + lvs_false, # Anzahl Leute insg.
         ratio = lvs_true/(count_people)) %>% # Ratio
  ungroup()

## time_date_data erstellen

time_date_data <- distinct(subset(time_data, 
                                 select = -c(lvs, time, position, id)))

## als RDS speichern

saveRDS(time_date_data, file = "Daten/time_date_data.RDS")
saveRDS(time_data, file = "Daten/time_data.RDS")
