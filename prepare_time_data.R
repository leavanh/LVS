# Diese R-Datei erstellt die RDS-Datei time_data.RDS

# erklären was passiert

# Zum Glück haben wir an Tagen an denen keine Messungen vom Tag vorher 
# vorliegen (21.12.18 und 25.12.18) keine Messungen in dem kritischen
# Intervall

## Pakete laden

if (!require("lubridate")) install.packages("lubridate")
library("lubridate")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

## Funktion laden

source("functions.R", encoding = "UTF-8")

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
    # neue Werte zuweisen
    time_data[[i, "date"]] <- lvs_date_data[[
                                lvs_date_data$date == date_new, "date"]]
    time_data[[i, "day"]] <- lvs_date_data[[
      lvs_date_data$date == date_new, "day"]]
  }
  # mit der nächsten Messung weitermachen
  i <- i + 1
}
