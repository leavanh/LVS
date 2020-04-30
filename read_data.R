# Diese R Datei lädt alle Datensätze ein und bereitet sie so vor, dass sie
# danach gut benutzt werden können
# Sie erstellt die RDS-Dateien data.RDS, date_data.RDS und min_data.RDS

## Pakete laden

library("readxl")
library("lubridate")
library("tidyverse")
library("mgcv")

## Exceltabellen einlesen

all_date <- read_excel("Daten/Projektdaten_DAV (muss_aufbereitet_werden).xlsx",
                       sheet = "all_Date", range = "A1:V115")

all_checkpoint_stats <- read_excel(
  "Daten/Projektdaten_DAV (muss_aufbereitet_werden).xlsx",
  sheet = "all_CheckPoint_stats", 
  range = "A315:E38290",
  col_names = 
    c("id","type", "date", "time", "position"))

day_length <- read_excel("Daten/sunhours.xlsx",
                         col_names = c("date", "sunrise", "sunset", "day_length"))

# Variablen in all_date umbenennen

colnames(all_date) <- c("day", "date", "count_all", "count_selected",
                        "count_beacon", "count_infrared", "ratio", "snowhight",
                        "temperature", "precipitation", "solar_radiation",
                        "avalanche_report_down", "avalanche_report_top", 
                        "avalanche_report_border", "avalanche_report_comment",
                        "avalanche_1", "avalanche_2", "avalanche_3", 
                        "avalanche_4","day_weekday", "day_weekend", "holiday" )

# Reihen die nicht Beacon oder Infrared sind aus all_checkpoint_stats
# rauslöschen

all_checkpoint_stats <- subset(all_checkpoint_stats,
                               type %in% c("Beacon", "Infrared"))

# date in day_length in POSIXct umwandeln

day_length$date <- as.POSIXct(day_length$date, format = "%d %B %Y", tz = "MESZ")

## day_length und all_date zusammenführen

date_data <- left_join(all_date, day_length, by = "date")

## Tagesindikatoren in logical umkodieren

for (k in c("day_weekend", "holiday")) { 
  date_data[,k] <- c(!is.na(date_data [,k])) 
}

## Tage löschen

# vom 07.01.19 mit einschließlich 15.01.19 waren die Checkpoints überdeckt und 
# die Messungen werden entfernt

all_checkpoint_stats <- filter(all_checkpoint_stats,
                               !(date %within% interval(ymd("2019-01-07"),
                                                      ymd("2019-01-15"))))

## Alles auf Winterzeit kodieren
# Sonnenauf- und Untergang soll manuell auf Winterzeit umgestellt werden
# Umstellung am 31.03.19: 2 wurde zu 3 Uhr -> wieder zurück

date_data[date_data$date >= as.POSIXct("2019-03-31", tz = "MESZ"), "sunrise"] <- 
  date_data[date_data$date >= as.POSIXct("2019-03-31", tz = "MESZ"),] %>%
  pull(sunrise) - hours(1)

date_data[date_data$date >= as.POSIXct("2019-03-31", tz = "MESZ"), "sunset"] <- 
  date_data[date_data$date >= as.POSIXct("2019-03-31", tz = "MESZ"),] %>%
  pull(sunset) - hours(1)

## Neue Variablen berechnen

# Neuschnee

date_data$snow_diff <- date_data$snowhight - lag(date_data$snowhight, 
                                       default = first(date_data$snowhight),
                                       by = date_data$date)

# temperature in 6 Kategorien

date_data$int_temperature <- as.integer(cut(date_data$temperature, seq(-8,10,3),
                                       right = FALSE, labels = c(1:6)))

# int_ Variablen

date_data$int_date <- as.integer(as.Date(date_data$date, format = "%d/%m/%Y"))
date_data$int_day <- as.integer(factor(date_data$day,
                      levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag",
                                    "Freitag", "Samstag", "Sonntag")))

# Residuen

temp_gam <- gam(temperature ~ s(int_date, bs = "ps", k = 20),
                data = date_data, method = "REML")
solar_rad_gam <- gam(log(solar_radiation) ~ s(int_date, bs = "ps", k = 20),
                     data = date_data, method = "REML") # log not strong enough
snow_gam <- gam(snowhight ~ s(int_date, bs = "ps", k = 20),
                data = date_data, method = "REML")

date_data$res_temperature <- residuals.gam(temp_gam)
date_data$res_solar_radiation <- residuals.gam(solar_rad_gam)
date_data$res_snowhight <- residuals.gam(snow_gam)

## date_data und all_checkpoint_stats zusammenführen

data <- full_join(all_checkpoint_stats, date_data, by = "date")

## Uhrzeit umkodieren

# Messungen zwischen 0 und 4 am Morgen sollen dem vorherigen Tag zugeordnet
# werden

# Zum Glück haben wir an Tagen an denen keine Messungen vom Tag vorher 
# vorliegen (25.12.18 und 16.01.19) keine Messungen in dem kritischen
# Intervall

# Schleife

time_interval <- interval(
  as.POSIXct("1899-12-31 00:00:00", tz = "MESZ"),
  as.POSIXct("1899-12-31 03:59:59", tz = "MESZ")
)

for(i in 1:nrow(data)) {
  # Zeit der Beobachtung 
  time_i <- data[[i, "time"]]
  # Datum der Beobachtung
  date_i <- data[[i, "date"]]
  # prüfen, ob überhaupt Messungen an dem Tag vorhanden
  if(!is.na(time_i)) {
  # prüfen, ob Zeit von 0 bis 4 Uhr ist und umkodiert werden muss
  if(time_i %within% time_interval) {
    # Datum des Tags davor
    date_new <- date_i - days(1)
    # Reihe der Beobachtung herausfinden
    row_i <- which(date_data$date == date_new)
    # Uhrzeit am "nächsten Tag gemessen" (um in Plot darstellen zu können)
    data[[i, "time"]] <- data[[i, "time"]] + days(1)
    # neue Werte zuweisen
    data[[i, "date"]] <- date_data[[row_i, "date"]]
    data[[i, "day"]] <- date_data[[row_i, "day"]]
    data[[i, "ratio"]] <- date_data[[row_i, "ratio"]]
    data[[i, "snowhight"]] <- date_data[[row_i, "snowhight"]]
    data[[i, "snow_diff"]] <- date_data[[row_i, "snow_diff"]]
    data[[i, "temperature"]] <- date_data[[row_i, "temperature"]]
    data[[i, "solar_radiation"]] <- date_data[[row_i,
                                                        "solar_radiation"]]
    data[[i, "avalanche_report_down"]] <- date_data[[
      row_i, "avalanche_report_down"]]
    data[[i, "avalanche_report_top"]] <- date_data[[
      row_i, "avalanche_report_top"]]
    data[[i, "avalanche_report_border"]] <- date_data[[
      row_i,
      "avalanche_report_border"]]
    data[[i, "avalanche_report_comment"]] <- date_data[[
      row_i, 
      "avalanche_report_comment"]]
    data[[i, "day_weekend"]] <- date_data[[row_i, "day_weekend"]]
    data[[i, "holiday"]] <- date_data[[row_i, "holiday"]]
    data[[i, "sunrise"]] <- date_data[[row_i, "sunrise"]]
    data[[i, "sunset"]] <- date_data[[row_i, "sunset"]]
    data[[i, "int_temperature"]] <- date_data[[row_i, "int_temperature"]]
    data[[i, "int_date"]] <- date_data[[row_i, "int_date"]]
    data[[i, "int_day"]] <- date_data[[row_i, "int_day"]]
    data[[i, "res_temperature"]] <- date_data[[row_i, "res_temperature"]]
    data[[i, "res_snowhight"]] <- date_data[[row_i, "res_snowhight"]]
    data[[i, "res_solar_radiation"]] <- date_data[[row_i,
                                                   "res_solar_radiation"]]
  }}
  # mit der nächsten Messung weitermachen
  i <- i + 1
}

## neue Variablen berechnen

# type Beacon oder Infrared in lvs TRUE oder FALSE umkodieren

# count_beacon und count_infrared wird neu berechnet und heißen ab jetzt
# lvs_true und lvs_false
# count_people (absolute Anzahl der Leute an dem Tag) wird hinzugefügt

# Anteil wird neu berechnet

# avalanche_report_down und avalanche_report_top werden zu einem Durchschnitt
# zusammengefasst

data <- group_by(data, date) %>%
  # neue Variablen hinzufügen
  mutate(lvs = as.logical(type == "Beacon"), # type zu lvs
          avalanche_report = # Avalanche_report
           (avalanche_report_down + avalanche_report_top)/2,
         lvs_true = sum(type == "Beacon"), # Anzahl Beaconmessung
         lvs_false = sum(type == "Infrared"), # Anzahl Infrarotmessungen
         count_people = lvs_true + lvs_false, # Anzahl Leute insg.
         ratio = lvs_true/(count_people)) %>%  # Anteil
  ungroup()

# außerdem die Werte für jede Minute berechnen

data <- group_by(data, date, hour(time), minute(time)) %>%
  # neu berechnen
  mutate(lvs_true_min = sum(type == "Beacon"), # Anzahl mit LVS
         lvs_false_min = sum(type == "Infrared"), # Anzahl ohne LVS
         count_people_min = lvs_true_min + lvs_false_min, # Anzahl
         # Leute insg.
         ratio_min = lvs_true_min/(count_people_min)) %>% # Ratio
  ungroup()

## nur wichtige Variablen behalten

data <- subset(data, select = c(id, lvs, position, time, date, int_date, day,
                                int_day, day_weekend, holiday,
                                snowhight, snow_diff, res_snowhight,
                                temperature, int_temperature, res_temperature,
                                solar_radiation, res_solar_radiation, 
                                avalanche_report, sunrise, sunset,
                                lvs_true, lvs_false, count_people, ratio,
                                lvs_true_min, lvs_false_min, count_people_min,
                                ratio_min))

## factors für position festlegen

data$position <- factor(data$position)

## neue data erstellen

# date_data

date_data <- distinct(subset(data, 
                             select = -c(lvs, time, position, id, lvs_true_min,
                                         lvs_false_min, count_people_min,
                                         ratio_min))) %>%
              subset(date >= as.Date("2018-12-25")) # erst ab dem 25.

# min_data

min_data <- distinct(subset(data, 
                             select = -c(lvs, position, id))) %>%
  subset(date >= as.Date("2018-12-25")) # erst ab dem 25.

## als RDS speichern

saveRDS(date_data, file = "Daten/date_data.RDS")

saveRDS(min_data, file = "Daten/min_data.RDS")

saveRDS(data, file = "Daten/data.RDS")

