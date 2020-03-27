# Diese R Datei lädt alle Datensätze ein und bereitet sie so vor, dass sie
# danach gut benutzt werden können
# Sie erstellt die RDS-Dateien data.RDS und date_data.RDS

## Pakete laden

if (!require("readxl")) install.packages("readxl")
library("readxl")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

## Exceltabellen einlesen

all_date <- read_excel("Daten/Projektdaten_DAV (muss_aufbereitet_werden).xlsx",
                       sheet = "all_Date", range = "A1:V115")

all_checkpoint_stats <- read_excel(
  "Daten/Projektdaten_DAV (muss_aufbereitet_werden).xlsx",
  sheet = "all_CheckPoint_stats", 
  range = "A8:E38290",
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
                        "avalanche_4", "day_weekday","day_weekend", "holiday" )

# Reihen die nicht Beacon oder Infrared sind aus all_checkpoint_stats
# rauslöschen

all_checkpoint_stats <- subset(all_checkpoint_stats,
                               type %in% c("Beacon", "Infrared"))

# date in POSIXct umwandeln

day_length$date <- as.POSIXct(day_length$date, format = "%d %B %Y", tz = "UTC")

## day_length und all_date zusammenführen

date_data <- left_join(all_date, day_length, by = "date")

## unnötige Variablen entfernen

date_data <- subset(date_data, select = -c(count_all, count_selected, 
                                           count_beacon, count_infrared,
                                           ratio, precipitation, avalanche_1, 
                                           avalanche_2, avalanche_3, 
                                           avalanche_4))

## Tagesindikatoren in logical umkodieren

for (k in c("day_weekday", "day_weekend", "holiday")) { 
  date_data[,k] <- c(!is.na(date_data [,k])) 
}

## date_data und all_checkpoint_stats zusammenführen

data <- left_join(all_checkpoint_stats, date_data, by = "date")

## neue Variablen berechnen

# count_beacon und count_infrared wird neu berechnet und heißen ab jetzt
# lvs_true und lvs_false
# count_people (absolute Anzahl der Leute an dem Tag) wird hinzugefügt

# Ratio wird neu berechnet

# avalanche_report_down und avalanche_report_top werden zu einem Durchschnitt
# zusammengefasst

data <- group_by(data, date) %>%
  # neue Variablen hinzufügen
  mutate(lvs_true = sum(type == "Beacon"), # Anzahl Beaconmessung
         lvs_false = sum(type == "Infrared"), # Anzahl Infrarotmessungen
         count_people = lvs_true + lvs_false, # Anzahl Leute insg.
         ratio = lvs_true/(count_people), #ratio
         avalanche_report = # Avalanche_report
           (avalanche_report_down + avalanche_report_top)/2) %>%
  ungroup()

## factors festlegen

data$type <- factor(data$type)
data$position <- factor(data$position)
data$day <- factor(data$day,
                   levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag",
                              "Freitag", "Samstag", "Sonntag"))

## neue date_data erstellen

date_data <- distinct(subset(data, 
                             select = -c(lvs, time, position, id)))

## als RDS speichern

saveRDS(date_data, file = "Daten/date_data.RDS")

saveRDS(data, file = "Daten/data.RDS")