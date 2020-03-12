# Diese R-Datei erstellt die RDS-Dateien data.RDS und date_data.RDS

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
                                         precipitation, avalanche_1, 
                                         avalanche_2, avalanche_3, 
                                         avalanche_4))

## Tagesindikatoren in logical umkodieren

for (k in c("day_weekday", "day_weekend", "holiday")) { 
  date_data[,k] <- c(!is.na(date_data [,k])) 
}

## Ratio und Avalanche_report berechnen

# alte Ratio wird hier durch die neu berechnete überschrieben

# avalanche_report_down und avalanche_report_top werden zu einem Durchschnitt
# zusammengefasst

date_data <- mutate(date_data, ratio = count_beacon/count_infrared,
                    avalanche_report = 
                      (avalanche_report_down + avalanche_report_top)/2)

# überlegen, was mit ratios > 1 und Inf passiert!
# -> Problem wird durch die Umkodierung in prepare_lvs_data.R gelöst

## factors festlegen

all_checkpoint_stats$type <- factor(all_checkpoint_stats$type)
all_checkpoint_stats$position <- factor(all_checkpoint_stats$position)
date_data$day <- factor(date_data$day,
                   levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag",
                              "Freitag", "Samstag", "Sonntag"))
date_data$avalanche_report_down <- factor(date_data$avalanche_report_down)
date_data$avalanche_report_top <- factor(date_data$avalanche_report_top)
date_data$avalanche_report <- factor(date_data$avalanche_report)

## date_data und all_checkpoint_stats zusammenführen

data <- left_join(all_checkpoint_stats, date_data, by = "date")

## Daten prüfen

# setdiff(full_join(all_checkpoint_stats, date_data, by = "date"), data)
# an drei Tagen wurde keine Person gemessen (vielleicht Gebiet gesperrt?)

# subset(all_date, !(all_date$count_infrared >= all_date$count_beacon))
# an 8 Tagen wurden mehr LVS-Geräte als Personen gemessen!

## als RDS speichern

saveRDS(date_data, file = "Daten/date_data.RDS")

saveRDS(data, file = "Daten/data.RDS")