## Pakete laden

if (!require("readxl")) install.packages("readxl")
library("readxl")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

## Exceltabelle einlesen

all_date <- read_excel("Daten/Projektdaten_DAV (muss_aufbereitet_werden).xlsx",
                       sheet = "all_Date", range = "A1:V115")
all_checkpoint_stats <- read_excel(
  "Daten/Projektdaten_DAV (muss_aufbereitet_werden).xlsx",
                                   sheet = "all_CheckPoint_stats", 
                                   range = "A8:E38290",
                                   col_names = 
                                     c("id","type", "date", "time", "position"))

# Variablen in all_date umbenennen

colnames(all_date) <- c("day", "date", "count_all", "count_selected",
                        "count_beacon", "count_infrared", "ratio", "snowhight",
                        "temperature", "precipitation", "solar_radiation",
                        "avalanche_report_down", "avalanche_report_top", 
                        "avalanche_report_border", "avalanche_report_comment",
                        "avalanche_1", "avalanche_2", "avalanche_3", 
                        "avalanche_4", "day_weekday","day_weekend", "holiday" )

# Reihen die nicht Beacon oder Infrared sind aus all_checkpoint_stats rauslöschen

all_checkpoint_stats <- subset(all_checkpoint_stats,
                               type %in% c("Beacon", "Infrared"))

## sunhours-Tabelle einlesen

day_length <- read_excel("Daten/sunhours.xlsx",
                       col_names = c("date", "sunrise", "sunset", "day_length"))

# date in POSIXct umwandeln

sunhours$date <- as.POSIXct(sunhours$date, format = "%d %B %Y", tz = "UTC")

## Tabellen zusammenführen

# sunhours und all_date

date_data <- left_join(all_date, day_length, by = "date")

# date_data und all_checkpoint_stats

data <- full_join(all_checkpoint_stats, date_data, by = "date")

## Daten prüfen

# setdiff(full_join(all_checkpoint_stats, date_data, by = "date"), data)
# an drei Tagen wurde keine Person gemessen (vielleicht Gebiet gesperrt?)

# subset(all_date, !(all_date$count_infrared >= all_date$count_beacon))
# an 8 Tagen wurden mehr LVS-Geräte als Personen gemessen!

## unnötige Variablen entfernen

data <- subset(data, select = -c(id, count_all, count_selected, precipitation,
                                 avalanche_1, avalanche_2, avalanche_3, 
                                 avalanche_4, sunrise, sunset))

## Tagesindikatoren umkodieren

for (k in 16:18) { 
  data[,k] <- c(!is.na(data [,k])) 
}

## Ratio berechnen

# alte Ratio wird hier durch die neu berechnete überschrieben

data <- mutate(data, ratio = count_beacon/count_infrared)

# überlegen, was mit ratios > 1 und Inf passiert!

## factors festlegen

data$type <- factor(data$type)
data$position <- factor(data$position)
data$day <- factor(data$day,
                   levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag",
                              "Freitag", "Samstag", "Sonntag"))
data$avalanche_report_down <- factor(data$avalanche_report_down)
data$avalanche_report_top <- factor(data$avalanche_report_top)

## als RDS speichern

saveRDS(data, file = "Daten/data.RDS")
