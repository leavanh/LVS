## Pakete laden

library("tidyverse")
library("patchwork")
library("GGally")
library("mgcv")
library("mgcViz")
library("lubridate")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")
min_data <- readRDS(file = "Daten/min_data.RDS")

# NAs rauswerfen

date_data_noNA <- date_data[!is.na(date_data$count_people),]
data_noNA <- data[!is.na(data$lvs),]
min_data_noNA <- min_data[!is.na(min_data$count_people_min),]

## Deskriptive Auswertung

source("descriptive.R", encoding = "UTF-8")

str(data)
summary_list
# -> mehr Messungen bei S
# -> am wenigsten Messungen Montags, am meisten am Wochenende
date_data_plot
# -> day_length hängt vollkommen von date ab (Achtung beim Modell!)
date_lvs/date_ratio
date_position
date_snowhight | date_temperature | date_solar_radiation
(snowhight_ratio | solar_radiation_ratio) / 
  (temperature_ratio | avalanche_ratio)
snowhight_solar_radiation
time_lvs

## Modell fitting

# Datumsmodell

source("date_model.R", encoding = "UTF-8")

# Tagesmodell

#source("day_model.R", encoding = "UTF-8")

## Verschiedene Szenarien vergleichen

# neue Daten laden

source("data2.R", encoding = "UTF-8")
