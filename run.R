## Pakete laden

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("patchwork")) install.packages("patchwork")
library("patchwork")

if (!require("GGally")) install.packages("GGally")
library("GGally")

if (!require("mgcv")) install.packages("mgcv")
library("mgcv")

if (!require("mgcViz")) install.packages("mgcViz")
library("mgcViz")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")

# NAs rauswerfen

date_data_noNA <- date_data[!is.na(date_data$count_people),]
data_noNA <- data[!is.na(data$lvs),]

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
date_snowhight + date_temperature + date_solar_radiation
(snowhight_ratio + solar_radiation_ratio) / 
  (temperature_ratio + avalanche_ratio)
snowhight_solar_radiation
time_lvs

## Modell fitting

# Datumsmodell

source("date_model.R", encoding = "UTF-8")
