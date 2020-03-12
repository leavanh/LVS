## Pakete laden

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("patchwork")) install.packages("patchwork")
library("patchwork")

if (!require("GGally")) install.packages("GGally")
library("GGally")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")

## Deskriptive Auswertung

source("descriptive.R", encoding = "UTF-8")

str(data)
summary_list
# -> mehr Messungen bei S
# -> am wenigsten Messungen Montags, am meisten am Wochenende
date_data_plot
# -> sunhours hängt vollkommen von date ab (Achtung beim Modell!)
(date_type + date_ratio)/
  (date_snowhight + date_temperature + date_solar_radiation)
# -> hohe Schwankung am Anfang (bei ratio), wie zu erklären?
snowhight_ratio / solar_radiation_ratio / temperature_ratio
snowhight_solar_radiation
time_type
avalanche_plot