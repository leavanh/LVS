## Pakete laden

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("patchwork")) install.packages("patchwork")
library("patchwork")

if (!require("GGally")) install.packages("GGally")
library("GGally")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")

## Datumsdatensatz erstellen

date_data <- distinct(subset(data, select = -c(type, time)))

## Deskriptive Auswertung

source("descriptive.R", encoding = "UTF-8")

str(data)
summary_list
# -> mehr Messungen bei S
# -> am wenigsten Messungen Montags, am meisten am Wochenende
date_data_plot
# -> sunhours hängt vollkommen von date ab (Achtung beim Modell!)
date_plots
# -> hohe Schwankung am Anfang (bei ratio), wie zu erklären?
ratio_plots
snowhight_solar_radiation
time_type
avalanche_plot