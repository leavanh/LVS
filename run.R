## Pakete laden

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("patchwork")) install.packages("patchwork")
library("patchwork")

if (!require("GGally")) install.packages("GGally")
library("GGally")

if (!require("mfx")) install.packages("mfx")
library("mfx")

if (!require("geepack")) install.packages("geepack")
library("geepack")

if (!require("nlme")) install.packages("nlme")
library("nlme")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")
lvs_data <- readRDS(file = "Daten/lvs_data.RDS")
lvs_date_data <- readRDS(file = "Daten/lvs_date_data.RDS")
time_data <- readRDS(file = "Daten/time_data.RDS")
time_date_data <- readRDS(file = "Daten/time_date_data.RDS")

## Deskriptive Auswertung

source("descriptive.R", encoding = "UTF-8")

str(lvs_data)
summary_list
# -> mehr Messungen bei S
# -> am wenigsten Messungen Montags, am meisten am Wochenende
lvs_date_data_plot
# -> day_length hängt vollkommen von date ab (Achtung beim Modell!)
date_type/date_lvs/lvs_date_ratio/date_diff_plot
# -> hohe Schwankung am Anfang, wie zu erklären?
date_snowhight + date_temperature + date_solar_radiation
snowhight_ratio / solar_radiation_ratio / temperature_ratio
snowhight_solar_radiation
(time_type/time_lvs)
avalanche_position_plot + avalanche_mean_plot

## Modell fitting

source("logprob.R", encoding = "UTF-8")
