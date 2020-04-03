## Pakete laden

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("patchwork")) install.packages("patchwork")
library("patchwork")

if (!require("GGally")) install.packages("GGally")
library("GGally")

if (!require("mgcv")) install.packages("mgcv")
library("mgcv")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")

# NAs rauswerfen

date_data_noNA <- date_data[!is.na(date_data$count_people),]

## Deskriptive Auswertung

source("descriptive.R", encoding = "UTF-8")

str(data)
summary_list
# -> mehr Messungen bei S
# -> am wenigsten Messungen Montags, am meisten am Wochenende
date_data_plot
# -> day_length hängt vollkommen von date ab (Achtung beim Modell!)
date_type/date_ratio
date_position
date_snowhight + date_temperature + date_solar_radiation
(snowhight_ratio + solar_radiation_ratio) / 
  (temperature_ratio + avalanche_ratio)
snowhight_solar_radiation
time_type

## Modell fitting

source("model.R", encoding = "UTF-8")

# Diagnostikplots immer als 2x2 darstellen

par(mfrow=c(2,2))

# Modell 1: nur stetige Variablen, Datum nicht beachtet

summary(model_1)

gam.check(model_1)

# Verteilung falsch! Autokorrelierte Daten nicht beachtet
# response vs. fitted values: ein paar response bei 0

plot(model_1, pages = 1, residuals = TRUE, pch = 19, cex = .3)
# auch durch linear zu ersetzen teilweise?
# oversmoothing bei solar radiation?

# Modell 2: auch kategorielle Variablen, Datum nicht beachtet

summary(model_2)

gam.check(model_2)
# Verteilung falsch! Autokorrelierte Daten nicht beachtet
# response vs. fitted values: ein paar response bei 0

plot(model_2, pages = 1, residuals = TRUE, pch = 19, cex = .3)
# siehe model_1

# Modell 3: Datum hinzufügen (noch nicht als Autokorrelation)

summary(model_3)

gam.check(model_3)
# Verteilung falsch! Autokorrelierte Daten nicht beachtet
# k ist zu niedrig

plot(model_3, pages = 1, residuals = TRUE, pch = 19, cex = .3)
# temperatur vielleicht nur linear rein?

# Modell 4: Autokorrelation hinzufügen

summary(model_4$gam)

plot(model_4$gam, pages = 1, residuals = TRUE, pch = 19, cex = .3)
# auch durch linear zu ersetzen bei temperature?

# Modell 5: Wochentage statt nur Wochenende und avalanche_report als smooth

summary(model_5$gam)

plot(model_5$gam, pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0)
