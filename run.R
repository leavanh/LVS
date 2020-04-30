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

source("date_model.R", encoding = "UTF-8")
source("day_model.R", encoding = "UTF-8")

# Daten von 18/19

date_model <- date_model_function(date_data)
date_model$summary # Übersicht (mit Signifikant)
print(plot(date_model$Viz, trans = plogis) + 
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model$summary$p.coeff) # parametrische Effekt

# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...

#gam.check(date_model$model, type = "deviance")
#concurvity = concurvity(date_model$model, full = TRUE)
#concurvity(date_model$model, full = FALSE)
#acf(date_model$model$residuals)
#pacf(date_model$model$residuals)


## Verschiedene Szenarien vergleichen

set.seed(42)

# neue Daten laden

source("data1.R", encoding = "UTF-8")
source("data2.R", encoding = "UTF-8")
source("data3.R", encoding = "UTF-8")

# Modelle fitten

date_model1 <- date_model_function(date_data1)
date_model1$summary # Übersicht (mit Signifikant)
print(plot(date_model1$Viz, trans = plogis) + 
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model1$summary$p.coeff) # parametrische Effekt


date_model2 <- date_model_function(date_data2)
date_model2$summary # Übersicht (mit Signifikant)
print(plot(date_model2$Viz, trans = plogis) + 
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model2$summary$p.coeff) # parametrische Effekt

date_model3 <- date_model_function(date_data3)
date_model3$summary # Übersicht (mit Signifikant)
print(plot(date_model3$Viz, trans = plogis) + 
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model3$summary$p.coeff) # parametrische Effekt
