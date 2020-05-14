
## Pakete laden

library("tidyverse")
library("patchwork")
library("GGally")
library("mgcv")
library("mgcViz")
library("lubridate")
library("gridExtra")
library("ggformula")

## Theme (Ästhetik) der Plots festsetzen

theme_set(theme_minimal())


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

# Verlauf der Sonneneinstrahlung

solar_radiation_max




### Modell fitting

source("date_model.R", encoding = "UTF-8")
source("day_model.R", encoding = "UTF-8")

source("smooth_plots_date_model.R", encoding = "UTF-8")
source("smooth_plots_day_model.R", encoding = "UTF-8")


## Date_model

date_model <- date_model_function(date_data)
date_model$summary # Übersicht (mit Signifikanz)
plogis(date_model$summary$p.coeff) # parametrische Effekt

# Plots

date_model_plots <- plots_date_model(date_model)

# Übersicht non-parametrischer Plots

gridPrint(grobs = date_model_plots$grid,
          top = "Smooth-Plots im Date-Model")

# Einzelplots

date_model_plots$date
date_model_plots$day
date_model_plots$avalanche
date_model_plots$solar_radiation
date_model_plots$temperature
date_model_plots$snowhight

# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...

# gam.check(date_model$model, type = "deviance")
# concurvity(date_model$model, full = TRUE)
# concurvity(date_model$model, full = FALSE)
# acf(date_model$model$residuals)
# pacf(date_model$model$residuals)


# Day_model

start <- print(Sys.time()) # we want to know how long computation takes

# min_data_noNA_sample <- min_data_noNA[sample(nrow(min_data_noNA), 1000), ]

day_models <- day_model_function(min_data_noNA)
day_model <- day_models$model
# day_model_gamm <- day_models$model_gamm

# saveRDS(day_model_gamm, file = "day_model_gamm.RDS")

end <- print(Sys.time())
print(end - start)

day_models$summary # Übersicht (mit Signifikanz)
plogis(day_models$summary$p.coeff) # parametrische Effekt

# Plots

day_model_plots <- plots_day_model(day_models)

# Übersicht nonparametrischer Plots

gridPrint(grobs = day_model_plots$grid,
          top = "Smooth-Plots im Day-Model")

# Einzelplots

day_model_plots$date_time
day_model_plots$day
day_model_plots$avalanche
day_model_plots$solar_radiation
# day_model_plots$temperature
day_model_plots$snowhight


# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...

# gam.check(day_model, type = "deviance")
# concurvity(day_model, full = TRUE)
# concurvity(day_model, full = FALSE)
# acf(day_model$residuals)
# pacf(day_model$residuals)

## folgendes bitte ignorieren ich brauchte nur nen platz das kurz zu speichern



# gam.check(day_model)
# gam.check(day_model2$gam)
# # plot(day_model2$gam$linear.predictors, day_model2$lme$residuals[, "fixed"])
# 
# acf(day_model2$lme$residuals[, "fixed"])
# pacf(day_model2$lme$residuals[, "fixed"])




#### Messfehleranalyse


## Deskriptive Analyse der neuen Daten




## Verschiedene Szenarien vergleichen

set.seed(42)

# Datensätze der Szenarien laden

source("data_general.R", encoding = "UTF-8")
source("data_group.R", encoding = "UTF-8")
source("data_night.R", encoding = "UTF-8")
source("data_temp.R", encoding = "UTF-8")

source("get_legend.R", encoding = "UTF-8")
source("scenarios_plots_date_model.R", encoding = "UTF-8")
source("scenarios_plots_day_model.R", encoding = "UTF-8")


# Modelle fitten

date_model_general <- date_model_function(date_data_general)
date_model_general$summary # Übersicht (mit Signifikanz)
plogis(date_model_general$summary$p.coeff) # parametrische Effekt

day_models_general <- day_model_function(min_data_general)
day_model_general <- day_models_general$model
day_models_general$summary # Übersicht (mit Signifikanz)
plogis(day_models_general$summary$p.coeff) # parametrische Effekt


date_model_group <- date_model_function(date_data_group)
date_model_group$summary # Übersicht (mit Signifikanz)
plogis(date_model_group$summary$p.coeff) # parametrische Effekt

day_models_group <- day_model_function(min_data_group)
day_model_group <- day_models_group$model
day_models_group$summary # Übersicht (mit Signifikanz)
plogis(day_models_group$summary$p.coeff) # parametrische Effekt


date_model_night <- date_model_function(date_data_night)
date_model_night$summary # Übersicht (mit Signifikanz)
plogis(date_model_night$summary$p.coeff) # parametrische Effekt

day_models_night <- day_model_function(min_data_night)
day_model_night <- day_models_night$model
day_models_night$summary # Übersicht (mit Signifikanz)
plogis(day_models_night$summary$p.coeff) # parametrische Effekt

date_model_temp <- date_model_function(date_data_temp)
date_model_temp$summary # Übersicht (mit Signifikanz)
plogis(date_model_temp$summary$p.coeff) # parametrische Effekt

day_models_temp <- day_model_function(min_data_temp)
day_model_temp <- day_models_temp$model
day_models_temp$summary # Übersicht (mit Signifikanz)
plogis(day_models_temp$summary$p.coeff) # parametrische Effekt

# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...
# Hier das jeweilige Modell einsetzen

# gam.check(day_model, type = "deviance")
# concurvity(day_model, full = TRUE)
# concurvity(day_model, full = FALSE)
# acf(day_model$residuals)
# pacf(day_model$residuals)


# Plots für den Vergleich der Szenarien

# Für das Date Model

# Übersicht

grid.arrange(plots_scenarios_date_model_comparison_grid)

# Einzelplots

plots_scenarios_date_model_comparison$date
plots_scenarios_date_model_comparison$avalanche
plots_scenarios_date_model_comparison$temperature
plots_scenarios_date_model_comparison$day
plots_scenarios_date_model_comparison$solar_radiation
plots_scenarios_date_model_comparison$snowhight

# Für das Day Model

# Übersicht

grid.arrange(plots_scenarios_day_model_comparison_grid)

# Einzelplots

plots_scenarios_day_model_comparison$day
plots_scenarios_day_model_comparison$avalanche
# plots_scenarios_day_model_comparison$temperature
plots_scenarios_day_model_comparison$solar_radiation
plots_scenarios_day_model_comparison$snowhight
