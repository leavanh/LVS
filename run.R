
### Vorbereitung ###############################################################

## Pakete laden

library("tidyverse") # für Datenbearbeitung und Plots
library("patchwork") 
library("GGally")
library("mgcv") # für das GAM-Modell
library("mgcViz") # für die Visualisierung im GAM-Modell
library("lubridate") # für die Arbeit mit Zeitvariablen
library("gridExtra") # für die Erstellung von Plotrastern
library("ggformula")
library("gtools") # für p-wert-format
library("cowplot") # für get_legend
library("readxl") # zum Einlesen von Excel-Dateien

## Theme (Ästhetik) der Plots festsetzen

theme_set(theme_classic() + theme(legend.position = "top"))


## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")
min_data <- readRDS(file = "Daten/min_data.RDS")

# NAs rauswerfen

date_data_noNA <- date_data[!is.na(date_data$count_people),]
data_noNA <- data[!is.na(data$lvs),]
min_data_noNA <- min_data[!is.na(min_data$count_people_min),]



### Deskriptive Analyse ########################################################


source("descriptive.R", encoding = "UTF-8")

str(data)
summary(date_data)
date_data_plot
date_lvs/date_ratio
date_position
date_snowhight|date_snowdiff
date_temperature
boxplot_cloud_cover|boxplot_avalanche_report
holiday_plot
day_plot
(snowhight_ratio | cloud_cover_ratio) / 
  (temperature_ratio | avalanche_ratio)
time_lvs



### Modell fitting #############################################################


source("date_model.R", encoding = "UTF-8")
source("day_model.R", encoding = "UTF-8")

source("smooth_plots_date_model.R", encoding = "UTF-8")
source("smooth_plots_day_model.R", encoding = "UTF-8")


## Date_model

date_model <- date_model_function(date_data_noNA)

date_model$summary # Übersicht (mit Signifikanz)
plogis(date_model$summary$p.coeff[1]) # nicht Ferientag
plogis(date_model$summary$p.coeff[1] + date_model$summary$p.coeff[2]) # Ferientag

# Plots einlesen

date_model_plots <- plots_date_model(date_model)

# Übersicht non-parametrischer Plots

gridPrint(grobs = date_model_plots$grid,
          top = "Smooth-Plots im Date-Model",
          ncol = 3)

# Einzelplots

# date_model_plots$date
# date_model_plots$day
# date_model_plots$avalanche
# date_model_plots$cloud_cover
# date_model_plots$temperature
# date_model_plots$snow_diff

# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...

# gam.check(date_model$model, type = "deviance")
# concurvity(date_model$model, full = TRUE)
# concurvity(date_model$model, full = FALSE)
# acf(date_model$model$residuals)
# pacf(date_model$model$residuals)


## Day_model


day_model <- day_model_function(min_data_noNA)

day_model$summary # Übersicht (mit Signifikanz)
plogis(day_model$summary$p.coeff[1]) # nicht Ferientag
plogis(day_model$summary$p.coeff[1] + day_model$summary$p.coeff[2]) # Ferientag


# Plots einlesen

day_model_plots <- plots_day_model(day_model)

# Übersicht nonparametrischer Plots

gridPrint(grobs = day_model_plots$grid,
          top = "Smooth-Plots im Day-Model",
          ncol = 3)

# Einzelplots

 day_model_plots$date_time
# day_model_plots$day
# day_model_plots$avalanche
# day_model_plots$cloud_cover
# day_model_plots$temperature
# day_model_plots$snow_diff


# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...

# gam.check(day_model$model, type = "deviance")
# concurvity(day_model$model, full = TRUE)
# concurvity(day_model$model, full = FALSE)
# acf(day_model$model$residuals)
# pacf(day_model$model$residuals)



### Deskriptive Analyse der neuen Daten ########################################

 source("prepare_data_1920.R", encoding = "UTF-8")
 source("messfehler_plots.R", encoding = "UTF-8")
 
 
# Tabelle mit Gesamtwerten der manuellen Zählung nach Art
 
zlg_beide_sums
 
# Anteil "Skitourengänger" an Gesamtzahl der von Studenten gemessenen Personen:
# 
  
zlg_beide_bereinigt_sums$sum[zlg_beide_bereinigt_sums$type == "SG_gesamt"] / 
  zlg_beide_bereinigt_sums$sum[zlg_beide_bereinigt_sums$type == "gesamt"]

# Unterschätzung (Anteil an Checkpointmessungen, die man für den wahren Wert
# hinzufügen müsste):
# 

(zlg_beide_bereinigt_sums$sum[zlg_beide_sums$type == "gesamt"] /
  zlg_beide_bereinigt_sums$sum[zlg_beide_sums$type == "checkpoint"]) - 1

unterschaetzung_plot


## Plots

# Erfassung nach Art
erf_art_plot
# Erfassung nach Zeit
plot(erf_zeit_grid)
#Erfassung nach Gruppengröße (absolut/relativ)
erf_gruppe_abs_plot
erf_gruppe_rel_plot




### Verschiedene Szenarien vergleichen #########################################

set.seed(42)

# Datensätze der Szenarien laden

source("data_general.R", encoding = "UTF-8")
source("data_group.R", encoding = "UTF-8")
source("data_night.R", encoding = "UTF-8")
source("data_temp.R", encoding = "UTF-8")

source("scenarios_plots_date_model.R", encoding = "UTF-8")
source("scenarios_plots_day_model.R", encoding = "UTF-8")

source("scenario_general_plots_date_model.R", encoding = "UTF-8")
source("scenario_general_plots_day_model.R", encoding = "UTF-8")


# Modelle für jedes Szenario fitten

data_general <- data_general_function(0.25)

date_model_general <- date_model_function(data_general$date_data)
date_model_general$summary # Übersicht (mit Signifikanz)
plogis(date_model_general$summary$p.coeff[1]) # nicht Ferientag
plogis(date_model_general$summary$p.coeff[1] +
         date_model_general$summary$p.coeff[2]) # Ferientag

day_model_general <- day_model_function(data_general$data)
day_model_general$summary # Übersicht (mit Signifikanz)
plogis(day_model_general$summary$p.coeff[1]) # nicht Ferientag
plogis(day_model_general$summary$p.coeff[1] +
         day_model_general$summary$p.coeff[2]) # Ferientag


date_model_group <- date_model_function(date_data_group)
date_model_group$summary # Übersicht (mit Signifikanz)
plogis(date_model_group$summary$p.coeff[1]) # nicht Ferientag
plogis(date_model_group$summary$p.coeff[1] +
         date_model_group$summary$p.coeff[2]) # Ferientag

day_model_group <- day_model_function(min_data_group)
day_model_group$summary # Übersicht (mit Signifikanz)
plogis(day_model_group$summary$p.coeff[1]) # nicht Ferientag
plogis(day_model_group$summary$p.coeff[1] +
         day_model_group$summary$p.coeff[2]) # Ferientag


date_model_night <- date_model_function(date_data_night)
date_model_night$summary # Übersicht (mit Signifikanz)
plogis(date_model_night$summary$p.coeff[1]) # nicht Ferientag
plogis(date_model_night$summary$p.coeff[1] +
         date_model_night$summary$p.coeff[2]) # Ferientag

day_model_night <- day_model_function(min_data_night)
day_model_night$summary # Übersicht (mit Signifikanz)
plogis(day_model_night$summary$p.coeff[1]) # nicht Ferientag
plogis(day_model_night$summary$p.coeff[1] +
         day_model_night$summary$p.coeff[2]) # Ferientag

date_model_temp <- date_model_function(date_data_temp)
date_model_temp$summary # Übersicht (mit Signifikanz)
plogis(date_model_temp$summary$p.coeff[1]) # nicht Ferientag
plogis(date_model_temp$summary$p.coeff[1] +
         date_model_temp$summary$p.coeff[2]) # Ferientag

day_model_temp <- day_model_function(min_data_temp)
day_model_temp$summary # Übersicht (mit Signifikanz)
plogis(day_model_temp$summary$p.coeff[1]) # nicht Ferientag
plogis(day_model_temp$summary$p.coeff[1] +
         day_model_temp$summary$p.coeff[2]) # Ferientag

# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...
# Hier das jeweilige Modell einsetzen

# gam.check(day_model$model, type = "deviance")
# concurvity(day_model$model, full = TRUE)
# concurvity(day_model$model, full = FALSE)
# acf(day_model$model$residuals)
# pacf(day_model$model$residuals)


## Plots für den Vergleich der verschiedenen Szenarien

# Für das Date Model

# Übersicht

plots_scenarios_date_model <- list(date_model_plots,
                                   plots_date_model(date_model_general),
                                   plots_date_model(date_model_group),
                                   plots_date_model(date_model_night),
                                   plots_date_model(date_model_temp))

date_model_comparison_plots <- 
date_model_comparison_plots_function(plots_scenarios_date_model)

grid.arrange(date_model_comparison_plots$grid)

# Einzelplots

# date_model_comparison_plots$day
# date_model_comparison_plots$avalanche
# date_model_comparison_plots$cloud_cover
# date_model_comparison_plots$temperature
# date_model_comparison_plots$snow_diff
# date_model_comparison_plots$date

# Für das Day Model

# Übersicht

plots_scenarios_day_model <- list(day_model_plots,
                                   plots_day_model(day_model_general),
                                   plots_day_model(day_model_group),
                                   plots_day_model(day_model_night),
                                   plots_day_model(day_model_temp))

day_model_comparison_plots <- 
  day_model_comparison_plots_function(plots_scenarios_day_model)

grid.arrange(day_model_comparison_plots$grid)

# Einzelplots

# day_model_comparison_plots$day
# day_model_comparison_plots$avalanche
# day_model_comparison_plots$cloud_cover
# day_model_comparison_plots$temperature
# day_model_comparison_plots$snow_diff

# day_model_comparison_plots$date_time_original
# day_model_comparison_plots$date_time_general
# day_model_comparison_plots$date_time_group
# day_model_comparison_plots$date_time_night
# day_model_comparison_plots$date_time_temp


## Plots für den Vergleich von Szenario 1 mit unterschiedlichen Werten

# Für das Date Model

# Übersicht

plots_general_date_model <- 
  list(date_model_plots,
       plots_date_model(date_model_function(data_general_function(0.1)$date_data)),
       plots_date_model(date_model_function(data_general_function(0.2)$date_data)),
       plots_date_model(date_model_function(data_general_function(0.3)$date_data)),
       plots_date_model(date_model_function(data_general_function(0.4)$date_data)),
       plots_date_model(date_model_function(data_general_function(0.5)$date_data)))

date_model_general_comparison_plots <- 
  date_model_general_comparison_function(plots_general_date_model)

grid.arrange(date_model_general_comparison_plots$grid)

# Einzelplots

# plots_scenario_general_date_model_comparison$day
# plots_scenario_general_date_model_comparison$avalanche
# plots_scenario_general_date_model_comparison$cloud_cover
# plots_scenario_general_date_model_comparison$temperature
# plots_scenario_general_date_model_comparison$snow_diff
# plots_scenario_general_date_model_comparison$date

# Für das Day Model

# Übersicht

plots_general_day_model <- 
  list(day_model_plots,
       plots_day_model(day_model_function(data_general_function(0.1)$data)),
       plots_day_model(day_model_function(data_general_function(0.2)$data)),
       plots_day_model(day_model_function(data_general_function(0.3)$data)),
       plots_day_model(day_model_function(data_general_function(0.4)$data)),
       plots_day_model(day_model_function(data_general_function(0.5)$data)))

day_model_general_comparison_plots <- 
  day_model_general_comparison_function(plots_general_day_model)

grid.arrange(day_model_general_comparison_plots$grid)

# Einzelplots

# day_model_general_comparison_plots$day
# day_model_general_comparison_plots$avalanche
# day_model_general_comparison_plots$cloud_cover
# day_model_general_comparison_plots$temperature
# day_model_general_comparison_plots$snow_diff

# day_model_general_comparison_plots$date_time_original
# day_model_general_comparison_plots$date_time_10
# day_model_general_comparison_plots$date_time_20
# day_model_general_comparison_plots$date_time_30
# day_model_general_comparison_plots$date_time_40
# day_model_general_comparison_plots$date_time_50

