
### Vorbereitung ###############################################################

## Pakete laden

library("tidyverse") # für Datenbearbeitung und Plots
library("patchwork") # um Plots nebeneinander anzuzeigen
library("GGally") # plottet mehrere Variablen gleichzeitig
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

# NAs (fehlende Werte) rauswerfen

date_data_noNA <- date_data[!is.na(date_data$count_people),]
data_noNA <- data[!is.na(data$lvs),]
min_data_noNA <- min_data[!is.na(min_data$count_people_min),]



### Deskriptive Analyse ########################################################

# Einige Plots die die Daten deskriptiv beschreiben
# Ausgegraut an den Tagen wo keine Checkpointmessungen möglich waren

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

# Hier werden die beiden Modelle auf die Daten angewandt
# Um etwas am Modell zu ändern muss die jeweilige in source genannte Datei
# bearbeitet werden
# Hier zu sehen sind nur die Ergebnisse
# Außerdem sind im Anschluss Befehle zur Modelldiagnose gegeben

source("day_model.R", encoding = "UTF-8")
source("time_model.R", encoding = "UTF-8")

source("smooth_plots_day_model.R", encoding = "UTF-8")
source("smooth_plots_time_model.R", encoding = "UTF-8")


## Tagesmodell
# Daten sind gruppiert nach Tag

day_model <- day_model_function(date_data_noNA) # Modell fitten

day_model$summary # Übersicht (mit Signifikanz)
# die hier gezeigten Werte für Intercept und Ferientag müssen noch zurück-
# transformiert werden um als zu erwartender Anteil von Personen mit LVS-
# Gerät (wenn die anderen Variablen konstant) interpretiert werden zu können

# Die transformierten Werte sind in den folgenden zwei Zeilen zu sehen

plogis(day_model$summary$p.coeff[1]) # Intercept
plogis(day_model$summary$p.coeff[1] + day_model$summary$p.coeff[2]) # Ferientag


# Plots einlesen

day_model_plots <- plots_day_model(day_model) 

# Übersicht non-parametrischer Plots

gridPrint(grobs = day_model_plots$grid,
          top = "Smooth-Plots im Tagesmodell",
          ncol = 3)

# Einzelplots (zur Ausgabe den Hashtag in der jeweiligen Zeile entfernen)

# day_model_plots$date
# day_model_plots$day
# day_model_plots$avalanche
# day_model_plots$cloud_cover
# day_model_plots$temperature
# day_model_plots$snow_diff

# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...

# gam.check() kann einem zeigen, ob die Anzahl der Knoten groß genug ist:
# gam.check(day_model$model, type = "deviance")

# Hier kann man sehen, ob concurvity auftritt:
# je größer die Zahl desto größer die concurvity
# zur besseren Interpretation der Zahlen davor einmal
# options(scipen = 999)
# ausführen
# concurvity(day_model$model, full = TRUE)
# concurvity(day_model$model, full = FALSE)

# Untersuchung auf Autokorrelation:
# acf ist der Plot aus der Präsentation
# pacf zeigt die partielle Autokorrelation
# acf(day_model$model$residuals)
# pacf(day_model$model$residuals)


## Zeitmodell

# Daten sind gruppiert nach Minute


time_model <- time_model_function(min_data_noNA) # Modell fitten

time_model$summary # Übersicht (mit Signifikanz)
plogis(time_model$summary$p.coeff[1]) # Intercept
plogis(time_model$summary$p.coeff[1] + time_model$summary$p.coeff[2]) # Ferientag


# Plots einlesen

time_model_plots <- plots_time_model(time_model)

# Übersicht nonparametrischer Plots

gridPrint(grobs = time_model_plots$grid,
          top = "Smooth-Plots im Zeitmodell",
          ncol = 3)

# Einzelplots

 time_model_plots$date_time
# time_model_plots$day
# time_model_plots$avalanche
# time_model_plots$cloud_cover
# time_model_plots$temperature
# time_model_plots$snow_diff


# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...

# gam.check(time_model$model, type = "deviance")
# concurvity(time_model$model, full = TRUE)
# concurvity(time_model$model, full = FALSE)
# acf(time_model$model$residuals)
# pacf(time_model$model$residuals)



### Deskriptive Analyse der Messfehler-Daten ###################################
 
# Es wurde die Zählung der Studenten mit den vom Messgerät übermittelten Daten
# verglichen
# Da die Messgerät-Uhrzeit nicht immer mit der Studenten-Uhrzeit übereinstimmt
# wurden für Vergleiche, bei denen die Uhrzeit eine Rolle spielt (Gruppengröße,
# Messungen nach Uhrzeit), nur die Angaben der Studenten zur Erfassung
# verglichen. Für die Berechnung der Unterschätzung insgesamt wurden jedoch alle
# Messungen der Studenten zusammengezählt und mit der Anzahl der tatsächlich vom
# Checkpoint erfassten Messungen verglichen.

source("read_data_messfehler.R", encoding = "UTF-8") # Warnmeldung ignorieren
source("messfehler_plots.R", encoding = "UTF-8")
 
 
# Tabelle mit Gesamtwerten der manuellen Zählung nach Art
# SG: Skitourengänger
# aK: anderer Kontakt
# beide: beide zusammengerechnet
 
zlg_beide_sums
 
# Anteil "Skitourengänger" an Gesamtzahl der von Studenten gemessenen Personen:
  
zlg_beide_bereinigt_sums$sum[zlg_beide_bereinigt_sums$type == "SG_gesamt"] / 
  zlg_beide_bereinigt_sums$sum[zlg_beide_bereinigt_sums$type == "gesamt"]

# Unterschätzung (Anteil an Checkpointmessungen, die man für den wahren Wert
# hinzufügen müsste): 22,4%

(zlg_beide_bereinigt_sums$sum[zlg_beide_sums$type == "gesamt"] /
  zlg_beide_bereinigt_sums$sum[zlg_beide_sums$type == "checkpoint"]) - 1

## Plots

# absolute Anzahl der Checkpointmessunge gegenpber alle von Studenten
# angegebenen Kontakte

unterschaetzung_plot

# Anzahl der von Studenten angegebenen Kontakte, unterteil nacht Art des
# Kontakts und ob erfasst oder nicht erfasst

erf_art_plot

# Anzahl der erfassten/nicht erfasstenKontakte nach Uhrzeit, aufgeteilt für 
# jeden Zeitraum der manuellen Messungen

plot(erf_zeit_grid)

# Anzahl und Anteil der erfassten/nicht erfassten Kontakte nach Gruppengröße

erf_gruppe_abs_plot
erf_gruppe_rel_plot # Warnung ignorieren



### Verschiedene Szenarien vergleichen #########################################

# Für jedes mit euch besprochene Szenario wird hier ein neuer Datensatz erzeugt
# Danach werden die Modelle darauf angewandt

# Um zu sehen wie genau die Datensätze erzeugt werden müsst ihr in die in
# source() genannten Dateien schauen


# Da die Daten zufällig erzeugt werden benutzen wir einen seed
# so kommt bei jedem Durchlauf das selbe Ergebnis raus

set.seed(42)

# Datensätze der Szenarien laden

# Generelle Unterschätzung

data_general <- readRDS(file = "Daten/data_general.RDS")
data_general_10 <- readRDS(file = "Daten/data_general_10.RDS")
data_general_20 <- readRDS(file = "Daten/data_general_20.RDS")
data_general_30 <- readRDS(file = "Daten/data_general_30.RDS")
data_general_40 <- readRDS(file = "Daten/data_general_40.RDS")
data_general_50 <- readRDS(file = "Daten/data_general_50.RDS")

# Je mehr Menschen, desto mehr wird unterschätzt

min_data_group <- readRDS(file = "Daten/min_data_group.RDS")
date_data_group <- readRDS(file = "Daten/date_data_group.RDS")

# In der Nacht werden 50% der nicht-LVS-Messungen entfernt

min_data_night <- readRDS(file = "Daten/min_data_night.RDS")
date_data_night <- readRDS(file = "Daten/date_data_night.RDS")

# Je niedriger die Temperatur desto mehr Messungen werden hinzugefügt

min_data_temp <- readRDS(file = "Daten/min_data_temp.RDS")
date_data_temp <- readRDS(file = "Daten/date_data_temp.RDS")

# Die folgenden Dateien erzeugen die Plots

# Funktionen zur Erstellung der Vergleichsplots laden

source("scenarios_plots_day_model.R", encoding = "UTF-8")
source("scenarios_plots_time_model.R", encoding = "UTF-8")

source("scenario_general_plots_day_model.R", encoding = "UTF-8")
source("scenario_general_plots_time_model.R", encoding = "UTF-8")


# Modelle für jedes Szenario fitten
# Die Modelle anzuwenden läuft genauso ab wie auch weiter oben:

day_model_general <- day_model_function(data_general$date_data)
day_model_general$summary # Übersicht (mit Signifikanz)
plogis(day_model_general$summary$p.coeff[1]) # Intercept
plogis(day_model_general$summary$p.coeff[1] +
         day_model_general$summary$p.coeff[2]) # Ferientag

time_model_general <- time_model_function(data_general$min_data)
time_model_general$summary # Übersicht (mit Signifikanz)
plogis(time_model_general$summary$p.coeff[1]) # Intercept
plogis(time_model_general$summary$p.coeff[1] +
         time_model_general$summary$p.coeff[2]) # Ferientag


day_model_group <- day_model_function(date_data_group)
day_model_group$summary # Übersicht (mit Signifikanz)
plogis(day_model_group$summary$p.coeff[1]) # Intercept
plogis(day_model_group$summary$p.coeff[1] +
         day_model_group$summary$p.coeff[2]) # Ferientag

time_model_group <- time_model_function(min_data_group)
time_model_group$summary # Übersicht (mit Signifikanz)
plogis(time_model_group$summary$p.coeff[1]) # Intercept
plogis(time_model_group$summary$p.coeff[1] +
         time_model_group$summary$p.coeff[2]) # Ferientag


day_model_night <- day_model_function(date_data_night)
day_model_night$summary # Übersicht (mit Signifikanz)
plogis(day_model_night$summary$p.coeff[1]) # Intercept
plogis(day_model_night$summary$p.coeff[1] +
         day_model_night$summary$p.coeff[2]) # Ferientag

time_model_night <- time_model_function(min_data_night)
time_model_night$summary # Übersicht (mit Signifikanz)
plogis(time_model_night$summary$p.coeff[1]) # Intercept
plogis(time_model_night$summary$p.coeff[1] +
         time_model_night$summary$p.coeff[2]) # Ferientag

day_model_temp <- day_model_function(date_data_temp)
day_model_temp$summary # Übersicht (mit Signifikanz)
plogis(day_model_temp$summary$p.coeff[1]) # Intercept
plogis(day_model_temp$summary$p.coeff[1] +
         day_model_temp$summary$p.coeff[2]) # Ferientag

time_model_temp <- time_model_function(min_data_temp)
time_model_temp$summary # Übersicht (mit Signifikanz)
plogis(time_model_temp$summary$p.coeff[1]) # Intercept
plogis(time_model_temp$summary$p.coeff[1] +
         time_model_temp$summary$p.coeff[2]) # Ferientag

# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...
# Hier das jeweilige Modell einsetzen

# gam.check(time_model$model, type = "deviance")
# concurvity(time_model$model, full = TRUE)
# concurvity(time_model$model, full = FALSE)
# acf(time_model$model$residuals)
# pacf(time_model$model$residuals)


## Plots für den Vergleich der verschiedenen Szenarien

# Für das Tagesmodell

# Erst werden für die Erzeugung der Plots Listen erstellt

# Liste mit Tagesmodell je Szenario

scenarios_day_model <- 
  list(day_model,
       day_model_general,
       day_model_group,
       day_model_night,
       day_model_temp)

# Liste mit Plots je Szenario für das Tagesmodell

plots_scenarios_day_model <- list(day_model_plots,
                                   plots_day_model(day_model_general),
                                   plots_day_model(day_model_group),
                                   plots_day_model(day_model_night),
                                   plots_day_model(day_model_temp))

# Liste mit Vergleichsplots

day_model_comparison_plots <- 
day_model_comparison_plots_function(plots_scenarios_day_model)

# Hier werden tatsächlich Plots ausgegeben

# Überblick über Vergleichsplots

grid.arrange(day_model_comparison_plots$grid)

# Einzelplots

# day_model_comparison_plots$day
# day_model_comparison_plots$avalanche
# day_model_comparison_plots$cloud_cover
# day_model_comparison_plots$temperature
# day_model_comparison_plots$snow_diff
# day_model_comparison_plots$date

# Für das Zeitmodell

# Erstellung von Listen

# Liste mit Zeitmodell je Szenario

scenarios_time_model <- 
  list(time_model,
       time_model_general,
       time_model_group,
       time_model_night,
       time_model_temp)

# Liste mit Plots je Szenario für das Zeitmodell

plots_scenarios_time_model <- list(time_model_plots,
                                   plots_time_model(time_model_general),
                                   plots_time_model(time_model_group),
                                   plots_time_model(time_model_night),
                                   plots_time_model(time_model_temp))

# Liste mit Vergleichsplots

time_model_comparison_plots <- 
  time_model_comparison_plots_function(plots_scenarios_time_model)

# Hier werden die Plots tatssächlich ausgegeben

# Überblick über Vergleichsplots

grid.arrange(time_model_comparison_plots$grid)

# Einzelplots

# time_model_comparison_plots$day
# time_model_comparison_plots$avalanche
# time_model_comparison_plots$cloud_cover
# time_model_comparison_plots$temperature
# time_model_comparison_plots$snow_diff

# time_model_comparison_plots$date_time_original
# time_model_comparison_plots$date_time_general
# time_model_comparison_plots$date_time_group
# time_model_comparison_plots$date_time_night
# time_model_comparison_plots$date_time_temp


# Tabellen mit p-Werten für das jeweilige Modell

# Datei, in der die Tabellen erstellt werden, laden

source("p_values_scenarios.R", encoding = "UTF-8")

# Tabelle mit p-Werten je Szenario für das Tagesmodell

p_values_scenarios_day_model

# Tabelle mit p-Werten je Szenario für das Zeitmodell

p_values_scenarios_time_model






## Plots für den Vergleich von Szenario 1 (generelle Unterschätzung) mit 
# unterschiedlichen Werten

# es werden in 10%-Schritten mehr Daten dem Szenario "generelle Unterschätzung"
# folgend hinzugefügt

# Für das Tagesmodell

# Listen erstellen

# Liste mit Modellen je Anteil

general_day_model <- 
  list(day_model,
       day_model_function(data_general_10$date_data),
       day_model_function(data_general_20$date_data),
       day_model_function(data_general_30$date_data),
       day_model_function(data_general_40$date_data),
       day_model_function(data_general_50$date_data))

# Liste mit Plots je Anteil

plots_general_day_model <- 
  list(day_model_plots,
       plots_day_model(general_day_model[[2]]),
       plots_day_model(general_day_model[[3]]),
       plots_day_model(general_day_model[[4]]),
       plots_day_model(general_day_model[[5]]),
       plots_day_model(general_day_model[[6]]))

# Liste mit Vergleichsplots

day_model_general_comparison_plots <- 
  day_model_general_comparison_function(plots_general_day_model)

# Hier werden tatsächlich Plots ausgegeben

# Überblick über Vergleichsplots

grid.arrange(day_model_general_comparison_plots$grid)

# Einzelplots

# plots_scenario_general_day_model_comparison$day
# plots_scenario_general_day_model_comparison$avalanche
# plots_scenario_general_day_model_comparison$cloud_cover
# plots_scenario_general_day_model_comparison$temperature
# plots_scenario_general_day_model_comparison$snow_diff
# plots_scenario_general_day_model_comparison$date

# Für das Zeitmodell

# Übersicht

# Listen erstellen

# Liste mit Modellen je Anteil

general_time_model <- 
  list(time_model,
       time_model_function(data_general_10$min_data),
       time_model_function(data_general_20$min_data),
       time_model_function(data_general_30$min_data),
       time_model_function(data_general_40$min_data),
       time_model_function(data_general_50$min_data))

# Liste mit Plots je Anteil

plots_general_time_model <- 
  list(time_model_plots,
       plots_time_model(general_time_model[[2]]),
       plots_time_model(general_time_model[[3]]),
       plots_time_model(general_time_model[[4]]),
       plots_time_model(general_time_model[[5]]),
       plots_time_model(general_time_model[[6]]))

# Liste mit Vergleichsplots

time_model_general_comparison_plots <- 
  time_model_general_comparison_function(plots_general_time_model)

# Hier werden tatsächlich Plots ausgegeben

# Überblick über Vergleichsplots

grid.arrange(time_model_general_comparison_plots$grid)

# Einzelplots

# time_model_general_comparison_plots$day
# time_model_general_comparison_plots$avalanche
# time_model_general_comparison_plots$cloud_cover
# time_model_general_comparison_plots$temperature
# time_model_general_comparison_plots$snow_diff

# time_model_general_comparison_plots$date_time_original
# time_model_general_comparison_plots$date_time_10
# time_model_general_comparison_plots$date_time_20
# time_model_general_comparison_plots$date_time_30
# time_model_general_comparison_plots$date_time_40
# time_model_general_comparison_plots$date_time_50


# Tabellen mit p-Werten für das jeweilige Modell

# Date, in der die Tabellen erstellt werden, laden

source("p_values_general.R", encoding = "UTF-8")

# Tabelle mit p-Werten je Anteil für das Tagesmodell

p_values_general_day_model

# Tabelle mit p-Werten je Anteil für das Zeitmodell

p_values_general_time_model

