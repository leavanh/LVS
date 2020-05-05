## Pakete laden

library("tidyverse")
library("patchwork")
library("GGally")
library("mgcv")
library("mgcViz")
library("lubridate")
library("gridExtra")
library("ggformula")

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

solar_radiation_max

## Modell fitting


# Date

date_model <- date_model_function(date_data)
date_model$summary # Übersicht (mit Signifikant)
print(plot(date_model$Viz, trans = plogis) + 
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model$summary$p.coeff) # parametrische Effekt

# Plots zum Date Model

source("smooth_plots_date_model.R", encoding = "UTF-8")

date_model_date
date_model_day
date_model_avalanche
date_model_solar_radiation
date_model_solar_radiation
date_model_temperature
date_model_snowhight



# Möglichkeiten das Modell zu "checken"
# Passen die Basis-Funktionen? Gibt es Autocorrelation? ...

#gam.check(date_model$model, type = "deviance")
#concurvity = concurvity(date_model$model, full = TRUE)
#concurvity(date_model$model, full = FALSE)
#acf(date_model$model$residuals)
#pacf(date_model$model$residuals)

# Day

start <- print(Sys.time()) # we want to know how long computation takes

day_models <- day_model_function(min_data_noNA)
day_model <- day_models$model
day_model_gamm <- day_models$model_gamm

saveRDS(day_model_gamm, file = "day_model_gamm.RDS")

end <- print(Sys.time())
print(end - start)

# Plots zum Day Model

source("smooth_plots_day_model.R", encoding = "UTF-8")


day_model_date_time
day_model_day
day_model_avalanche
day_model_solar_radiation
day_model_solar_radiation
day_model_temperature
day_model_snowhight


## folgendes bitte ignorieren ich brauchte nur nen platz das kurz zu speichern

# min_data_noNA_sample <- min_data_noNA[sample(nrow(min_data_noNA), 1000), ]

# ## Untersuchen
# 
# par(mfrow=c(2,2))
# 
# #anschauen
# 
# day_model
# 
# summary.gam(day_model, dispersion = day_model$deviance/day_model$df.residual)
# summary.gam(day_model2$gam)
# # use plogis() to convert to a probability
# 
# gam.check(day_model)
# gam.check(day_model2$gam)
# # plot(day_model2$gam$linear.predictors, day_model2$lme$residuals[, "fixed"])
# 
# #concurvity(day_model, full = TRUE)
# #concurvity(day_model, full = FALSE)
# acf(day_model$residuals)
# pacf(day_model$residuals)
# acf(day_model2$lme$residuals[, "fixed"])
# pacf(day_model2$lme$residuals[, "fixed"])
# 
# # ROC Kurve
# #plot.roc(data_noNA$lvs, day_model$fitted.values) # setting levels?
# 
# AIC(day_model)
# 
# # als gamViz speichern
# 
# day_Viz <- getViz(day_model)
# day_Viz2 <- getViz(day_model2$gam)
# 
# print(plot(day_Viz, shade = TRUE, seWithMean = TRUE,
#            shift = coef(day_model)[1], trans = plogis) + ylim(0,1), pages = 1)
# 
# plot(sm(day_Viz, select = 1), trans = plogis)  + labs(y="Datum", x="Uhrzeit") + 
#   l_fitRaster() + l_rug() +
#   scale_y_continuous(breaks=c(17910,17940,17970,18000), 
#                      labels=c("14-01-2019","13-02-2019","15-03-2019","14-04-2019")) +
#   scale_x_continuous(breaks=c(-2209060800,-2209050000,-2209039200,-2209028400,
#                               -2209017600, -2209006800,
#                               -2208996000, -2208985200, -2208974460), 
#                      labels=c("04:00","07:00","10:00","13:00", "16:00", "19:00",
#                               "22:00", "01:00", "03:59")) +
#   ggtitle("Smoothfunktion für Uhrzeit und Datum")
# 
# print(plot(day_Viz2, shade = TRUE, seWithMean = TRUE,
#            shift = coef(day_model)[1], trans = plogis) + ylim(0,1), pages = 1)
# plot(sm(day_Viz2, select = 1), trans = plogis) + l_fitRaster() + l_rug()
# 
# # plot(day_model, 
# #      pages = 1, residuals = FALSE, pch = 19, cex = .3, scale = 0, 
# #      shade = TRUE, seWithMean = TRUE, shift = coef(day_model)[1],
# #      trans = plogis)
# # plot(sm(day_Viz, 6), trans = plogis) +
# #   l_fitRaster() + l_fitContour() + l_points()

## Verschiedene Szenarien vergleichen

set.seed(42)

# neue Daten laden

source("data_general.R", encoding = "UTF-8")
source("data_group.R", encoding = "UTF-8")
source("data_night.R", encoding = "UTF-8")
source("data_temp.R", encoding = "UTF-8")


# Modelle fitten

date_model_general <- date_model_function(date_data_general)
date_model_general$summary # Übersicht (mit Signifikant)
print(plot(date_model_general$Viz, trans = plogis) +
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model_general$summary$p.coeff) # parametrische Effekt

date_model_group <- date_model_function(date_data_group)
date_model_group$summary # Übersicht (mit Signifikant)
print(plot(date_model_group$Viz, trans = plogis) +
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model_group$summary$p.coeff) # parametrische Effekt

date_model_night <- date_model_function(date_data_night)
date_model_night$summary # Übersicht (mit Signifikant)
print(plot(date_model_night$Viz, trans = plogis) +
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model_night$summary$p.coeff) # parametrische Effekt

date_model_temp <- date_model_function(date_data_temp)
date_model_temp$summary # Übersicht (mit Signifikant)
print(plot(date_model_temp$Viz, trans = plogis) +
        ylim(0,1), pages = 1) # non-parametrische Plots
plogis(date_model_temp$summary$p.coeff) # parametrische Effekt
