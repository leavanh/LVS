# Funktion die das time_model erstellt
# time_model: Uhrzeit der Messung wird ebenfalls beachtet

time_model_function <- function(
  min_data_noNA # Datensatz mit Uhrzeit
) {
  
# time wird als Zahl umcodiert
# das ist für die Berechnung nötig, hat inhaltlich aber keine Auswirkungen
  
min_data_noNA$num_time <- as.numeric(min_data_noNA$time)
  
## Modell

# time_model fitten
# bs: "basis spline" (tp: thin plate, cp: cyclic p-spline, ps: p-spline)
# k: Anzahl der Knoten (können weiter unten auch manuell definiert werden)

time_model <- gam(
  cbind(lvs_true_min, lvs_false_min) # Zielvariable (gruppiert nach Minute)
  ~ s(num_time, int_date, bs = "tp", k = 30) +
    s(int_day, bs = "cp", k = 7) + # max. 7 Knoten da nicht mehr Ausprägungen
    s(avalanche_report, bs = "ps", k = 5) + # max. 5 da nicht mehr Ausprägungen
    s(temperature, bs = "ps", k = 10) +
    s(cloud_cover, bs = "ps", k = 10) +
    s(snow_diff, bs = "ps", k = 10) +
    holiday,
  knots = list( # es müssen an jedem Rand noch 2 Knoten außerhalb des 
    # Wertebereichs hinzugefügt werden
    c(), # int_date und int_time
    c(), # avalanche report
    c(), # int_day
    # temperature
    c(-14.7, -11.3, -7.9, -4.5, -2.8, -1.4, 0.2, 1.7, 2.3, 3.6, 5.2, 8.4, 9.7, 
      10.3, 11.6),
    # cloud_cover
    c(-15, -5, 0, 1, 2, 3, 5.4, 21, 71, 80, 90, 95, 100, 105, 115),
    # snow_diff
    c(-27, -20, -13, -6, -5, -4, -4, -3, -1, 0, 2, 10, 40, 70, 100)
  ),
  data = min_data_noNA,
  method = "REML", # Methode die die Bestrafung schätzt ist REML
  family = binomial(link = "logit"))


day_Viz <- getViz(time_model) # hilft uns einfacher zu plotten


## Diese Werte gibt die Funktion zurück

time_model_list <- list(
  
  model = time_model,
  
  summary = summary.gam(time_model, 
                        dispersion = # der Dispersionsparameter
                          time_model$deviance/time_model$df.residual),
  
  Viz = day_Viz
)

return(time_model_list)
}
