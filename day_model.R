# Funktion die das day_model erstellt
# day_model: Uhrzeit der Messung wird außer Acht 
# gelassen (es zählen nur die Tage)

day_model_function <- function(
  date_data # Datensatz ohne Uhrzeit
) {

## Modell
  
# day_model fitten
# bs: "basis spline" (cp: cyclic p-spline, ps: p-spline)
# k: Anzahl der Knoten (können weiter unten auch manuell definiert werden)

day_model <- gam(
  cbind(lvs_true, lvs_false) # Zielvariable (gruppiert nach Tag)
  ~ s(int_date, bs = "ps", k = 15) + 
    s(avalanche_report, bs = "ps", k = 5) + # max. 5 da nicht mehr Ausprägungen
    s(int_day, bs = "cp", k = 7) + # max. 7 Knoten da nicht mehr Ausprägungen
    s(temperature, bs = "ps", k = 10) +
    s(cloud_cover_daily, bs = "ps", k = 10) +
    s(snow_diff, bs = "ps", k = 10) +
    holiday,
  knots = list( # es müssen an jedem Rand noch 2 Knoten außerhalb des 
    # Wertebereichs hinzugefügt werden
    # int_date
    c(17876, 17883, 17890, 17897, 17913, 17920, 17927, 17934, 17941, 17948, 
      17955, 17962, 17969, 17976, 17983, 17990, 17997, 18004, 18011),
    c(), # avalanche report
    c(), # int_day
    # temperature
    c(-14.7, -11.3, -7.9, -4.5, -2.8, -1.4, 0.2, 1.7, 2.3, 3.6, 5.2, 8.4, 9.7, 
      10.3, 11.6),
    # cloud_cover_daily
    c(-15, -7, 0, 5, 15, 30, 50, 63, 77, 84, 90, 95, 100, 105, 110),
    # snow_diff
    c(-27, -20, -13, -6, -5, -4, -4, -3, -1, 0, 2, 10, 40, 70, 100)
  ),
  data = date_data,
  method = "REML", # Methode die die Bestrafung schätzt ist REML
  family = binomial(link = "logit"))

date_Viz <- getViz(day_model) # hilft uns einfacher zu plotten



## Diese Werte gibt die Funktion zurück

day_model_list <- list(
  
  model = day_model,
  
  summary = summary.gam(day_model,
                        dispersion = # der Dispersionsparameter
                          day_model$deviance/day_model$df.residual),
  
  Viz = date_Viz
)

return(day_model_list)
}
