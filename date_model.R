# Funktion die das date_model erstellt
# date_model: Uhrzeit der Messung wird außer Acht 
# gelassen (es zählen nur die Tage)

date_model_function <- function(
  date_data # Datensatz ohne Uhrzeit
) {

## Modell

date_model <- gam(
  cbind(lvs_true, lvs_false) ~ s(int_date, bs = "ps", k = 13) + 
    s(avalanche_report, bs = "ps", k = 5) +
    s(int_day, bs = "cp", k = 7) +
    s(temperature, bs = "ps", k = 10) +
    s(solar_radiation_prop, bs = "ps", k = 10) +
    s(snow_diff, bs = "ps", k = 10) +
    holiday,
  knots = list(
    # int_date
    c(17880, 17885, 17890, 17895, 17915, 17920, 17925, 17935, 17945, 17955,
      17965, 17975, 17985, 17995, 18000, 18005, 18010),
    c(), # avalanche report
    c(), # int_day
    # temperature
    c(-15, -12, -7, -3, -2, -1, 0, 1, 2, 3, 7, 11, 14, 17),
    # solar_radiation_prop
    c(),
    # snow_diff
    c(-30, -25, -15, -10, -7, -5, -2, 0, 2, 6, 10, 20, 50, 60)
  ),
  data = date_data,
  method = "REML",
  family = binomial(link = "logit"))

date_Viz <- getViz(date_model)

# Knoten extrahieren
# date_model$smooth[[n_smooth]]$knots

## Werte zurückgeben Funktion

date_model_list <- list(
  
  model = date_model,
  
  summary = summary.gam(date_model,
                        dispersion = date_model$deviance/date_model$df.residual),
  # use plogis() to convert to a probability
  
  Viz = date_Viz
)

return(date_model_list)
}
