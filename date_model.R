# Funktion die das date_model erstellt
# date_model: Uhrzeit der Messung wird außer Acht 
# gelassen (es zählen nur die Tage)

date_model_function <- function(
  date_data # Datensatz ohne Uhrzeit
) {

## Modell

date_model <- gam(
  cbind(lvs_true, lvs_false) ~ s(int_date, bs = "ps", k = 15) + 
    s(avalanche_report, bs = "ps", k = 5) +
    s(int_day, bs = "cp", k = 7) +
    s(temperature, bs = "ps", k = 10) +
    s(cloud_cover_daily, bs = "ps", k = 10) +
    s(snow_diff, bs = "ps", k = 10) +
    holiday,
  knots = list(
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
  method = "REML",
  family = binomial(link = "logit"))

date_Viz <- getViz(date_model)



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
