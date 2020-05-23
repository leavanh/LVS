# Funktion die das day_model erstellt
# day_model: Uhrzeit der Messung wird ebenfalls beachtet

day_model_function <- function(
  min_data_noNA # Datensatz mit Uhrzeit
) {
  
# time wird als Zahl umcodiert
  
min_data_noNA$num_time <- as.numeric(min_data_noNA$time + hours(1)) # 1h extra 
# wegen timezone
  
## Modell

# day-model fitten

day_model <- gam(
  cbind(lvs_true_min, lvs_false_min) ~ s(num_time, int_date, bs = "tp", k = 30) +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    s(temperature, bs = "ps", k = 10) +
    s(solar_radiation_prop, bs = "ps", k = 10) +
    s(snow_diff, bs = "ps", k = 10) +
    holiday,
  knots = list(
    c(), # int_date und int_time
    c(), # avalanche report
    c(), # int_day
    # temperature
    c(-14.7, -11.3, -7.9, -4.5, -2.8, -1.4, 0.2, 1.7, 2.3, 3.6, 5.2, 8.4, 9.7, 
      10.3, 11.6),
    # solar_radiation_prop
    c(),
    # snow_diff
    c(-27, -20, -13, -6, -5, -4, -4, -3, -1, 0, 2, 10, 40, 70, 100)
  ),
  data = min_data_noNA,
  method = "REML",
  family = binomial(link = "logit"))


day_Viz <- getViz(day_model)


## Werte zurückgeben Funktion

day_model_list <- list(
  
  model = day_model,
  
  summary = summary.gam(day_model, 
                        dispersion = day_model$deviance/day_model$df.residual),
  
  Viz = day_Viz
)

return(day_model_list)
}
