# Funktion die das day_model erstellt
# day_model: Uhrzeit der Messung wird ebenfalls beachtet

day_model_function <- function(
  min_data_noNA # Datensatz mit Uhrzeit
) {
  
# time wird als Zahl umcodiert
  
min_data_noNA$num_time <- as.numeric(min_data_noNA$time)
  
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
