# Funktion die das day_model erstellt
# day_model: Uhrzeit der Messung wird ebenfalls beachtet

day_model_function <- function(
  min_data_noNA # Datensatz mit Uhrzeit
) {
  
# time wird als Zahl umcodiert
  
min_data_noNA$num_time <- as.numeric(min_data_noNA$time)
  
## Modell

# day-model fitten (ohne autocorrelation)

day_model <- gam(
  cbind(lvs_true_min, lvs_false_min) ~ s(num_time, int_date, bs = "tp", k = 40) +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    s(res_temperature, bs = "ps", k = 15) +
    s(res_solar_radiation, bs = "ps", k = 15) +
    s(res_snowhight, bs = "ps", k = 15) +
    holiday,
  data = min_data_noNA,
  method = "REML",
  family = binomial(link = "logit"))

# day_model2 (mit autocorrelation)

day_model2 <- gamm(
  cbind(lvs_true_min, lvs_false_min) ~ s(num_time, int_date, bs = "tp", k = 40) +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    s(res_temperature, bs = "ps", k = 15) +
    s(res_solar_radiation, bs = "ps", k = 15) +
    s(res_snowhight, bs = "ps", k = 15) +
    holiday,
  data = min_data_noNA,
  correlation = corCAR1(0.138, form = ~ int_date*num_time),
  method = "REML",
  family = binomial(link = "logit"))

day_Viz <- getViz(day_model)
day_Viz2 <- getViz(day_model2$gam)


## Werte zurückgeben Funktion

day_model_list <- list(
  
  model = day_model,
  model_gamm = day_model2,
  
  summary = summary.gam(day_model, 
                        dispersion = day_model$deviance/day_model$df.residual),
  summary_gamm = summary.gam(day_model2$gam),
  
  Viz = day_Viz,
  Viz_gamm = day_Viz2
)

return(day_model_list)
}
