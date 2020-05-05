# Funktion die das date_model erstellt
# date_model: Uhrzeit der Messung wird außer Acht 
# gelassen (es zählen nur die Tage)

date_model_function <- function(
  date_data # Datensatz ohne Uhrzeit
) {

## Modell

date_model <- gam(
  cbind(lvs_true, lvs_false) ~ s(int_date, bs = "ps", k = 25) + 
    s(avalanche_report, bs = "ps", k = 5) +
    s(int_day, bs = "cp", k = 7) +
    s(res_temperature, bs = "ps", k = 15) +
    s(res_solar_radiation, bs = "ps", k = 15) +
    s(res_snowhight, bs = "ps", k = 15) +
    holiday,
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

source("smooth_plots_date_model.R")

return(date_model_list)
}
