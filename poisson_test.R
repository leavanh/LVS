
# Poissonverteilte Zufallszahlen erstellen

pois <- rpois(nrow(date_data), lambda = 20)

# Zum Datensatz hinzufügen

pois_data <- cbind(date_data, pois)

# Modell mit Poissonzielvariable und -family

pois_model <- gam(
                pois ~ s(temperature, bs = "ps", k = 10) +
                  s(snowhight, bs = "ps", k = 20) + 
                  s(solar_radiation, bs = "ps", k = 20) +
                  s(int_date, bs = "ps", k = 30) +
                  s(int_day, bs = "cp", k = 7) + 
                  s(avalanche_report, bs = "ps", k = 5) +
                  holiday,
                data = pois_data,
                family = poisson)

# sollte immer nahe 1 liegen:

pois_model$deviance/pois_model$df.residual
