### Modelle

# bei allen Modellen sind p-splines verwendet worden

## Modell 1: nur stetige Variablen, Datum nicht beachtet

model_1 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps"),
  data = date_data,
  family = binomial(link = "logit"))

## Modell 2: auch kategorielle Variablen, Datum nicht beachtet

model_2 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") + 
    avalanche_report + day + holiday,
  data = date_data,
  family = binomial(link = "logit"))

## Modell 3: Datum hinzufügen (noch nicht als Autokorrelation)

# dazu muss es als integer vorliegen und umcodiert werden

date_data$int_date <- as.integer(as.Date(date_data$date, format = "%d/%m/%Y"))

# Modell spezifizieren

model_3 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") + 
    s(int_date, bs = "ps") + 
    avalanche_report + day + holiday,
  data = date_data,
  family = binomial(link = "logit"))
