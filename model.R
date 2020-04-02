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
    avalanche_report + day_weekend + holiday,
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
    avalanche_report + day_weekend + holiday,
  data = date_data,
  family = binomial(link = "logit"))

## Modell 4: Autokorrelation hinzufügen

model_4 <- gamm(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") +
    s(int_date, bs = "ps") +
    avalanche_report + day_weekend + holiday,
  correlation = corAR1(form = ~ int_date),
  data = date_data,
  family = binomial(link = "logit"))

## Modell 5: bisher aufgetretene Probleme lösen
# 1. Knoten kontrollieren
# 2. Wochentage statt nur Wochenende

# wird umcodiert als Zahl

date_data$int_day <- as.integer(date_data$day)

# Modell

model_5 <- gamm(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps", k = 30) +
    s(snowhight, bs = "ps", k = 15) + 
    s(solar_radiation, bs = "ps", k = 10) +
    s(int_date, bs = "ps", k = 30) +
    avalanche_report + day + holiday,
  correlation = corAR1(form = ~ int_date),
  data = date_data,
  family = binomial(link = "logit"))


