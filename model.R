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
# nur stetige Variablen

model_4 <- gamm(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") +
    s(int_date, bs = "ps"),
  correlation = corAR1(form = ~ int_date),
  data = date_data,
  family = binomial(link = "logit"))

## Modell 5: kategorielle Variablen hinzufügen

model_5 <- gamm(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") +
    s(int_date, bs = "ps") +
  avalanche_report + day_weekend + holiday,
  correlation = corAR1(form = ~ int_date),
  data = date_data,
  family = binomial(link = "logit"))

## Modell 6: bisher aufgetretene Probleme lösen

model_6 <- gamm(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") +
    avalanche_report + day_weekend + holiday,
    correlation = corAR1(form = ~ int_date),
  data = date_data,
  family = binomial(link = "logit"))

