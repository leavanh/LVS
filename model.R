### Modelle

# bei allen Modellen sind p-splines verwendet worden

## Modell 1: nur stetige Variablen, Datum nicht beachtet

# Modell spezifizieren

model_1 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps"),
  data = date_data,
  family = binomial(link = "logit"))

summary(model_1)

# Diagnostikplots checken

par(mfrow = c(2,2))
gam.check(model_1)

# Verteilung falsch! Autokorrelierte Daten nicht beachtet

# plotten

plot(model_1, pages = 1, residuals = TRUE)

## Modell 2: auch kategorielle Variablen, Datum nicht beachtet

# Modell spezifizieren

model_2 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") + 
    avalanche_report + day + holiday,
  data = date_data,
  family = binomial(link = "logit"))

summary(model_2)

# Diagnostikplots checken

par(mfrow = c(2,2))
gam.check(model_2)

# Verteilung falsch! Autokorrelierte Daten nicht beachtet
# ein Ausreißer ist in response vs. fitted values zu sehen

# plotten

plot(model_2, pages = 1, residuals = TRUE)

## Modell 3: Datum hinzufügen

# dazu muss es als integer vorliegen und umcodiert werden

date_data$int_date <- as.integer(as.Date(date_data$date, format = "%d/%m/%Y"))

# Modell spezifizieren

model_3 <- gamm(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") + 
    avalanche_report + day + holiday,
  correlation = corAR1(form = ~ int_date),
  data = date_data,
  family = binomial(link = "logit"))

summary(model_3)

# Diagnostikplots checken

par(mfrow = c(2,2))
gam.check(model_3)

# noch was dazu schreiben

# plotten

plot(model_3, pages = 1, residuals = TRUE)
