### Modelle

## Modell 1: nur stetige Variablen, Datum nicht beachtet

# Modell spezifizieren

model_1 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature) + s(snowhight) + 
    s(solar_radiation),
  data = date_data,
  family = binomial(link = "logit"),
  method = "REML")

summary(model_1)

# Modelldiagnose checken

par(mfrow = c(2,2))
gam.check(model_1)

# noch was dazu schreiben

# plotten

plot(model_1)

## Modell 2: auch kategorielle Variablen, Datum nicht beachtet

# Modell spezifizieren

model_2 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature) + s(snowhight) + 
    s(solar_radiation) + avalanche_report + day + holiday,
  data = date_data,
  family = binomial(link = "logit"),
  method = "REML")

summary(model_2)

# Modelldiagnose checken

par(mfrow = c(2,2))
gam.check(model_2)

# noch was dazu schreiben

# plotten

plot(model_2)

## Modell 3: Datum hinzufügen

# dazu muss es als integer vorliegen und umcodiert werden

date_data$int_date <- as.integer(as.Date(date_data$date, format = "%d/%m/%Y"))

# Modell spezifizieren

model_3 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature) + s(snowhight) + 
    s(solar_radiation) + 
    s(int_date) + avalanche_report + day + holiday,
  data = date_data,
  family = binomial(link = "logit"),
  method = "REML")

summary(model_3)

# Modelldiagnose checken

par(mfrow = c(2,2))
gam.check(model_3)

# noch was dazu schreiben

# plotten

plot(model_3)

## Modell 3: Datum hinzufügen

# dazu muss es als integer vorliegen und umcodiert werden

date_data$int_date <- as.integer(as.Date(date_data$date, format = "%d/%m/%Y"))

# Modell spezifizieren

model_3 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature) + s(snowhight) + 
    s(solar_radiation) + 
    s(int_date) + avalanche_report + day + holiday,
  data = date_data,
  family = binomial(link = "logit"),
  method = "REML")

summary(model_3)

# Modelldiagnose checken

par(mfrow = c(2,2))
gam.check(model_3)

# noch was dazu schreiben

# plotten

plot(model_3)