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

# Modell 5: Wochentage statt nur Wochenende und avalanche_report als smooth

# day wird umcodiert als Zahl

date_data$int_day <- as.integer(date_data$day)

# Modell

model_5 <- gamm(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") +
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") +
    s(int_date, bs = "ps") +
    s(int_day, bs = "cc", k = 7) + 
    s(avalanche_report, bs = "ps", k = 5) +
    holiday,
  correlation = corAR1(form = ~ int_date),
  data = date_data,
  family = binomial(link = "logit"))


## Auswertung

# Diagnostikplots immer als 2x2 darstellen

par(mfrow=c(2,2))

# Modell 1: nur stetige Variablen, Datum nicht beachtet

summary(model_1)

gam.check(model_1)

# Verteilung falsch! Autokorrelierte Daten nicht beachtet
# response vs. fitted values: ein paar response bei 0

plot(model_1, pages = 1, residuals = TRUE, pch = 19, cex = .3)
# auch durch linear zu ersetzen teilweise?
# oversmoothing bei solar radiation?

# Modell 2: auch kategorielle Variablen, Datum nicht beachtet

summary(model_2)

gam.check(model_2)
# Verteilung falsch! Autokorrelierte Daten nicht beachtet
# response vs. fitted values: ein paar response bei 0

plot(model_2, pages = 1, residuals = TRUE, pch = 19, cex = .3)
# siehe model_1

# Modell 3: Datum hinzufügen (noch nicht als Autokorrelation)

summary(model_3)

gam.check(model_3)
# Verteilung falsch! Autokorrelierte Daten nicht beachtet
# k ist zu niedrig

plot(model_3, pages = 1, residuals = TRUE, pch = 19, cex = .3)
# temperatur vielleicht nur linear rein?

# Modell 4: Autokorrelation hinzufügen

summary(model_4$gam)

plot(model_4$gam, pages = 1, residuals = TRUE, pch = 19, cex = .3)
# auch durch linear zu ersetzen bei temperature?

# Modell 5: Wochentage statt nur Wochenende und avalanche_report als smooth

summary(model_5$gam)

plot(model_5$gam, pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0)