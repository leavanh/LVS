# date wird umcodiert als Zahl

date_data$int_date <- as.integer(as.Date(date_data$date, format = "%d/%m/%Y"))

# day wird umcodiert als Zahl

date_data$int_day <- as.integer(date_data$day)

## Modell


date_model <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps", k = 10) +
    s(snowhight, bs = "ps", k = 20) + 
    s(solar_radiation, bs = "ps", k = 20) +
    s(int_date, bs = "ps", k = 30) +
    s(int_day, bs = "cp", k = 7) + 
    s(avalanche_report, bs = "ps", k = 5) +
    holiday,
  data = date_data,
  family = binomial(link = "logit"))

#anschauen

par(mfrow=c(2,2))


summary(date_model)

gam.check(date_model, type = "deviance")

plot(date_model, pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0)
