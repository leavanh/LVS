# date wird umcodiert als Zahl

data$int_date <- as.integer(as.Date(data$date, format = "%d/%m/%Y"))

# day wird umcodiert als Zahl

data$int_day <- as.integer(data$day)

## Modell

day_model <- gam(
  type ~ s(temperature, bs = "ps", k = 10) +
    s(snowhight, bs = "ps", k = 20) + 
    s(solar_radiation, bs = "ps", k = 20) +
    s(int_date, bs = "ps", k = 30) +
    s(int_day, bs = "cp", k = 7) + 
    s(avalanche_report, bs = "ps", k = 5) +
    holiday,
  data = data,
  family = binomial(link = "logit"))

#anschauen

par(mfrow=c(2,2))


summary(day_model)

gam.check(day_model, type = "deviance")

plot(dayasds_model, pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0)
