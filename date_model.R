# date wird umcodiert als Zahl

date_data$int_date <- as.integer(as.Date(date_data$date, format = "%d/%m/%Y"))

# day wird umcodiert als Zahl

date_data$int_day <- as.integer(date_data$day)

## Modell


date_model <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") +
    s(snow_diff, bs = "ps") + 
    s(solar_radiation, bs = "ps") +
    s(int_date, bs = "ps") + 
    s(avalanche_report, bs = "ps", k = 5) +
    holiday + day_weekend,
  data = date_data,
  method = "REML",
  family = binomial(link = "logit"))

#anschauen

par(mfrow=c(2,2))


summary.gam(date_model, dispersion = date_model$deviance/date_model$df.residual)
# use plogis() to convert to a probability

gam.check(date_model, type = "deviance")
concurvity(date_model, full = TRUE)
concurvity(date_model, full = FALSE)
# -> date und snowhight
acf(date_model$residuals)
pacf(date_model$residuals)

plot(date_model, 
     pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0, 
     shade = TRUE, seWithMean = TRUE, shift = coef(date_model)[1],
     trans = plogis)

