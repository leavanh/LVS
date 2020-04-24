## Modell


date_model <- gam(
  cbind(lvs_true, lvs_false) ~ s(snow_diff, bs = "ps", k = 10) +
    s(int_date, bs = "ps", k = 25) + 
    s(avalanche_report, bs = "ps", k = 5) +
    s(int_day, bs = "cp", k = 7) +
    holiday + int_solar_radiation + int_temperature,
  data = date_data,
  method = "REML",
  family = binomial(link = "logit"))

# anschauen

par(mfrow=c(2,2))

date_model
summary.gam(date_model, dispersion = date_model$deviance/date_model$df.residual)
# use plogis() to convert to a probability

gam.check(date_model, type = "deviance")
concurvity(date_model, full = TRUE)
concurvity(date_model, full = FALSE)
# -> date und snowhight
acf(date_model$residuals)
pacf(date_model$residuals)

# plotten

plot(date_model, 
     pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0, 
     shade = TRUE, seWithMean = TRUE, shift = coef(date_model)[1],
     trans = plogis)

