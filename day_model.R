# time wird als Zahl umcodiert

min_data$num_time <- as.numeric(min_data$time)

## Modell

# day-model fitten (ohne autocorrelation)

day_model <- gam(
  cbind(lvs_true_min, lvs_false_min) ~ s(int_date, num_time, bs = "tp", k = 40) +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    s(res_temperature, bs = "ps", k = 15) +
    s(res_solar_radiation, bs = "ps", k = 15) +
    s(res_snowhight, bs = "ps", k = 15) +
    holiday,
  data = min_data,
  method = "REML",
  family = binomial(link = "logit"))

# day_model2 (mit autocorrelation)

day_model2 <- gamm(
  cbind(lvs_true_min, lvs_false_min) ~ s(int_date, num_time, bs = "tp", k = 40) +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    s(res_temperature, bs = "ps", k = 15) +
    s(res_solar_radiation, bs = "ps", k = 15) +
    s(res_snowhight, bs = "ps", k = 15) +
    holiday,
  data = min_data_noNA,
  correlation = corAR1(form = ~ int_date*num_time),
  method = "REML",
  family = binomial(link = "logit"))

## Untersuchen

par(mfrow=c(2,2))

#anschauen

day_model
summary.gam(day_model, dispersion = day_model$deviance/day_model$df.residual)
# use plogis() to convert to a probability

gam.check(day_model)

#concurvity(day_model, full = TRUE)
#concurvity(day_model, full = FALSE)
#acf(day_model$residuals)
#pacf(day_model$residuals)

# ROC Kurve
#plot.roc(data_noNA$lvs, day_model$fitted.values) # setting levels?

AIC(day_model)

# als gamViz speichern

day_Viz <- getViz(day_model)

print(plot(day_Viz, shade = TRUE, seWithMean = TRUE,
           shift = coef(day_model)[1], trans = plogis) + ylim(0,1), pages = 1)
plot(sm(day_Viz, select = 1), trans = plogis) + l_fitRaster() + l_rug()

# plot(day_model, 
#      pages = 1, residuals = FALSE, pch = 19, cex = .3, scale = 0, 
#      shade = TRUE, seWithMean = TRUE, shift = coef(day_model)[1],
#      trans = plogis)
# plot(sm(day_Viz, 6), trans = plogis) +
#   l_fitRaster() + l_fitContour() + l_points()
