# time wird als Zahl umcodiert

min_data$num_time <- as.numeric(min_data$time)
min_data_noNA$num_time <- as.numeric(min_data_noNA$time)

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

min_data_noNA_sample <- min_data_noNA[sample(nrow(min_data_noNA), 1000), ]

day_model2 <- gamm(
  cbind(lvs_true_min, lvs_false_min) ~ s(int_date, num_time, bs = "tp", k = 40) +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    s(res_temperature, bs = "ps", k = 15) +
    s(res_solar_radiation, bs = "ps", k = 15) +
    s(res_snowhight, bs = "ps", k = 15) +
    holiday,
  data = min_data_noNA,
  correlation = corCAR1(0.138, form = ~ int_date*num_time),
  method = "REML",
  family = binomial(link = "logit"))

saveRDS(day_model2, file = "day_model2.RDS")
## Untersuchen

par(mfrow=c(2,2))

#anschauen

day_model

summary.gam(day_model, dispersion = day_model$deviance/day_model$df.residual)
summary.gam(day_model2$gam)
# use plogis() to convert to a probability

gam.check(day_model)
gam.check(day_model2$gam)
# plot(day_model2$gam$linear.predictors, day_model2$lme$residuals[, "fixed"])

#concurvity(day_model, full = TRUE)
#concurvity(day_model, full = FALSE)
acf(day_model$residuals)
pacf(day_model$residuals)
acf(day_model2$lme$residuals[, "fixed"])
pacf(day_model2$lme$residuals[, "fixed"])

# ROC Kurve
#plot.roc(data_noNA$lvs, day_model$fitted.values) # setting levels?

AIC(day_model)

# als gamViz speichern

day_Viz <- getViz(day_model)
day_Viz2 <- getViz(day_model2$gam)

print(plot(day_Viz, shade = TRUE, seWithMean = TRUE,
           shift = coef(day_model)[1], trans = plogis) + ylim(0,1), pages = 1)

plot(sm(day_Viz, select = 1), trans = plogis)  + labs(y="Uhrzeit", x="Datum") + l_fitRaster() + l_rug() +
  scale_x_continuous(breaks=c(17910,17940,17970,18000), labels=c("14-01-2019","13-02-2019","15-03-2019","14-04-2019")) +
  scale_y_continuous(breaks=c(-2209060800,-2209050000,-2209039200,-2209028400, -2209017600, -2209006800,
                              -2208996000, -2208985200, -2208974460), 
                     labels=c("04:00","07:00","10:00","13:00", "16:00", "19:00",
                              "22:00", "01:00", "03:59")) +
  ggtitle("Smoothfunktion für Datum und Uhrzeit")

print(plot(day_Viz2, shade = TRUE, seWithMean = TRUE,
           shift = coef(day_model)[1], trans = plogis) + ylim(0,1), pages = 1)
plot(sm(day_Viz2, select = 1), trans = plogis) + l_fitRaster() + l_rug()

# plot(day_model, 
#      pages = 1, residuals = FALSE, pch = 19, cex = .3, scale = 0, 
#      shade = TRUE, seWithMean = TRUE, shift = coef(day_model)[1],
#      trans = plogis)
# plot(sm(day_Viz, 6), trans = plogis) +
#   l_fitRaster() + l_fitContour() + l_points()
