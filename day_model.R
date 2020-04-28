# time wird als Zahl umcodiert

data$num_time <- as.numeric(data$time)

## Modell

# day-model fitten (ohne autocorrelation)

day_model <- gam(
  as.numeric(lvs) ~ s(int_date, num_time, bs = "tp", k = 40) +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    s(res_temperature, bs = "ps", k = 15) +
    s(res_solar_radiation, bs = "ps", k = 15) +
    s(res_snowhight, bs = "ps", k = 15) +
    holiday,
  data = data,
  method = "REML",
  family = binomial(link = "logit"))

# day_model2 (mit autocorrelation)

day_model2 <- gamm(
  as.numeric(lvs) ~ s(int_date, num_time, bs = "tp", k = 40) +
    s(snow_diff, bs = "ps", k = 15) +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    holiday + int_solar_radiation + int_temperature,
  data = data,
  correlation = corAR1(form = ~ int_date*num_time),
  method = "REML",
  family = binomial(link = "logit"))

## Untersuchen

#anschauen

day_model
summary(day_model)
# use plogis() to convert to a probability

gam.check(day_model)

#concurvity(day_model, full = TRUE)
#concurvity(day_model, full = FALSE)
#acf(day_model$residuals)
#pacf(day_model$residuals)

# ROC Kurve
plot.roc(data_noNA$lvs, day_model$fitted.values) # setting levels?

AIC(day_model)

# als gamViz speichern

day_Viz <- getViz(day_model)

print(plot(day_Viz, shade = TRUE, seWithMean = TRUE,
           shift = coef(day_model)[1], trans = plogis) + ylim(0,1), pages = 1)

plot(sm(day_Viz, select = 1), trans = plogis)  + labs(y="Uhrzeit", x="Datum") + l_fitRaster() + l_rug() +
  scale_x_continuous(breaks=c(17910,17940,17970,18000), labels=c("14-01-2019","13-02-2019","15-03-2019","14-04-2019")) +
  scale_y_continuous(breaks=c(-2209060800,-2209050000,-2209039200,-2209028400, -2209017600, -2209006800,
                              -2208996000, -2208985200, -2208974460), 
                     labels=c("04:00","07:00","10:00","13:00", "16:00", "19:00",
                              "22:00", "01:00", "03:59")) +
  ggtitle("Smoothfunktion für Datum und Uhrzeit")

# plot(day_model, 
#      pages = 1, residuals = FALSE, pch = 19, cex = .3, scale = 0, 
#      shade = TRUE, seWithMean = TRUE, shift = coef(day_model)[1],
#      trans = plogis)
# plot(sm(day_Viz, 6), trans = plogis) +
#   l_fitRaster() + l_fitContour() + l_points()
