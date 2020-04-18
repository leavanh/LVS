# date, time, day, day_length wird umcodiert als Zahl

data$int_date <- as.integer(as.Date(data$date, format = "%d/%m/%Y"))
data$num_time <- as.numeric(data$time)
data$int_day <- as.integer(data$day)
data$num_day_length <- as.numeric(data$day_length)

## Modell

day_model <- gam(
  as.numeric(lvs) ~ s(temperature, bs = "ps") +
    s(snow_diff, bs = "ps") + 
    s(solar_radiation, bs = "ps") +
    s(int_day, bs = "cp", k = 7) +
    s(avalanche_report, bs = "ps", k = 5) +
    #te(int_date, num_time, bs = "ps") +
    s(int_date, num_time) +
    holiday,
  data = data,
  family = binomial(link = "logit"))

## Untersuchen

# als gamViz speichern

day_Viz <- getViz(day_model)

#anschauen

summary(day_model)
# use plogis() to convert to a probability

#concurvity(day_model, full = TRUE)
#concurvity(day_model, full = FALSE)
#acf(day_model$residuals)
#pacf(day_model$residuals)

# ROC Kurve
plot.roc(data_noNA$lvs, day_model$fitted.values)

AIC(day_model)

print(plot(day_Viz, shade = TRUE, seWithMean = TRUE,
           shift = coef(day_model)[1], trans = plogis) + ylim(0,1), pages = 1)
plot(sm(day_Viz, select = 6), trans = plogis) + l_fitRaster() + l_rug()

# plot(day_model, 
#      pages = 1, residuals = FALSE, pch = 19, cex = .3, scale = 0, 
#      shade = TRUE, seWithMean = TRUE, shift = coef(day_model)[1],
#      trans = plogis)
# plot(sm(day_Viz, 6), trans = plogis) +
#   l_fitRaster() + l_fitContour() + l_points()
