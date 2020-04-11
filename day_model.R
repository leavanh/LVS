# date, time, day, day_length wird umcodiert als Zahl

data$int_date <- as.integer(as.Date(data$date, format = "%d/%m/%Y"))
data$num_time <- as.numeric(data$time)
data$int_day <- as.integer(data$day)
data$num_day_length <- as.numeric(data$day_length)

# Zeit als Zeit seit Sonnenaufgang und bis Sonnenuntergang

data$t_since_sunrise <- as.numeric(data$time - data$sunrise)

## Modell

day_model <- gam(
  as.numeric(lvs) ~ s(temperature, bs = "ps") +
    s(snow_diff, bs = "ps") + 
    s(solar_radiation, bs = "ps") +
    s(avalanche_report, bs = "ps", k = 5) +
    s(t_since_sunrise, bs = "ps") +
    s(int_date, bs = "ps") +
    day_weekend +
    holiday + 
    position,
  data = data,
  family = binomial(link = "logit"))

#anschauen

par(mfrow=c(2,2))


summary(day_model)
# use plogis() to convert to a probability

gam.check(day_model)
concurvity(day_model, full = TRUE)
concurvity(day_model, full = FALSE)
acf(day_model$residuals)
pacf(day_model$residuals)

plot(day_model, 
     pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0, 
     shade = TRUE, seWithMean = TRUE, shift = coef(date_model)[1],
     trans = plogis)
