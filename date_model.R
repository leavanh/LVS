# date wird umcodiert als Zahl

date_data$int_date <- as.integer(as.Date(date_data$date, format = "%d/%m/%Y"))

# day wird umcodiert als Zahl

date_data$int_day <- as.integer(date_data$day)


model_3 <- gam(
  cbind(lvs_true, lvs_false) ~ s(temperature, bs = "ps") + 
    s(snowhight, bs = "ps") + 
    s(solar_radiation, bs = "ps") + 
    s(int_date, bs = "ps") + 
    avalanche_report + day_weekend + holiday,
  data = date_data,
  family = binomial(link = "logit"))